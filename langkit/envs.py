"""
This modules provides an API to specify the lexical environments to build for
each node ("env specs").

See the "Lexical environment" section in Langkit's Sphinx doc for an
introduction to their usage.
"""

from __future__ import annotations

import dataclasses
from enum import Enum
from funcy import lsplit_by
from itertools import count
from typing import Type, cast, overload

from langkit import names
from langkit.compile_context import CompileCtx, get_context
from langkit.compiled_types import (
    ASTNodeType,
    CompiledType,
    MemberNames,
    T,
    TypeRepo,
)
from langkit.diagnostics import Location, check_source_language, error
from langkit.expressions import (
    AbstractExpression,
    FieldAccess,
    Literal,
    NodeVariable,
    PropertyDef,
    construct,
    resolve_property,
)


class RefKind(Enum):
    """
    Represents the kind of a referenced env. Here are the different kinds:

    * transitive: The reference is transitive, e.g. it will be explored in
      every case (whether the lookup is recursive or not). It will be explored
      *before* parent environments.

    * prioritary: The reference is non transitive, e.g. it will be explored
      only if the lookup on the env is recursive. It will be explored *before*
      parent environments.

    * normal: The reference is non transitive, e.g. it will be explored only
      if the lookup on the env is recursive. It will be explored *after*
      parent environments.
    """
    transitive = "Transitive"
    prioritary = "Prioritary"
    normal = "Normal"


class EnvSpec:
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

    PROPERTY_COUNT = count(0)

    def __init__(
        self,
        owner: ASTNodeType,
        location: Location,
        *actions: EnvAction,
    ) -> None:
        """
        :param owner: Node type associated to this environment specification.
        :param location: Source location for this env spec.
        :param actions: A list of environment actions to execute.
        """
        self.location = location
        self.owner = owner
        self.node_var = NodeVariable(owner)

        self.initial_env: SetInitialEnv | None = None
        """
        The SetInitialEnv action associated to this EnvSpec, if any.
        Initialized during the parsing of actions.
        """

        # Analyze the given list of actions
        self._parse_actions(list(actions))
        self.pre_actions: list[EnvAction]
        self.post_actions: list[EnvAction]
        self.actions: list[EnvAction]

        self.adds_env = any(isinstance(a, AddEnv) for a in self.pre_actions)
        """
        Whether this spec create an environment.
        """

        self.properties_created = False
        """
        When types are defined in Lkt sources, properties are created during
        types lowering: no need to do it in the "create_properties" method.
        """

    def _parse_actions(self, actions: list[EnvAction]) -> None:
        """
        Analyze the given list of actions and extract pre/post actions, i.e.
        actions executed before and after handling children.
        """
        def filter(
            cls: Type[EnvAction],
            sequence: list[EnvAction],
        ) -> list[EnvAction]:
            """
            Return the number of ``cls`` instances in ``sequence``.
            """
            return [a for a in sequence if isinstance(a, cls)]

        # If present, allow Do actions to come before SetInitialEnv
        first_actions = []
        if any(isinstance(a, SetInitialEnv) for a in actions):
            while actions and isinstance(actions[0], Do):
                first_actions.append(actions.pop(0))

        # After that, allow at most one call to SetInitialEnv
        self.initial_env = None
        if actions and isinstance(actions[0], SetInitialEnv):
            check_source_language(
                isinstance(actions[0], SetInitialEnv),
                'The initial environment must come first after the potential'
                ' do()',
                location=actions[0].location,
            )
            self.initial_env = cast(SetInitialEnv, actions.pop(0))
            first_actions.append(self.initial_env)

        spurious_sie = filter(SetInitialEnv, actions)
        if spurious_sie:
            error(
                "set_initial_env can only be preceded by do()",
                location=spurious_sie[0].location,
            )

        add_envs = filter(AddEnv, actions)
        if len(add_envs) > 1:
            error(
                "There can be at most one call to add_env()",
                location=add_envs[1].location,
            )

        # Separate actions that must occur before and after the handling of
        # children. Get also rid of the HandleChildren delimiter action.
        pre, post = lsplit_by(lambda a: not isinstance(a, HandleChildren),
                              actions)
        post = post and post[1:]

        post_add_envs = filter(AddEnv, post)
        if post_add_envs:
            error(
                'add_env() must occur before processing children',
                location=post_add_envs[0].location,
            )

        self.pre_actions = first_actions + pre
        self.post_actions = post
        self.actions = self.pre_actions + self.post_actions

    @overload
    def create_internal_property(self,
                                 name: str,
                                 expr: None,
                                 type: CompiledType | None) -> None: ...

    @overload
    def create_internal_property(
        self,
        name: str,
        expr: AbstractExpression,
        type: CompiledType | None,
    ) -> PropertyDef: ...

    def create_internal_property(
        self,
        name: str,
        expr: AbstractExpression | None,
        type: CompiledType | None,
    ) -> PropertyDef | None:
        """
        Create an internal property for this env spec.

        If ``expr`` is None, do not create a property and return None.

        :param name: Lower-case name to use to create this property name.
            Since the property is internal, the name is decorated.
        """
        if expr is None:
            return None

        p = PropertyDef(
            self.owner,
            MemberNames.for_internal(name),
            Location.builtin,
            expr,
            public=False,
            type=type,
        )
        p.location = getattr(expr, 'location') or self.location
        return p

    def create_properties(self, context: CompileCtx) -> None:
        """
        Turn the various abstract expression attributes for this env spec into
        internal properties and add them to `astnode`.
        """
        if not self.properties_created:
            for action in self.actions:
                action.create_internal_properties(self)

    def register_categories(self, context: CompileCtx) -> None:
        """
        Compilation pass to register all category names from RefEnvs actions.
        """
        for action in self.actions:
            if isinstance(action, RefEnvs) and action.category:
                low_name = action.category.lower
                check_source_language(
                    low_name not in ('nocat', 'default'),
                    '{} is not a valid name for a referenced env category'
                    .format(low_name),
                    location=action.location,
                )
                context.ref_cats.add(action.category)

    def check_spec(self, context: CompileCtx) -> None:
        """
        ASTNode pass which checks that properties generated by the env spec are
        conforming. This relies on type information and property attributes
        (privacy, implicit envs), so it must run only after these can be
        computed.
        """
        has_add_env = False

        for action in self.actions:
            action.check()

            if isinstance(action, AddEnv):
                check_source_language(
                    not has_add_env,
                    "There can be only one add_env action per EnvSpec",
                    location=action.location,
                )
                has_add_env = True

    def _render_field_access(self, p: PropertyDef) -> str:
        """
        Helper to render a simple field access to the property P in the context
        of an environment specification.

        :param p: The property to access. It must accept no explicit argument.
        """
        assert not p.natural_arguments

        with PropertyDef.bind_none():
            return FieldAccess.Expr(
                construct(self.node_var), p, []
            ).render_expr()

    @property
    def initial_env_expr(self) -> str:
        """
        The initial environment expression.
        """
        assert self.initial_env
        return self._render_field_access(self.initial_env.env_prop)


class EnvAction:

    resolver: PropertyDef | TypeRepo.Defer | None = None
    """
    Some env actions use resolvers, that are property that will yield a lexical
    environment from a Node. To facilitate accessing it in general, we'll set a
    class attribute to None on the base class.
    """

    def __init__(self, location: Location) -> None:
        self.location = location

    @property
    def str_location(self) -> str:
        return ('unknown location'
                if self.location is None else
                self.location.gnu_style_repr())

    def check(self) -> None:
        """
        Check that the env action is legal.
        """
        pass

    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        """
        Create properties needed for the emission of this env action.
        """
        pass

    def rewrite_property_refs(self,
                              mapping: dict[PropertyDef, PropertyDef]) -> None:
        """
        Rewrite `PropertyDef` references according to `mapping`. See
        CompileCtx.lower_properties_dispatching.

        :param mapping: PropertyDef reference substitution mapping.
        """
        pass


class AddEnv(EnvAction):

    def __init__(
        self,
        location: Location,
        no_parent: bool = False,
        transitive_parent: AbstractExpression | PropertyDef | None = None,
        names: AbstractExpression | PropertyDef | None = None,
    ) -> None:
        super().__init__(location)
        self.no_parent = no_parent
        if isinstance(transitive_parent, PropertyDef):
            self.transitive_parent_prop: PropertyDef | None = transitive_parent
        else:
            self.transitive_parent_prop = None
            self.transitive_parent = (
                transitive_parent or Literal(Location.builtin, False)
            )
        if isinstance(names, PropertyDef):
            self.names_prop: PropertyDef | None = names
        else:
            self.names_prop = None
            self.names = names

    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        self.transitive_parent_prop = env_spec.create_internal_property(
            'env_trans_parent', self.transitive_parent, T.Bool
        )
        self.names_prop = env_spec.create_internal_property(
            'env_names', self.names, T.Symbol.array
        )


class AddToEnv(EnvAction):

    @dataclasses.dataclass
    class KVParams:
        """
        Arguments for the "add_to_env_kv()" action constructor.
        """
        key: AbstractExpression
        value: AbstractExpression
        dest_env: AbstractExpression | None
        metadata: AbstractExpression | None
        resolver: PropertyDef | None

    def __init__(
        self,
        location: Location,
        mappings: AbstractExpression | PropertyDef,
        resolver: PropertyDef | TypeRepo.Defer | None,
    ) -> None:
        super().__init__(location)
        if isinstance(mappings, PropertyDef):
            self.mappings_prop: PropertyDef = mappings
        else:
            self.mappings = mappings
        self.resolver = resolver

        self.kv_params: AddToEnv.KVParams | None = None

    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        self.mappings_prop = env_spec.create_internal_property(
            'env_mappings', self.mappings, None
        )

    def rewrite_property_refs(self,
                              mapping: dict[PropertyDef, PropertyDef]) -> None:
        if self.resolver is not None:
            resolver = resolve_property(self.resolver)
            self.resolver = mapping.get(resolver, resolver)

    def check(self) -> None:
        ctx = get_context()
        resolver = self.resolver = resolve_property(self.resolver)
        location = self.mappings_prop.location
        mapping_type = self.mappings_prop.type
        if mapping_type.matches(T.EnvAssoc):
            ctx.has_env_assoc = True
        elif mapping_type.matches(T.EnvAssoc.array):
            ctx.has_env_assoc = True
            ctx.has_env_assoc_array = True
        else:
            error(
                "The bindings expression in environment specification must"
                " must be either an env_assoc or an array of env_assocs:"
                f" got {mapping_type.dsl_name} instead",
                location=location,
            )

        if resolver:
            # Ask for the creation of untyped wrappers for all
            # properties used as entity resolvers.
            resolver.require_untyped_wrapper()

            check_source_language(
                resolver.type.matches(T.entity),
                "Entity resolver properties must return entities (got"
                f" {resolver.type.dsl_name})",
                location=location,
            )
            check_source_language(
                not resolver.dynamic_vars,
                "Entity resolver properties must have no dynamically bound"
                " variable",
                location=location,
            )
            check_source_language(
                not resolver.natural_arguments,
                "Entity resolver properties must have no argument",
                location=location,
            )


class RefEnvs(EnvAction):
    """
    Couple of a property and an expression to evaluate referenced envs.
    """

    def __init__(
        self,
        location: Location,
        resolver: PropertyDef | TypeRepo.Defer,
        nodes_expr: AbstractExpression | PropertyDef,
        kind: RefKind = RefKind.normal,
        dest_env: AbstractExpression | PropertyDef | None = None,
        cond: AbstractExpression | PropertyDef | None = None,
        category: str | None = None,
        shed_rebindings: bool = False,
    ) -> None:
        """
        All nodes that nodes_expr yields must belong to the same analysis unit
        as the AST node that triggers this RefEnvs. Besides, the lexical
        environment to which these referenced environments are added must also
        belong to the same analysis unit. Attempts to add referenced
        environments that do not respect these rules will trigger a
        Property_Error.

        :param resolver: Property that takes no argument (explicit or implicit)
            appart from Self, and that returns a lexical environment.

        :param nodes_expr: Abstract expression that returns an array of AST
            nodes. Each node will be given to the above resolver in order to
            get corresponding referenced lexical envs.  In this array, null
            nodes are allowed: they are simply discarded.

        :param kind: Kind of reference.

        :param dest_env: Optional expression designating the destination env.
        """
        assert resolver
        assert nodes_expr

        super().__init__(location)

        self.resolver: PropertyDef | TypeRepo.Defer = resolver
        if isinstance(nodes_expr, PropertyDef):
            self.nodes_property = nodes_expr
        else:
            self.nodes_expr = nodes_expr
        self.kind = kind
        if isinstance(dest_env, PropertyDef):
            self.dest_env_prop: PropertyDef | None = dest_env
        else:
            self.dest_env_prop = None
            self.dest_env = dest_env
        if isinstance(cond, PropertyDef):
            self.cond_prop: PropertyDef | None = cond
        else:
            self.cond_prop = None
            self.cond = cond
        self.category = category and names.Name.from_lower(category)
        self.shed_rebindings = shed_rebindings

    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        """
        Create the property that returns the list of nodes to resolve into
        referenced lexical envs.
        """
        self.nodes_property = env_spec.create_internal_property(
            'ref_env_nodes', self.nodes_expr, T.root_node.array
        )

        self.dest_env_prop = env_spec.create_internal_property(
            'env_dest', self.dest_env, T.LexicalEnv
        )

        self.cond_prop = env_spec.create_internal_property(
            'ref_cond', self.cond, T.Bool
        )

    def check(self) -> None:
        """
        Check that the resolver property is conforming.
        """
        ctx = get_context()
        ctx.has_ref_env = True

        self.resolver = resolver = resolve_property(self.resolver)
        resolver.require_untyped_wrapper()

        check_source_language(
            resolver.type.matches(T.LexicalEnv),
            'Referenced environment resolver must return a lexical'
            ' environment (not {})'.format(
                resolver.type.dsl_name
            ),
            location=self.location,
        )
        check_source_language(
            not resolver.natural_arguments,
            'Referenced environment resolver must take no argument',
            location=self.location,
        )
        check_source_language(
            not resolver.dynamic_vars,
            'Referenced environment resolver must have no dynamically bound'
            ' variable',
            location=self.location,
        )

    def rewrite_property_refs(self,
                              mapping: dict[PropertyDef, PropertyDef]) -> None:
        resolver = resolve_property(self.resolver)
        self.resolver = mapping.get(resolver, resolver)


class HandleChildren(EnvAction):
    """
    Stub class to delimit pre and post env actions.
    """
    pass


class SetInitialEnv(EnvAction):
    def __init__(
        self,
        location: Location,
        env_expr: AbstractExpression | PropertyDef,
    ):
        super().__init__(location)
        if isinstance(env_expr, PropertyDef):
            self.env_prop = env_expr
        else:
            self.env_expr = env_expr

    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        self.env_prop = env_spec.create_internal_property(
            'initial_env', self.env_expr, T.DesignatedEnv
        )


class Do(EnvAction):
    def __init__(
        self,
        location: Location,
        expr: AbstractExpression | PropertyDef,
    ) -> None:
        super().__init__(location)
        if isinstance(expr, PropertyDef):
            self.do_property = expr
        else:
            self.expr = expr

    def create_internal_properties(self, spec: EnvSpec) -> None:
        self.do_property = spec.create_internal_property(
            'env_do', self.expr, None
        )
