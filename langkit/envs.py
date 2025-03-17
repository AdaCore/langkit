"""
This modules provides an API to specify the lexical environments to build for
each node ("env specs").

See the "Lexical environment" section in Langkit's Sphinx doc for an
introduction to their usage.
"""

from __future__ import annotations

import abc
from enum import Enum
from funcy import lsplit_by
from itertools import count
from typing import Type, cast

from langkit import names
from langkit.compile_context import CompileCtx
from langkit.compiled_types import ASTNodeType, T
from langkit.diagnostics import Location, check_source_language, error
from langkit.expressions import FieldAccess, LocalVars, PropertyDef


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

        # TODO (eng/libadalang/langkit#880): Get rid of this dummy local vars
        # business once abstract expressions are gone: we will be able to have
        # a simple VariableExpr for node_var.
        local_vars = LocalVars()
        self.node_var = local_vars.create(
            location=Location.builtin,
            codegen_name="Self",
            type=self.owner,
            manual_decl=True,
            scope=local_vars.root_scope,
        )

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

        return FieldAccess.Expr(
            None, self.node_var.ref_expr, p, []
        ).render_expr()

    @property
    def initial_env_expr(self) -> str:
        """
        The initial environment expression.
        """
        assert self.initial_env
        return self._render_field_access(self.initial_env.env_prop)


class EnvAction:

    def __init__(self, context: CompileCtx, location: Location) -> None:
        self.context = context
        self.location = location

    @property
    def str_location(self) -> str:
        return self.location.gnu_style_repr()

    @property
    def resolvers(self) -> list[PropertyDef]:
        """
        Return the list of properties used by this env action.
        """
        return [p for p in self._resolvers if p]

    @abc.abstractproperty
    def _resolvers(self) -> list[PropertyDef | None]:
        """
        Actual implementation for the ``resolvers`` property. Can return None
        items for convenience: ``resolvers`` filters them out.
        """
        ...

    def check(self) -> None:
        """
        Check that the env action is legal.
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
        context: CompileCtx,
        location: Location,
        no_parent: bool,
        transitive_parent: PropertyDef | None,
        names: PropertyDef | None,
    ):
        super().__init__(context, location)
        self.no_parent = no_parent
        self.transitive_parent_prop = transitive_parent
        self.names_prop = names

    @property
    def _resolvers(self) -> list[PropertyDef | None]:
        return [self.transitive_parent_prop, self.names_prop]


class AddToEnv(EnvAction):

    def __init__(
        self,
        context: CompileCtx,
        location: Location,
        mappings: PropertyDef,
        resolver: PropertyDef | None,
    ):
        super().__init__(context, location)
        self.mappings_prop = mappings
        self.resolver = resolver

    @property
    def _resolvers(self) -> list[PropertyDef | None]:
        return [self.mappings_prop, self.resolver]

    def rewrite_property_refs(self,
                              mapping: dict[PropertyDef, PropertyDef]) -> None:
        if self.resolver is not None:
            self.resolver = mapping.get(self.resolver, self.resolver)

    def check(self) -> None:
        location = self.mappings_prop.location
        mapping_type = self.mappings_prop.type
        if mapping_type.matches(T.EnvAssoc):
            self.context.has_env_assoc = True
        elif mapping_type.matches(T.EnvAssoc.array):
            self.context.has_env_assoc = True
            self.context.has_env_assoc_array = True
        else:
            error(
                "The bindings expression in environment specification must"
                " must be either an env_assoc or an array of env_assocs:"
                f" got {mapping_type.dsl_name} instead",
                location=location,
            )

        if self.resolver:
            # Ask for the creation of untyped wrappers for all
            # properties used as entity resolvers.
            self.resolver.require_untyped_wrapper()

            check_source_language(
                self.resolver.type.matches(T.entity),
                "Entity resolver properties must return entities (got"
                f" {self.resolver.type.dsl_name})",
                location=location,
            )
            check_source_language(
                not self.resolver.dynamic_var_args,
                "Entity resolver properties must have no dynamically bound"
                " variable",
                location=location,
            )
            check_source_language(
                not self.resolver.natural_arguments,
                "Entity resolver properties must have no argument",
                location=location,
            )


class RefEnvs(EnvAction):
    """
    Couple of a property and an expression to evaluate referenced envs.
    """

    def __init__(
        self,
        context: CompileCtx,
        location: Location,
        resolver: PropertyDef,
        nodes_expr: PropertyDef,
        kind: RefKind,
        dest_env: PropertyDef | None,
        cond: PropertyDef | None,
        category: str | None,
        shed_rebindings: bool,
    ):
        """
        All nodes that nodes_expr yields must belong to the same analysis unit
        as the AST node that triggers this RefEnvs. Besides, the lexical
        environment to which these referenced environments are added must also
        belong to the same analysis unit. Attempts to add referenced
        environments that do not respect these rules will trigger a
        Property_Error.

        :param resolver: Property that takes no argument (explicit or implicit)
            appart from Self, and that returns a lexical environment.

        :param nodes_expr: Property that returns an array of AST nodes. Each
            node will be given to the above resolver in order to get
            corresponding referenced lexical envs.  In this array, null nodes
            are allowed: they are simply discarded.

        :param kind: Kind of reference.

        :param dest_env: Optional expression designating the destination env.
        """
        assert resolver
        assert nodes_expr

        super().__init__(context, location)

        self.resolver = resolver
        self.nodes_property = nodes_expr
        self.kind = kind
        self.dest_env_prop = dest_env
        self.cond_prop = cond
        self.category = category and names.Name.from_lower(category)
        self.shed_rebindings = shed_rebindings

    @property
    def _resolvers(self) -> list[PropertyDef | None]:
        return [
            self.resolver,
            self.nodes_property,
            self.dest_env_prop,
            self.cond_prop,
        ]

    def check(self) -> None:
        """
        Check that the resolver property is conforming.
        """
        self.context.has_ref_env = True

        self.resolver.require_untyped_wrapper()

        check_source_language(
            self.resolver.type.matches(T.LexicalEnv),
            'Referenced environment resolver must return a lexical'
            ' environment (not {})'.format(
                self.resolver.type.dsl_name
            ),
            location=self.location,
        )
        check_source_language(
            not self.resolver.natural_arguments,
            'Referenced environment resolver must take no argument',
            location=self.location,
        )
        check_source_language(
            not self.resolver.dynamic_var_args,
            'Referenced environment resolver must have no dynamically bound'
            ' variable',
            location=self.location,
        )

    def rewrite_property_refs(self,
                              mapping: dict[PropertyDef, PropertyDef]) -> None:
        self.resolver = mapping.get(self.resolver, self.resolver)


class HandleChildren(EnvAction):
    """
    Stub class to delimit pre and post env actions.
    """
    @property
    def _resolvers(self) -> list[PropertyDef | None]:
        return []


class SetInitialEnv(EnvAction):
    def __init__(
        self,
        context: CompileCtx,
        location: Location,
        env_expr: PropertyDef,
    ):
        super().__init__(context, location)
        self.env_prop = env_expr

    @property
    def _resolvers(self) -> list[PropertyDef | None]:
        return [self.env_prop]


class Do(EnvAction):
    def __init__(
        self,
        context: CompileCtx,
        location: Location,
        expr: PropertyDef,
    ):
        super().__init__(context, location)
        self.do_property = expr

    @property
    def _resolvers(self) -> list[PropertyDef | None]:
        return [self.do_property]
