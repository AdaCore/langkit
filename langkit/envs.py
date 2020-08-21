"""
This module contains the public API and the implementation for lexical
environments specifications.

At a high level, the user adds environment specifications to node types the
following way::


    class MyNode(RootNode):
        ...

        env_spec = EnvSpec([
            list_of_actions
        ], other_options)


Read the documentation below for more details.
"""

from __future__ import annotations

from enum import Enum
from funcy import lsplit_by
from itertools import count
from typing import Dict, List, Optional, cast, overload

from langkit import names
from langkit.compile_context import CompileCtx, get_context
from langkit.compiled_types import (ASTNodeType, AbstractNodeData,
                                    CompiledType, T)
from langkit.diagnostics import (Context, check_source_language,
                                 extract_library_location)
from langkit.expressions import (AbstractExpression, FieldAccess, PropertyDef,
                                 Self, resolve_property, unsugar)


# Public API for env actions

def add_env(no_parent: bool = False,
            transitive_parent: bool = False) -> AddEnv:
    """
    Add an environment linked to the current node. This env action must be
    called as a pre action. The only actions that can precede this one in pre
    actions are add_to_env actions with the current env as a destination. Also,
    there can be only one add_env action per EnvSpec.

    :param no_parent: If passed, the new env will be created with no parent
        env.
    """
    return AddEnv(no_parent, transitive_parent)


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


def reference(nodes: AbstractExpression,
              through: PropertyDef,
              kind: RefKind = RefKind.normal,
              dest_env: Optional[AbstractExpression] = None,
              cond: Optional[AbstractExpression] = None,
              category: Optional[str] = None,
              shed_corresponding_rebindings: bool = False,
              unsound: bool = False) -> RefEnvs:
    """
    Reference a group of lexical environments, that will be lazily yielded by
    calling the `through` property on the array of nodes `nodes`.

    :param nodes: An expression that yields a list of nodes.
    :param through: A property reference.

    :param kind: Kind of reference.
    :param dest_env: If passed, the destination environment for this reference.
    :param cond: If passed, an expression evaluating to a boolean condition. If
        False, reference won't be made and the others expressions won't be
        evaluated.

    :param category: If passed, must be a string representing a category name.
        String must represent a valid Ada name. A category in set of possible
        referenced envs categories will be implicitly created for each unique
        string passed to a call to reference, in a given compilation context.

    :param shed_corresponding_rebindings: If True, when shedding rebindings
        during an env lookup, this referenced env will be followed to check,
        and eventually shed rebindings associated to the referenced env.

    :param unsound: Whether ``dest_env`` is allowed to return foreign
        environments.
    """
    return RefEnvs(through, nodes, kind, dest_env=dest_env, cond=cond,
                   category=category and category.lower(),
                   shed_rebindings=shed_corresponding_rebindings,
                   unsound=unsound)


def add_to_env(mappings: AbstractExpression,
               resolver: Optional[PropertyDef] = None,
               unsound: bool = False) -> AddToEnv:
    """
    Specify elements to add to the lexical environment.

    :param mappings: One or several mappings of key to value to add to the
        environment. Must be either of type T.env_assoc, or T.env_assoc.array.
        All values must belong to the same unit as the node that owns this
        EnvSpec. See langkit.expressions.envs.new_env_assoc for more precision
        on how to create an env assoc.

    :param resolver: Optional property that returns an AST node. If provided,
        the lexical environment lookup that will try to return the given
        mappings will first run this property on all nodes and return its
        result instead.

    :param unsound: Whether ``dest_env`` is allowed to return foreign
        environments.
    """
    return AddToEnv(mappings, resolver, unsound)


def add_to_env_kv(key: AbstractExpression,
                  val: AbstractExpression,
                  dest_env: Optional[AbstractExpression] = None,
                  metadata: Optional[AbstractExpression] = None,
                  resolver: Optional[PropertyDef] = None,
                  unsound: bool = False) -> AddToEnv:
    """
    Specify a single element to add to the lexical environment. See
    langkit.expressions.envs.new_env_assoc for more precision about the first
    four arguments.

    :param resolver: Optional property that returns an AST node. If provided,
        the lexical environment lookup that will try to return the given
        mappings will first run this property on all nodes and return its
        result instead.

    :param unsound: Whether ``dest_env`` is allowed to return foreign
        environments.
    """
    from langkit.expressions import new_env_assoc

    return add_to_env(
        mappings=new_env_assoc(key, val, dest_env, metadata),
        resolver=resolver,
        unsound=unsound,
    )


def handle_children() -> HandleChildren:
    """
    Handle the node's children lexical environments.
    """
    return HandleChildren()


def set_initial_env(env_expr: AbstractExpression,
                    unsound: bool = False) -> SetInitialEnv:
    """
    Action that sets the initial env in which the rest of the environment
    actions are evaluated. Except for Do() hooks, this action must be first in
    the list of action.

    :param unsound: Whether ``env_expr`` is allowed to return foreign
        environments.
    """
    return SetInitialEnv(env_expr, unsound)


def do(expr: AbstractExpression) -> Do:
    """
    Evaluate given expression for its side effects, discarding its result.
    """
    return Do(expr)


class EnvSpec:
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

    PROPERTY_COUNT = count(0)

    def __init__(self, *actions: EnvAction) -> None:
        """
        :param actions: A list of environment actions to execute.
        """
        self.location = extract_library_location()

        self.ast_node: Optional[ASTNodeType] = None
        """
        ASTNodeType subclass associated to this environment specification.
        Initialized when creating ASTNodeType subclasses.
        """

        self.initial_env: Optional[SetInitialEnv] = None
        """
        The SetInitialEnv action associated to this EnvSpec, if any.
        Initialized during the parsing of actions.
        """

        self.initial_env_prop: Optional[PropertyDef] = None
        """
        Property that returns the initial environment that environment
        actions will use. If left to None, just inherit the parent node's
        ``children_env``.

        Initialized when compiling this EnvSpec.
        """

        # Analyze the given list of actions
        self._parse_actions(list(actions))

        self.adds_env = any(isinstance(a, AddEnv) for a in self.pre_actions)
        """
        Whether this spec create an environment.
        """

    def _parse_actions(self, actions: List[EnvAction]) -> None:
        """
        Analyze the given list of actions and extract pre/post actions, i.e.
        actions executed before and after handling children.
        """
        # If present, allow Do actions to come before SetInitialEnv
        self.pre_initial_env_actions = []
        if any(isinstance(a, SetInitialEnv) for a in actions):
            while actions and isinstance(actions[0], Do):
                self.pre_initial_env_actions.append(actions.pop(0))

        # After that, allow one call to SetInitialEnv
        self.initial_env = None
        if actions and isinstance(actions[0], SetInitialEnv):
            self.initial_env = cast(SetInitialEnv, actions.pop(0))

        pre, post = lsplit_by(
            lambda a: not isinstance(a, HandleChildren), actions
        )

        # Get rid of the HandleChildren delimiter action
        post = post and post[1:]

        self.pre_actions = pre
        self.post_actions = post
        self.actions = self.pre_actions + self.post_actions

    @property
    def diagnostic_context(self) -> Context:
        """
        Diagnostic context for env specs.
        """
        assert self.location is not None
        return Context(self.location)

    @overload
    def create_internal_property(self,
                                 name: str,
                                 expr: None,
                                 type: Optional[CompiledType]) -> None: ...

    @overload
    def create_internal_property(
        self,
        name: str,
        expr: AbstractExpression,
        type: Optional[CompiledType]
    ) -> PropertyDef: ...

    def create_internal_property(
        self,
        name: str,
        expr: Optional[AbstractExpression],
        type: Optional[CompiledType]
    ) -> Optional[PropertyDef]:
        """
        Create an internal property for this env spec.

        If ``expr`` is None, do not create a property and return None.
        Otherwise, unsugar it.

        :param name: Lower-case name to use to create this property name.
            Since the property is internal, the name is decorated.
        """
        assert self.ast_node is not None

        if expr is None:
            return None

        expr = unsugar(expr)
        p = PropertyDef(
            expr, AbstractNodeData.PREFIX_INTERNAL,
            name=names.Name('{}_{}'.format(name, next(self.PROPERTY_COUNT))),
            public=False, type=type, ignore_warn_on_node=True
        )
        p._indexing_name = '_{}'.format(p.original_name.lower)
        p._original_name = names.Name.from_lower(p._indexing_name)
        p.location = getattr(expr, 'location') or self.location
        self.ast_node.add_field(p)
        return p

    def create_properties(self, context: CompileCtx) -> None:
        """
        Turn the various abstract expression attributes for this env spec into
        internal properties and add them to `astnode`.
        """

        for action in self.pre_initial_env_actions:
            action.create_internal_properties(self)

        self.initial_env_prop = self.create_internal_property(
            'Initial_Env',
            None if self.initial_env is None else self.initial_env.env_expr,
            T.LexicalEnv
        )

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
                    .format(low_name)
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

        with self.diagnostic_context:

            for action in self.actions:
                action.check()

                if isinstance(action, AddEnv):
                    check_source_language(not has_add_env,
                                          'There can be only one add_env'
                                          ' action per EnvSpec')
                    has_add_env = True

    def _render_field_access(self, p: PropertyDef) -> str:
        """
        Helper to render a simple field access to the property P in the context
        of an environment specification.

        :param p: The property to access. It must accept no explicit argument.
        """
        assert not p.natural_arguments

        with PropertyDef.bind_none(), \
                Self.bind_type(self.ast_node):
            return FieldAccess.Expr(
                Self.construct_nocheck(), p, []
            ).render_expr()

    @property
    def initial_env_expr(self) -> str:
        """
        The initial environment expression.
        """
        assert self.initial_env_prop
        return self._render_field_access(self.initial_env_prop)


class EnvAction:

    resolver: Optional[PropertyDef] = None
    """
    Some env actions use resolvers, that are property that will yield a lexical
    environment from a Node. To facilitate accessing it in general, we'll set a
    class attribute to None on the base class.

    :type: PropertyDef
    """

    def __init__(self) -> None:
        self.location = extract_library_location()

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
                              mapping: Dict[PropertyDef, PropertyDef]) -> None:
        """
        Rewrite `PropertyDef` references according to `mapping`. See
        CompileCtx.lower_properties_dispatching.

        :param mapping: PropertyDef reference substitution mapping.
        """
        pass


class AddEnv(EnvAction):
    def __init__(self,
                 no_parent: bool = False,
                 transitive_parent: bool = False) -> None:
        super().__init__()
        self.no_parent = no_parent
        self.transitive_parent = transitive_parent

    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        self.transitive_parent_prop = env_spec.create_internal_property(
            'Env_Trans_Parent', unsugar(self.transitive_parent), T.Bool
        )


class AddToEnv(EnvAction):

    def __init__(self,
                 mappings: AbstractExpression,
                 resolver: Optional[PropertyDef],
                 unsound: bool) -> None:
        super().__init__()
        self.mappings = mappings
        self.resolver = resolver
        self.unsound = unsound

        self.mappings_prop: PropertyDef

    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        self.mappings_prop = env_spec.create_internal_property(
            'Env_Mappings', self.mappings, None
        )

    def rewrite_property_refs(self,
                              mapping: Dict[PropertyDef, PropertyDef]) -> None:
        self.resolver = (
            self.resolver
            and mapping.get(self.resolver, self.resolver)
        )

    def check(self) -> None:
        ctx = get_context()
        self.resolver = resolve_property(self.resolver)
        with self.mappings_prop.diagnostic_context:
            mapping_type = self.mappings_prop.type
            if mapping_type.matches(T.env_assoc):
                ctx.has_env_assoc = True
            elif mapping_type.matches(T.env_assoc.array):
                ctx.has_env_assoc = True
                ctx.has_env_assoc_array = True
            else:
                check_source_language(
                    False,
                    'The bindings expression in environment specification must'
                    ' must be either an env_assoc or an array of env_assocs:'
                    ' got {} instead'.format(mapping_type.dsl_name)
                )

            if self.resolver:
                # Ask for the creation of untyped wrappers for all
                # properties used as entity resolvers.
                self.resolver.require_untyped_wrapper()

                check_source_language(
                    self.resolver.type.matches(T.entity),
                    'Entity resolver properties must return entities'
                    ' (got {})'.format(self.resolver.type.dsl_name)
                )
                check_source_language(
                    not self.resolver.dynamic_vars,
                    'Entity resolver properties must have no dynamically'
                    ' bound variable'
                )
                check_source_language(
                    not self.resolver.natural_arguments,
                    'Entity resolver properties must have no argument'
                )


class RefEnvs(EnvAction):
    """
    Couple of a property and an expression to evaluate referenced envs.
    """

    def __init__(self,
                 resolver: PropertyDef,
                 nodes_expr: AbstractExpression,
                 kind: RefKind = RefKind.normal,
                 dest_env: Optional[AbstractExpression] = None,
                 cond: Optional[AbstractExpression] = None,
                 category: Optional[str] = None,
                 shed_rebindings: bool = False,
                 unsound: bool = False) -> None:
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

        super().__init__()

        self.resolver: PropertyDef = resolver
        self.nodes_expr = nodes_expr
        self.kind = kind
        self.dest_env = dest_env
        self.cond = cond
        self.category = category and names.Name.from_lower(category)
        self.shed_rebindings = shed_rebindings
        self.unsound = unsound

        self.nodes_property: Optional[PropertyDef] = None
        """
        This holds the property that returns a list of nodes to pass to the
        resolver. It is None before the property is built.
        """

    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        """
        Create the property that returns the list of nodes to resolve into
        referenced lexical envs.
        """
        self.nodes_property = env_spec.create_internal_property(
            'Ref_Env_Nodes', self.nodes_expr, T.root_node.array
        )

        self.dest_env_prop = env_spec.create_internal_property(
            'Env_Dest', self.dest_env, T.LexicalEnv
        )

        self.cond_prop = env_spec.create_internal_property(
            'Ref_Cond', self.cond, T.Bool
        )

    def check(self) -> None:
        """
        Check that the resolver property is conforming.
        """
        ctx = get_context()
        ctx.has_ref_env = True

        self.resolver = resolve_property(self.resolver)
        self.resolver.require_untyped_wrapper()

        check_source_language(
            self.resolver.type.matches(T.LexicalEnv),
            'Referenced environment resolver must return a lexical'
            ' environment (not {})'.format(
                self.resolver.type.dsl_name
            )
        )
        check_source_language(
            not self.resolver.natural_arguments,
            'Referenced environment resolver must take no argument'
        )
        check_source_language(
            not self.resolver.dynamic_vars,
            'Referenced environment resolver must have no dynamically bound'
            ' variable'
        )

    def rewrite_property_refs(self,
                              mapping: Dict[PropertyDef, PropertyDef]) -> None:
        self.resolver = mapping.get(self.resolver, self.resolver)


class HandleChildren(EnvAction):
    """
    Stub class to delimit pre and post env actions.
    """
    pass


class ExprHolderAction(EnvAction):
    def __init__(self, env_expr: AbstractExpression) -> None:
        super().__init__()
        self.env_expr = env_expr


class SetInitialEnv(ExprHolderAction):

    def __init__(self, env_expr: AbstractExpression, unsound: bool) -> None:
        super().__init__(env_expr)
        self.unsound = unsound

    def check(self) -> None:
        # Check is not normally called on this, so if it is called it means
        # that a SetInitialEnv instance has found its way into a regular action
        # list.
        check_source_language(
            False,
            "set_initial_env can only be preceded by do()"
        )


class Do(ExprHolderAction):
    def create_internal_properties(self, env_spec: EnvSpec) -> None:
        self.do_property = env_spec.create_internal_property(
            'Env_Do', self.env_expr, None
        )
