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

from __future__ import absolute_import, division, print_function

from enum import Enum
from funcy import lsplit_by
from itertools import count

from langkit import names
from langkit.compile_context import get_context
from langkit.compiled_types import AbstractNodeData, T
from langkit.diagnostics import (Context, check_source_language,
                                 extract_library_location)
from langkit.expressions import (FieldAccess, PropertyDef, Self,
                                 resolve_property, unsugar)


# Public API for env actions

def add_env(no_parent=False, transitive_parent=False):
    """
    Add an environment linked to the current node. This env action must be
    called as a pre action. The only actions that can precede this one in pre
    actions are add_to_env actions with the current env as a destination. Also,
    there can be only one add_env action per EnvSpec.

    :param bool no_parent: If passed, the new env will be created with no
        parent env.
    :rtype: EnvAction
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


def reference(nodes, through,
              kind=RefKind.normal, dest_env=None, cond=None,
              category=None, shed_corresponding_rebindings=False):
    """
    Reference a group of lexical environments, that will be lazily yielded by
    calling the `through` property on the array of nodes `nodes`.

    :param AbstractExpression nodes: An expression that yields a list of nodes.
    :param PropertyDef through: A property reference.

    :param RefKind kind: Kind of reference.
    :param AbstractExpression|None dest_env: If passed, the destination
        environment for this reference.
    :param AbstractExpression|None cond: If passed, an expression evaluating to
        a boolean condition. If False, reference won't be made and the others
        expressions won't be evaluated.

    :param str category: If passed, must be a string representing a category
        name. String must represent a valid Ada name. A category in set of
        possible referenced envs categories will be implicitly created for each
        unique string passed to a call to reference, in a given compilation
        context.

    :param bool shed_corresponding_rebindings: If True, when shedding
        rebindings during an env lookup, this referenced env will be followed
        to check, and eventually shed rebindings associated to the referenced
        env.

    :rtype: RefEnvs
    """
    return RefEnvs(through, nodes, kind, dest_env=dest_env, cond=cond,
                   category=category and category.lower(),
                   shed_rebindings=shed_corresponding_rebindings)


def add_to_env(mappings, resolver=None):
    """
    Specify elements to add to the lexical environment.

    :param AbstractExpression mappings: One or several mappings of key to value
        to add to the environment. Must be either of type T.env_assoc, or
        T.env_assoc.array. All values must belong to the same unit as the node
        that owns this EnvSpec. See langkit.expressions.envs.new_env_assoc for
        more precision on how to create an env assoc.
    :param PropertyDef|None resolver: Optional property that returns an AST
        node. If provided, the lexical environment lookup that will try to
        return the given mappings will first run this property on all nodes and
        return its result instead.
    """
    return AddToEnv(mappings, resolver)


def add_to_env_kv(key, val, dest_env=None, metadata=None, resolver=None):
    """
    Specify a single element to add to the lexical environment. See
    langkit.expressions.envs.new_env_assoc for more precision about the first
    four arguments.

    :type key: AbstractExpression
    :type val: AbstractExpression
    :type dest_env: AbstractExpression
    :type metadata: AbstractExpression metadata
    :param PropertyDef|None resolver: Optional property that returns an AST
        node. If provided, the lexical environment lookup that will try to
        return the given mappings will first run this property on all nodes and
        return its result instead.
    """
    from langkit.expressions import new_env_assoc

    return add_to_env(
        mappings=new_env_assoc(key, val, dest_env, metadata),
        resolver=resolver
    )


def handle_children():
    """
    Handle the node's children lexical environments.

    :rtype: HandleChildren
    """
    return HandleChildren()


def set_initial_env(env_expr):
    """
    Action that sets the initial env in which the rest of the environment
    actions are evaluated. Except for Do() hooks, this action must be first in
    the list of action.

    :rtype: SetInitialEnv
    """
    return SetInitialEnv(env_expr)


def do(expr):
    """
    Evaluate given expression for its side effects, discarding its result.

    :rtype: CallEnvHook
    """
    return Do(expr)


class EnvSpec(object):
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

    PROPERTY_COUNT = count(0)

    def __init__(self, *actions):
        """
        :param list[EnvAction] actions: A list of environment actions to
            execute.
        """
        self.location = extract_library_location()

        self.ast_node = None
        """
        ASTNodeType subclass associated to this environment specification.
        Initialized when creating ASTNodeType subclasses.
        :type: langkit.compiled_types.ASTNodeType
        """

        # Analyze the given list of actions
        self._parse_actions(actions)

        # Property that returns the initial environment taht environment
        # actions will use.
        self.initial_env_prop = None
        ":type: PropertyDef"

        self.adds_env = any(isinstance(a, AddEnv) for a in self.pre_actions)
        """
        Whether this spec create an environment.
        """

    def _parse_actions(self, actions):
        """
        Analyze the given list of actions and extract pre/post actions, i.e.
        actions executed before and after handling children.

        :rtype: (list[EnvAction], list[EnvAction])
        """
        actions = list(actions)

        # If present, allow Do actions to come before SetInitialEnv
        self.pre_initial_env_actions = []
        if any(isinstance(a, SetInitialEnv) for a in actions):
            while actions and isinstance(actions[0], Do):
                self.pre_initial_env_actions.append(actions.pop(0))

        # After that, allow one call to SetInitialEnv
        self.initial_env = None
        if actions and isinstance(actions[0], SetInitialEnv):
            self.initial_env = actions.pop(0)

        pre, post = lsplit_by(
            lambda a: not isinstance(a, HandleChildren), actions
        )

        # Get rid of the HandleChildren delimiter action
        post = post and post[1:]

        self.pre_actions = pre
        self.post_actions = post
        self.actions = self.pre_actions + self.post_actions

    @property
    def diagnostic_context(self):
        """
        Diagnostic context for env specs.
        """

        ctx_message = 'in env spec'
        return Context(ctx_message, self.location)

    def create_internal_property(self, name, expr, type):
        """
        Create an internal property for this env spec.

        If ``expr`` is None, do not create a property and return None.
        Otherwise, unsugar it.

        :param str name: Lower-case name to use to create this property name.
            Since the property is internal, the name is decorated.
        """
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

    def create_properties(self, context):
        """
        Turn the various abstract expression attributes for this env spec into
        internal properties and add them to `astnode`.

        :param langkit.compile_context.CompileCtx context: Current context.
        """

        for action in self.pre_initial_env_actions:
            action.create_internal_properties(self)

        self.initial_env_prop = self.create_internal_property(
            'Initial_Env',
            self.initial_env and self.initial_env.env_expr,
            T.LexicalEnv
        )

        for action in self.actions:
            action.create_internal_properties(self)

    def register_categories(self, context):
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

    def check_spec(self, context):
        """
        ASTNode pass which checks that properties generated by the env spec are
        conforming. This relies on type information and property attributes
        (privacy, implicit envs), so it must run only after these can be
        computed.

        :param langkit.compile_context.CompileCtx context: Current context.
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

    def _render_field_access(self, p):
        """
        Helper to render a simple field access to the property P in the context
        of an environment specification.

        :param PropertyDef p: The property to access. It must accept no
            explicit argument.
        :rtype: str
        """
        assert not p.natural_arguments

        with PropertyDef.bind_none(), \
                Self.bind_type(self.ast_node):
            return FieldAccess.Expr(
                Self.construct_nocheck(), p, []
            ).render_expr()

    @property
    def initial_env_expr(self):
        """
        The initial environment expression.
        :rtype: str
        """
        return self._render_field_access(self.initial_env_prop)


class EnvAction(object):

    resolver = None
    """
    Some env actions use resolvers, that are property that will yield a lexical
    environment from a Node. To facilitate accessing it in general, we'll set a
    class attribute to None on the base class.

    :type: PropertyDef
    """

    def check(self):
        """
        Check that the env action is legal.
        """
        pass

    def create_internal_properties(self, env_spec):
        """
        Create properties needed for the emission of this env action.
        """
        pass

    def rewrite_property_refs(self, mapping):
        """
        Rewrite `PropertyDef` references according to `mapping`. See
        CompileCtx.lower_properties_dispatching.

        :param dict[PropertyDef, PropertyDef] mapping: PropertyDef reference
            substitution mapping.
        """
        pass


class AddEnv(EnvAction):
    def __init__(self, no_parent=False, transitive_parent=False):
        self.no_parent = no_parent
        self.transitive_parent = transitive_parent

    def create_internal_properties(self, env_spec):
        self.transitive_parent_prop = env_spec.create_internal_property(
            'Env_Trans_Parent', self.transitive_parent, T.Bool
        )


class AddToEnv(EnvAction):
    def __init__(self, mappings, resolver):
        self.mappings = mappings
        self.resolver = resolver

    def check(self):
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

    def create_internal_properties(self, env_spec):
        self.mappings_prop = env_spec.create_internal_property(
            'Env_Mappings', self.mappings, None
        )

    def rewrite_property_refs(self, mapping):
        self.resolver = mapping.get(self.resolver, self.resolver)


class RefEnvs(EnvAction):
    """
    Couple of a property and an expression to evaluate referenced envs.
    """

    def __init__(self, resolver, nodes_expr,
                 kind=RefKind.normal, dest_env=None, cond=None, category=None,
                 shed_rebindings=False):
        """
        All nodes that nodes_expr yields must belong to the same analysis unit
        as the AST node that triggers this RefEnvs. Besides, the lexical
        environment to which these referenced environments are added must also
        belong to the same analysis unit. Attempts to add referenced
        environments that do not respect these rules will trigger a
        Property_Error.

        :param PropertyDef resolver: Property that takes no argument
            (explicit or implicit) appart from Self, and that returns a lexical
            environment.

        :param RefKind kind: Kind of reference.

        :param AbstractExpression|None dest_env: Optional expression
            designating the destination env.

        :param AbstractExpression nodes_expr: Abstract expression that
            returns an array of AST nodes. Each node will be given to the above
            resolver in order to get corresponding referenced lexical envs.
            In this array, null nodes are allowed: they are simply discarded.
        """
        assert resolver
        assert nodes_expr

        self.resolver = resolver
        ":type: PropertyDef"

        self.nodes_expr = nodes_expr
        ":type: AbstractExpression"

        self.nodes_property = None
        """
        :type: PropertyDef

        This holds the property that returns a list of nodes to pass to the
        resolver. It is None before the property is built.
        """

        self.kind = kind
        self.dest_env = dest_env
        self.cond = cond
        self.category = category and names.Name.from_lower(category)
        self.shed_rebindings = shed_rebindings

    def create_internal_properties(self, env_spec):
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

    def check(self):
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

    def rewrite_property_refs(self, mapping):
        self.resolver = mapping.get(self.resolver, self.resolver)


class HandleChildren(EnvAction):
    """
    Stub class to delimit pre and post env actions.
    """
    pass


class ExprHolderAction(EnvAction):
    def __init__(self, env_expr):
        self.env_expr = env_expr


class SetInitialEnv(ExprHolderAction):

    def check(self):
        # Check is not normally called on this, so if it is called it means
        # that a SetInitialEnv instance has found its way into a regular action
        # list.
        check_source_language(
            False,
            "set_initial_env can only be preceded by do()"
        )


class Do(ExprHolderAction):
    def create_internal_properties(self, env_spec):
        self.do_property = env_spec.create_internal_property(
            'Env_Do', self.env_expr, None
        )
