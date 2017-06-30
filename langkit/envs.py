from __future__ import absolute_import, division, print_function

from itertools import count

from langkit import names
from langkit.compiled_types import AbstractNodeData, T, lexical_env_type
from langkit.diagnostics import check_source_language, extract_library_location
from langkit.expressions import FieldAccess, PropertyDef, Self, construct

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


# Public API for env actions

def add_env():
    """
    Add an environment linked to the current node. This env action must be
    called as a pre action. The only actions that can precede this one in pre
    actions are add_to_env actions with the current env as a destination.

    :rtype: EnvAction
    """
    return AddEnv()


def reference(nodes, through):
    """
    Reference a group of lexical environments, that will be lazily yielded by
    calling the `through` property on the array of nodes `nodes`.

    :param AbstractExpression nodes: An expression that yields a list of nodes.
    :param PropertyDef through: A property reference.

    :rtype: RefEnvs
    """
    return RefEnvs(through, nodes)


def add_to_env(mappings, dest_env=None, metadata=None, resolver=None):
    """
    Specify elements to add to the lexical environment.

    :param AbstractExpression mappings: One or several mappings of key to value
        to add to the environment. Must be either of type T.env_assoc, or
        T.env_assoc.array.

    :param AbstractExpression dest_env: The destination environment in which to
        add the elements.
    :param AbstractExpression metadata: Optional expression for metadata.
    :param PropertyDef|None resolver: Optional property that returns an AST
        node. If provided, the lexical environment lookup that will try to
        return the given mappings will first run this property on all nodes and
        return its result instead.
    :return:
    """
    return AddToEnv(mappings, dest_env, metadata, resolver)


class EnvSpec(object):
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

    PROPERTY_COUNT = count(0)

    def __init__(self,
                 pre_actions=[],
                 post_actions=[],
                 initial_env=None,
                 env_hook_arg=None,
                 call_parents=True):
        """

        :param bool add_env: Wether to add a new scoped lexical environment.
            The new environment will be linked to the corresponding AST node
            and will have the AST node's lexical environment as a parent.

        :param add_to_env: Eiter an AddToEnv named tuple, or a list of them.
            Used to add elements to the lexical environment. See add_to_env's
            doc for more details.
        :type add_to_env: AddToEnv|[AddToEnv]

        :param RefEnvs|list[RefEnvs] ref_envs: If this env spec introduces
            referenced environments, this must be a RefEnvs instance (or a list
            of them) to describe how to compute the environments to reference.
            This will register referenced environments to this node' "self"
            environment.

        :param RefEnvs|list[RefEnvs] post_ref_envs: Like ref_envs, but
            evaluated after after the children have been processed.

        :param AbstractExpression initial_env: If supplied, this env will be
            used as the lexical environment to execute the rest of the actions.
            For example, if you pass an initial_env, and add_env, then an env
            will be added to the env passed as initial_env, and the node
            concerned by this env specification will have initial_env as a
            parent indirectly.

        :param AbstractExpression env_hook_arg: Does nothing if left to None.
            If supplied, it must be an abstract expression that resolves to a
            node. This expression will be evaluated and passed to the
            environment hook.
        """
        self.location = extract_library_location()

        self.ast_node = None
        """
        ASTNodeType subclass associated to this environment specification.
        Initialized when creating ASTNodeType subclasses.
        :type: langkit.compiled_types.ASTNodeType
        """

        # The following attributes (unresolved_*) contain abstract expressions
        # used to describe various environment behaviors. They all have
        # corresponding attributes that embed them as properties: see below.

        self._unresolved_initial_env = initial_env
        ":type: AbstractExpression"

        self.pre_actions = list(pre_actions)
        self.post_actions = list(post_actions)
        self.actions = self.pre_actions + self.post_actions

        self._unresolved_env_hook_arg = env_hook_arg
        ":type: AbstractExpression"

        # These are the property attributes

        self.initial_env = None
        ":type: PropertyDef"

        self.env_hook_arg = None
        ":type: PropertyDef"

        self.call_parents = call_parents
        "Whether to call parents env specs or not"

        self.adds_env = any(isinstance(a, AddEnv) for a in self.pre_actions)

    def create_properties(self, context):
        """
        Turn the various abstract expression attributes for this env spec into
        internal properties and add them to `astnode`.

        :param langkit.compile_context.CompileCtx context: Current context.
        """

        def create_internal_property(name, expr, type):
            if expr is None:
                return None

            p = PropertyDef(
                expr, AbstractNodeData.PREFIX_INTERNAL,
                name=names.Name('_{}_{}'.format(name,
                                                next(self.PROPERTY_COUNT))),
                public=False, type=type, ignore_warn_on_node=True
            )
            p.location = getattr(expr, 'location') or self.location
            self.ast_node.add_field(p)
            return p

        self.initial_env = create_internal_property(
            'Initial_Env', self._unresolved_initial_env, lexical_env_type
        )

        for action in self.actions:
            action.create_internal_properties(create_internal_property)

        self.env_hook_arg = create_internal_property(
            'Env_Hook_Arg', self._unresolved_env_hook_arg, T.root_node
        )

    def check_spec(self, context):
        """
        ASTNode pass which checks that properties generated by the env spec are
        conforming. This relies on type information and property attributes
        (privacy, implicit envs), so it must run only after these can be
        computed.

        :param langkit.compile_context.CompileCtx context: Current context.
        """
        for action in self.actions:
            action.check()

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
            return FieldAccess.Expr(construct(Self), p, []).render_expr()

    @property
    def initial_env_expr(self):
        """
        The initial environment expression.
        :rtype: str
        """
        return self._render_field_access(self.initial_env)

    @property
    def env_hook_enabled(self):
        """
        Return whether the environment hook must be called.

        :rtype: bool
        """
        return bool(self.env_hook_arg)

    @property
    def env_hook_arg_expr(self):
        """
        The expression for the environment hook argument.

        This is not available when "self.env_hook_enabled" is False.

        :rtype: str
        """
        return self._render_field_access(self.env_hook_arg)


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

    def create_internal_properties(self, create_property):
        """
        Create properties needed for the emission of this env action.
        """
        pass


class AddEnv(EnvAction):
    pass


class AddToEnv(EnvAction):
    def __init__(self, mappings, dest_env, metadata, resolver):
        self.mappings = mappings
        self.dest_env = dest_env
        self.metadata = metadata
        self.resolver = resolver

    def check(self):
        with self.mappings_prop.diagnostic_context:
            check_source_language(
                self.mappings_prop.type.matches(T.env_assoc) or
                self.mappings_prop.type.matches(T.env_assoc.array),
                'The bindings expression in environment specification '
                ' must be either an env_assoc or an array of env_assocs: '
                'got {} instead'.format(
                    self.mappings_prop.type.name.camel
                )
            )
            if self.resolver:
                # Ask for the creation of untyped wrappers for all
                # properties used as entity resolvers.
                self.resolver.require_untyped_wrapper()

                check_source_language(
                    self.resolver.type.matches(T.entity),
                    'Entity resolver properties must return entities'
                    ' (got {})'.format(self.resolver.type.name.camel)
                )
                check_source_language(
                    not self.resolver.dynamic_vars,
                    'Entity resolver properties must have no dynamically'
                    ' bound variable'
                )

    def create_internal_properties(self, create_property):
        self.mappings_prop = create_property(
            'Env_Mappings', self.mappings, None
        )
        self.dest_env_prop = create_property(
            'Env_Dest', self.dest_env, lexical_env_type
        )
        self.metadata_prop = create_property(
            'MD', self.metadata, T.defer_env_md
        )


class RefEnvs(EnvAction):
    """
    Couple of a property and an expression to evaluate referenced envs.
    """

    def __init__(self, resolver, nodes_expr):
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

    def create_internal_properties(self, create_property):
        """
        Create the property that returns the list of nodes to resolve into
        referenced lexical envs.
        """
        self.nodes_property = create_property(
            'Ref_Env_Nodes', self.nodes_expr, T.root_node.array
        )

    def check(self):
        """
        Check that the resolver property is conforming.
        """
        if isinstance(self.resolver, T.Defer):
            self.resolver = self.resolver.get()
        self.resolver.require_untyped_wrapper()

        check_source_language(
            self.resolver.type.matches(lexical_env_type),
            'Referenced environment resolver must return a lexical'
            ' environment (not {})'.format(
                self.resolver.type.name.camel
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
