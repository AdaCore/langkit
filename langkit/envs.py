from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from collections import namedtuple
from itertools import count

from langkit import names
from langkit.compiled_types import AbstractNodeData, LexicalEnvType, T
from langkit.diagnostics import check_source_language
from langkit.expressions import (
    Env, FieldAccess, PropertyDef, Self, construct
)


AddToEnv = namedtuple("AddToEnv", ["mappings", "dest_env", "metadata",
                                   "is_post", "resolver"])


class RefEnvs(object):
    """
    Couple of a property and an expression to evaluate referenced envs.
    """

    def __init__(self, resolver, nodes_expr):
        """
        :param PropertyDef|None resolver: Property that takes no argument
            (explicit or implicit) and that returns a lexical environment.

        :param AbstractExpression|None nodes_expr: Abstract expression that
            returns an array of AST nodes. Each node will be given to the above
            resolver in order to get corresponding referenced lexical envs.
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

    def create_nodes_property(self, create_internal_property):
        """
        Create the property that returns the list of nodes to resolve into
        referenced lexical envs.
        """
        self.nodes_property = create_internal_property(
            'Ref_Env_Nodes', self.nodes_expr, T.root_node.array_type()
        )

    def check_resolver(self):
        """
        Check that the resolver property is conforming.
        """
        if isinstance(self.resolver, T.Defer):
            self.resolver = self.resolver.get()
        self.resolver.require_untyped_wrapper()

        check_source_language(
            self.resolver.type.matches(LexicalEnvType),
            'Referenced environment resolver must return a lexical'
            ' environment (not {})'.format(
                self.resolver.type.name().camel
            )
        )
        check_source_language(
            not self.resolver.explicit_arguments,
            'Referenced environment resolver must take no argument'
        )


def add_to_env(mappings, dest_env=None, metadata=None, is_post=False,
               resolver=None):
    """
    Specify elements to add to the lexical environment.

    :param AbstractExpression mappings: One or several mappings of key to value
        to add to the environment. Must be either of type T.env_assoc, or
        T.env_assoc.array_type().

    :param AbstractExpression dest_env: The destination environment in which to
        add the elements.
    :param AbstractExpression metadata: Optional expression for metadata.
    :param bool is_post: Whether to execute the add_to_env action after
        children have been treated.
    :param PropertyDef|None resolver: Optional property that returns an AST
        node. If provided, the lexical environment lookup that will try to
        return the given mappings will first run this property on all nodes and
        return its result instead.
    :return:
    """
    return AddToEnv(mappings, dest_env, metadata, is_post, resolver)


class EnvSpec(object):
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

    PROPERTY_COUNT = count(0)

    def __init__(self,
                 add_env=False,
                 add_to_env=None,
                 ref_envs=[],
                 post_ref_envs=[],
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

        :param RefEnvs|list[RefEnvs] ref_envs: Like ref_envs, but evaluated
            after after the children have been processed.

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

        self.ast_node = None
        """
        ASTNode subclass associated to this environment specification.
        Initialized when creating ASTNode subclasses.
        :type: langkit.compiled_types.ASTNode
        """

        self._add_env = add_env
        ":type: bool"

        # The following attributes (unresolved_*) contain abstract expressions
        # used to describe various environment behaviors. They all have
        # corresponding attributes that embed them as properties: see below.

        self._unresolved_initial_env = initial_env
        ":type: AbstractExpression"

        self._unresolved_envs_expressions = []
        ":type: list[AddToEnv]"

        self.envs_expressions = []
        ":type: list[AddToEnv]"

        if add_to_env:
            check_source_language(
                isinstance(add_to_env, AddToEnv)
                or isinstance(add_to_env, list),
                "Wrong parameter for add_to_env: Expected AddToEnv named-tuple"
                " or list of AddToEnv"
            )

            self._unresolved_envs_expressions = (
                [add_to_env] if isinstance(add_to_env, AddToEnv)
                else add_to_env
            )

        self.ref_envs = ([ref_envs]
                         if isinstance(ref_envs, RefEnvs) else ref_envs)
        self.post_ref_envs = ([post_ref_envs]
                              if isinstance(post_ref_envs, RefEnvs)
                              else post_ref_envs)

        self._unresolved_env_hook_arg = env_hook_arg
        ":type: AbstractExpression"

        # These are the property attributes

        self.initial_env = None
        ":type: PropertyDef"

        self.env_hook_arg = None
        ":type: PropertyDef"

        self.has_post_actions = False

        self.call_parents = call_parents
        "Whether to call parents env specs or not"

    def create_properties(self):
        """
        Turn the various abstract expression attributes for this env spec into
        internal properties.

        :rtype: list[PropertyDef]
        """
        result = []

        def create_internal_property(name, expr, type):
            if expr is None:
                return None

            # Set has_implicit_env for these internal properties so that they
            # can use a default environment that the context gives. This
            # default will be the Self_Env of the parent node, which is always
            # the same, regardless of being run in the populate lexical env
            # pass or later on. See Initial_Env_Getter_Fn functions for the
            # code that fetches this default environment.
            p = PropertyDef(
                expr, AbstractNodeData.PREFIX_INTERNAL,
                name=names.Name('_{}_{}'.format(name,
                                                next(self.PROPERTY_COUNT))),
                public=False, type=type, has_implicit_env=True
            )
            result.append(p)
            return p

        self.initial_env = create_internal_property(
            'Initial_Env', self._unresolved_initial_env, LexicalEnvType
        )

        self.envs_expressions = [
            add_to_env(
                create_internal_property('Env_Mappings', exprs.mappings, None),
                create_internal_property('Env_Dest', exprs.dest_env,
                                         LexicalEnvType),
                create_internal_property('MD', exprs.metadata, T.env_md),
                exprs.is_post,
                resolver=exprs.resolver,
            ) for exprs in self._unresolved_envs_expressions
        ]

        self.has_post_actions = any([e.is_post for e in self.envs_expressions])

        for ref_envs in self.ref_envs:
            ref_envs.create_nodes_property(create_internal_property)
        for ref_envs in self.post_ref_envs:
            ref_envs.create_nodes_property(create_internal_property)

        self.env_hook_arg = create_internal_property(
            'Env_Hook_Arg', self._unresolved_env_hook_arg, T.root_node
        )

        return result

    def check_properties(self):
        """
        Method to implement an ASTNode pass, which checks that properties
        generated by the env spec are conforming. This relies on type
        information and property attributes (privacy, implicit envs), so it
        must run only after these can be computed.

        :rtype: bool
        """
        for ref_envs in self.ref_envs:
            ref_envs.check_resolver()
        for ref_envs in self.post_ref_envs:
            ref_envs.check_resolver()

        for bindings_prop, _, _, _, resolver in self.envs_expressions:
            with bindings_prop.diagnostic_context():
                check_source_language(
                    bindings_prop.type.matches(T.env_assoc) or
                    bindings_prop.type.matches(T.env_assoc.array_type()),
                    'The bindings expression in environment specification '
                    ' must be either an env_assoc or an array of env_assocs: '
                    'got {} instead'.format(
                        bindings_prop.type.name().camel
                    )
                )
                if resolver:
                    # Ask for the creation of untyped wrappers for all
                    # properties used as entity resolvers.
                    resolver.require_untyped_wrapper()

                    check_source_language(
                        resolver.type.matches(T.entity),
                        'Entity resolver properties must return entities'
                        ' (got {})'.format(resolver.type.name().camel)
                    )
                    check_source_language(
                        not resolver.has_implicit_env,
                        'Entity resolver properties must not use implicit'
                        ' environments'
                    )

    def _render_field_access(self, p):
        """
        Helper to render a simple field access to the property P in the context
        of an environment specification.

        :param PropertyDef p: The property to access. It must accept no
            explicit argument.
        :rtype: str
        """
        assert not p.explicit_arguments

        with PropertyDef.bind_none(), \
                Self.bind_type(self.ast_node), \
                Env.bind_name('Current_Env'):
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
