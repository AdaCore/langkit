from __future__ import absolute_import, division, print_function

from funcy import split_by
from itertools import count

from langkit import names
from langkit.compiled_types import AbstractNodeData, T
from langkit.diagnostics import (
    check_source_language, extract_library_location, Context
)
from langkit.expressions import (FieldAccess, PropertyDef, Self, construct,
                                 resolve_property)

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

def add_env(no_parent=False):
    """
    Add an environment linked to the current node. This env action must be
    called as a pre action. The only actions that can precede this one in pre
    actions are add_to_env actions with the current env as a destination.

    :param bool no_parent: If passed, the new env will be created with no
        parent env.
    :rtype: EnvAction
    """
    return AddEnv(no_parent)


def reference(nodes, through, transitive=False, visible_to_children=False):
    """
    Reference a group of lexical environments, that will be lazily yielded by
    calling the `through` property on the array of nodes `nodes`.

    :param AbstractExpression nodes: An expression that yields a list of nodes.
    :param PropertyDef through: A property reference.
    :param bool visible_to_children: If true, then the referenced environment
        will be visible to the node, and the children of the node on which
        the reference acts.

        By default this is false, to prevent infinite recursions that can
        happen if any children of node does an env-lookup as part of its env
        spec. Use this flag if you need this reference to be visible to
        children of node, and are sure that it can cause no infinite recursion.

    :rtype: RefEnvs
    """
    return RefEnvs(through, nodes, transitive, visible_to_children)


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


def handle_children():
    """
    Handle the node's children lexical environments.

    :rtype: HandleChildren
    """
    return HandleChildren()


def set_initial_env(env_expr):
    """
    Action that sets the initial env in which the rest of the environment
    actions are evaluated. This action must be first or second in the list of
    action, can only be preceded by call_env_hook.

    :rtype: SetInitialEnv
    """
    return SetInitialEnv(env_expr)


def call_env_hook(env_expr):
    """
    Call the env-hook, that is an externally defined procedure, with env_expr
    as argument.

    :rtype: CallEnvHook
    """
    return CallEnvHook(env_expr)


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

        actions = list(actions)

        self.env_hook = None
        if isinstance(actions and actions[0], CallEnvHook):
            self.env_hook = actions.pop(0)
            ":type: SetInitialEnv"

        self.initial_env = None
        if isinstance(actions and actions[0], SetInitialEnv):
            self.initial_env = actions.pop(0)
            ":type: SetInitialEnv"

        pre, post = split_by(
            lambda a: not isinstance(a, HandleChildren), actions
        )

        # Get rid of the HandleChildren delimiter action
        post = post and post[1:]

        self.pre_actions = pre
        self.post_actions = post
        self.actions = self.pre_actions + self.post_actions

        # These are the property attributes

        self.initial_env_prop = None
        ":type: PropertyDef"

        self.env_hook_arg = None
        ":type: PropertyDef"

        self.adds_env = any(isinstance(a, AddEnv) for a in self.pre_actions)

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
        """
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

    def create_properties(self, context):
        """
        Turn the various abstract expression attributes for this env spec into
        internal properties and add them to `astnode`.

        :param langkit.compile_context.CompileCtx context: Current context.
        """

        self.initial_env_prop = self.create_internal_property(
            'Initial_Env',
            self.initial_env and self.initial_env.env_expr,
            T.LexicalEnvType
        )

        for action in self.actions:
            action.create_internal_properties(self)

        self.env_hook_arg_prop = self.create_internal_property(
            'Env_Hook_Arg',
            self.env_hook and self.env_hook.env_expr,
            T.root_node
        )

    def check_spec(self, context):
        """
        ASTNode pass which checks that properties generated by the env spec are
        conforming. This relies on type information and property attributes
        (privacy, implicit envs), so it must run only after these can be
        computed.

        :param langkit.compile_context.CompileCtx context: Current context.
        """
        with self.diagnostic_context:
            for action in self.actions:
                action.check()

            pre_addenv, post_addenv = split_by(
                lambda a: not isinstance(a, AddEnv), self.pre_actions
            )

            if post_addenv:
                check_source_language(
                    all(a.dest_env for a in post_addenv
                        if isinstance(a, AddToEnv)),
                    "add_to_env actions happening after add_env must have an"
                    " explicit destination env"
                )

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
        return self._render_field_access(self.initial_env_prop)

    @property
    def env_hook_enabled(self):
        """
        Return whether the environment hook must be called.

        :rtype: bool
        """
        return bool(self.env_hook_arg_prop)

    @property
    def env_hook_arg_expr(self):
        """
        The expression for the environment hook argument.

        This is not available when "self.env_hook_enabled" is False.

        :rtype: str
        """
        return self._render_field_access(self.env_hook_arg_prop)


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


class AddEnv(EnvAction):
    def __init__(self, no_parent=False):
        self.no_parent = no_parent


class AddToEnv(EnvAction):
    def __init__(self, mappings, dest_env, metadata, resolver):
        self.mappings = mappings
        self.dest_env = dest_env
        self.metadata = metadata
        self.resolver = resolver

    def check(self):
        self.resolver = resolve_property(self.resolver)
        with self.mappings_prop.diagnostic_context:
            check_source_language(
                self.mappings_prop.type.matches(T.env_assoc) or
                self.mappings_prop.type.matches(T.env_assoc.array),
                'The bindings expression in environment specification '
                ' must be either an env_assoc or an array of env_assocs: '
                'got {} instead'.format(
                    self.mappings_prop.type.dsl_name
                )
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

    def create_internal_properties(self, env_spec):
        self.mappings_prop = env_spec.create_internal_property(
            'Env_Mappings', self.mappings, None
        )
        self.dest_env_prop = env_spec.create_internal_property(
            'Env_Dest', self.dest_env, T.LexicalEnvType
        )
        self.metadata_prop = env_spec.create_internal_property(
            'MD', self.metadata, T.defer_env_md
        )


class RefEnvs(EnvAction):
    """
    Couple of a property and an expression to evaluate referenced envs.
    """

    def __init__(self, resolver, nodes_expr, transitive=False,
                 visible_to_children=False):
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

        :param bool visible_to_children: If true, then the referenced
            environment will be visible to the node, and the children of the
            node on which reference acts.

            By default this is false, to prevent infinite recursions that can
            happen if any children of node does an env-lookup as part of its
            env spec. Use this flag if you need this reference to be visible to
            children of node, and are sure that it can cause no infinite
            recursion.
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

        self.transitive = transitive

        self.visible_to_children = visible_to_children

    def create_internal_properties(self, env_spec):
        """
        Create the property that returns the list of nodes to resolve into
        referenced lexical envs.
        """
        self.nodes_property = env_spec.create_internal_property(
            'Ref_Env_Nodes', self.nodes_expr, T.root_node.array
        )

    def check(self):
        """
        Check that the resolver property is conforming.
        """
        self.resolver = resolve_property(self.resolver)
        self.resolver.require_untyped_wrapper()

        check_source_language(
            self.resolver.type.matches(T.LexicalEnvType),
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
            "set_initial_env can only be preceded by call_env_hook"
        )


class CallEnvHook(ExprHolderAction):
    def check(self):
        # Check is not normally called on this, so if it is called it means
        # that a CallEnvHook instance has found its way into a regular action
        # list.
        check_source_language(
            "set_initial_env must be first in the action list"
        )


class Do(ExprHolderAction):
    def create_internal_properties(self, env_spec):
        self.do_property = env_spec.create_internal_property(
            'Env_Do', self.env_expr, None
        )
