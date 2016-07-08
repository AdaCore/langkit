from collections import namedtuple
from itertools import count

from langkit import names
from langkit.compiled_types import (
    AbstractNodeData, LexicalEnvType, StructMetaclass, Symbol
)
from langkit.diagnostics import check_source_language
from langkit.expressions import Env, FieldAccess, PropertyDef, Self, construct


AddToEnv = namedtuple("AddToEnv", ["key", "val", "dest_env"])


def add_to_env(key, val, dest_env=None):
    return AddToEnv(key, val, dest_env)


class EnvSpec(object):
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

    PROPERTY_COUNT = count(0)

    def __init__(self,
                 add_env=False,
                 add_to_env=None,
                 ref_envs=None,
                 initial_env=None,
                 env_hook_arg=None):
        """

        :param bool add_env: Wether to add a new scoped lexical environment.
            The new environment will be linked to the corresponding AST node
            and will have the AST node's lexical environment as a parent.

        :param add_to_env: Tuple of expressions, the first one returning
            the name under which the elements must be added, the second one
            returning the element or elements to add to the environment. For
            the moment, the element returned by the first expression must be a
            node with a token property, and the second expression must be a
            single element of type ASTNode.
        :type add_to_env: (AbstractExpression, AbstractExpression)

        :param AbstractExpression ref_envs: if an AbstractExpression returning
            a list of environments is supplied, the topmost environment in the
            environment resolution will be altered to include the list of
            environments as referenced environments. TODO: Not yet implemented!

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

        assert ref_envs is None, "Ref envs is not implemented yet!"

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

        self._unresolved_ref_envs = ref_envs
        ":type: AbstractExpression"

        self._unresolved_env_hook_arg = env_hook_arg
        ":type: AbstractExpression"

        # These are the property attributes

        self.initial_env = None
        ":type: PropertyDef"

        self.ref_envs = None
        ":type: PropertyDef"

        self.env_hook_arg = None
        ":type: PropertyDef"

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

            p = PropertyDef(
                expr, AbstractNodeData.PREFIX_INTERNAL,
                name=names.Name('_{}_{}'.format(name,
                                                next(self.PROPERTY_COUNT))),
                private=True, type=type
            )
            result.append(p)
            return p

        # We are doing this when creating ASTNode subclasses, so there's no
        # context yet. So fetch the root grammar class in StructMetaclass
        # instead.
        node_type = StructMetaclass.root_grammar_class

        self.initial_env = create_internal_property(
            'Initial_Env', self._unresolved_initial_env, LexicalEnvType
        )

        self.envs_expressions = [
            add_to_env(
                create_internal_property('Env_Key', exprs.key, None),
                create_internal_property('Env_Value', exprs.val, node_type)
            ) for exprs in self._unresolved_envs_expressions
        ]

        # TODO: what is the expected type for this one?
        self.ref_envs = create_internal_property(
            'Ref_Envs', self._unresolved_ref_envs, None
        )

        self.env_hook_arg = create_internal_property(
            'Env_Hook_Arg', self._unresolved_env_hook_arg, node_type
        )

        return result

    def prepare(self):
        """
        Method call by CompileCtx.compute_properties. Used to check that
        properties generated by the env spec are conforming.

        :rtype: bool
        """
        for key_prop, _, _ in self.envs_expressions:
            with key_prop.diagnostic_context():
                check_source_language(
                    key_prop.type.matches(Symbol) or
                    key_prop.type.matches(Symbol.array_type()),
                    'The key expression in environment specification must be'
                    ' either a symbol or an array of symbol: got {}'
                    ' instead'.format(
                        key_prop.type.name().camel
                    )
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
