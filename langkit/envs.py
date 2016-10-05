from collections import namedtuple
from itertools import count

from langkit import names
from langkit.compiled_types import AbstractNodeData, LexicalEnvType, Symbol, T
from langkit.diagnostics import check_source_language
from langkit.expressions import Env, FieldAccess, PropertyDef, Self, construct


AddToEnv = namedtuple("AddToEnv", ["key", "val", "dest_env",
                                   "metadata", "is_post"])


def add_to_env(key, val, dest_env=None, metadata=None, is_post=False):
    """
    Specify elements to add to the lexical environment.

    :param AbstractExpression key: An abstract expression resolving either
        to a symbol, or to a list of symbols, specifying the key(s) under which
        to add elements.
    :param AbstractExpression val: An abstract expression resolving to a
        subtype of the root class, or a list of them, specifying the values
        to add.
    :param AbstractExpression dest_env: The destination environment in which to
        add the elements.
    :param AbstractExpression metadata: Optional expression for metadata.
    :param bool is_post: Whether to execute the add_to_env action after
        children have been treated.
    :return:
    """
    return AddToEnv(key, val, dest_env, metadata, is_post)


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

            p = PropertyDef(
                expr, AbstractNodeData.PREFIX_INTERNAL,
                name=names.Name('_{}_{}'.format(name,
                                                next(self.PROPERTY_COUNT))),
                private=True, type=type
            )
            result.append(p)
            return p

        self.initial_env = create_internal_property(
            'Initial_Env', self._unresolved_initial_env, LexicalEnvType
        )

        self.envs_expressions = [
            add_to_env(
                create_internal_property('Env_Key', exprs.key, None),
                create_internal_property('Env_Value', exprs.val, None),
                create_internal_property('Env_Dest', exprs.dest_env,
                                         LexicalEnvType),
                create_internal_property('MD', exprs.metadata, T.env_md),
                exprs.is_post
            ) for exprs in self._unresolved_envs_expressions
        ]

        self.has_post_actions = any([e.is_post for e in self.envs_expressions])

        self.ref_envs = create_internal_property(
            'Ref_Envs', self._unresolved_ref_envs, LexicalEnvType.array_type()
        )

        self.env_hook_arg = create_internal_property(
            'Env_Hook_Arg', self._unresolved_env_hook_arg, T.root_node
        )

        return result

    def prepare(self):
        """
        Method call by CompileCtx.compute_properties. Used to check that
        properties generated by the env spec are conforming.

        :rtype: bool
        """
        for key_prop, val_prop, _, _, _ in self.envs_expressions:
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

                check_source_language(
                    val_prop.type.matches(T.root_node)
                    or (val_prop.type.is_collection
                        and val_prop.type.element_type().matches(T.root_node)),
                    'The val expression in environment specification must be'
                    ' either a node or an array of nodes: got {}'
                    ' instead'.format(
                        val_prop.type.name().camel
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
