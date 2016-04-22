from itertools import count

from langkit import names
from langkit.compiled_types import (
    AbstractNodeData, LexicalEnvType, StructMetaClass, Symbol
)
from langkit.diagnostics import check_source_language
from langkit.expressions import Env, FieldAccess, PropertyDef, Self, construct


class EnvSpec(object):
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

    PROPERTY_COUNT = count(0)

    def __init__(self,
                 add_env=False,
                 add_to_env=None,
                 ref_envs=None,
                 initial_env=None):
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
        """

        unr_key, unr_value = add_to_env if add_to_env else (None, None)

        assert ref_envs is None, (
            "Ref envs is not implemented yet!"
        )

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

        self._unresolved_env_key = unr_key
        ":type: AbstractExpression"

        self._unresolved_env_value = unr_value
        ":type: AbstractExpression"

        self._unresolved_ref_envs = ref_envs
        ":type: AbstractExpression"

        # These are the property attributes

        self.initial_env = None
        ":type: PropertyDef"

        self.add_to_env_key = None
        ":type: PropertyDef"

        self.add_to_env_value = None
        ":type: PropertyDef"

        self.ref_envs = None
        ":type: PropertyDef"

    def create_properties(self):
        """
        Turn the various abstract expression attributes for this env spec into
        internal properties.

        :rtype: None
        """
        result = []

        def create_internal_property(name, expr, type):
            if expr is None:
                return None

            p = PropertyDef(
                AbstractNodeData.PREFIX_INTERNAL,
                expr,
                name=names.Name('_{}_{}'.format(name,
                                                next(self.PROPERTY_COUNT))),
                private=True, type=type
            )
            result.append(p)
            return p

        # We are doing this when creating ASTNode subclasses, so there's no
        # context yet. So fetch the root grammar class in StructMetaClass
        # instead.
        node_type = StructMetaClass.root_grammar_class

        self.initial_env = create_internal_property(
            'Initial_Env', self._unresolved_initial_env, LexicalEnvType
        )

        # This property can return either a single symbol or an array of these,
        # so we cannot specify a single type here. So we will do this just
        # before using it: see add_to_env_key_expr.
        self.add_to_env_key = create_internal_property(
            'Env_Key', self._unresolved_env_key, None
        )
        self.add_to_env_value = create_internal_property(
            'Env_Value', self._unresolved_env_value, node_type
        )

        # TODO: what is the expected type for this one?
        self.ref_envs = create_internal_property(
            'Ref_Envs', self._unresolved_ref_envs, None
        )

        return result

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
    def add_to_env_key_expr(self):
        """
        The expression for the key of the environment to add in add_to_env.
        :rtype: str
        """
        key_prop = self.add_to_env_key
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
        return self._render_field_access(self.add_to_env_key)

    @property
    def add_to_env_value_expr(self):
        """
        The expression for the value of the environment to add in add_to_env.
        :rtype: str
        """
        return self._render_field_access(self.add_to_env_value)

    @property
    def is_adding_to_env(self):
        """
        Return whether this specification adds an entry to the environment.
        :rtype: bool
        """
        return bool(self.add_to_env_key)
