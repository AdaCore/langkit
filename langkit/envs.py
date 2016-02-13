from langkit import expressions


class EnvSpec(object):
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

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
        :type add_to_env:
            (expressions.AbstractExpression, expressions.AbstractExpression)

        :param langkit.expressions.base.AbstractExpression ref_envs: if an
            AbstractExpression returning a list of environments is supplied,
            the topmost environment in the environment resolution will be
            altered to include the list of environments as referenced
            environments. TODO: Not yet implemented!

        :param expressions.AbstractExpression initial_env: If supplied,
            this env will be used as the lexical environment to execute the
            rest of the actions. For example, if you pass an initial_env, and
            add_env, then an env will be added to the env passed as
            initial_env, and the node concerned by this env specification will
            have initial_env as a parent indirectly.
        """

        self._add_env = add_env
        ":type: bool"

        self._unresolved_initial_env = initial_env
        ":type: expressions.AbstractExpression"

        self._initial_env = None
        ":type: expressions.ResolvedExpression"

        self._unresolved_add_to_env = add_to_env
        """
        :type: (expressions.AbstractExpression, expression.AbstractExpression)
        """

        self._add_to_env = None
        """
        :type: (expressions.ResolvedExpression, expressions.ResolvedExpression)
        """

        assert ref_envs is None, (
            "Ref envs is not implemented yet!"
        )

        self._ref_envs = ref_envs
        """
        :type: expressions.AbstractExpression
        """

    def check_simple_expr(self, expr):
        """
        Helper method to check that the expression is valid for evaluation
        in the env spec context.

        :param AbstractExpression expr: The expression to check.
        """
        assert (
            expr == expressions.Self
            or (isinstance(expr, expressions.FieldAccess)
                and expr.receiver == expressions.Self)
        ), ("Only simple expressions consisting of a reference to"
            " Self, or a Field/Property access on Self, are allowed in"
            " the expressions in a lexical environment specification")

    @staticmethod
    def render_expr(expr):
        """
        Helper to render a simple expression in the context of an
        environment specification.

        :param ResolvedExpression expr: The expression to render.
        :rtype: str
        """
        with expressions.Env.bind_name("Parent_Env"):
            return expr.render_expr()

    @property
    def initial_env(self):
        """
        The initial environment expression.
        :rtype: str
        """
        return self.render_expr(self._initial_env)

    @property
    def add_to_env_key(self):
        """
        The expression for the key of the environment to add in add_to_env.
        :rtype: str
        """
        return self.render_expr(self._add_to_env[0])

    @property
    def add_to_env_val(self):
        """
        The expression for the value of the environment to add in add_to_env.
        :rtype: str
        """
        return self.render_expr(self._add_to_env[1])

    def compute(self, ast_node_type):
        """
        Compute the fields necessary for code generation. For the moment,
        only self.add_to_env needs computation.

        :param langkit.compiled_types.ASTNode ast_node_type: The ASTNode
            type this environment specification is attached to.
        """
        if self._unresolved_initial_env:
            self.check_simple_expr(self._unresolved_initial_env)
            with expressions.Self.bind_type(ast_node_type):
                self._initial_env = self._unresolved_initial_env.construct()

        if self._unresolved_add_to_env:
            for e in self._unresolved_add_to_env:
                self.check_simple_expr(e)

            with expressions.Self.bind_type(ast_node_type):
                kexpr, vexpr = self._unresolved_add_to_env
                self._add_to_env = kexpr.construct(), vexpr.construct()
