from langkit import expressions


class EnvSpec(object):
    """
    Class defining a lexical environment specification for an ASTNode subclass.
    """

    def __init__(self, add_env=False, add_to_env=None, ref_envs=None):
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

        :param AbstractExpression ref_envs: if an AbstractExpression
            returning a list of environments is supplied, the topmost
            environment in the environment resolution will be altered to
            include the list of environments as referenced environments.
            TODO: Not yet implemented!
        """
        self._add_env = add_env
        ":type: bool"

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

    def check_add_to_env_expr(self):
        """
        Helper method to check that the expressions in add_to_env are valid.
        """
        if self._add_to_env:
            for e in self._unresolved_add_to_env:
                assert (
                    e == expressions.Self
                    or (isinstance(e, expressions.FieldAccess)
                        and e.receiver == expressions.Self)
                ), ("Only simple expressions consisting of a reference to"
                    " Self, or a Field/Property access on Self, are allowed in"
                    " the expressions for add_to_env")

    def compute(self, ast_node_type):
        """
        Compute the fields necessary for code generation. For the moment,
        only self.add_to_env needs computation.

        :param langkit.compiled_types.ASTNode ast_node_type: The ASTNode
            type this environment specification is attached to.
        """

        self.check_add_to_env_expr()

        if self._unresolved_add_to_env:
            with expressions.Self.bind(ast_node_type):
                kexpr, vexpr = self._unresolved_add_to_env
                self._add_to_env = kexpr.construct(), vexpr.construct()
