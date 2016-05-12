from langkit import names
from langkit.compiled_types import LexicalEnvType, EnvElement, Token
from langkit.expressions.base import (
    AbstractVariable, AbstractExpression, ArrayExpr, BuiltinCallExpr,
    ResolvedExpression, construct, PropertyDef
)

Env = AbstractVariable(names.Name("Current_Env"), type=LexicalEnvType)
EmptyEnv = AbstractVariable(names.Name("AST_Envs.Empty_Env"),
                            type=LexicalEnvType)


class EnvGet(AbstractExpression):
    """
    Expression for lexical environment get operation.
    """

    class Expr(ResolvedExpression):
        def __init__(self, env_expr, token_expr, resolve_unique):
            """
            :param ResolvedExpression env_expr: The expression representing the
                env to get from.
            :param ResolvedExpression token_expr: The expression representing
                the token key.
            """
            self.env_expr = env_expr
            self.token_expr = token_expr
            self.resolve_unique = resolve_unique
            self.result_var = PropertyDef.get().vars.create(
                'Env_Get_Result', self.type,
                PropertyDef.get_scope()
            )

            self.type.add_to_context()

        @property
        def type(self):
            """
            :rtype: compiled_types.ArrayType
            """
            return (
                EnvElement if self.resolve_unique else EnvElement.array_type()
            )

        def render_pre(self):
            result_expr = (
                "{} (0)" if self.resolve_unique else "Create ({})"
            ).format("AST_Envs.Get ({}, Get_Symbol ({}))".format(
                self.env_expr.render_expr(), self.token_expr.render_expr()
            ))

            return '\n'.join([
                self.env_expr.render_pre(),
                self.token_expr.render_pre(),
                '{} := {};'.format(self.result_var.name, result_expr),
            ])

        def render_expr(self):
            return str(self.result_var.name)

        def __repr__(self):
            return '<EnvGet.Expr>'

    def __init__(self, env_expr, token_expr, resolve_unique=False):
        """
        :param AbstractExpression env_expr: Expression that will yield the env
            to get the element from.
        :param AbstractExpression token_expr: Expression that will yield the
            token to use as a key on the env.
        :param bool resolve_unique: Wether we want an unique result or not.
            NOTE: For the moment, nothing will be done to ensure that only one
            result is available. The implementation will just take the first
            result.
        """
        super(EnvGet, self).__init__()
        self.env_expr = env_expr
        self.token_expr = token_expr
        self.resolve_unique = resolve_unique
        # TODO: Add a filter here. This will wait further developments in the
        # array machinery.

    def construct(self):
        return EnvGet.Expr(construct(self.env_expr, LexicalEnvType),
                           construct(self.token_expr, Token),
                           self.resolve_unique)


class EnvBind(AbstractExpression):
    """
    Expression that will evaluate a subexpression in the context of a
    particular lexical environment. Not meant to be used directly, but instead
    via the eval_in_env shortcut.
    """

    class Expr(ResolvedExpression):
        def __init__(self, env_expr, to_eval_expr):
            self.to_eval_expr = to_eval_expr
            self.env_expr = env_expr

            # Declare a variable that will hold the value of the
            # bound environment.
            self.static_type = self.to_eval_expr.type
            self.env_var = PropertyDef.get().vars.create(
                "New_Env", LexicalEnvType,
                PropertyDef.get_scope()
            )

        def render_pre(self):
            # First, compute the environment to bind using the current one and
            # store it in the "New_Env" local variable.
            result = (
                '{env_expr_pre}\n'
                '{env_var} := {env_expr};\n'
                'Inc_Ref ({env_var});'.format(
                    env_expr_pre=self.env_expr.render_pre(),
                    env_expr=self.env_expr.render_expr(),
                    env_var=self.env_var.name
                )
            )

            # Then we can compute the nested expression with the bound
            # environment.
            with Env.bind_name(self.env_var.name):
                return '{}\n{}'.format(result, self.to_eval_expr.render_pre())

        def render_expr(self):
            # We just bind the name of the environment placeholder to our
            # variable.
            with Env.bind_name(self.env_var.name):
                return self.to_eval_expr.render_expr()

        def __repr__(self):
            return '<EnvBind.Expr>'

    def __init__(self, env_expr, to_eval_expr):
        """

        :param AbstractExpression env_expr: An expression that will return a
            lexical environment in which we will eval to_eval_expr.
        :param AbstractExpression to_eval_expr: The expression to eval.
        """
        super(EnvBind, self).__init__()
        self.env_expr = env_expr
        self.to_eval_expr = to_eval_expr

    def construct(self):
        return EnvBind.Expr(construct(self.env_expr, LexicalEnvType),
                            construct(self.to_eval_expr))


class EnvOrphan(AbstractExpression):
    """
    Expression that will create a lexical environment copy with no parent.
    """

    def __init__(self, env_expr):
        """
        :param AbstractExpression env_expr: Expression that will return a
            lexical environment.
        """
        super(EnvOrphan, self).__init__()
        self.env_expr = env_expr

    def construct(self):
        return BuiltinCallExpr(
            'AST_Envs.Orphan',
            LexicalEnvType,
            [construct(self.env_expr, LexicalEnvType)],
            create_temporary='Orphan_Env'
        )


class EnvGroup(AbstractExpression):
    """
    Expression that will return a lexical environment thata logically groups
    together multiple lexical environments.
    """

    def __init__(self, *env_exprs):
        super(EnvGroup, self).__init__()
        self.env_exprs = list(env_exprs)

    def construct(self):
        env_exprs = [construct(e, LexicalEnvType) for e in self.env_exprs]
        return BuiltinCallExpr(
            'Group', LexicalEnvType,
            [ArrayExpr(env_exprs, LexicalEnvType)]
        )


class EnvGroupArray(AbstractExpression):
    """
    Expression that will return a lexical environment that logically groups
    together multiple lexical environments from an array of lexical
    environments.
    """

    def __init__(self, env_array_expr):
        """
        :param AbstractExpression env_array_expr: Expression that will return
            an array of lexical environments. If this array is empty, the empty
            environment is returned.
        """
        super(EnvGroupArray, self).__init__()
        self.env_array_expr = env_array_expr

    def construct(self):
        return BuiltinCallExpr(
            'Group', LexicalEnvType,
            [construct(self.env_array_expr, LexicalEnvType.array_type())]
        )
