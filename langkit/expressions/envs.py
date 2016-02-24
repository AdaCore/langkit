from langkit import names
from langkit.compiled_types import LexicalEnvType, EnvElement, Token
from langkit.expressions.base import (
    AbstractVariable, AbstractExpression, ResolvedExpression, construct,
    Property
)

Env = AbstractVariable(names.Name("Current_Env"), type=LexicalEnvType)


class EnvGet(AbstractExpression):
    """
    Expression for lexical environment get operation.
    """

    class Expr(ResolvedExpression):
        def __init__(self, env_expr, token_expr, resolve_unique):
            """
            :param langkit.expressions.base.ResolvedExpression env_expr: The
                expression representing the env to get from.
            :param langkit.expressions.base.ResolvedExpression token_expr: The
                expression representing the token key.
            """
            self.env_expr = env_expr
            self.token_expr = token_expr
            self.resolve_unique = resolve_unique
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
            return "{}\n{}".format(self.env_expr.render_pre(),
                                   self.token_expr.render_pre())

        def render_expr(self):
            return (
                "{} (0)" if self.resolve_unique else "Create ({})"
            ).format("AST_Envs.Get ({}, Symbol_Type ({}.Text))".format(
                self.env_expr.render_expr(), self.token_expr.render_expr()
            ))

    def __init__(self, env_expr, token_expr,
                 resolve_unique=False):
        """
        :param langkit.expressions.base.AbstractExpression env_expr:
            Expression that will yield the env to get the element from.
        :param langkit.expressions.base.AbstractExpression token_expr:
            Expression that will yield the token to use as a key on the env.
        :param bool resolve_unique: Wether we want an unique result or not.
            NOTE: For the moment, nothing will be done to ensure that only one
            result is available. The implementation will just take the first
            result.
        """
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
            self.env_var = Property.get().vars(
                names.Name("New_Env"), LexicalEnvType
            )

        def render_pre(self):
            # We assign to our environment variable the value of the result
            # of the environment expression.
            return "{}\n{}\n{} := {};".format(
                self.to_eval_expr.render_pre(), self.env_expr.render_pre(),
                self.env_var.name, self.env_expr.render_expr()
            )

        def render_expr(self):
            # We just bind the name of the environment placeholder to our
            # variable.
            with Env.bind_name(self.env_var.name):
                return self.to_eval_expr.render_expr()

        @property
        def type(self):
            return self.to_eval_expr.type

    def __init__(self, env_expr, to_eval_expr):
        """

        :param langkit.expressions.base.AbstractExpression env_expr: An
            expression that will return a lexical environment in which we will
            eval to_eval_expr.
        :param langkit.expressions.base.AbstractExpression to_eval_expr: The
            expression to eval.
        """
        self.env_expr = env_expr
        self.to_eval_expr = to_eval_expr

    def construct(self):
        return EnvBind.Expr(construct(self.env_expr, LexicalEnvType),
                            construct(self.to_eval_expr))
