from functools import partial

from langkit import names
from langkit.compiled_types import (
    EnvElement, LexicalEnvType, Token, BoolType, T
)
from langkit.expressions.base import (
    AbstractVariable, AbstractExpression, ArrayExpr, BuiltinCallExpr,
    ResolvedExpression, construct, PropertyDef, BasicExpr, auto_attr,
    auto_attr_custom
)

Env = AbstractVariable(names.Name("Current_Env"), type=LexicalEnvType)
EmptyEnv = AbstractVariable(names.Name("AST_Envs.Empty_Env"),
                            type=LexicalEnvType)


@auto_attr_custom("get")
@auto_attr_custom("resolve_unique", resolve_unique=True)
def env_get(env_expr, token_expr, resolve_unique=False):
    """
    Expression for lexical environment get operation.

    :param AbstractExpression env_expr: Expression that will yield the env
        to get the element from.
    :param AbstractExpression token_expr: Expression that will yield the
        token to use as a key on the env.
    :param bool resolve_unique: Wether we want an unique result or not.
        NOTE: For the moment, nothing will be done to ensure that only one
        result is available. The implementation will just take the first
        result.
    """

    array_expr = 'AST_Envs.Get ({}, Get_Symbol ({}))'

    make_expr = partial(
        BasicExpr, result_var_name="Env_Get_Result",
        sub_exprs=[construct(env_expr, LexicalEnvType),
                   construct(token_expr, Token)]
    )

    if resolve_unique:
        return make_expr("Get ({}, 0)".format(array_expr), EnvElement)
    else:
        EnvElement.array_type().add_to_context()
        return make_expr("Create ({})".format(array_expr),
                         EnvElement.array_type())


class EnvBindExpr(ResolvedExpression):

    def __init__(self, env_expr, to_eval_expr):
        self.to_eval_expr = to_eval_expr
        self.env_expr = env_expr

        # Declare a variable that will hold the value of the
        # bound environment.
        self.static_type = self.to_eval_expr.type
        self.env_var = PropertyDef.get().vars.create("New_Env",
                                                     LexicalEnvType)

        super(EnvBindExpr, self).__init__()

    def _render_pre(self):
        # First, compute the environment to bind using the current one and
        # store it in the "New_Env" local variable.
        #
        # We need to keep the environment live during the bind operation.
        # That is why we store this environment in a temporary so that it
        # is automatically deallocated when leaving the scope.
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

    def _render_expr(self):
        # We just bind the name of the environment placeholder to our
        # variable.
        with Env.bind_name(self.env_var.name):
            return self.to_eval_expr.render_expr()

    def __repr__(self):
        return '<EnvBind.Expr>'


@auto_attr
def eval_in_env(env_expr, to_eval_expr):
    """
    Expression that will evaluate a subexpression in the context of a
    particular lexical environment. Not meant to be used directly, but instead
    via the eval_in_env shortcut.

    :param AbstractExpression env_expr: An expression that will return a
        lexical environment in which we will eval to_eval_expr.
    :param AbstractExpression to_eval_expr: The expression to eval.
    """
    return EnvBindExpr(construct(env_expr, LexicalEnvType),
                       construct(to_eval_expr))


@auto_attr
def env_orphan(env_expr):
    """
    Expression that will create a lexical environment copy with no parent.

    :param AbstractExpression env_expr: Expression that will return a
        lexical environment.
    """
    return BuiltinCallExpr(
        'AST_Envs.Orphan',
        LexicalEnvType,
        [construct(env_expr, LexicalEnvType)],
        'Orphan_Env'
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
            [ArrayExpr(env_exprs, LexicalEnvType)],
            'Group_Env'
        )


@auto_attr
def env_group(env_array_expr):
    """
    Expression that will return a lexical environment that logically groups
    together multiple lexical environments from an array of lexical
    environments.

    :param AbstractExpression env_array_expr: Expression that will return
        an array of lexical environments. If this array is empty, the empty
        environment is returned.
    """
    return BuiltinCallExpr(
        'Group', LexicalEnvType,
        [construct(env_array_expr, LexicalEnvType.array_type())],
        'Group_Env'
    )


@auto_attr
def is_visible_from(referenced_env, base_env):
    """
    Expression that will return whether an env's associated compilation unit is
    visible from another env's compilation unit.

    TODO: This is mainly exposed on envs because the CompilationUnit type is
    not exposed in the DSL yet. We might want to change that eventually if
    there are other compelling reasons to do it.
    """
    return BuiltinCallExpr(
        'Is_Visible_From', BoolType,
        [construct(base_env, LexicalEnvType),
         construct(referenced_env, LexicalEnvType)]
    )


@auto_attr
def env_node(env):
    """
    Return the node associated to this environment.
    """
    return BasicExpr('{}.Node', T.root_node, [construct(env, LexicalEnvType)])
