from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from contextlib import contextmanager
from functools import partial

from langkit import names
from langkit.compiled_types import (
    BoolType, LexicalEnvType, Symbol, T, Token, EnvRebindingsType
)
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractVariable, AbstractExpression, ArrayExpr, BasicExpr,
    BuiltinCallExpr, GetSymbol, PropertyDef, ResolvedExpression, Self,
    auto_attr, auto_attr_custom, construct
)


class EnvVariable(AbstractVariable):
    """
    Singleton abstract variable for the implicit environment parameter.
    """

    default_name = names.Name("Current_Env")

    def __init__(self):
        super(EnvVariable, self).__init__(
            self.default_name,
            type=LexicalEnvType
        )
        self._is_bound = False

    @property
    def has_ambient_env(self):
        """
        Return whether ambient environment value is available.

        If there is one, this is either the implicit environment argument for
        the current property, or the currently bound environment (using
        eval_in_env).

        :rtype: bool
        """
        return PropertyDef.get().has_implicit_env or self.is_bound

    @property
    def is_bound(self):
        """
        Return whether Env is bound, i.e. if it can be used in the current
        context.

        :rtype: bool
        """
        return self._is_bound

    @contextmanager
    def bind(self):
        """
        Tag Env as being bound.

        This is used during the "construct" pass to check that all uses of Env
        are made in a context where it is legal.
        """
        saved_is_bound = self._is_bound
        self._is_bound = True
        yield
        self._is_bound = saved_is_bound

    @contextmanager
    def bind_default(self, prop):
        """
        Context manager to setup the default Env binding for "prop".

        This means: no binding if this property has no implicit env argument,
        and the default one if it has one.

        :type prop: PropertyDef
        """
        if prop.has_implicit_env:
            with self.bind_name(self.default_name):
                yield
        else:
            saved_is_bound = self._is_bound
            self._is_bound = False
            yield
            self._is_bound = saved_is_bound

    def construct(self):
        check_source_language(
            self.has_ambient_env,
            'This property has no implicit environment parameter: please use'
            ' the eval_in_env construct to bind an environment first.'
        )
        return super(EnvVariable, self).construct()

    def __repr__(self):
        return '<Env>'


@auto_attr_custom("get")
@auto_attr_custom("get_sequential", sequential=True)
@auto_attr_custom("resolve_unique", resolve_unique=True)
def env_get(self, env_expr, symbol_expr, resolve_unique=False,
            sequential=False, recursive=True):
    """
    Expression for lexical environment get operation.

    :param AbstractExpression env_expr: Expression that will yield the env
        to get the element from.
    :param AbstractExpression|str symbol_expr: Expression that will yield the
        symbol to use as a key on the env, or a string to turn into a symbol.
    :param bool resolve_unique: Wether we want an unique result or not.
        NOTE: For the moment, nothing will be done to ensure that only one
        result is available. The implementation will just take the first
        result.
    :param bool sequential: Whether resolution needs to be sequential or not.
    :param bool recursive: Whether lookup must be performed recursively on
        parent environments.
    """

    current_prop = PropertyDef.get()
    if current_prop:
        current_prop.set_uses_env()

    if not isinstance(symbol_expr, (AbstractExpression, basestring)):
        check_source_language(
            False,
            'Invalid key argument for Env.get: {}'.format(repr(symbol_expr))
        )

    sym_expr = construct(symbol_expr)
    if sym_expr.type == Token:
        sym_expr = GetSymbol.construct_static(sym_expr)
    check_source_language(
        sym_expr.type == Symbol,
        "Wrong type for symbol expr: {}".format(sym_expr.type)
    )

    sub_exprs = [construct(env_expr, LexicalEnvType), sym_expr]

    if sequential:
        # Pass the From parameter if the user wants sequential semantics
        array_expr = ('AST_Envs.Get'
                      '  (Self => {},'
                      '   Key => {},'
                      '   From => {},'
                      '   Recursive => {})')
        sub_exprs.append(construct(Self, T.root_node))
    else:
        array_expr = 'AST_Envs.Get (Self => {}, Key => {}, Recursive => {})'
    sub_exprs.append(construct(recursive, BoolType))

    make_expr = partial(BasicExpr, result_var_name="Env_Get_Result",
                        operands=sub_exprs, abstract_expr=self)

    if resolve_unique:
        return make_expr("Get ({}, 0)".format(array_expr),
                         T.root_node.env_el())
    else:
        T.root_node.env_el().array_type().add_to_context()
        return make_expr("Create ({})".format(array_expr),
                         T.root_node.env_el().array_type())


class EnvBindExpr(ResolvedExpression):

    def __init__(self, env_expr, to_eval_expr, abstract_expr=None):
        self.to_eval_expr = to_eval_expr
        self.env_expr = env_expr

        # Declare a variable that will hold the value of the
        # bound environment.
        self.static_type = self.to_eval_expr.type
        self.env_var = PropertyDef.get().vars.create("New_Env",
                                                     LexicalEnvType)

        super(EnvBindExpr, self).__init__(abstract_expr=abstract_expr)

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

    @property
    def subexprs(self):
        return {'env': self.env_expr, 'expr': self.to_eval_expr}

    def __repr__(self):
        return '<EnvBind.Expr>'


@auto_attr
def eval_in_env(self, env_expr, to_eval_expr):
    """
    Expression that will evaluate a subexpression in the context of a
    particular lexical environment. Not meant to be used directly, but instead
    via the eval_in_env shortcut.

    :param AbstractExpression env_expr: An expression that will return a
        lexical environment in which we will eval to_eval_expr.
    :param AbstractExpression to_eval_expr: The expression to eval.
    """
    env_resolved_expr = construct(env_expr, LexicalEnvType)
    with Env.bind():
        return EnvBindExpr(env_resolved_expr, construct(to_eval_expr),
                           abstract_expr=self)


@auto_attr
def env_orphan(self, env_expr):
    """
    Expression that will create a lexical environment copy with no parent.

    :param AbstractExpression env_expr: Expression that will return a
        lexical environment.
    """
    return BuiltinCallExpr(
        'AST_Envs.Orphan',
        LexicalEnvType,
        [construct(env_expr, LexicalEnvType)],
        'Orphan_Env',
        abstract_expr=self,
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
def env_group(self, env_array_expr):
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
        'Group_Env',
        abstract_expr=self,
    )


@auto_attr
def is_visible_from(self, referenced_env, base_env):
    """
    Expression that will return whether an env's associated compilation unit is
    visible from another env's compilation unit.

    :param AbstractExpression base_env: The environment from which we want
        to check visibility.
    :param AbstractExpression referenced_env: The environment referenced
        from base_env, for which we want to check visibility.
    """
    return BuiltinCallExpr(
        'Is_Visible_From', BoolType,
        [construct(referenced_env, LexicalEnvType),
         construct(base_env, LexicalEnvType)],
        abstract_expr=self,
    )


@auto_attr
def env_node(self, env):
    """
    Return the node associated to this environment.

    :param AbstractExpression env: The source environment.
    """
    return BasicExpr('{}.Node', T.root_node, [construct(env, LexicalEnvType)],
                     abstract_expr=self)


@auto_attr
def env_parent(self, env):
    """
    Return this env's parent env.

    :param AbstractExpression env: The source environment.
    """
    return BasicExpr(
        'AST_Envs.Get_Env ({}.Parent)',
        T.LexicalEnvType, [construct(env, LexicalEnvType)],
        abstract_expr=self,
    )


def make_combine(self, l_rebindings, r_rebindings):
    return BasicExpr('AST_Envs.Combine ({}, {})',
                     EnvRebindingsType, [l_rebindings, r_rebindings],
                     abstract_expr=self)


@auto_attr
def combine(self, l_rebindings, r_rebindings):
    """
    Combine the two env rebindings given as arguments.
    """
    return make_combine(self,
                        construct(l_rebindings, EnvRebindingsType),
                        construct(r_rebindings, EnvRebindingsType))


@auto_attr
def rebind_env(self, env, to_rebind, rebind_to):
    """
    Returns a new environment based on `env` where `to_rebind` is rebound to
    `rebind_to`.
    """
    return BuiltinCallExpr(
        'Rebind_Env', LexicalEnvType,
        [construct(env, LexicalEnvType),
         construct(to_rebind, LexicalEnvType),
         construct(rebind_to, LexicalEnvType)],
        'Rebound_Env',
        abstract_expr=self,
    )


@auto_attr
def make_env_el(self, node):
    """
    Construct an env element from node, including context (env rebindings).
    """
    from langkit.expressions import New

    p = PropertyDef.get()
    check_source_language(p, "make_env_el has to be used in a property")
    p.set_uses_env()

    node_expr = construct(node, T.root_node)
    md_expr = New.StructExpr(T.env_md, {f.name: construct(False)
                                        for f in T.env_md.get_fields()})
    return New.StructExpr(
        node_expr.type.env_el(),
        {names.Name('md'): md_expr,
         names.Name('el'): node_expr,
         names.Name('parents_bindings'): construct(p.env_rebinding_arg.var)},
        abstract_expr=self,
    )


Env = EnvVariable()
EmptyEnv = AbstractVariable(names.Name("AST_Envs.Empty_Env"),
                            type=LexicalEnvType)
