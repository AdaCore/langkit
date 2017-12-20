from __future__ import absolute_import, division, print_function

from langkit import names
from langkit.compiled_types import T, no_compiled_type
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, CallExpr, ComputingExpr,
    FieldAccessExpr, GetSymbol, LiteralExpr, NullCheckExpr, NullExpr,
    PropertyDef, Self, attr_call, auto_attr, construct, dsl_document
)
from langkit.expressions.utils import array_aggr, assign_var


@attr_call('get')
def get(env, symbol, recursive=True):
    """
    Perform a lexical environment lookup. Look for nodes that are associated to
    the given `symbol` in the `env` lexical environment.

    If `recursive` is true (the default), do a recursive lookup in parent
    environments and referenced ones. Otherwise, only look into `env`.
    """
    return EnvGet(env, symbol, recursive=recursive)


@attr_call('get_sequential')
def get_sequential(env, symbol, sequential_from, recursive=True):
    """
    Like :dsl:`get`, but do a sequential lookup: discard AST nodes that belong
    to the same unit as the `sequential_from` node and that appear before it.
    """
    return EnvGet(env, symbol, sequential=True,
                  sequential_from=sequential_from, recursive=recursive)


@attr_call('get_first')
def get_first(env, symbol, recursive=True):
    """
    Like :dsl:`get`, but only return the first entity found, or a null entity
    if no entity is found.
    """
    return EnvGet(env, symbol, recursive=recursive, only_first=True)


@attr_call('get_first_sequential')
def get_first_sequential(env, symbol, sequential_from, recursive=True):
    """
    Like :dsl:`get_sequential`, but only return the first entity found, or a
    null entity if no entity is found.
    """
    return EnvGet(env, symbol, sequential=True,
                  sequential_from=sequential_from, recursive=recursive,
                  only_first=True)


class EnvGet(AbstractExpression):
    """
    Expression to perform a lexical environment lookup.
    """

    class Expr(ComputingExpr):
        def __init__(self, env_expr, key_expr, recursive_expr,
                     sequential=False, sequential_from=None,
                     only_first=False, abstract_expr=None):
            self.env_expr = env_expr
            self.key_expr = key_expr
            self.recursive_expr = recursive_expr
            self.sequential = sequential
            self.sequential_from = sequential_from

            self.static_type = (
                T.root_node.entity if only_first
                else T.root_node.entity.array
            )

            self.only_first = only_first
            super(EnvGet.Expr, self).__init__('Env_Get_Result',
                                              abstract_expr=abstract_expr)

            PropertyDef.get().set_uses_envs()

        def _render_pre(self):
            result = [
                self.env_expr.render_pre(),
                self.key_expr.render_pre(),
                self.recursive_expr.render_pre(),
            ]
            args = [('Self', self.env_expr.render_expr()),
                    ('Key', self.key_expr.render_expr()),
                    ('Recursive', self.recursive_expr.render_expr())]

            # Pass the From parameter if the user wants sequential semantics
            if self.sequential_from:
                result.append(self.sequential_from.render_pre())
                args.append(('From', self.sequential_from.render_expr()))

            if self.only_first:
                result_expr = 'AST_Envs.Get_First ({})'.format(
                    ', '.join('{} => {}'.format(n, v) for n, v in args)
                )
            else:
                result_expr = 'Create (AST_Envs.Get ({}))'.format(
                    ', '.join('{} => {}'.format(n, v) for n, v in args)
                )

            # In both cases above, the expression is going to be a function
            # call that returns a new ownership share, so there is no need for
            # an inc-ref for the storage in the result variable.
            result.append(assign_var(self.result_var.ref_expr,
                                     result_expr,
                                     requires_incref=False))

            return '\n'.join(result)

        @property
        def subexprs(self):
            return {
                'env': self.env_expr,
                'key': self.key_expr,
                'recursive': self.recursive_expr,
                'sequential_from': self.sequential_from,
            }

    def __init__(self, env, symbol, sequential=False, sequential_from=Self,
                 recursive=True, only_first=False):
        """
        :param AbstractExpression env: Expression that will yield the env to
            get the element from.
        :param AbstractExpression|str symbol: Expression that will yield the
            symbol to use as a key on the env, or a string to turn into a
            symbol.
        :param bool sequential: Whether resolution needs to be sequential or
            not.
        :param AbstractExpression sequential_from: If resolution needs to be
            sequential, must be an expression to use as the reference node.
        :param AbstractExpression recursive: Expression that must return a
            boolean, which controls whether lookup must be performed
            recursively on parent environments.
        """
        super(EnvGet, self).__init__()

        check_source_language(
            isinstance(symbol, (AbstractExpression, basestring)),
            'Invalid key argument for Env.get: {}'.format(repr(symbol))
        )

        self.env = env
        self.symbol = symbol
        self.sequential = sequential
        self.sequential_from = sequential_from
        self.recursive = recursive
        self.only_first = only_first

    def construct(self):
        env_expr = construct(self.env, T.LexicalEnvType)

        sym_expr = construct(self.symbol)
        if sym_expr.type == T.TokenType:
            sym_expr = GetSymbol.construct_static(sym_expr)
        check_source_language(
            sym_expr.type == T.SymbolType,
            'Wrong type for symbol expr: {}'.format(sym_expr.type.dsl_name)
        )

        from_expr = (construct(self.sequential_from, T.root_node)
                     if self.sequential else None)

        recursive_expr = construct(self.recursive, T.BoolType)

        return EnvGet.Expr(env_expr, sym_expr, recursive_expr, self.sequential,
                           from_expr, self.only_first,
                           abstract_expr=self)

    def __repr__(self):
        return '<EnvGet({}, {})>'.format(self.env, self.symbol)


@auto_attr
def env_orphan(self, env):
    """
    Return a copy of the `env` lexical environment which has no parent.

    :param AbstractExpression env: Expression that will return a
        lexical environment.
    """
    return CallExpr('Orphan_Env', 'AST_Envs.Orphan', T.LexicalEnvType,
                    [construct(env, T.LexicalEnvType)],
                    abstract_expr=self)


@dsl_document
class EnvGroup(AbstractExpression):
    """
    Like :dsl:`env_group`, but take a fixed list of input lexical environments.
    """

    def __init__(self, *env_exprs):
        super(EnvGroup, self).__init__()
        self.env_exprs = list(env_exprs)

    def construct(self):
        env_exprs = [construct(e, T.LexicalEnvType) for e in self.env_exprs]
        array_arg = LiteralExpr(array_aggr(['{}' for _ in env_exprs]),
                                no_compiled_type, env_exprs)
        return CallExpr('Group_Env', 'Group', T.LexicalEnvType,
                        [array_arg],
                        abstract_expr=self)

    def __repr__(self):
        return '<EnvGroup>'


@auto_attr
def env_group(self, env_array, with_md=None):
    """
    Return a new lexical environment that logically groups together multiple
    environments. `env_array` must be an array that contains the environments
    to be grouped. If it is empty, the empty environment is returned.

    If provided, `with_md` must be a metadata structure: it will be made the
    default metadata for this lexical environment.

    :type env_array: AbstractExpression
    :type with_md: AbstractExpression
    """
    from langkit.expressions import No

    if not with_md:
        with_md = No(T.env_md)

    return CallExpr('Group_Env', 'Group', T.LexicalEnvType,
                    [construct(env_array, T.LexicalEnvType.array),
                     construct(with_md, T.env_md)],
                    abstract_expr=self)


@auto_attr
def is_visible_from(self, referenced_env, base_env):
    """
    Return whether the analysis unit associated to the `referenced_env` lexical
    environment is visible from `base_env`'s.

    :param AbstractExpression base_env: The environment from which we want
        to check visibility.
    :param AbstractExpression referenced_env: The environment referenced
        from base_env, for which we want to check visibility.
    """
    return IsVisibleFromExpr(referenced_env, base_env, abstract_expr=self)


class IsVisibleFromExpr(CallExpr):
    def __init__(self, referenced_env, base_env, abstract_expr=None):
        super(IsVisibleFromExpr, self).__init__(
            'Is_Visible', 'Is_Visible_From', T.BoolType,
            [construct(referenced_env, T.LexicalEnvType),
             construct(base_env, T.LexicalEnvType)],
            abstract_expr=abstract_expr
        )

        PropertyDef.get().set_uses_envs()


@auto_attr
def env_node(self, env):
    """
    Return the node associated to the `env` environment.

    :param AbstractExpression env: The source environment.
    """
    return CallExpr('Env_Node', 'AST_Envs.Env_Node', T.root_node,
                    [construct(env, T.LexicalEnvType)],
                    abstract_expr=self)


@auto_attr
def env_parent(self, env):
    """
    Return the parent of the `env` lexical environment.

    :param AbstractExpression env: The source environment.
    """
    return CallExpr(
        'Env_Parent', 'AST_Envs.Parent', T.LexicalEnvType,
        [construct(env, T.LexicalEnvType)],
        abstract_expr=self,
    )


def make_append_rebinding(self, rebindings, old_env, new_env):
    return CallExpr('Rebinding', 'AST_Envs.Append_Rebinding',
                    T.EnvRebindingsType,
                    [rebindings, old_env, new_env],
                    abstract_expr=self)


@auto_attr
def append_rebinding(self, rebindings, old_env, new_env):
    """
    Functionally append a rebinding from `old_env` to `new_env` (two lexical
    environments) on top of `rebindings` (a chain of rebindings).
    """
    return make_append_rebinding(
        self,
        construct(rebindings, T.EnvRebindingsType),
        construct(old_env, T.LexicalEnvType),
        construct(new_env, T.LexicalEnvType)
    )


def construct_non_null_rebindings(rebindings):
    """
    Construct `rebindings` to be an environment rebindings value and wrap it in
    a non-null check.

    :param AbstractExpression rebindings: Rebindings-returning expression to
        process.
    :rtype: ResolvedExpression
    """
    return NullCheckExpr(construct(rebindings, T.EnvRebindingsType))


@auto_attr
def rebindings_old_env(self, rebindings):
    """
    Return the lexical environment that is remapped by `rebindings`.

    :param AbstractExpression rebindings: Input expression, must evaluate to a
        rebindings.
    :rtype: ResolvedExpression
    """
    return FieldAccessExpr(construct_non_null_rebindings(rebindings),
                           'Old_Env', T.LexicalEnvType,
                           do_explicit_incref=True, abstract_expr=self)


@auto_attr
def rebindings_new_env(self, rebindings):
    """
    Return the lexical environment that `rebindings` remaps to.

    :param AbstractExpression rebindings: Input expression, must evaluate to a
        rebindings.
    :rtype: ResolvedExpression
    """
    return FieldAccessExpr(construct_non_null_rebindings(rebindings),
                           'New_Env', T.LexicalEnvType,
                           do_explicit_incref=True, abstract_expr=self)


@auto_attr
def rebindings_parent(self, rebindings):
    """
    Return the parent rebindings for `rebindings`.

    :param AbstractExpression rebindings: Input expression, must evaluate to a
        rebindings.
    :rtype: ResolvedExpression
    """
    return FieldAccessExpr(construct_non_null_rebindings(rebindings),
                           'Parent', T.EnvRebindingsType,
                           do_explicit_incref=False, abstract_expr=self)


@auto_attr
def concat_rebindings(self, lhs, rhs):
    """
    Combine rebindings from the `lhs` and `rhs` environment rebindings.
    """
    return CallExpr('Rebinding', 'AST_Envs.Combine',
                    T.EnvRebindingsType,
                    [construct(lhs, T.EnvRebindingsType),
                     construct(rhs, T.EnvRebindingsType)],
                    abstract_expr=self)


@auto_attr
def rebind_env(self, env, rebindings):
    """
    Return a new environment based on `env` to include the given `rebindings`.
    """
    return CallExpr('Rebound_Env', 'Rebind_Env', T.LexicalEnvType,
                    [construct(env, T.LexicalEnvType),
                     construct(rebindings, T.EnvRebindingsType)],
                    abstract_expr=self)


def make_as_entity(node_expr, entity_info=None, null_check=True,
                   abstract_expr=None):
    """
    Helper for as_entity. Takes a resolved expression instead of an abstract
    one.

    :param ResolvedExpression node_expr: The AST node expression to wrap as an
        entity.
    :param ResolvedExpression|None entity_info: Expression to use as the entity
        information. If provided, its type must be T.entity_info. Otherwise,
        the ambient entity info is used.
    """
    from langkit.expressions import If, IsNull, New

    entity_type = node_expr.type.entity

    # If we use the ambient entity info, make the current property an entity
    # one.
    if entity_info is None:
        p = PropertyDef.get()
        p.set_uses_entity_info()
        entity_info = construct(p.entity_info_arg)

    # Expression tree sharing is forbidden, so if we need to reference the
    # result of the input node expression multiple times, create a variable to
    # hold the input node.
    node_ref = (node_expr.create_result_var('Node_For_Entity')
                if null_check else node_expr)

    entity_expr = New.StructExpr(
        entity_type, {names.Name('El'): node_ref,
                      names.Name('Info'): entity_info},
        result_var_name=names.Name.from_lower('as_entity'),
    )

    result = If.Expr(
        IsNull.construct_static(node_expr),
        NullExpr(entity_type),
        entity_expr,
        entity_type,
        abstract_expr=abstract_expr
    ) if null_check else entity_expr

    result.abstract_expr = abstract_expr
    return result


@auto_attr
def as_entity(self, node):
    """
    Wrap `node` into an entity. This uses environment rebindings from the
    context.
    """

    p = PropertyDef.get()
    check_source_language(p, "as_entity has to be used in a property")

    check_source_language(
        p._uses_entity_info is not False,
        'This property has been explicitely tagged as not using entity info,'
        ' so .as_entity is invalid here'
    )

    # We want to keep original type of node, so no downcast
    node_expr = construct(node, T.root_node, downcast=False)

    ret = make_as_entity(node_expr, abstract_expr=self)
    ret.create_result_var('Ent')
    return ret


@auto_attr
def as_bare_entity(self, node):
    """
    Wrap `node` into an entity, using default entity information (in
    particular, no rebindings).
    """
    node_expr = construct(node, T.root_node, downcast=False)
    ret = make_as_entity(node_expr, entity_info=NullExpr(T.entity_info),
                         abstract_expr=self)
    ret.create_result_var('Ent')
    return ret


EmptyEnv = AbstractVariable(names.Name("AST_Envs.Empty_Env"),
                            type=T.LexicalEnvType)
