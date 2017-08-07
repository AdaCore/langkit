from __future__ import absolute_import, division, print_function

from functools import partial

from langkit import names
from langkit.compiled_types import (
    T, bool_type, lexical_env_type, env_rebindings_type, no_compiled_type,
    symbol_type, token_type
)
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, BasicExpr, CallExpr, GetSymbol,
    LiteralExpr, NullExpr, PropertyDef, Self, auto_attr, auto_attr_custom,
    construct
)
from langkit.expressions.utils import array_aggr


@auto_attr_custom("get")
@auto_attr_custom("get_sequential", sequential=True)
@auto_attr_custom("resolve_unique", resolve_unique=True)
def env_get(self, env_expr, symbol_expr, resolve_unique=False,
            sequential=False, sequential_from=Self, recursive=True):
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
    :param AbstractExpression sequential_from: If resolution needs to be
        sequential, must be an expression to use as the reference node.
    :param bool recursive: Whether lookup must be performed recursively on
        parent environments.
    """
    check_source_language(
        isinstance(symbol_expr, (AbstractExpression, basestring)),
        'Invalid key argument for Env.get: {}'.format(repr(symbol_expr))
    )

    sym_expr = construct(symbol_expr)
    if sym_expr.type == token_type:
        sym_expr = GetSymbol.construct_static(sym_expr)
    check_source_language(
        sym_expr.type == symbol_type,
        "Wrong type for symbol expr: {}".format(sym_expr.type)
    )

    args = [('Self', construct(env_expr, lexical_env_type)),
            ('Key', sym_expr),
            ('Recursive', construct(recursive, bool_type))]

    # Pass the From parameter if the user wants sequential semantics
    if sequential:
        args.append(('From', construct(sequential_from, T.root_node)))

    array_expr = 'AST_Envs.Get ({})'.format(', '.join('{} => {{}}'.format(n)
                                                      for n, _ in args))

    # In both cases below, the BasicExpr template is going to be a function
    # call that returns a new ownership share, so there is no need for an
    # inc-ref for the storage in the result variable.
    make_expr = partial(BasicExpr, 'Env_Get_Result',
                        operands=[e for _, e in args],
                        requires_incref=False,
                        abstract_expr=self)

    if resolve_unique:
        return make_expr("Get ({}, 0)".format(array_expr),
                         T.root_node.entity)
    else:
        return make_expr("Create ({})".format(array_expr),
                         T.root_node.entity.array)


@auto_attr
def env_orphan(self, env_expr):
    """
    Expression that will create a lexical environment copy with no parent.

    :param AbstractExpression env_expr: Expression that will return a
        lexical environment.
    """
    return CallExpr('Orphan_Env', 'AST_Envs.Orphan', lexical_env_type,
                    [construct(env_expr, lexical_env_type)],
                    abstract_expr=self)


class EnvGroup(AbstractExpression):
    """
    Expression that will return a lexical environment that logically groups
    together multiple lexical environments.
    """

    def __init__(self, *env_exprs):
        super(EnvGroup, self).__init__()
        self.env_exprs = list(env_exprs)

    def construct(self):
        env_exprs = [construct(e, lexical_env_type) for e in self.env_exprs]
        array_arg = LiteralExpr(array_aggr(['{}' for _ in env_exprs]),
                                no_compiled_type, env_exprs)
        return CallExpr('Group_Env', 'Group', lexical_env_type,
                        [array_arg],
                        abstract_expr=self)

    def __repr__(self):
        return '<EnvGroup>'


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
    return CallExpr('Group_Env', 'Group', lexical_env_type,
                    [construct(env_array_expr, lexical_env_type.array)],
                    abstract_expr=self)


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
    return CallExpr('Is_Visible', 'Is_Visible_From', bool_type,
                    [construct(referenced_env, lexical_env_type),
                     construct(base_env, lexical_env_type)],
                    abstract_expr=self)


@auto_attr
def env_node(self, env):
    """
    Return the node associated to this environment.

    :param AbstractExpression env: The source environment.
    """
    return BasicExpr('Env_Node', '{}.Node', T.root_node,
                     [construct(env, lexical_env_type)],
                     abstract_expr=self)


@auto_attr
def env_parent(self, env):
    """
    Return this env's parent env.

    :param AbstractExpression env: The source environment.
    """
    parent_env_getter = LiteralExpr(lexical_env_type, '{}.Parent',
                                    [construct(env, lexical_env_type)])
    return CallExpr(
        'Parent', 'AST_Envs.Get_Env', T.lexical_env_type, [parent_env_getter],
        abstract_expr=self,
    )


def make_append_rebinding(self, rebindings, to_rebind, rebind_to):
    return CallExpr('Rebinding', 'AST_Envs.Append_Rebinding',
                    env_rebindings_type,
                    [rebindings, to_rebind, rebind_to],
                    abstract_expr=self)


@auto_attr
def append_rebinding(self, rebindings, to_rebind, rebind_to):
    """
    Functionally append a rebinding of to_rebind -> rebind_to to rebindings.
    """
    return make_append_rebinding(
        self,
        construct(rebindings, env_rebindings_type),
        construct(to_rebind, lexical_env_type),
        construct(rebind_to, lexical_env_type)
    )


@auto_attr
def rebind_env(self, env, rebindings):
    """
    Return a new environment based on `env` to include the given `rebindings`.
    """
    return CallExpr('Rebound_Env', 'Rebind_Env', lexical_env_type,
                    [construct(env, lexical_env_type),
                     construct(rebindings, env_rebindings_type)],
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
        entity_info = construct(p.entity_info_arg.var)

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
    Construct an entity from node, including context (env rebindings).
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
    Construct an entity from node, with default entity info.
    """
    node_expr = construct(node, T.root_node, downcast=False)
    ret = make_as_entity(node_expr, entity_info=NullExpr(T.entity_info),
                         abstract_expr=self)
    ret.create_result_var('Ent')
    return ret


EmptyEnv = AbstractVariable(names.Name("AST_Envs.Empty_Env"),
                            type=lexical_env_type)
