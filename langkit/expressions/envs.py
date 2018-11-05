from __future__ import absolute_import, division, print_function

from langkit import names
from langkit.compiled_types import T, get_context
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, CallExpr, ComputingExpr,
    GetSymbol, NullCheckExpr, NullExpr,
    PropertyDef, Self, attr_call, auto_attr, construct
)
from langkit.expressions.utils import assign_var


@attr_call('get')
def get(env, symbol, lookup=None, from_node=None, categories=None):
    """
    Perform a lexical environment lookup. Look for nodes that are associated to
    the given `symbol` in the `env` lexical environment.

    If ``lookup`` is None (the default), it will take the default
    value ``LookupKind.recursive`` which will do a recursive lookup in
    parent environments and referenced ones.

    If ``lookup`` evaluates to ``LookupKind.flat``, only lookup own
    env and transitive referenced/parent envs.

    If ``lookup`` evaluates to ``LookupKind.minimal``, only lookup
    own env.

    If `from_node` is not None, do a sequential lookup: discard AST nodes that
    belong to the same unit as `from_node` node and that appear before it.
    """
    from langkit.dsl import LookupKind
    if not lookup:
        lookup = LookupKind.recursive

    return EnvGet(env, symbol, lookup=lookup,
                  sequential_from=from_node, categories=categories)


@attr_call('get_first')
def get_first(env, symbol, lookup=None, from_node=None, categories=None):
    """
    Like :dsl:`get`, but only return the first entity found, or a null entity
    if no entity is found.
    """
    from langkit.dsl import LookupKind
    if not lookup:
        lookup = LookupKind.recursive

    return EnvGet(env, symbol, lookup=lookup, only_first=True,
                  sequential_from=from_node, categories=categories)


class EnvGet(AbstractExpression):
    """
    Expression to perform a lexical environment lookup.
    """

    class Expr(ComputingExpr):
        def __init__(self, env_expr, key_expr, lookup_kind_expr,
                     sequential_from=None,
                     only_first=False, abstract_expr=None,
                     categories=None):
            self.env_expr = env_expr
            self.key_expr = key_expr
            self.lookup_kind_expr = lookup_kind_expr
            self.sequential_from = sequential_from
            self.categories = categories

            self.static_type = (
                T.root_node.entity if only_first
                else T.root_node.entity.array
            )

            self.only_first = only_first
            super(EnvGet.Expr, self).__init__('Env_Get_Result',
                                              abstract_expr=abstract_expr)

            PropertyDef.get().set_uses_envs()

        def _render_pre(self):
            if self.categories:
                cat_arg = "({})".format(", ".join(
                    "{} => {}".format(cat_name, cat_val)
                    for cat_name, cat_val in self.categories.items()
                ))
            else:
                cat_arg = "All_Cats"

            result = [
                self.env_expr.render_pre(),
                self.key_expr.render_pre(),
                self.lookup_kind_expr.render_pre(),
            ]
            args = [('Self', self.env_expr.render_expr()),
                    ('Key', self.key_expr.render_expr()),
                    ('Lookup_Kind', 'To_Lookup_Kind_Type ({})'.format(
                        self.lookup_kind_expr.render_expr()
                    )),
                    ('Categories', cat_arg)]

            # Pass the From parameter if the user wants sequential semantics
            if self.sequential_from:
                result.append(self.sequential_from.render_pre())
                args.append(('From', self.sequential_from.render_expr()))

            if self.only_first:
                result_expr = 'AST_Envs.Get_First ({})'.format(
                    ', '.join('{} => {}'.format(n, v) for n, v in args)
                )
            else:
                result_expr = '{} (AST_Envs.Get ({}))'.format(
                    self.type.constructor_name,
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
                'lookup_kind': self.lookup_kind_expr,
                'sequential_from': self.sequential_from,
            }

    def __init__(self, env, symbol, lookup,
                 sequential_from=Self,
                 only_first=False,
                 categories=None):
        """
        :param AbstractExpression env: Expression that will yield the env to
            get the element from.
        :param AbstractExpression|str symbol: Expression that will yield the
            symbol to use as a key on the env, or a string to turn into a
            symbol.
        :param AbstractExpression sequential_from: If resolution needs to be
            sequential, must be an expression to use as the reference node.
        :param AbstractExpression lookup: Expression that must return an
            LookupKind, which controls whether lookup must be performed
            recursively on parent/referenced environments.
        """
        super(EnvGet, self).__init__()

        check_source_language(
            isinstance(symbol, (AbstractExpression, basestring)),
            'Invalid key argument for Env.get: {}'.format(repr(symbol))
        )

        self.env = env
        self.symbol = symbol
        self.sequential_from = sequential_from
        self.lookup_kind = lookup
        self.only_first = only_first
        self.categories = categories

    def construct(self):
        env_expr = construct(self.env, T.LexicalEnv)

        sym_expr = construct(self.symbol)

        if sym_expr.type.is_ast_node:
            check_source_language(
                sym_expr.type.is_token_node,
                'AST node type for key (here: {}) must be a token node'.format(
                    sym_expr.type.dsl_name
                )
            )
            sym_expr = GetSymbol.construct_static(sym_expr)

        check_source_language(
            sym_expr.type == T.Symbol,
            'Invalid key type: {}'.format(sym_expr.type.dsl_name)
        )

        from_expr = (construct(self.sequential_from, T.root_node)
                     if self.sequential_from is not None else None)

        lookup_kind_expr = construct(self.lookup_kind, T.LookupKind)

        ctx = get_context()

        if self.categories:
            check_source_language(
                isinstance(self.categories, dict),
                "Categories should be a dict"
            )

            self.categories = {cat.lower(): val
                               for cat, val in self.categories.items()}

            check_source_language(
                self.categories.get('others', None)
                or all(self.categories.get(cat, None) for cat in ctx.ref_cats),
                'Categories for env.get do not contain mappings for all'
                ' categories'
            )

            check_source_language(
                all(isinstance(val, bool) for val in self.categories.values()),
                "Categories values should be bool"
            )

            self.categories = {
                names.Name.from_lower(cat).camel_with_underscores
                if cat != 'others' else 'others': val
                for cat, val in self.categories.items()
            }

        return EnvGet.Expr(env_expr, sym_expr, lookup_kind_expr,
                           from_expr, self.only_first,
                           abstract_expr=self, categories=self.categories)

    def __repr__(self):
        return '<EnvGet({}, {})>'.format(self.env, self.symbol)


@auto_attr
def env_orphan(self, env):
    """
    Return a copy of the `env` lexical environment which has no parent.

    :param AbstractExpression env: Expression that will return a
        lexical environment.
    """
    return CallExpr('Orphan_Env', 'AST_Envs.Orphan', T.LexicalEnv,
                    [construct(env, T.LexicalEnv)],
                    abstract_expr=self)


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

    return CallExpr('Group_Env', 'Group', T.LexicalEnv,
                    [construct(env_array, T.LexicalEnv.array),
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
            'Is_Visible', 'Is_Visible_From', T.Bool,
            [construct(referenced_env, T.LexicalEnv),
             construct(base_env, T.LexicalEnv)],
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
                    [construct(env, T.LexicalEnv)],
                    abstract_expr=self)


@auto_attr
def env_parent(self, env):
    """
    Return the parent of the `env` lexical environment.

    :param AbstractExpression env: The source environment.
    """
    return CallExpr(
        'Env_Parent', 'AST_Envs.Parent', T.LexicalEnv,
        [construct(env, T.LexicalEnv)],
        abstract_expr=self,
    )


def make_append_rebinding(self, rebindings, old_env, new_env):
    return CallExpr('Rebinding', 'AST_Envs.Append_Rebinding',
                    T.EnvRebindings,
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
        construct(rebindings, T.EnvRebindings),
        construct(old_env, T.LexicalEnv),
        construct(new_env, T.LexicalEnv)
    )


def construct_non_null_rebindings(rebindings):
    """
    Construct `rebindings` to be an environment rebindings value and wrap it in
    a non-null check.

    :param AbstractExpression rebindings: Rebindings-returning expression to
        process.
    :rtype: ResolvedExpression
    """
    return NullCheckExpr(construct(rebindings, T.EnvRebindings))


@auto_attr
def concat_rebindings(self, lhs, rhs):
    """
    Combine rebindings from the `lhs` and `rhs` environment rebindings.
    """
    return CallExpr('Rebinding', 'AST_Envs.Combine',
                    T.EnvRebindings,
                    [construct(lhs, T.EnvRebindings),
                     construct(rhs, T.EnvRebindings)],
                    abstract_expr=self)


@auto_attr
def rebind_env(self, env, rebindings):
    """
    Return a new environment based on `env` to include the given `rebindings`.
    """
    return CallExpr('Rebound_Env', 'Rebind_Env', T.LexicalEnv,
                    [construct(env, T.LexicalEnv),
                     construct(rebindings, T.EnvRebindings)],
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
        entity_type, {names.Name('Node'): node_ref,
                      names.Name('Info'): entity_info},
        result_var_name=names.Name.from_lower('as_entity'),
    )

    result = If.Expr(
        IsNull.construct_static(node_expr),
        NullExpr(entity_type),
        entity_expr,
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
        'This property has been explicitly tagged as not using entity info, so'
        ' .as_entity is invalid here'
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
                            type=T.LexicalEnv)
