from typing import Any, Optional, cast

from langkit import names
from langkit.compiled_types import T, get_context
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, BindableLiteralExpr, CallExpr,
    ComputingExpr, GetSymbol, Literal, No, NullCheckExpr, NullExpr,
    PropertyDef, ResolvedExpression, Self, attr_call, auto_attr, construct,
    dsl_document, resolve_property
)
from langkit.expressions.utils import assign_var


@dsl_document
class RefCategories(AbstractExpression):
    """
    Build a set of categories.
    """

    class Expr(BindableLiteralExpr):
        def __init__(self, cats, abstract_expr=None):
            self.cats = cats
            super().__init__(
                self.render_private_ada_constant(),
                T.RefCategories,
                abstract_expr=abstract_expr
            )

        def render_private_ada_constant(self):
            all_cats = get_context().ref_cats
            return '({})'.format(', '.join(sorted(
                '{} => {}'.format(name.camel_with_underscores,
                                  name in self.cats)
                for name in all_cats
            )))

        # This type is not available in public APIs, so there is no need to
        # implement the other rendering properties.

        @property
        def subexprs(self):
            return {'cats': self.cats}

    def __init__(self, default=False, **kwargs):
        super().__init__()
        self.default = default
        self.cat_map = kwargs

    def construct(self):
        check_source_language(isinstance(self.default, bool),
                              'Invalid categories default')

        all_cats = get_context().ref_cats
        cats = set(all_cats) if self.default else set()

        # Compute the list of requested categories
        for key, value in self.cat_map.items():
            name = names.Name.from_lower(key)
            check_source_language(name in all_cats,
                                  'Invalid category: {}'.format(key))
            check_source_language(isinstance(value, bool),
                                  'Invalid status for {}'.format(key))
            if value:
                cats.add(name)
            else:
                cats.discard(name)

        return self.Expr(cats, abstract_expr=self)


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
        def __init__(self, env_expr, key_expr, lookup_kind_expr, categories,
                     sequential_from=None,
                     only_first=False, abstract_expr=None):
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
            super().__init__('Env_Get_Result', abstract_expr=abstract_expr)

            PropertyDef.get().set_uses_envs()

        def _render_pre(self):
            result = [
                self.env_expr.render_pre(),
                self.key_expr.render_pre(),
                self.lookup_kind_expr.render_pre(),
                self.categories.render_pre()
            ]
            args = [('Self', self.env_expr.render_expr()),
                    ('Key', self.key_expr.render_expr()),
                    ('Lookup_Kind', 'To_Lookup_Kind_Type ({})'.format(
                        self.lookup_kind_expr.render_expr()
                    )),
                    ('Categories', self.categories.render_expr())]

            # Pass the From parameter if the user wants sequential semantics
            if self.sequential_from:
                result.append(self.sequential_from.render_pre())
                args.append(('From', self.sequential_from.render_expr()))

            if self.only_first:
                result_expr = 'AST_Envs.Get_First ({})'.format(
                    ', '.join('{} => {}'.format(n, v) for n, v in args)
                )
            else:
                result_expr = (
                    'Construct_Entity_Array (AST_Envs.Get ({}))'.format(
                        ', '.join('{} => {}'.format(n, v) for n, v in args)
                    )
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
                'categories': self.categories,
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
        super().__init__()

        check_source_language(
            isinstance(symbol, (AbstractExpression, str)),
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

        # If no category is provided, consider they are all requested
        if self.categories is None:
            categories = RefCategories.Expr(get_context().ref_cats)
        else:
            categories = construct(
                self.categories,
                T.RefCategories,
                'Invalid categories: {expected} expected but got {expr_type}'
            )

        return EnvGet.Expr(env_expr, sym_expr, lookup_kind_expr, categories,
                           from_expr, self.only_first,
                           abstract_expr=self)


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
        super().__init__(
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


def new_env_assoc(key, value, dest_env=None, metadata=None):
    """
    Create a new env assoc, providing basic defaults when fields are not
    specified.

    :param AbstractExpression key: The symbol for which to associate a value.
    :param AbstractExpression value: The node to associate to the key.
    :param AbstractExpression dest_env: The environment in which to insert the
        mapping (a DesignatedEnv struct value). If left to None, use the
        current environment.
    :param AbstractExpression metadata: Additional metadata to associate to the
        node.
    """
    return T.env_assoc.new(
        key=key,
        value=value,
        dest_env=current_env() if dest_env is None else dest_env,
        metadata=No(T.defer_env_md) if metadata is None else metadata
    )


def no_env() -> AbstractExpression:
    """
    Return a ``DesignatedEnv`` struct to mean no destination environment.
    """
    return T.DesignatedEnv.new(
        kind=T.DesignatedEnvKind.resolve_value("none"),
        env_name=No(T.Symbol),
        direct_env=No(T.LexicalEnv),
    )


def named_env(name: AbstractExpression,
              or_current: bool = False) -> AbstractExpression:
    """
    Return an ``DesignatedEnv`` struct to mean a named env.

    :param or_current: If True, return ``current_env()`` when ``name``
        evaluates to ``No(T.Symbol)``. Otherwise, return the named env for the
        null symbol instead, which is equivalent to ``no_env()``.
    """
    if or_current:
        return cast(Any, name).then(
            lambda non_null_name:
            T.DesignatedEnv.new(
                kind=T.DesignatedEnvKind.resolve_value("named_env"),
                env_name=non_null_name,
                direct_env=No(T.LexicalEnv),
            ),
            default_val=current_env(),
        )
    else:
        return T.DesignatedEnv.new(
            kind=T.DesignatedEnvKind.resolve_value("named_env"),
            env_name=name,
            direct_env=No(T.LexicalEnv),
        )


def direct_env(env: AbstractExpression,
               or_current: bool = False) -> AbstractExpression:
    """
    Return an ``DesignatedEnv`` struct to mean a direct environment value.

    :param or_current: If True, return ``current_env()`` when ``env`` evaluates
        to ``No(T.LexicalEnv)``. Otherwise, return the empty env direct
        environment instead, which is equivalent to ``no_env()``.
    """
    if or_current:
        return cast(Any, env).then(
            lambda non_null_env:
            T.DesignatedEnv.new(
                kind=T.DesignatedEnvKind.resolve_value("direct_env"),
                env_name=No(T.Symbol),
                direct_env=non_null_env,
            ),
            default_val=current_env(),
        )
    else:
        return T.DesignatedEnv.new(
            kind=T.DesignatedEnvKind.resolve_value("direct_env"),
            env_name=No(T.Symbol),
            direct_env=env,
        )


def current_env() -> AbstractExpression:
    """
    Return an ``DesignatedEnv`` struct to mean the current environment.
    """
    return T.DesignatedEnv.new(
        kind=T.DesignatedEnvKind.resolve_value("current_env"),
        env_name=No(T.Symbol),
        direct_env=No(T.LexicalEnv),
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


@dsl_document
class DynamicLexicalEnv(AbstractExpression):
    """
    Build a dynamic lexical environment.

    Lookup through such environments uses the given property to determine the
    list of (symbol, node) associations to be analyzed. This property must take
    no argument and return an array of ``T.inner_env_assoc`` structs.

    This is opposed to the environments created during PLE which, at the end of
    the PLE stage, contain a static mapping of such assocations.  Note that
    building such environments is only possible in lazy field initializers.
    """

    class Expr(CallExpr):
        def __init__(self,
                     assocs_getter: PropertyDef,
                     assoc_resolver: Optional[PropertyDef],
                     transitive_parent: ResolvedExpression,
                     abstract_expr: Optional[AbstractExpression] = None):
            self.assocs_getter = assocs_getter
            self.assoc_resolver = assoc_resolver
            self.transitive_parent = transitive_parent

            assocs_getter_ref = "{}'Access".format(self.assocs_getter.name)
            assoc_resolver_ref = (
                'null'
                if self.assoc_resolver is None
                else "{}'Access".format(self.assoc_resolver.name)
            )
            super().__init__(
                'Dyn_Env',
                'Create_Dynamic_Lexical_Env',
                T.LexicalEnv,
                [construct(Self), assocs_getter_ref, assoc_resolver_ref,
                 transitive_parent],
                abstract_expr=abstract_expr,
            )

        @property
        def subexprs(self):
            return {'assocs_getter': self.assocs_getter,
                    'assoc_resolver': self.assoc_resolver,
                    'transitive_parent': self.transitive_parent}

        def __repr__(self):
            return '<DynamicLexicalEnv.Expr>'

    def __init__(self, assocs_getter, assoc_resolver=None,
                 transitive_parent=Literal(True)):
        self.assocs_getter = assocs_getter
        self.assoc_resolver = assoc_resolver
        self.transitive_parent = transitive_parent
        super().__init__()

    def construct(self) -> ResolvedExpression:
        # Make sure this expression is allowed in the current expression
        # context.
        current_prop = PropertyDef.get()
        check_source_language(
            current_prop.lazy_field,
            "Dynamic lexical environment creation can only happen inside a"
            " lazy field initializer"
        )

        # Sanitize assocs_getter: make sure we have a property reference, then
        # make sure it has the expected signature.

        assocs_getter = resolve_property(self.assocs_getter).root_property
        assocs_getter.require_untyped_wrapper()

        expected_rtype = T.inner_env_assoc.array
        check_source_language(
            assocs_getter.type.matches(expected_rtype),
            '"assocs_getter" must return an array of {} (got {})'
            .format(expected_rtype.element_type.dsl_name,
                    assocs_getter.type.dsl_name)
        )
        check_source_language(not assocs_getter.arguments,
                              '"assocs_getter" cannot accept arguments')

        # Likewise for assoc_resolver, is present
        assoc_resolver = None
        if self.assoc_resolver:
            assoc_resolver = resolve_property(
                self.assoc_resolver
            ).root_property
            assoc_resolver.require_untyped_wrapper()

            check_source_language(
                assoc_resolver.type.matches(T.entity),
                '"assoc_resolver" must return a {} (got {})'
                .format(T.entity.dsl_name, assoc_resolver.type.dsl_name)
            )
            check_source_language(not assoc_resolver.arguments,
                                  '"assoc_resolver" cannot accept arguments')

        # Should this environment has a transitive parent?
        transitive_parent = construct(self.transitive_parent, T.Bool)

        return self.Expr(assocs_getter, assoc_resolver, transitive_parent,
                         abstract_expr=self)


EmptyEnv = AbstractVariable(names.Name("Empty_Env"), type=T.LexicalEnv)
