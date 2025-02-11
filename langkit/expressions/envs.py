from __future__ import annotations

from langkit import names
from langkit.compiled_types import ASTNodeType, T, get_context
from langkit.diagnostics import Location, check_source_language, error
from langkit.expressions.base import (
    AbstractExpression,
    AbstractVariable,
    BindableLiteralExpr,
    CallExpr,
    ComputingExpr,
    Literal,
    No,
    NullCheckExpr,
    NullExpr,
    PropertyDef,
    ResolvedExpression,
    abstract_expression_from_construct,
    construct,
    dsl_document,
    resolve_property,
)
from langkit.expressions.structs import New
from langkit.expressions.utils import assign_var


@dsl_document
class RefCategories(AbstractExpression):
    """
    Build a set of categories.
    """

    class Expr(BindableLiteralExpr):
        def __init__(
            self,
            cats: set[names.Name],
            abstract_expr: AbstractExpression | None = None,
        ):
            self.cats = cats
            super().__init__(
                self.render_private_ada_constant(),
                T.RefCategories,
                abstract_expr=abstract_expr
            )

        def render_private_ada_constant(self) -> str:
            all_cats = get_context().ref_cats
            return '({})'.format(', '.join(sorted(
                '{} => {}'.format(name.camel_with_underscores,
                                  name in self.cats)
                for name in all_cats
            )))

        # This type is not available in public APIs, so there is no need to
        # implement the other rendering properties.

        def render_public_ada_constant(self) -> str:
            raise NotImplementedError

        def render_introspection_constant(self) -> str:
            raise NotImplementedError

        def render_java_constant(self) -> str:
            raise NotImplementedError

        def render_ocaml_constant(self) -> str:
            raise NotImplementedError

        def render_python_constant(self) -> str:
            raise NotImplementedError

        @property
        def subexprs(self) -> dict:
            return {'cats': self.cats}

    def __init__(
        self,
        location: Location,
        default: bool = False,
        **kwargs: bool,
    ):
        super().__init__(location)
        self.default = default
        self.cat_map = kwargs

    def construct(self) -> ResolvedExpression:
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


@abstract_expression_from_construct
def env_get(
    self: AbstractExpression,
    only_first: bool,
    env: AbstractExpression,
    symbol: AbstractExpression,
    lookup: AbstractExpression | None = None,
    from_node: AbstractExpression | None = None,
    categories: AbstractExpression | None = None,
) -> ResolvedExpression:
    """
    Perform a lexical environment lookup. Look for nodes that are associated to
    the given `symbol` in the `env` lexical environment.

    If ``only_first`` is true, this returns the first entity found in the
    environment. If false, this returns an array for all entities found in the
    environment.

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

    return EnvGetExpr(
        env_expr=construct(env, T.LexicalEnv),
        key_expr=construct(symbol, T.Symbol),
        lookup_kind_expr=construct(
            lookup or T.LookupKind.resolve_value("recursive")
        ),
        categories_expr=(
            # If no category is provided, consider they are all requested
            RefCategories.Expr(get_context().ref_cats)
            if categories is None else
            construct(
                categories,
                T.RefCategories,
                'Invalid categories: {expected} expected but got {expr_type}'
            )
        ),
        sequential_from_expr=(
            None if from_node is None else construct(from_node, T.root_node)
        ),
        only_first=only_first,
        abstract_expr=self,
    )


class EnvGetExpr(ComputingExpr):
    """
    Expression to perform a lexical environment lookup.
    """
    def __init__(
        self,
        env_expr: ResolvedExpression,
        key_expr: ResolvedExpression,
        lookup_kind_expr: ResolvedExpression,
        categories_expr: ResolvedExpression,
        sequential_from_expr: ResolvedExpression | None = None,
        only_first: bool = False,
        abstract_expr: AbstractExpression | None = None,
    ):
        assert isinstance(env_expr, ResolvedExpression)
        assert isinstance(key_expr, ResolvedExpression)
        assert isinstance(lookup_kind_expr, ResolvedExpression)
        assert isinstance(categories_expr, ResolvedExpression)
        assert (
            sequential_from_expr is None
            or isinstance(sequential_from_expr, ResolvedExpression)
        )

        self.env_expr = env_expr
        self.key_expr = key_expr
        self.lookup_kind_expr = lookup_kind_expr
        self.sequential_from_expr = sequential_from_expr
        self.categories_expr = categories_expr

        self.static_type = (
            T.root_node.entity if only_first else T.root_node.entity.array
        )

        self.only_first = only_first
        super().__init__('Env_Get_Result', abstract_expr=abstract_expr)

        PropertyDef.get().set_uses_envs()

    def _render_pre(self) -> str:
        result = [
            self.env_expr.render_pre(),
            self.key_expr.render_pre(),
            self.lookup_kind_expr.render_pre(),
            self.categories_expr.render_pre()
        ]
        args = [('Self', self.env_expr.render_expr()),
                ('Key', f"Thin ({self.key_expr.render_expr()})"),
                ('Lookup_Kind', 'To_Lookup_Kind_Type ({})'.format(
                    self.lookup_kind_expr.render_expr()
                )),
                ('Categories', self.categories_expr.render_expr())]

        # Pass the From parameter if the user wants sequential semantics
        if self.sequential_from_expr:
            result.append(self.sequential_from_expr.render_pre())
            args.append(('From', self.sequential_from_expr.render_expr()))

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
    def subexprs(self) -> dict:
        return {
            'env': self.env_expr,
            'key': self.key_expr,
            'lookup_kind': self.lookup_kind_expr,
            'categories': self.categories_expr,
            'sequential_from': self.sequential_from_expr,
        }


@abstract_expression_from_construct
def env_orphan(
    self: AbstractExpression,
    env: AbstractExpression,
) -> ResolvedExpression:
    """
    Return a copy of the ``env`` lexical environment which has no parent.

    :param env: Expression that will return a lexical environment.
    """
    return CallExpr('Orphan_Env', 'AST_Envs.Orphan', T.LexicalEnv,
                    [construct(env, T.LexicalEnv)],
                    abstract_expr=self)


@abstract_expression_from_construct
def shed_rebindings(
    self: AbstractExpression,
    env: AbstractExpression,
    entity_info: AbstractExpression,
) -> ResolvedExpression:
    """
    Return a new ``entity_info`` struct in which rebindings are shedded
    according to the location of the given lexical environment. This simply
    forwards the call to langkit support's ``Shed_Entity_Info`` subprogram.
    """
    return CallExpr(
        'Shed_Entity_Info', 'AST_Envs.Shed_Rebindings', T.EntityInfo,
        [construct(entity_info, T.EntityInfo), construct(env, T.LexicalEnv)],
        abstract_expr=self
    )


@abstract_expression_from_construct
def env_group(
    self: AbstractExpression,
    env_array: AbstractExpression,
    with_md: AbstractExpression | None = None,
) -> ResolvedExpression:
    """
    Return a new lexical environment that logically groups together multiple
    environments. ``env_array`` must be an array that contains the environments
    to be grouped. If it is empty, the empty environment is returned.

    If provided, ``with_md`` must be a metadata structure: it will be made the
    default metadata for this lexical environment.
    """
    if not with_md:
        with_md = No(Location.builtin, T.env_md)

    return CallExpr('Group_Env', 'Group', T.LexicalEnv,
                    [construct(env_array, T.LexicalEnv.array),
                     construct(with_md, T.env_md)],
                    abstract_expr=self)


@abstract_expression_from_construct
def is_visible_from(
    self: AbstractExpression,
    referenced_env: AbstractExpression,
    base_env: AbstractExpression,
) -> ResolvedExpression:
    """
    Return whether the analysis unit associated to the ``referenced_env``
    lexical environment is visible from ``base_env``'s.

    :param base_env: The environment from which we want to check visibility.
    :param referenced_env: The environment referenced from base_env, for which
        we want to check visibility.
    """
    p = PropertyDef.get()
    p.set_uses_envs()
    return CallExpr(
        'Is_Visible', 'Is_Visible_From', T.Bool,
        [
            construct(p.node_var),
            construct(referenced_env, T.LexicalEnv),
            construct(base_env, T.LexicalEnv),
        ],
        abstract_expr=self,
    )


@abstract_expression_from_construct
def env_node(
    self: AbstractExpression,
    env: AbstractExpression,
) -> ResolvedExpression:
    """
    Return the node associated to the ``env`` environment.
    """
    return CallExpr('Env_Node', 'AST_Envs.Env_Node', T.root_node,
                    [construct(env, T.LexicalEnv)],
                    abstract_expr=self)


@abstract_expression_from_construct
def env_parent(
    self: AbstractExpression,
    env: AbstractExpression,
) -> ResolvedExpression:
    """
    Return the parent of the ``env`` lexical environment.
    """
    return CallExpr(
        'Env_Parent', 'AST_Envs.Parent', T.LexicalEnv,
        [construct(env, T.LexicalEnv)],
        abstract_expr=self,
    )


def make_append_rebinding(
    self: AbstractExpression,
    rebindings: ResolvedExpression,
    old_env: ResolvedExpression,
    new_env: ResolvedExpression,
) -> ResolvedExpression:
    return CallExpr('Rebinding', 'AST_Envs.Append_Rebinding',
                    T.EnvRebindings,
                    [rebindings, old_env, new_env],
                    abstract_expr=self)


@abstract_expression_from_construct
def append_rebinding(
    self: AbstractExpression,
    rebindings: AbstractExpression,
    old_env: AbstractExpression,
    new_env: AbstractExpression,
) -> ResolvedExpression:
    """
    Functionally append a rebinding from ``old_env`` to ``new_env`` (two
    lexical environments) on top of ``rebindings`` (a chain of rebindings).
    """
    return make_append_rebinding(
        self,
        construct(rebindings, T.EnvRebindings),
        construct(old_env, T.LexicalEnv),
        construct(new_env, T.LexicalEnv)
    )


def construct_non_null_rebindings(
    rebindings: AbstractExpression
) -> ResolvedExpression:
    """
    Construct ``rebindings%` to be an environment rebindings value and wrap it
    in a non-null check.
    """
    return NullCheckExpr(construct(rebindings, T.EnvRebindings))


@abstract_expression_from_construct
def concat_rebindings(
    self: AbstractExpression,
    lhs: AbstractExpression,
    rhs: AbstractExpression,
) -> ResolvedExpression:
    """
    Combine rebindings from the ``lhs`` and ``rhs`` environment rebindings.
    """
    return CallExpr('Rebinding', 'AST_Envs.Combine',
                    T.EnvRebindings,
                    [construct(lhs, T.EnvRebindings),
                     construct(rhs, T.EnvRebindings)],
                    abstract_expr=self)


@abstract_expression_from_construct
def rebind_env(
    self: AbstractExpression,
    env: AbstractExpression,
    rebindings: AbstractExpression,
) -> ResolvedExpression:
    """
    Return a new environment based on ``env`` to include the given
    ``rebindings``.
    """
    return CallExpr('Rebound_Env', 'Rebind_Env', T.LexicalEnv,
                    [construct(env, T.LexicalEnv),
                     construct(rebindings, T.EnvRebindings)],
                    abstract_expr=self)


def make_as_entity(
    node_expr: ResolvedExpression,
    entity_info: ResolvedExpression | None = None,
    null_check: bool = True,
    abstract_expr: AbstractExpression | None = None,
) -> ResolvedExpression:
    """
    Helper for as_entity. Takes a resolved expression instead of an abstract
    one.

    :param node_expr: The AST node expression to wrap as an entity.
    :param entity_info: Expression to use as the entity information. If
        provided, its type must be T.EntityInfo. Otherwise, the ambient entity
        info is used.
    """
    from langkit.expressions import If, make_is_null

    assert isinstance(node_expr.type, ASTNodeType)
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
        make_is_null(node_expr),
        NullExpr(entity_type),
        entity_expr,
        abstract_expr=abstract_expr
    ) if null_check else entity_expr

    result.abstract_expr = abstract_expr
    return result


@abstract_expression_from_construct
def as_entity(
    self: AbstractExpression,
    node: AbstractExpression,
) -> ResolvedExpression:
    """
    Wrap ``node`` into an entity. This uses environment rebindings from the
    context.
    """
    p = PropertyDef.get_or_none()
    if p is None:
        error("as_entity has to be used in a property")

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


@abstract_expression_from_construct
def as_bare_entity(
    self: AbstractExpression,
    node: AbstractExpression,
) -> ResolvedExpression:
    """
    Wrap ``node`` into an entity, using default entity information (in
    particular, no rebindings).
    """
    node_expr = construct(node, T.root_node, downcast=False)
    ret = make_as_entity(node_expr, entity_info=NullExpr(T.EntityInfo),
                         abstract_expr=self)
    ret.create_result_var('Ent')
    return ret


@dsl_document
class DynamicLexicalEnv(AbstractExpression):
    """
    Build a dynamic lexical environment.

    Lookup through such environments uses the given property to determine the
    list of (symbol, node) associations to be analyzed. This property must take
    no argument and return an array of ``T.InnerEnvAssoc`` structs.

    This is opposed to the environments created during PLE which, at the end of
    the PLE stage, contain a static mapping of such assocations.  Note that
    building such environments is only possible in lazy field initializers.
    """

    class Expr(CallExpr):
        def __init__(
            self,
            assocs_getter: PropertyDef,
            assoc_resolver: PropertyDef | None,
            transitive_parent: ResolvedExpression,
            abstract_expr: AbstractExpression | None = None,
        ):
            p = PropertyDef.get()
            self.assocs_getter = assocs_getter
            self.assoc_resolver = assoc_resolver
            self.transitive_parent = transitive_parent

            assocs_getter_ref = f"{self.assocs_getter.names.codegen}'Access"
            assoc_resolver_ref = (
                'null'
                if self.assoc_resolver is None
                else f"{self.assoc_resolver.names.codegen}'Access"
            )
            super().__init__(
                'Dyn_Env',
                'Create_Dynamic_Lexical_Env',
                T.LexicalEnv,
                [
                    construct(p.node_var),
                    assocs_getter_ref,
                    assoc_resolver_ref,
                    transitive_parent,
                    'Self.Unit.Context.Symbols',
                ],
                abstract_expr=abstract_expr,
            )

        @property
        def subexprs(self) -> dict:
            return {'assocs_getter': self.assocs_getter,
                    'assoc_resolver': self.assoc_resolver,
                    'transitive_parent': self.transitive_parent}

        def __repr__(self) -> str:
            return '<DynamicLexicalEnv.Expr>'

    def __init__(
        self,
        location: Location,
        assocs_getter: PropertyDef,
        assoc_resolver: PropertyDef | None = None,
        transitive_parent: AbstractExpression = Literal(
            Location.builtin, True
        ),
    ):
        self.assocs_getter = assocs_getter
        self.assoc_resolver = assoc_resolver
        self.transitive_parent = transitive_parent
        super().__init__(location)

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

        assocs_getter = resolve_property(self.assocs_getter).root
        assocs_getter.require_untyped_wrapper()

        expected_rtype = T.InnerEnvAssoc.array
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
            ).root
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


EmptyEnv = AbstractVariable(
    Location.builtin, names.Name("Empty_Env"), type=T.LexicalEnv
)
