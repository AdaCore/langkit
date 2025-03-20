from __future__ import annotations

from langkit import names
from langkit.compiled_types import ASTNodeType, T, get_context
from langkit.expressions.base import (
    BindableLiteralExpr,
    CallExpr,
    ComputingExpr,
    Expr,
    ExprDebugInfo,
    NullExpr,
    PropertyDef,
)
from langkit.expressions.structs import New
from langkit.expressions.utils import assign_var


class RefCategoriesExpr(BindableLiteralExpr):
    """
    Build a set of categories.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        cats: set[names.Name],
    ):
        self.cats = cats
        super().__init__(
            debug_info, self.render_private_ada_constant(), T.RefCategories
        )

    def render_private_ada_constant(self) -> str:
        all_cats = get_context().ref_cats
        return "({})".format(
            ", ".join(
                sorted(
                    "{} => {}".format(
                        name.camel_with_underscores, name in self.cats
                    )
                    for name in all_cats
                )
            )
        )

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
        return {"cats": self.cats}


class EnvGetExpr(ComputingExpr):
    """
    Expression to perform a lexical environment lookup.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        env_expr: Expr,
        key_expr: Expr,
        lookup_kind_expr: Expr,
        categories_expr: Expr,
        sequential_from_expr: Expr | None = None,
        only_first: bool = False,
    ):
        assert isinstance(env_expr, Expr)
        assert isinstance(key_expr, Expr)
        assert isinstance(lookup_kind_expr, Expr)
        assert isinstance(categories_expr, Expr)
        assert sequential_from_expr is None or isinstance(
            sequential_from_expr, Expr
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
        super().__init__(debug_info, "Env_Get_Result")

        PropertyDef.get().set_uses_envs()

    def _render_pre(self) -> str:
        result = [
            self.env_expr.render_pre(),
            self.key_expr.render_pre(),
            self.lookup_kind_expr.render_pre(),
            self.categories_expr.render_pre(),
        ]
        args = [
            ("Self", self.env_expr.render_expr()),
            ("Key", f"Thin ({self.key_expr.render_expr()})"),
            (
                "Lookup_Kind",
                "To_Lookup_Kind_Type ({})".format(
                    self.lookup_kind_expr.render_expr()
                ),
            ),
            ("Categories", self.categories_expr.render_expr()),
        ]

        # Pass the From parameter if the user wants sequential semantics
        if self.sequential_from_expr:
            result.append(self.sequential_from_expr.render_pre())
            args.append(("From", self.sequential_from_expr.render_expr()))

        if self.only_first:
            result_expr = "AST_Envs.Get_First ({})".format(
                ", ".join("{} => {}".format(n, v) for n, v in args)
            )
        else:
            result_expr = "Construct_Entity_Array (AST_Envs.Get ({}))".format(
                ", ".join("{} => {}".format(n, v) for n, v in args)
            )

        # In both cases above, the expression is going to be a function
        # call that returns a new ownership share, so there is no need for
        # an inc-ref for the storage in the result variable.
        result.append(
            assign_var(
                self.result_var.ref_expr, result_expr, requires_incref=False
            )
        )

        return "\n".join(result)

    @property
    def subexprs(self) -> dict:
        return {
            "env": self.env_expr,
            "key": self.key_expr,
            "lookup_kind": self.lookup_kind_expr,
            "categories": self.categories_expr,
            "sequential_from": self.sequential_from_expr,
        }


def make_env_orphan(debug_info: ExprDebugInfo | None, env: Expr) -> Expr:
    """
    Return an expression to compute a parent-lesscopy of a lexical
    environment.

    :param debug_info: Debug information for the expression to return.
    :param env: Expression for the input lexical environment.
    """
    return CallExpr(
        debug_info, "Orphan_Env", "AST_Envs.Orphan", T.LexicalEnv, [env]
    )


def make_shed_rebindings(
    debug_info: ExprDebugInfo | None,
    env: Expr,
    entity_info: Expr,
) -> Expr:
    """
    Return an expression to compute a copy of the given ``entity_info`` struct
    in which rebindings are shedded according to the location of the given
    lexical environment. This simply forwards the call to langkit support's
    ``Shed_Entity_Info`` subprogram.

    :param debug_info: Debug information for the expression to return.
    :param env: Expression for the input lexical environment.
    :param entity_info: Expression for the input ``EntityInfo`` struct.
    """
    return CallExpr(
        debug_info,
        "Shed_Entity_Info",
        "AST_Envs.Shed_Rebindings",
        T.EntityInfo,
        [entity_info, env],
    )


def make_env_group(
    debug_info: ExprDebugInfo | None,
    env_array: Expr,
    with_md: Expr,
) -> Expr:
    """
    Return an expression to compute a new lexical environment that logically
    groups together all environments computed by the ``env_array`` expression.
    If that array of lexical envs is empty, this evaluates to the empty
    environment.

    ``with_md`` must evaluate a metadata structure: it will be made the
    default metadata for the created lexical environment.
    """
    return CallExpr(
        debug_info, "Group_Env", "Group", T.LexicalEnv, [env_array, with_md]
    )


def make_is_visible_from(
    debug_info: ExprDebugInfo | None,
    referenced_env: Expr,
    base_env: Expr,
) -> Expr:
    """
    Return an expression that computes whether the analysis unit associated to
    the ``referenced_env`` lexical environment is visible from ``base_env``'s.

    :param base_env: The environment from which we want to check visibility.
    :param referenced_env: The environment referenced from base_env, for which
        we want to check visibility.
    """
    p = PropertyDef.get()
    p.set_uses_envs()
    return CallExpr(
        debug_info,
        "Is_Visible",
        "Is_Visible_From",
        T.Bool,
        [p.node_var.ref_expr, referenced_env, base_env],
    )


def make_append_rebinding(
    debug_info: ExprDebugInfo | None,
    rebindings: Expr,
    old_env: Expr,
    new_env: Expr,
) -> Expr:
    """
    Return an expression to append a rebinding from ``old_env`` to ``to_env``
    (two expressions that must evaluate to lexical environments) on top of
    ``rebindings`` (an existing chain of rebindings).
    """
    return CallExpr(
        debug_info,
        "Rebinding",
        "AST_Envs.Append_Rebinding",
        T.EnvRebindings,
        [rebindings, old_env, new_env],
    )


def make_concat_rebindings(
    debug_info: ExprDebugInfo | None,
    lhs: Expr,
    rhs: Expr,
) -> Expr:
    """
    Combine rebindings from the ``lhs`` and ``rhs`` environment rebindings.
    """
    return CallExpr(
        debug_info,
        "Rebinding",
        "AST_Envs.Combine",
        T.EnvRebindings,
        [lhs, rhs],
    )


def make_rebind_env(
    debug_info: ExprDebugInfo | None,
    env: Expr,
    rebindings: Expr,
) -> Expr:
    """
    Return an expression to create a new environment based on ``env`` to
    include the given ``rebindings``.
    """
    return CallExpr(
        debug_info,
        "Rebound_Env",
        "Rebind_Env",
        T.LexicalEnv,
        [env, rebindings],
    )


def make_as_entity(
    debug_info: ExprDebugInfo | None,
    node_expr: Expr,
    entity_info: Expr | None = None,
    null_check: bool = True,
) -> Expr:
    """
    Helper for as_entity. Takes an expression instead of an abstract one.

    :param node_expr: The AST node expression to wrap as an entity.
    :param entity_info: Expression to use as the entity information. If
        provided, its type must be T.EntityInfo. Otherwise, the ambient entity
        info is used.
    """
    from langkit.expressions import IfExpr, make_is_null

    assert isinstance(node_expr.type, ASTNodeType)
    entity_type = node_expr.type.entity

    # If we use the ambient entity info, make the current property an entity
    # one.
    if entity_info is None:
        p = PropertyDef.get()
        p.set_uses_entity_info()
        entity_info = p.entity_info_arg.ref_expr

    # Expression tree sharing is forbidden, so if we need to reference the
    # result of the input node expression multiple times, create a variable to
    # hold the input node.
    node_ref = (
        node_expr.create_result_var("Node_For_Entity")
        if null_check
        else node_expr
    )

    entity_expr = New.StructExpr(
        None if null_check else debug_info,
        entity_type,
        {names.Name("Node"): node_ref, names.Name("Info"): entity_info},
        result_var_name=names.Name.from_lower("as_entity"),
    )

    result = (
        IfExpr(
            debug_info,
            make_is_null(None, node_expr),
            NullExpr(None, entity_type),
            entity_expr,
        )
        if null_check
        else entity_expr
    )
    return result


class DynamicLexicalEnvExpr(CallExpr):
    """
    Build a dynamic lexical environment.

    Lookup through such environments uses the given property to determine the
    list of (symbol, node) associations to be analyzed. This property must take
    no argument and return an array of ``T.InnerEnvAssoc`` structs.

    This is opposed to the environments created during PLE which, at the end of
    the PLE stage, contain a static mapping of such assocations.  Note that
    building such environments is only possible in lazy field initializers.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        assocs_getter: PropertyDef,
        assoc_resolver: PropertyDef | None,
        transitive_parent: Expr,
    ):
        p = PropertyDef.get()
        self.assocs_getter = assocs_getter
        self.assoc_resolver = assoc_resolver
        self.transitive_parent = transitive_parent

        assocs_getter_ref = f"{self.assocs_getter.names.codegen}'Access"
        assoc_resolver_ref = (
            "null"
            if self.assoc_resolver is None
            else f"{self.assoc_resolver.names.codegen}'Access"
        )
        super().__init__(
            debug_info,
            "Dyn_Env",
            "Create_Dynamic_Lexical_Env",
            T.LexicalEnv,
            [
                p.node_var.ref_expr,
                assocs_getter_ref,
                assoc_resolver_ref,
                transitive_parent,
                "Self.Unit.Context.Symbols",
            ],
        )

    @property
    def subexprs(self) -> dict:
        return {
            "assocs_getter": self.assocs_getter,
            "assoc_resolver": self.assoc_resolver,
            "transitive_parent": self.transitive_parent,
        }

    def __repr__(self) -> str:
        return "<DynamicLexicalEnv.Expr>"
