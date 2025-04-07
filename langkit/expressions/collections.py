from __future__ import annotations

from dataclasses import dataclass
from itertools import count

from langkit import names
from langkit.compiled_types import (
    ArrayType,
    CompiledType,
    EntityType,
    SetType,
    get_context,
)
from langkit.diagnostics import check_source_language
from langkit.expressions.base import (
    AbstractNodeData,
    CallExpr,
    ComputingExpr,
    Expr,
    ExprDebugInfo,
    FieldAccessExpr,
    LocalVars,
    T,
    VariableExpr,
    render,
)


@dataclass
class InitializedVar:
    """
    Variable with an optional initializer.
    """

    var: LocalVars.LocalVar
    init_expr: Expr | None = None


class BaseCollectionExpr(ComputingExpr):
    """
    Common ancestor for expressions iterating on a collection.
    """

    _counter = count()

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        result_var_name: str,
        result_type: CompiledType,
        common: BaseCollectionExpr.ConstructCommonResult,
    ):
        self.static_type = result_type

        self.collection = common.collection_expr

        self.codegen_element_var = common.codegen_element_var
        self.user_element_var = common.user_element_var
        self.index_var = common.index_var

        self.iter_vars = common.iter_vars

        self.inner_expr = common.inner_expr
        self.inner_scope = common.inner_scope

        super().__init__(debug_info, result_var_name)

    @property
    def subexprs(self) -> dict:
        return {
            "collection": self.collection,
            "iter-vars-initalizers": [v.init_expr for v in self.iter_vars],
            "inner_expr": self.inner_expr,
        }

    def _bindings(self) -> list[VariableExpr]:
        return [v.var.ref_expr for v in self.iter_vars]

    @dataclass(frozen=True)
    class ConstructCommonResult:
        """
        Holder for the result of the "construct_common" method.
        """

        collection_expr: Expr
        """
        Expression corresponding to the collection on which the iteration is
        done.
        """

        codegen_element_var: VariableExpr
        """
        Variable which, for each iteration, contains the "raw" collection
        element that is processed. See the docstring for ``user_element_var``.
        """

        user_element_var: VariableExpr
        """
        Variable which, for each iteration, contains the collection element
        that as seen by user code.

        For instance, when iterating over a ``ChildNode.list.entity``,
        ``codegen_element_var`` contains each child node as a bare root node
        (all list nodes are implemented that way) while ``user_element_var``
        contains each child node as a ``ChildNode.entity`` (which is what user
        code deals with).
        """

        index_var: VariableExpr | None
        """
        The index variable as a expression, if required.
        """

        iter_vars: list[InitializedVar]
        """
        List of iteration variables and their initialization expression (when
        applicable).
        """

        inner_expr: Expr
        """
        Expression to be evaluated for each collection item.
        """

        inner_scope: LocalVars.Scope
        """
        Local variable scope for the body of the iteration.
        """


class MapExpr(BaseCollectionExpr):
    """
    Expression that is the result of a map expression evaluation.
    """

    pretty_class_name = "Map"

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        common: BaseCollectionExpr.ConstructCommonResult,
        filter: Expr | None = None,
        do_concat: bool = False,
        take_while: Expr | None = None,
    ):
        element_type = (
            common.inner_expr.type.element_type
            if do_concat
            else common.inner_expr.type
        )
        super().__init__(debug_info, "Map_Result", element_type.array, common)

        self.take_while = take_while
        self.filter = filter
        self.do_concat = do_concat

        # The generated code for map uses a vector to build the result
        assert isinstance(self.type, ArrayType)
        self.type.require_vector()

    def __repr__(self) -> str:
        return "<MapExpr {}: {} -> {}{}>".format(
            self.collection,
            self.user_element_var,
            self.inner_expr,
            " (if {})".format(self.filter) if self.filter else "",
        )

    def _render_pre(self) -> str:
        return render("properties/map_ada", map=self, Name=names.Name)

    @property
    def subexprs(self) -> dict:
        result = super().subexprs
        if self.take_while:
            result["take_while"] = self.take_while
        if self.filter:
            result["filter"] = self.filter
        return result


class QuantifierExpr(BaseCollectionExpr):
    """
    Return whether `predicate` returns true for all the items in the input
    `collection`.

    For instance, this computes whether all integers in an array are positive:

    .. code:: python

        int_array.all(lambda i: i > 0)
    """

    pretty_class_name = "Quantifier"

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        kind: str,
        common: BaseCollectionExpr.ConstructCommonResult,
    ):
        """
        :param kind: Kind for this quantifier expression. 'all' will check that
            all items in "collection" fullfill "expr" while 'any' will check
            that at least one of them does.

        :param common: Common iteration expression parameters.
        """
        super().__init__(debug_info, "Quantifier_Result", T.Bool, common)
        self.kind = kind
        self.static_type = T.Bool

    def _render_pre(self) -> str:
        return render(
            "properties/quantifier_ada",
            quantifier=self,
            ALL=QuantifierExpr.ALL,
            ANY=QuantifierExpr.ANY,
            Name=names.Name,
        )

    @property
    def subexprs(self) -> dict:
        result = super().subexprs
        result["kind"] = self.kind
        return result

    def __repr__(self) -> str:
        return "<QuantifierExpr {}>".format(self.kind)

    # Available quantifier kinds
    ALL = "all"
    ANY = "any"


class ToSetExpr(BaseCollectionExpr):
    """
    Expression that is the result of calling `to_set()` on a collection.
    """

    pretty_class_name = "ToSet"

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        common: BaseCollectionExpr.ConstructCommonResult,
    ):
        element_type = common.inner_expr.type
        super().__init__(debug_info, "To_Set_Result", element_type.set, common)
        assert isinstance(self.type, SetType)

    def __repr__(self) -> str:
        return "<ToSetExpr>"

    def _render_pre(self) -> str:
        return render("properties/to_set_ada", to_set=self)


def make_length(debug_info: ExprDebugInfo | None, collection: Expr) -> Expr:
    """
    Return an expression to compute the length of a collection.

    :param debug_info: Debug information for the expression to return.
    :param collection: Input collection.
    """
    # Automatically unwrap entities
    if isinstance(collection.type, EntityType):
        collection = FieldAccessExpr(
            None,
            collection,
            "Node",
            collection.type.astnode,
            do_explicit_incref=False,
        )

    return CallExpr(debug_info, "Len", "Length", T.Int, [collection])


def make_unique(debug_info: ExprDebugInfo | None, array_expr: Expr) -> Expr:
    """
    Return an expression to create the copy of an array, removing duplicated
    items.

    :param debug_info: Debug information for the expression to return.
    :param array_expr: Expression for the input array.
    """
    from langkit.compile_context import AdaSourceKind

    array_type = array_expr.type
    assert isinstance(array_type, ArrayType)

    # Enable the generation of the function that does the actual work
    get_context().add_with_clause(
        "Implementation", AdaSourceKind.body, "Ada.Containers.Hashed_Sets"
    )
    array_type.require_unique_function()

    return CallExpr(
        debug_info, "Unique_Array", "Make_Unique", array_type, [array_expr]
    )


class SingletonExpr(ComputingExpr):
    pretty_class_name = "ArraySingleton"

    def __init__(self, debug_info: ExprDebugInfo | None, expr: Expr):
        self.expr = expr
        self.static_type = self.expr.type.array

        super().__init__(debug_info, "Singleton")

    def _render_pre(self) -> str:
        result_var = self.result_var.codegen_name
        t = self.type
        assert isinstance(t, ArrayType)
        return (
            self.expr.render_pre()
            + """
            {result_var} := {constructor} (Items_Count => 1);
            {result_var}.Items (1) := {item};
            {inc_ref}
        """.format(
                constructor=t.constructor_name,
                result_var=result_var,
                item=self.expr.render_expr(),
                inc_ref=(
                    "Inc_Ref ({}.Items (1));".format(result_var)
                    if self.expr.type.is_refcounted
                    else ""
                ),
            )
        )

    @property
    def subexprs(self) -> list:
        return [self.expr]


def make_concat(
    debug_info: ExprDebugInfo | None,
    left: Expr,
    right: Expr,
) -> Expr:
    """
    Create a concatenation expression (for arrays or strings).
    """
    # Handle strings as a special case
    if left.type.is_string_type:
        check_source_language(
            right.type.is_string_type,
            f"String type expected, got {right.type.lkt_name}",
        )
        return CallExpr(
            debug_info,
            "Concat_Result",
            "Concat_String",
            T.String,
            [left, right],
        )

    def check_array(typ: CompiledType) -> None:
        check_source_language(
            typ.is_array_type, f"Expected array type, got {typ.lkt_name}"
        )

    check_array(left.type)
    check_array(right.type)

    check_source_language(
        left.type == right.type,
        f"Got different array element types in concat:"
        f" {left.type.element_type.lkt_name} and"
        f" {right.type.element_type.lkt_name}",
    )

    return CallExpr(
        debug_info,
        "Concat_Result",
        "Concat",
        left.type,
        [left, right],
    )


def make_join(
    debug_info: ExprDebugInfo | None,
    separator: Expr,
    strings: Expr,
) -> Expr:
    """
    Return an expression that computes the concatenation of all strings in an
    array, with a separator between each.

    :param debug_info: Debug information for the expression to return.
    :param separator: Expression for the string separator.
    :param strings: Expression for the array of strings to concatenate.
    """
    return CallExpr(
        debug_info,
        "Join_Result",
        "Join_Strings",
        T.String,
        [separator, strings],
    )


def make_to_iterator(
    debug_info: ExprDebugInfo | None,
    prefix: Expr,
    node_data: AbstractNodeData,
    args: list[Expr | None],
) -> Expr:
    """
    Turn an array into an iterator.

    :param prefix: Expression for the array to turn into an iterator.
    :param node_data: "to_iterator" property that this expression calls in the
        DSL.
    :param args: Arguments for the "to_iterator" property (i.e. an empty list).
    :return: Expression for the iterator creator.
    """
    assert not args
    elt_type = prefix.type.element_type

    # Make sure we generate code for this iterator type
    elt_type.create_iterator(used=True)

    return CallExpr(
        debug_info,
        result_var_name="Iter",
        name=node_data.names.codegen,
        type=elt_type.iterator,
        exprs=[prefix, "Self.Unit.Context"],
        shadow_args=[node_data],
    )


class FindExpr(BaseCollectionExpr):
    """
    Return the first element in a collection that satisfies the given
    predicate.
    """

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        common: BaseCollectionExpr.ConstructCommonResult,
    ):
        super().__init__(
            debug_info, "Find_Result", common.user_element_var.type, common
        )

    def __repr__(self) -> str:
        return "<FindExpr>"

    def _render_pre(self) -> str:
        return render("properties/find_ada", find=self)
