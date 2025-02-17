from __future__ import annotations

from dataclasses import dataclass
from itertools import count
from typing import Callable

from langkit import names
from langkit.compiled_types import (
    ASTNodeType, ArrayType, CompiledType, EntityType, get_context,
)
from langkit.diagnostics import Location, check_source_language, error
from langkit.expressions.base import (
    AbstractExpression,
    AbstractNodeData,
    AbstractVariable,
    BooleanLiteralExpr,
    CallExpr,
    ComputingExpr,
    ExprDebugInfo,
    FieldAccessExpr,
    LambdaArgInfo,
    LocalVars,
    NullCheckExpr,
    PropertyDef,
    ResolvedExpression,
    SequenceExpr,
    T,
    UncheckedCastExpr,
    VariableExpr,
    abstract_expression_from_construct,
    construct,
    construct_var,
    render,
)
from langkit.expressions.envs import make_as_entity


def canonicalize_list(
    coll_expr: ResolvedExpression,
    to_root_list: bool = False
) -> tuple[ResolvedExpression, CompiledType]:
    """
    If `coll_expr` returns a bare node, return an expression that converts it
    to the generic list type (if to_root_list=False) or the root list type (if
    to_root_list=True). Also return the element type for this collection.
    """
    element_type = coll_expr.type.element_type
    if isinstance(coll_expr.type, ASTNodeType):
        # Compute the result's type according to to_root_list
        if to_root_list:
            dest_type = coll_expr.type
            while not dest_type.is_root_list_type:
                assert dest_type.base is not None
                dest_type = dest_type.base
        else:
            dest_type = get_context().generic_list_type
    return (coll_expr, element_type)


@dataclass
class InitializedVar:
    """
    Variable with an optional initializer.
    """
    var: VariableExpr
    init_expr: ResolvedExpression | None = None


class CollectionExpression(AbstractExpression):
    """
    Base class to provide common code for abstract expressions working on
    collections.
    """

    _counter = count()

    class BaseExpr(ComputingExpr):
        """
        Common ancestor for resolved expressions iterating on a collection.
        """

        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            result_var_name: str,
            result_type: CompiledType,
            common: CollectionExpression.ConstructCommonResult,
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
                'collection': self.collection,
                'iter-vars-initalizers': [v.init_expr for v in self.iter_vars],
                'inner_expr': self.inner_expr,
            }

        def _bindings(self) -> list[VariableExpr]:
            return [v.var for v in self.iter_vars]

    class ConstructCommonResult:
        """
        Holder for the result of the "construct_common" method.
        """

        def __init__(self,
                     collection_expr: ResolvedExpression,
                     codegen_element_var: VariableExpr,
                     user_element_var: VariableExpr,
                     index_var: VariableExpr | None,
                     iter_vars: list[InitializedVar],
                     inner_expr: ResolvedExpression,
                     inner_scope: LocalVars.Scope):
            self.collection_expr = collection_expr
            """
            Resolved expression corresponding to the collection on which the
            iteration is done.
            """

            self.codegen_element_var = codegen_element_var
            """
            Variable which, for each iteration, contains the "raw" collection
            element that is processed. See the docstring for
            ``user_element_var``.
            """

            self.user_element_var = user_element_var
            """
            Variable which, for each iteration, contains the collection element
            that as seen by user code.

            For instance, when iterating over a ``ChildNode.list.entity``,
            ``codegen_element_var`` contains each child node as a bare root
            node (all list nodes are implemented that way) while
            ``user_element_var`` contains each child node as a
            ``ChildNode.entity`` (which is what user code deals with).
            """

            self.index_var = index_var
            """
            The index variable as a resolved expression, if required.
            """

            self.inner_expr = inner_expr
            """
            Resolved expression to be evaluated for each collection item.
            """

            self.inner_scope = inner_scope
            """
            Local variable scope for the body of the iteration.
            """

            self.iter_vars = iter_vars
            """
            List of iteration variables and their initialization expression
            (when applicable).
            """

    def __init__(
        self,
        location: Location,
        collection: AbstractExpression,
        expr: AbstractExpression,
        lambda_arg_infos: list[LambdaArgInfo],
        element_var: AbstractVariable,
        index_var: AbstractVariable | None = None,
    ):
        """
        :param collection: Collection on which this map operation works.

        :param expr: Expression to evaluate for each item in ``collection`` (it
            can use ``element_var`` and ``index_var``).
        :param element_var: Variable that contains the collection element for
            the current iteration.
        :param index_var: Optional variable that contains the index of the
            current iteration.
        """
        super().__init__(location)
        self.collection = collection
        self.expr = expr
        self.lambda_arg_infos = lambda_arg_infos
        self.element_var = element_var
        self.index_var = index_var

    @classmethod
    def create_iteration_var(
        cls,
        existing_var: AbstractVariable | None,
        name_prefix: str,
        source_name: str | None = None,
        type: CompiledType | None = None
    ) -> AbstractVariable:
        """
        Create (when needed) an iteration variable and assign it (when
        provided) a source name.

        :param existing_var: Existing iteration variable. If provided, do not
            create a new variable.
        :param name_prefix: Prefix for the name of this variable in the
            generated code.
        :param source_name: If available, name of this variable in the DSL.
        :param type: Type for the variable.
        """
        if existing_var is None:
            result = AbstractVariable(
                Location.builtin,
                names.Name(f"{name_prefix}_{next(cls._counter)}"),
                type=type,
            )
        else:
            assert type == existing_var.type, (
                    f"Inconsistent type for {existing_var}: {type} and"
                    f" {existing_var.type}"
            )
            result = existing_var

        if result.source_name is None and source_name is not None:
            result.source_name = source_name

        return result

    def construct_common(self) -> CollectionExpression.ConstructCommonResult:
        """
        Construct and return the expressions commonly needed by collection
        expression subclasses.
        """

        @dataclass
        class AbstractInitializedVar:
            """
            Variable associated to an optional initialization expression.

            It is only once local variables for them are created (call to
            .create_local_variable below) that we can construct them (turn
            AbstractVariable instances into VariableExpr ones), hence this
            intermediate class before InitializedVar.
            """
            var: AbstractVariable
            init_expr_constructor: (
                Callable[[], ResolvedExpression] | None
            ) = None

            def construct(self) -> InitializedVar:
                return InitializedVar(
                    construct_var(self.var),
                    (
                        None
                        if self.init_expr_constructor is None else
                        self.init_expr_constructor()
                    ),
                )

        iter_vars: list[AbstractInitializedVar] = []

        current_scope = PropertyDef.get_scope()

        # Because of the discrepancy between the storage type in list nodes
        # (always root nodes) and the element type that user code deals with
        # (non-root list elements and/or entities), we may need to introduce
        # variables and initializing expressions. This is what the code below
        # does.

        # First, build the collection expression. From the result, we can
        # deduce the type of the user element variable.
        collection_expr = construct(self.collection)

        # If the collection is actually an entity, unwrap the bare list node
        # and save the entity info for later.
        with_entities = collection_expr.type.is_entity_type
        if with_entities:
            saved_entity_coll_expr, collection_expr, entity_info = (
                collection_expr.destructure_entity()
            )
            collection_expr = SequenceExpr(
                None, saved_entity_coll_expr, collection_expr
            )

        check_source_language(
            collection_expr.type.is_collection,
            'Cannot iterate on {}, which is not a collection'.format(
                collection_expr.type.dsl_name
            )
        )

        # Now that potential entity types are unwrapped, we can look for the
        # collection element type.
        elt_type = collection_expr.type.element_type
        if with_entities:
            elt_type = elt_type.entity
        user_element_var = self.element_var
        user_element_var.set_type(elt_type)
        iter_vars.append(AbstractInitializedVar(user_element_var))

        # Now that all the iteration variables are typed, ensure that type
        # annotations for lambda arguments are correct.
        LambdaArgInfo.check_list(self.lambda_arg_infos)

        # Node lists contain bare nodes: if the user code deals with entities,
        # create a variable to hold a bare node and initialize the user
        # variable using it.
        if with_entities:
            assert self.element_var is not None and self.element_var._name
            entity_var = iter_vars[-1]
            node_var = AbstractVariable(
                Location.builtin,
                names.Name('Bare') + self.element_var._name,
                type=elt_type.element_type
            )
            entity_var.init_expr_constructor = lambda: make_as_entity(
                None, construct(node_var), entity_info=entity_info
            )
            iter_vars.append(AbstractInitializedVar(node_var))

        # Node lists contain root nodes: if the user code deals with non-root
        # nodes, create a variable to hold the root bare node and initialize
        # the non-root node using it.
        if (
            collection_expr.type.is_list_type
            and not collection_expr.type.is_root_node
        ):
            assert self.element_var._name is not None
            typed_elt_var = iter_vars[-1]
            untyped_elt_var = AbstractVariable(
                Location.builtin,
                names.Name('Untyped') + self.element_var._name,
                type=get_context().root_node_type,
            )
            typed_elt_var.init_expr_constructor = lambda: UncheckedCastExpr(
                construct(untyped_elt_var), typed_elt_var.var.type
            )
            iter_vars.append(AbstractInitializedVar(untyped_elt_var))

        # Keep track of the ultimate "codegen" element variable. Unlike all
        # other iteration variable, it is the only one that will be defined by
        # the "for" loop in Ada (the other ones must be declared as regular
        # local variables).
        codegen_element_var = iter_vars[-1].var

        # If requested, create the index variable
        if self.index_var:
            iter_vars.append(AbstractInitializedVar(self.index_var))

        # Create a scope to contain the code that runs during an iteration,
        # create the necessary local variables and lower the iteration
        # expression.
        with current_scope.new_child() as inner_scope:
            for v in iter_vars:
                if v.var is not codegen_element_var:
                    v.var.create_local_variable(inner_scope)
            inner_expr = construct(self.expr)

        return self.ConstructCommonResult(
            collection_expr=collection_expr,
            codegen_element_var=construct_var(codegen_element_var),
            user_element_var=construct_var(user_element_var),
            index_var=(
                None
                if self.index_var is None else
                construct_var(self.index_var)
            ),
            iter_vars=[v.construct() for v in iter_vars],
            inner_expr=inner_expr,
            inner_scope=inner_scope,
        )


class Contains(CollectionExpression):
    """
    Return whether `item` is an existing element in `collection`.
    """

    def __init__(
        self,
        location: Location,
        collection: AbstractExpression,
        item: AbstractExpression,
    ):
        """
        :param collection: The collection of which to check membership.
        :param item: The item to check in "collection".
        """
        from langkit.expressions import Eq

        self.item = item

        iter_var = self.create_iteration_var(
            existing_var=None, name_prefix="Item"
        )
        super().__init__(
            location=location,
            collection=collection,
            expr=Eq(Location.builtin, iter_var, self.item),
            lambda_arg_infos=[],
            element_var=iter_var,
        )

    def construct(self) -> ResolvedExpression:
        """
        Construct a resolved expression for this.
        """
        r = self.construct_common()
        assert r.index_var is None

        # "collection" contains "item" if at least one element in
        # "collection" is equal to "item".
        return Quantifier.Expr(self.debug_info, Quantifier.ANY, r)


class Map(CollectionExpression):
    """
    Abstract expression that is the result of a map expression evaluation.
    """

    class Expr(CollectionExpression.BaseExpr):
        """
        Resolved expression that represents a map expression in the generated
        code.
        """
        pretty_class_name = 'Map'

        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            common: CollectionExpression.ConstructCommonResult,
            filter: ResolvedExpression | None = None,
            do_concat: bool = False,
            take_while: ResolvedExpression | None = None,
        ):
            element_type = (common.inner_expr.type.element_type
                            if do_concat
                            else common.inner_expr.type)
            super().__init__(
                debug_info, "Map_Result", element_type.array, common
            )

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
                " (if {})".format(self.filter) if self.filter else ""
            )

        def _render_pre(self) -> str:
            return render('properties/map_ada', map=self, Name=names.Name)

        @property
        def subexprs(self) -> dict:
            result = super().subexprs
            if self.take_while:
                result['take_while'] = self.take_while
            if self.filter:
                result['filter'] = self.filter
            return result

    def __init__(
        self,
        location: Location,
        kind: str,
        collection: AbstractExpression,
        expr: AbstractExpression,
        lambda_arg_infos: list[LambdaArgInfo],
        element_var: AbstractVariable,
        index_var: AbstractVariable | None = None,
        filter_expr: AbstractExpression | None = None,
        take_while_expr: AbstractExpression | None = None,
        do_concat: bool = False,
    ):
        """
        :param kind: Name for this operation (map, filter, ...).

        :param filter_expr: If provided, a boolean expression which determines
            whether to include or exclude an item from the collection.

        :param take_while_expr: If provided, a boolean expression which
            determines whether to continue the iteration.

        :param do_concat: If true, "expr" must return arrays, and this
            expression returns the concatenation of all the arrays "expr"
            returns.

        See CollectionExpression for the other parameters.
        """
        super().__init__(
            location,
            collection,
            expr,
            lambda_arg_infos,
            element_var,
            index_var,
        )

        self.kind_name = kind
        self.filter_expr = filter_expr
        self.take_while_expr = take_while_expr
        self.do_concat = do_concat

    def construct(self) -> ResolvedExpression:
        """
        Construct a resolved expression for this map operation.
        """
        r = self.construct_common()

        check_source_language(
            not self.do_concat or r.inner_expr.type.is_collection,
            'Cannot mapcat with expressions returning {} values (collections'
            ' expected instead)'.format(r.inner_expr.type.dsl_name)
        )

        with r.inner_scope.use():
            filter_expr = (construct(self.filter_expr, T.Bool)
                           if self.filter_expr else None)

            take_while_expr = (construct(self.take_while_expr, T.Bool)
                               if self.take_while_expr else None)

        return Map.Expr(
            self.debug_info, r, filter_expr, self.do_concat, take_while_expr
        )

    def __repr__(self) -> str:
        kind = names.Name.from_lower(self.kind_name)
        return f"<{kind.camel} at {self.location_repr}>"


@abstract_expression_from_construct
def as_array(
    self: AbstractExpression,
    list_expr: AbstractExpression,
) -> ResolvedExpression:
    """
    Turn a list node into an array for the same elements.

    This is basically a shortcut to a map operation with the identity function.
    """
    iter_var = Map.create_iteration_var(
        existing_var=None, name_prefix="Item"
    )
    abstract_result = Map(
        Location.builtin,
        kind="as_array",
        collection=list_expr,
        expr=iter_var,
        lambda_arg_infos=[],
        element_var=iter_var,
    )
    result = construct(abstract_result)
    assert isinstance(result, Map.Expr)
    check_source_language(
        result.collection.type.is_list_type,
        '.as_array input must be an AST list (here: {})'.format(
            result.collection.type.dsl_name
        )
    )
    return result


class Quantifier(CollectionExpression):
    """
    Return whether `predicate` returns true for all the items in the input
    `collection`.

    For instance, this computes whether all integers in an array are positive:

    .. code:: python

        int_array.all(lambda i: i > 0)
    """

    class Expr(CollectionExpression.BaseExpr):
        pretty_class_name = 'Quantifier'

        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            kind: str,
            common: CollectionExpression.ConstructCommonResult,
        ):
            """
            :param kind: Kind for this quantifier expression. 'all' will check
                that all items in "collection" fullfill "expr" while 'any' will
                check that at least one of them does.

            :param common: Common iteration expression parameters.
            """
            super().__init__(debug_info, "Quantifier_Result", T.Bool, common)
            self.kind = kind
            self.static_type = T.Bool

        def _render_pre(self) -> str:
            return render(
                'properties/quantifier_ada', quantifier=self,
                ALL=Quantifier.ALL, ANY=Quantifier.ANY, Name=names.Name
            )

        @property
        def subexprs(self) -> dict:
            result = super().subexprs
            result["kind"] = self.kind
            return result

        def __repr__(self) -> str:
            return '<Quantifier.Expr {}>'.format(self.kind)

    # Available quantifier kinds
    ALL = 'all'
    ANY = 'any'

    def __init__(
        self,
        location: Location,
        kind: str,
        collection: AbstractExpression,
        predicate: AbstractExpression,
        lambda_arg_infos: list[LambdaArgInfo],
        element_var: AbstractVariable,
        index_var: AbstractVariable | None = None,
    ):
        """
        :param kind: Quantifier kind. ALL that checks "predicate" holds on all
            elements in "collection" while ANY checks that it holds on at least
            one of them.
        :param predicate: Boolean expression to evaluate on elements in
            "collection".

        See CollectionExpression for the other parameters.
        """
        super().__init__(
            location=location,
            collection=collection,
            expr=predicate,
            lambda_arg_infos=lambda_arg_infos,
            element_var=element_var,
            index_var=index_var,
        )
        assert kind in (self.ALL, self.ANY)
        self.kind = kind

    def construct(self) -> ResolvedExpression:
        """
        Construct a resolved expression for this quantifier expression.

        :rtype: QuantifierExpr
        """
        r = self.construct_common()

        check_source_language(
            r.inner_expr.type.matches(T.Bool),
            'Wrong type for expression in quantifier: expected bool,'
            ' got {}'.format(r.inner_expr.type.dsl_name)
        )

        return Quantifier.Expr(self.debug_info, self.kind, r)

    def __repr__(self) -> str:
        return f"<{self.kind.capitalize()}Quantifier at {self.location_repr}>"


@abstract_expression_from_construct
def collection_get(
    self: AbstractExpression,
    collection: AbstractExpression,
    index: AbstractExpression,
    or_null: bool,
) -> ResolvedExpression:
    """
    Get the ``index``-th element from ``collection``.

    Indexes are 0-based. As in Python, ``index`` can be negative, to retrieve
    elements in reverse order. For instance, ``expr.at(-1)`` will return the
    last element.

    :param or_null: If true, the expression will return null if the index is
        not valid for the collection. If False, it will raise an exception.
    """
    p = PropertyDef.get()

    # index yields a 0-based index and all the Get primitives expect 0-based
    # indexes, so there is no need to fiddle indexes here.
    index_expr = construct(index, T.Int)

    coll_expr = construct(collection)
    as_entity = coll_expr.type.is_entity_type
    if as_entity:
        saved_coll_expr, coll_expr, entity_info = (
            coll_expr.destructure_entity()
        )

    check_source_language(
        coll_expr.type.is_collection,
        '.at prefix must be a collection: got {} instead'.format(
            coll_expr.type.dsl_name
        )
    )

    # We process null list nodes as empty lists, so insert a null check before
    # getting the collection item only if asked to raise an exception.
    if not or_null:
        if coll_expr.type.is_ast_node:
            coll_expr = NullCheckExpr(coll_expr)
        elif coll_expr.type.is_entity_type:
            coll_expr = NullCheckExpr(coll_expr, implicit_deref=True)

    coll_expr, element_type = canonicalize_list(coll_expr, to_root_list=True)

    or_null_expr = BooleanLiteralExpr(None, or_null)
    result: ResolvedExpression = CallExpr(
        None if as_entity else self.debug_info,
        "Get_Result",
        "Get",
        element_type,
        [construct(p.node_var), coll_expr, index_expr, or_null_expr],
    )

    if as_entity:
        result = SequenceExpr(
            self.debug_info,
            saved_coll_expr,
            make_as_entity(None, result, entity_info),
        )

    return result


@abstract_expression_from_construct
def length(
    self: AbstractExpression,
    collection: AbstractExpression,
) -> ResolvedExpression:
    """
    Compute the length of ``collection``.
    """
    coll_expr = construct(collection)
    orig_type = coll_expr.type

    # Automatically unwrap entities
    if isinstance(coll_expr.type, EntityType):
        coll_expr = FieldAccessExpr(
            None,
            coll_expr,
            'Node',
            coll_expr.type.astnode,
            do_explicit_incref=False,
        )

    check_source_language(
        coll_expr.type.is_collection,
        'Collection expected but got {} instead'.format(orig_type.dsl_name))

    coll_expr, _ = canonicalize_list(coll_expr)
    return CallExpr(self.debug_info, "Len", "Length", T.Int, [coll_expr])


@abstract_expression_from_construct
def unique(
    self: AbstractExpression,
    array: AbstractExpression,
) -> ResolvedExpression:
    """
    Return a copy of ``array`` with duplicated elements removed.
    """
    from langkit.compile_context import AdaSourceKind

    array_expr = construct(array)
    array_type = array_expr.type
    if not isinstance(array_type, ArrayType):
        error(
            'Array expected but got {} instead'.format(array_type.dsl_name)
        )
    element_type = array_type.element_type
    check_source_language(
        element_type.hashable,
        'Element type (here {}) must be hashable'.format(element_type.dsl_name)
    )

    # Enable the generation of the function that does the actual work
    get_context().add_with_clause('Implementation', AdaSourceKind.body,
                                  'Ada.Containers.Hashed_Sets')
    array_type.require_unique_function()

    return CallExpr(
        self.debug_info,
        "Unique_Array",
        "Make_Unique",
        array_type,
        [array_expr],
    )


class SingletonExpr(ComputingExpr):
    pretty_class_name = 'ArraySingleton'

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        expr: ResolvedExpression,
    ):
        self.expr = expr
        self.static_type = self.expr.type.array

        super().__init__(debug_info, "Singleton")

    def _render_pre(self) -> str:
        result_var = self.result_var.name
        t = self.type
        assert isinstance(t, ArrayType)
        return self.expr.render_pre() + """
            {result_var} := {constructor} (Items_Count => 1);
            {result_var}.Items (1) := {item};
            {inc_ref}
        """.format(
            constructor=t.constructor_name,
            result_var=result_var,
            item=self.expr.render_expr(),
            inc_ref=('Inc_Ref ({}.Items (1));'.format(result_var)
                     if self.expr.type.is_refcounted else '')
        )

    @property
    def subexprs(self) -> list:
        return [self.expr]


def make_concat(
    debug_info: ExprDebugInfo | None,
    left: ResolvedExpression,
    right: ResolvedExpression,
) -> ResolvedExpression:
    """
    Create a concatenation expression (for arrays or strings).
    """
    # Handle strings as a special case
    if left.type.is_string_type:
        check_source_language(
            right.type.is_string_type,
            f"String type expected, got {right.type.dsl_name}",
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
            typ.is_array_type, f"Expected array type, got {typ.dsl_name}"
        )

    check_array(left.type)
    check_array(right.type)

    check_source_language(
        left.type == right.type,
        f"Got different array element types in concat:"
        f" {left.type.element_type.dsl_name} and"
        f" {right.type.element_type.dsl_name}",
    )

    return CallExpr(
        debug_info,
        "Concat_Result",
        "Concat",
        left.type,
        [left, right],
    )


@abstract_expression_from_construct
def singleton(
    self: AbstractExpression,
    expr: AbstractExpression,
) -> ResolvedExpression:
    """
    Return a 1-sized array whose only item is ``expr``.
    """
    return SingletonExpr(self.debug_info, construct(expr))


@abstract_expression_from_construct
def join(
    self: AbstractExpression,
    separator: AbstractExpression,
    strings: AbstractExpression,
) -> ResolvedExpression:
    """
    Return the concatenation of all strings in an array, with a separator
    between each.
    """
    return CallExpr(
        self.debug_info,
        "Join_Result",
        "Join_Strings",
        T.String,
        [construct(separator, T.String), construct(strings, T.String.array)],
    )


def make_to_iterator(
    debug_info: ExprDebugInfo | None,
    prefix: ResolvedExpression,
    node_data: AbstractNodeData,
    args: list[ResolvedExpression | None],
) -> ResolvedExpression:
    """
    Turn an array into an iterator.

    :param prefix: Expression for the array to turn into an iterator.
    :param node_data: "to_iterator" property that this expression calls in the
        DSL.
    :param args: Arguments for the "to_iterator" property (i.e. an empty list).
    :return: Resolved expression for the iterator creator.
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


class Find(CollectionExpression):
    """
    Return the first element in a collection that satisfies the given
    predicate.
    """

    class Expr(CollectionExpression.BaseExpr):
        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            common: CollectionExpression.ConstructCommonResult,
        ):
            super().__init__(
                debug_info, "Find_Result", common.user_element_var.type, common
            )

        def __repr__(self) -> str:
            return "<FindExpr>"

        def _render_pre(self) -> str:
            return render("properties/find_ada", find=self)

    def __init__(
        self,
        location: Location,
        collection: AbstractExpression,
        predicate: AbstractExpression,
        lambda_arg_infos: list[LambdaArgInfo],
        element_var: AbstractVariable,
        index_var: AbstractVariable | None = None,
    ):
        super().__init__(
            location=location,
            collection=collection,
            expr=predicate,
            lambda_arg_infos=lambda_arg_infos,
            element_var=element_var,
            index_var=index_var,
        )

    def construct(self) -> ResolvedExpression:
        r = self.construct_common()

        check_source_language(
            r.inner_expr.type.matches(T.Bool),
            "Predicate must return a boolean, got"
            f" {r.inner_expr.type.dsl_name}"
        )

        return Find.Expr(self.debug_info, r)
