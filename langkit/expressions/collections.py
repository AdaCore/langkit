from __future__ import annotations

from dataclasses import dataclass
import inspect
from itertools import count
import types
from typing import Any, Callable, List, Optional, Tuple, Union

from langkit import names
from langkit.compiled_types import (
    ASTNodeType, ArrayType, CompiledType, EntityType, get_context,
)
from langkit.diagnostics import (
    check_multiple, check_source_language, check_type, error,
)
from langkit.expressions.base import (
    AbstractExpression, AbstractNodeData, AbstractVariable, CallExpr,
    ComputingExpr, FieldAccessExpr, LocalVars, NullCheckExpr, PropertyDef,
    ResolvedExpression, Self, SequenceExpr, T, UncheckedCastExpr, VariableExpr,
    attr_call, attr_expr, auto_attr, auto_attr_custom, construct,
    construct_var, render, unsugar
)
from langkit.expressions.envs import make_as_entity


# The following functions are used as "lambda" expressions in the DSL. We do
# not support Python 3 annotations for them (inspect.getargspec), so we cannot
# annotate them. We will likely get rid of these "lambda" during the transition
# to Lkt, so there is little interest to start supporting annotations in this
# context: just leave these functions un-annotated for now.

def collection_expr_identity(x):  # type: ignore
    return x


def collection_expr_none(x):  # type: ignore
    return None


builtin_collection_functions: Tuple[
    Callable[[AbstractExpression], AbstractExpression],
    Callable[[AbstractExpression], None],
] = (collection_expr_identity, collection_expr_none)


def canonicalize_list(
    coll_expr: ResolvedExpression,
    to_root_list: bool = False
) -> Tuple[ResolvedExpression, CompiledType]:
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
    init_expr: Optional[ResolvedExpression] = None


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

        def __init__(self,
                     result_var_name: str,
                     result_type: CompiledType,
                     common: CollectionExpression.ConstructCommonResult,
                     abstract_expr: Optional[AbstractExpression] = None):
            self.static_type = result_type

            self.collection = common.collection_expr

            self.codegen_element_var = common.codegen_element_var
            self.user_element_var = common.user_element_var
            self.index_var = common.index_var

            self.iter_vars = common.iter_vars

            self.inner_expr = common.inner_expr
            self.inner_scope = common.inner_scope

            super().__init__(result_var_name, abstract_expr=abstract_expr)

        @property
        def subexprs(self) -> dict:
            return {
                'collection': self.collection,
                'iter-vars-initalizers': [v.init_expr for v in self.iter_vars],
                'inner_expr': self.inner_expr,
            }

        def _bindings(self) -> List[VariableExpr]:
            return [v.var for v in self.iter_vars]

    class ConstructCommonResult:
        """
        Holder for the result of the "construct_common" method.
        """

        def __init__(self,
                     collection_expr: ResolvedExpression,
                     codegen_element_var: VariableExpr,
                     user_element_var: VariableExpr,
                     index_var: Optional[VariableExpr],
                     iter_vars: List[InitializedVar],
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

    def __init__(self, collection: AbstractExpression, expr: Any):
        """
        :param AbstractExpression collection: Collection on which this map
            operation works.

        :param expr: Function that takes the induction variable and returns an
            expression to evaluate for each item in "collection". If the
            function takes two parameters, the first one will also be the
            an induction variable for the iteration index.
        """
        super().__init__()
        self.collection = collection
        self.expr_fn = expr
        self.expr: AbstractExpression
        self.expr_initialized = False
        self.element_var: Optional[AbstractVariable] = None
        self.requires_index: bool = False
        self.index_var: Optional[AbstractVariable] = None

    def initialize(self,
                   expr: AbstractExpression,
                   element_var: AbstractVariable,
                   index_var: Optional[AbstractVariable] = None) -> None:
        """
        Initialize this expression using already expanded sub-expressions.

        This is useful when building expressions with item sub-expressions
        outside of our Python DSL.
        """
        self.expr = expr
        self.expr_initialized = True
        self.element_var = element_var
        self.requires_index = index_var is not None
        self.index_var = index_var

    def create_iteration_var(
        self,
        existing_var: Optional[AbstractVariable],
        name_prefix: str,
        source_name: Optional[str] = None,
        type: Optional[CompiledType] = None
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
                names.Name(f"{name_prefix}_{next(self._counter)}"),
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

    def prepare_iter_function(
        self,
        what: str,
        expr_fn: Union[Callable[[AbstractExpression], AbstractExpression],
                       Callable[[AbstractExpression, AbstractExpression],
                                AbstractExpression]]
    ) -> AbstractExpression:
        """
        Validate an iteration function, whether it only takes the iteration
        element or also the iteration index.

        Return the abstract expression to evaluate for each iteration.

        :param what: Purpose of this function. Used for diagnostics.
        :param expr_fn: Function to prepare.
        """
        check_type(
            expr_fn,
            types.FunctionType,
            "{what} passed to a collection expression must be a lambda or a"
            " function"
        )

        argspec = inspect.getfullargspec(expr_fn)

        check_multiple([
            (len(argspec.args) in (1, 2),
             'Invalid collection iteration lambda: only one'
             ' or two parameters expected'),
            (not argspec.varargs and not argspec.varkw,
             'Invalid collection iteration lambda: no *args or **kwargs'),
            (not argspec.defaults,
             'Invalid collection iteration lambda: No default values allowed'
             ' for arguments')
        ])

        # Get source names for the iteration variables
        index_required = False
        index_varname: Optional[str] = None
        element_varname: Optional[str] = None
        if len(argspec.args) == 2:
            index_required = True
            self.requires_index = True
            index_varname = argspec.args[0]
            element_varname = argspec.args[1]
        else:
            element_varname = argspec.args[0]

        # We are interested in names from user sources: disregard names from
        # the default functions function we define here.
        if expr_fn in builtin_collection_functions:
            index_varname = None
            element_varname = None

        # Make sure we have an iteration element variable
        self.element_var = self.create_iteration_var(
            self.element_var, "Item", element_varname
        )

        # Expand the function. If the index is required, make sure we have an
        # iteration variable for it.
        if index_required:
            self.index_var = self.create_iteration_var(
                self.index_var, "Index", index_varname, T.Int
            )
            expr = expr_fn(self.index_var, self.element_var)  # type: ignore
        else:
            expr = expr_fn(self.element_var)  # type: ignore

        return unsugar(expr)

    def do_prepare(self) -> None:
        # When this expression does not come from our Python DSL (see the
        # initialize method above), the sub-expression is ready to use: do not
        # try to expand the function.
        if self.expr_initialized:
            return

        self.expr = self.prepare_iter_function("mapping expression",
                                               self.expr_fn)
        self.expr_initialized = True

    def construct_common(self) -> CollectionExpression.ConstructCommonResult:
        """
        Construct and return the expressions commonly needed by collection
        expression subclasses.
        """
        assert self.element_var is not None

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
            collection_expr = SequenceExpr(saved_entity_coll_expr,
                                           collection_expr)

        check_source_language(
            collection_expr.type.is_collection,
            'Cannot iterate on {}, which is not a collection'.format(
                collection_expr.type.dsl_name
            )
        )

        # Now that potential entity types are unwrapped, we can look for its
        # element type.
        elt_type = collection_expr.type.element_type
        if with_entities:
            elt_type = elt_type.entity
        self.element_var.set_type(elt_type)
        user_element_var = construct(self.element_var)
        assert isinstance(user_element_var, VariableExpr)

        # List of element variables, and the associated initialization
        # expressions (when applicable).
        #
        # Start with the only element variable that exists at this point: the
        # one that the user code for each iteration uses directly. When
        # relevant, each step in the code below creates a new variable N and
        # initialize variable N-1 from it.
        element_vars: List[InitializedVar] = [InitializedVar(user_element_var)]

        # Node lists contain bare nodes: if the user code deals with entities,
        # create a variable to hold a bare node and initialize the user
        # variable using it.
        if with_entities:
            assert self.element_var is not None and self.element_var._name
            entity_var = element_vars[-1]
            node_var = AbstractVariable(
                names.Name('Bare') + self.element_var._name,
                type=elt_type.element_type
            )
            entity_var.init_expr = make_as_entity(
                construct(node_var), entity_info=entity_info
            )
            element_vars.append(InitializedVar(construct_var(node_var)))

        # Node lists contain root nodes: if the user code deals with non-root
        # nodes, create a variable to hold the root bare node and initialize
        # the non-root node using it.
        if (
            collection_expr.type.is_list_type
            and not collection_expr.type.is_root_node
        ):
            assert self.element_var._name is not None
            typed_elt_var = element_vars[-1]
            untyped_elt_var = AbstractVariable(
                names.Name('Untyped') + self.element_var._name,
                type=get_context().root_grammar_class
            )
            typed_elt_var.init_expr = UncheckedCastExpr(
                construct(untyped_elt_var), typed_elt_var.var.type
            )
            element_vars.append(InitializedVar(construct_var(untyped_elt_var)))

        # Keep track of the ultimate "codegen" element variable. Unlike all
        # other iteration variable, it is the only one that will be defined by
        # the "for" loop in Ada (the other ones must be declared as regular
        # local variables).
        codegen_element_var = element_vars[-1].var

        # Create a scope to contain the code that runs during an iteration and
        # lower the iteration expression.
        with current_scope.new_child() as inner_scope:
            inner_expr = construct(self.expr)

        # Build the list of all iteration variables
        iter_vars = list(element_vars)
        index_var = None
        if self.index_var:
            index_var = construct_var(self.index_var)
            iter_vars.append(InitializedVar(index_var))

        # Create local variables for all iteration variables that need it
        for v in iter_vars:
            if v.var != codegen_element_var:
                v.var.abstract_var.create_local_variable(inner_scope)

        return self.ConstructCommonResult(
            collection_expr,
            codegen_element_var,
            user_element_var,
            index_var,
            iter_vars,
            inner_expr,
            inner_scope,
        )


@attr_call('contains')
class Contains(CollectionExpression):
    """
    Return whether `item` is an existing element in `collection`.
    """

    def __init__(self,
                 collection: AbstractExpression,
                 item: AbstractExpression):
        """
        :param collection: The collection of which to check membership.
        :param item: The item to check in "collection".
        """
        self.item = item
        super().__init__(collection, lambda item: item.equals(self.item))

    def construct(self) -> ResolvedExpression:
        """
        Construct a resolved expression for this.
        """
        r = self.construct_common()
        assert r.index_var is None

        # "collection" contains "item" if at least one element in
        # "collection" is equal to "item".
        return Quantifier.Expr(Quantifier.ANY, r, abstract_expr=self)


@attr_call('filter')
def _filter(collection: AbstractExpression, filter: Any) -> AbstractExpression:
    """
    Filter elements in `collection`.

    This return an array that only contains items from `collection` for which
    the `filter` predicate returned true. For instance, to filter all null AST
    nodes in an array:

    .. code:: python

        node_array.filter(lambda n: Not(n.is_null))
    """
    return Map(collection, collection_expr_identity, filter)


@attr_call('filtermap')
def filtermap(collection: AbstractExpression,
              expr: Any,
              filter: Any) -> AbstractExpression:
    """
    Shortcut to perform :dsl:`filter` and :dsl:`map` in a single shot.
    """
    return Map(collection, expr, filter)


@attr_call('map')
def map(collection: AbstractExpression,
        expr: AbstractExpression) -> AbstractExpression:
    """
    Return an array of the results of evaluating `expr` to the items of
    `collection`.

    For instance, to return an array that contains all parents of an array of
    AST nodes:

    .. code:: python

        node_array.map(lambda n: n.parent)
    """
    return Map(collection, expr)


@attr_call('mapcat')
def mapcat(collection: AbstractExpression,
           expr: AbstractExpression) -> AbstractExpression:
    """
    Like :dsl:`map`, except that `expr` is expected to return arrays:
    this returns an array that is the concatenation of all the returned
    arrays.
    """
    return Map(collection, expr, do_concat=True)


@attr_call('take_while')
def take_while(collection: AbstractExpression,
               take_while_pred: AbstractExpression) -> AbstractExpression:
    """
    Return an array that contains all items in `collection` until the first one
    for which the `take_while_pred` predicate returns false.

    For instance, this return an array that contains the first parents of
    `node` whose type is a subtype of some `Scope` AST node type:

    .. code:: python

        node.parents.take_while(lambda n: n.is_a(Scope))
    """
    return Map(collection, collection_expr_identity,
               take_while_pred=take_while_pred)


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

        def __init__(self,
                     common: CollectionExpression.ConstructCommonResult,
                     filter: Optional[ResolvedExpression] = None,
                     do_concat: bool = False,
                     take_while: Optional[ResolvedExpression] = None,
                     abstract_expr: Optional[AbstractExpression] = None):
            element_type = (common.inner_expr.type.element_type
                            if do_concat
                            else common.inner_expr.type)
            super().__init__(
                "Map_Result",
                element_type.array,
                common,
                abstract_expr=abstract_expr,
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

    def __init__(self,
                 collection: AbstractExpression,
                 expr: Any,
                 filter: Any = collection_expr_none,
                 do_concat: bool = False,
                 take_while_pred: Any = collection_expr_none):
        """
        See CollectionExpression for the other parameters.

        :param filter: If provided, a function that takes an induction variable
            and that returns a boolean expression which says whether to include
            or exclude an item from the collection.

        :param do_concat: If true, "expr" must return arrays, and this
            expression returns the concatenation of all the arrays "expr"
            returns.

        :param take_while_pred: If provided, a function that takes an induction
            variable and that returns a boolean expression which says whether
            to continue the map or not.
        """
        super().__init__(collection, expr)

        self.filter_fn = filter

        self.take_while_pred = take_while_pred
        self.do_concat = do_concat
        self.filter_expr: Optional[AbstractExpression] = None
        self.take_while_expr: Optional[AbstractExpression] = None

    @classmethod
    def create_expanded(
        cls,
        collection: AbstractExpression,
        expr: AbstractExpression,
        element_var: AbstractVariable,
        index_var: Optional[AbstractVariable] = None,
        filter_expr: Optional[AbstractExpression] = None,
        take_while_expr: Optional[AbstractExpression] = None,
        do_concat: bool = False,
    ) -> Map:
        result = cls(collection, None, do_concat=do_concat)
        result.initialize(expr, element_var, index_var)
        result.filter_expr = filter_expr
        result.take_while_expr = take_while_expr
        return result

    def do_prepare(self) -> None:
        super().do_prepare()

        if self.filter_expr is None:
            self.filter_expr = self.prepare_iter_function("filter expression",
                                                          self.filter_fn)
        if self.take_while_expr is None:
            self.take_while_expr = self.prepare_iter_function(
                "take while expression",
                self.take_while_pred,
            )

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
            r,
            filter_expr,
            self.do_concat,
            take_while_expr,
            abstract_expr=self,
        )

    @property
    def kind(self) -> str:
        """
        Identify the specific kind for this map expression: simple map, mapcat,
        filter or take_while.
        """
        if self.expr_fn == collection_expr_identity:
            return (
                'take_while'
                if self.filter_fn == collection_expr_none else 'filter'
            )
        if self.filter_fn != collection_expr_none:
            return 'filter_map'
        return 'mapcat' if self.do_concat else 'map'

    def __repr__(self) -> str:
        kind = names.Name.from_lower(self.kind)
        return f"<{kind.camel} at {self.location_repr}>"


@auto_attr
def as_array(self: AbstractExpression,
             ast_list: AbstractExpression) -> ResolvedExpression:
    """
    Turn an AST list node into an array for the same elements.

    This is basically a shortcut to a map operation with the identity function.
    """
    abstract_result = Map(ast_list, expr=collection_expr_identity)
    abstract_result.prepare()
    result = construct(abstract_result)
    assert isinstance(result, Map.Expr)
    check_source_language(
        result.collection.type.is_list_type,
        '.as_array input must be an AST list (here: {})'.format(
            result.collection.type.dsl_name
        )
    )
    return result


@attr_call('all', kind='all')
@attr_call('any', kind='any',
           doc='Like :dsl:`all`, but return true as soon as the predicate'
               ' returns true for one collection item.')
class Quantifier(CollectionExpression):
    """
    Return whether `predicate` returns true for all the items in the input
    `collection`.

    For instance, this computes whether all integers in an array are positive:

    .. code:: python

        int_array.all(lambda i: i > 0)
    """

    class Expr(CollectionExpression.BaseExpr):
        static_type = T.Bool
        pretty_class_name = 'Quantifier'

        def __init__(self,
                     kind: str,
                     common: CollectionExpression.ConstructCommonResult,
                     abstract_expr: Optional[AbstractExpression] = None):
            """
            :param kind: Kind for this quantifier expression. 'all' will check
                that all items in "collection" fullfill "expr" while 'any' will
                check that at least one of them does.

            :param common: Common iteration expression parameters.

            :param abstract_expr: See ResolvedExpression's constructor.
            """
            super().__init__(
                "Quantifier_Result",
                T.Bool,
                common,
                abstract_expr=abstract_expr
            )
            self.kind = kind

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

    def __init__(self,
                 collection: AbstractExpression,
                 predicate: Any,
                 kind: str):
        """
        See CollectionExpression for the other parameters.

        :param predicate: Boolean expression to evaluate on elements in
            "collection".
        :param kind: Quantifier kind. ALL that checks "predicate" holds on all
            elements in "collection" while ANY checks that it holds on at least
            one of them.
        """
        super().__init__(collection, predicate)
        assert kind in (self.ALL, self.ANY)
        self.kind = kind

    @classmethod
    def create_expanded(
        cls,
        kind: str,
        collection: AbstractExpression,
        predicate: AbstractExpression,
        element_var: AbstractVariable,
        index_var: Optional[AbstractVariable] = None,
    ) -> Quantifier:
        result = cls(collection, None, kind)
        result.initialize(predicate, element_var, index_var)
        result.expr = predicate
        return result

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

        return Quantifier.Expr(self.kind, r, abstract_expr=self)

    def __repr__(self) -> str:
        return f"<{self.kind.capitalize()}Quantifier at {self.location_repr}>"


@auto_attr_custom('at', or_null=True)
@auto_attr_custom('at_or_raise', or_null=False,
                  doc='Like :dsl:`at`, but raise a property error when the'
                      ' index is out of bounds.')
def collection_get(self: AbstractExpression,
                   collection: AbstractExpression,
                   index: AbstractExpression,
                   or_null: bool) -> ResolvedExpression:
    """
    Get the `index`\\ -th element from `collection`.

    Indexes are 0-based. As in Python, `index` can be negative, to retrieve
    elements in reverse order. For instance, ``expr.at(-1)`` will return the
    last element.

    :param or_null: If true, the expression will return null if the index is
        not valid for the collection. If False, it will raise an exception.
    """
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

    or_null_expr = construct(or_null)
    result: ResolvedExpression = CallExpr(
        'Get_Result', 'Get', element_type,
        [construct(Self), coll_expr, index_expr, or_null_expr]
    )

    if as_entity:
        result = SequenceExpr(saved_coll_expr,
                              make_as_entity(result, entity_info))

    result.abstract_expr = self
    return result


@auto_attr
def length(self: AbstractExpression,
           collection: AbstractExpression) -> ResolvedExpression:
    """
    Compute the length of `collection`.
    """
    coll_expr = construct(collection)
    orig_type = coll_expr.type

    # Automatically unwrap entities
    if isinstance(coll_expr.type, EntityType):
        coll_expr = FieldAccessExpr(coll_expr, 'Node', coll_expr.type.astnode,
                                    do_explicit_incref=False)

    check_source_language(
        coll_expr.type.is_collection,
        'Collection expected but got {} instead'.format(orig_type.dsl_name))

    coll_expr, _ = canonicalize_list(coll_expr)
    return CallExpr('Len', 'Length', T.Int, [coll_expr], abstract_expr=self)


@auto_attr
def unique(self: AbstractExpression,
           array: AbstractExpression) -> ResolvedExpression:
    """
    Return a copy of `array` with duplicated elements removed.
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

    return CallExpr('Unique_Array', 'Make_Unique', array_type, [array_expr],
                    abstract_expr=self)


@attr_expr('singleton')
class CollectionSingleton(AbstractExpression):
    """
    Return a 1-sized array whose only item is `expr`.
    """

    class Expr(ComputingExpr):
        pretty_class_name = 'ArraySingleton'

        def __init__(self,
                     expr: ResolvedExpression,
                     abstract_expr: Optional[AbstractExpression] = None):
            self.expr = expr
            self.static_type = self.expr.type.array

            super().__init__('Singleton', abstract_expr=abstract_expr)

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

    def __init__(self, expr: AbstractExpression):
        """
        :param expr: The expression representing the single element to create
            the collection from.
        """
        super().__init__()
        self.expr = expr

    def construct(self) -> ResolvedExpression:
        return CollectionSingleton.Expr(construct(self.expr))


@attr_call('concat')
class Concat(AbstractExpression):
    """
    Return the concatenation of `array_1` and `array_2`. Both must be arrays of
    the same type, or both must be strings.
    """

    def __init__(self,
                 array_1: AbstractExpression,
                 array_2: AbstractExpression):
        """
        :param array_1: The first array expression.
        :param array_2: The second array expression.
        """
        super().__init__()
        self.array_1 = array_1
        self.array_2 = array_2

    @staticmethod
    def create_constructed(
        left: ResolvedExpression,
        right: ResolvedExpression,
        abstract_expr: AbstractExpression | None = None,
    ) -> ResolvedExpression:
        # Handle strings as a special case
        if left.type.is_string_type:
            check_source_language(
                right.type.is_string_type,
                "String type expected, got {}".format(right.type.dsl_name)
            )
            return CallExpr(
                "Concat_Result", "Concat_String", T.String, [left, right],
                abstract_expr=abstract_expr,
            )

        def check_array(typ: CompiledType) -> None:
            check_source_language(
                typ.is_array_type,
                "Expected array type, got {}".format(typ.dsl_name)
            )

        check_array(left.type)
        check_array(right.type)

        check_multiple([
            (left.type == right.type,
             "Got different array element types in concat: {} and {}".format(
                 left.type.element_type.dsl_name,
                 right.type.element_type.dsl_name
             )),
        ])

        return CallExpr('Concat_Result', 'Concat', left.type,
                        [left, right],
                        abstract_expr=abstract_expr)

    def construct(self) -> ResolvedExpression:
        left = construct(self.array_1)
        right = construct(self.array_2)
        return self.create_constructed(left, right)


@attr_call("join")
class Join(AbstractExpression):
    """
    Return the concatenation of all strings in an array, with a separator
    between each.
    """

    def __init__(self,
                 separator: AbstractExpression,
                 strings: AbstractExpression):
        super().__init__()
        self.separator = separator
        self.strings = strings

    def construct(self) -> ResolvedExpression:
        separator = construct(self.separator, T.String)
        strings = construct(self.strings, T.String.array)
        return CallExpr("Join_Result", "Join_Strings", T.String,
                        [separator, strings], abstract_expr=self)


def make_to_iterator(
    prefix: ResolvedExpression,
    node_data: AbstractNodeData,
    args: List[Optional[ResolvedExpression]],
    abstract_expr: Optional[AbstractExpression] = None
) -> ResolvedExpression:
    """
    Turn an array into an iterator.

    :param prefix: Expression for the array to turn into an iterator.
    :param node_data: "to_iterator" property that this expression calls in the
        DSL.
    :param args: Arguments for the "to_iterator" property (i.e. an empty list).
    :param abstract_expr: See ResolvedExpression's constructor.
    :return: Resolved expression for the iterator creator.
    """
    assert not args
    elt_type = prefix.type.element_type

    # Make sure we generate code for this iterator type
    elt_type.create_iterator(used=True)

    return CallExpr(
        result_var_name="Iter",
        name=node_data.name,
        type=elt_type.iterator,
        exprs=[prefix, "Self.Unit.Context"],
        shadow_args=[node_data],
        abstract_expr=abstract_expr,
    )


@attr_call("find")
class Find(CollectionExpression):
    """
    Return the first element in a collection that satisfies the given
    predicate.
    """

    class Expr(CollectionExpression.BaseExpr):
        def __init__(self,
                     common: CollectionExpression.ConstructCommonResult,
                     abstract_expr: Optional[AbstractExpression] = None):
            super().__init__(
                "Find_Result",
                common.user_element_var.type,
                common,
                abstract_expr=abstract_expr,
            )

        def __repr__(self) -> str:
            return "<FindExpr>"

        def _render_pre(self) -> str:
            return render("properties/find_ada", find=self)

    def __init__(self,
                 collection: AbstractExpression,
                 predicate: Any):
        super().__init__(collection, predicate)

    @classmethod
    def create_expanded(
        cls,
        collection: AbstractExpression,
        expr: AbstractExpression,
        element_var: AbstractVariable,
        index_var: Optional[AbstractVariable] = None,
    ) -> Find:
        result = cls(collection, None)
        result.initialize(expr, element_var, index_var)
        return result

    def construct(self) -> ResolvedExpression:
        r = self.construct_common()

        check_source_language(
            r.inner_expr.type.matches(T.Bool),
            "Predicate must return a boolean, got"
            f" {r.inner_expr.type.dsl_name}"
        )

        return Find.Expr(r, abstract_expr=self)
