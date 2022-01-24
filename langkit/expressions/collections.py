from __future__ import annotations

import inspect
from itertools import count
import types
from typing import Callable, List, Optional, Union

import funcy

from langkit import names
from langkit.compiled_types import CompiledType, get_context
from langkit.diagnostics import (
    check_multiple, check_source_language, check_type
)
from langkit.expressions.base import (
    AbstractExpression, AbstractNodeData, AbstractVariable, CallExpr,
    ComputingExpr, FieldAccessExpr, NullCheckExpr, PropertyDef,
    ResolvedExpression, SequenceExpr, T, UncheckedCastExpr, attr_call,
    attr_expr, auto_attr, auto_attr_custom, construct, render, unsugar
)
from langkit.expressions.envs import make_as_entity


def collection_expr_identity(x):
    return x


def collection_expr_none(x):
    return None


builtin_collection_functions = (collection_expr_identity, collection_expr_none)


def canonicalize_list(coll_expr, to_root_list=False):
    """
    If `coll_expr` returns a bare node, return an expression that converts it
    to the generic list type (if to_root_list=False) or the root list type (if
    to_root_list=True). Also return the element type for this collection.

    :type coll_expr: ResolvedExpression
    :rtype: (ResolvedExpression, CompiledType)
    """
    element_type = coll_expr.type.element_type
    if coll_expr.type.is_ast_node:
        # Compute the result's type according to to_root_list
        if to_root_list:
            dest_type = coll_expr.type
            while not dest_type.is_root_list_type:
                dest_type = dest_type.base
        else:
            dest_type = get_context().generic_list_type
    return (coll_expr, element_type)


class CollectionExpression(AbstractExpression):
    """
    Base class to provide common code for abstract expressions working on
    collections.
    """

    _counter = count()

    class ConstructCommonResult:
        """
        Holder for the result of the "construct_common" method.
        """

        def __init__(self, collection_expr, element_vars, index_var,
                     inner_expr, inner_scope):
            self.collection_expr = collection_expr
            """
            Resolved expression corresponding to the collection on which the
            iteration is done.

            :type: ResolvedExpression
            """

            self.element_vars = element_vars
            """
            List of "element" iteration variables, and their initialization
            expression, if any.

            we need a list of "element" iteration variables for code generation
            purposes. For instance, assuming we iterate on an entity that is an
            AST list, we need 3 variables::

              * one that contains the node whose type is the root one (AST
                lists contain only root nodes in the generated code);

              * one that contains the node that is casted to the proper type;

              * one that wraps this casted node as an entity.

            The first variable is the one used to expand iteration expressions
            (see the "user_element_var" property.  This is the one that must
            have a source name. The other ones are mere code generation
            temporaries.

            The last variable is the one that is used as the actual iteration
            variable in the generated code. This is the only one that will not
            require explicit initialization.

            :type: list[(ResolvedExpression, ResolvedExpression|None)]
            """

            self.index_var = index_var
            """
            The index variable as a resolved expression, if required.

            :type: ResolvedExpression|None
            """

            self.inner_expr = inner_expr
            """
            Resolved expression to be evaluated for each collection item.

            :type: ResolvedExpression
            """

            self.inner_scope = inner_scope
            """
            Local variable scope for the body of the iteration.

            :type: langkit.expressions.base.LocalVars.Scope
            """

    def __init__(self, collection, expr):
        """
        :param AbstractExpression collection: Collection on which this map
            operation works.

        :param expr: Function that takes the induction variable and returns an
            expression to evaluate for each item in "collection". If the
            function takes two parameters, the first one will also be the
            an induction variable for the iteration index.
        :type collection: AbstractExpression
        """
        super().__init__()
        self.collection = collection
        self.expr_fn = expr
        self.expr = None
        self.element_var = None
        self.requires_index = False
        self.index_var = None

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

        argspec = inspect.getargspec(expr_fn)

        check_multiple([
            (len(argspec.args) in (1, 2),
             'Invalid collection iteration lambda: only one'
             ' or two parameters expected'),
            (not argspec.varargs and not argspec.keywords,
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

    def do_prepare(self):
        # When this expression does not come from our Python DSL (see the
        # initialize method above), the sub-expression is ready to use: do not
        # try to expand the function.
        if self.expr is not None:
            return

        self.expr = self.prepare_iter_function("mapping expression",
                                               self.expr_fn)

    def construct_common(self):
        """
        Construct and return the expressions commonly needed by collection
        expression subclasses.

        :rtype: CollectionExpression.ConstructCommonResult
        """
        current_scope = PropertyDef.get_scope()

        # First, build the collection expression. From the result, we can
        # deduce the type of the element variable.
        collection_expr = construct(self.collection)
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

        elt_type = collection_expr.type.element_type
        if with_entities:
            elt_type = elt_type.entity
        self.element_var.set_type(elt_type)

        # List of "element" iteration variables
        elt_vars = [construct(self.element_var)]

        # List of initializing expressions for them
        elt_var_inits = []

        if with_entities:
            entity_var = elt_vars[-1]
            node_var = AbstractVariable(
                names.Name('Bare') + self.element_var._name,
                type=elt_type.element_type
            )
            elt_var_inits.append(make_as_entity(construct(node_var),
                                                entity_info=entity_info))
            elt_vars.append(construct(node_var))

        # If we are iterating over an AST list, then we get root grammar typed
        # values. We need to convert them to the more specific type to get the
        # rest of the expression machinery work.
        if collection_expr.type.is_list_type:
            typed_elt_var = elt_vars[-1]
            untyped_elt_var = AbstractVariable(
                names.Name('Untyped') + self.element_var._name,
                type=get_context().root_grammar_class
            )
            # Initialize the former last variable with a cast from the new last
            # variable and push the new last variable.
            elt_var_inits.append(UncheckedCastExpr(construct(untyped_elt_var),
                                                   typed_elt_var.type))
            elt_vars.append(construct(untyped_elt_var))

        # Only then we can build the inner expression
        with current_scope.new_child() as inner_scope:
            inner_expr = construct(self.expr)

        if with_entities:
            entity_var.abstract_var.create_local_variable(inner_scope)
        if collection_expr.type.is_list_type:
            typed_elt_var.abstract_var.create_local_variable(inner_scope)

        if self.index_var:
            self.index_var.add_to_scope(inner_scope)

        elt_var_inits.append(None)

        return self.ConstructCommonResult(
            collection_expr,
            funcy.lzip(elt_vars, elt_var_inits),
            construct(self.index_var) if self.index_var else None,
            inner_expr,
            inner_scope
        )


@attr_call('contains')
class Contains(CollectionExpression):
    """
    Return whether `item` is an existing element in `collection`.
    """

    def __init__(self, collection, item):
        """
        :param AbstractExpression collection: The collection of which to check
            membership.
        :param AbstractExpression item: The item to check in "collection".
        """
        self.item = item
        super().__init__(collection, lambda item: item.equals(self.item))

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: QuantifierExpr
        """
        r = self.construct_common()
        assert r.index_var is None

        # "collection" contains "item" if at least one element in
        # "collection" is equal to "item".
        return Quantifier.Expr(Quantifier.ANY, r.collection_expr, r.inner_expr,
                               r.element_vars, r.index_var, r.inner_scope)

    def __repr__(self):
        return '<Contains>'


@attr_call('filter')
def _filter(collection, filter):
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
def filtermap(collection, expr, filter):
    """
    Shortcut to perform :dsl:`filter` and :dsl:`map` in a single shot.
    """
    return Map(collection, expr, filter)


@attr_call('map')
def map(collection, expr):
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
def mapcat(collection, expr):
    """
    Like :dsl:`map`, except that `expr` is expected to return arrays:
    this returns an array that is the concatenation of all the returned
    arrays.
    """
    return Map(collection, expr, do_concat=True)


@attr_call('take_while')
def take_while(collection, take_while_pred):
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

    class Expr(ComputingExpr):
        """
        Resolved expression that represents a map expression in the generated
        code.
        """
        pretty_class_name = 'Map'

        def __init__(self, element_vars, index_var, collection, expr,
                     iter_scope, filter=None, do_concat=False, take_while=None,
                     abstract_expr=None):
            """
            :type element_vars: list[(ResolvedExpression,
                                      ResolvedExpression|None)]
            :type index_var: None|VarExpr
            :type collection: ResolvedExpression
            :type expr: ResolvedExpression
            :type iter_scope: langkit.expressions.base.LocalVars.Scope
            :type filter: ResolvedExpression
            :type do_concat: bool
            :type take_while: ResolvedExpression
            :type abstract_expr: AbstractExpression|None
            """
            self.take_while = take_while
            self.element_vars = element_vars
            self.index_var = index_var
            self.collection = collection
            self.expr = expr
            self.filter = filter
            self.do_concat = do_concat
            self.iter_scope = iter_scope

            element_type = (self.expr.type.element_type
                            if self.do_concat else
                            self.expr.type)
            self.static_type = element_type.array
            self.static_type.require_vector()

            with iter_scope.parent.use():
                super().__init__('Map_Result', abstract_expr=abstract_expr)

        def __repr__(self):
            return "<MapExpr {}: {} -> {}{}>".format(
                self.collection,
                self.element_vars[0],
                self.expr,
                " (if {})".format(self.filter) if self.filter else ""
            )

        def _render_pre(self):
            return render('properties/map_ada', map=self, Name=names.Name)

        @property
        def subexprs(self):
            result = {
                'collection': self.collection,
                'element-vars-initalizers': [e for _, e in self.element_vars],
                'expr': self.expr
            }
            if self.take_while:
                result['take_while'] = self.take_while
            if self.filter:
                result['filter'] = self.filter
            return result

        def _bindings(self):
            return filter(
                lambda v: v is not None,
                [v for v, _ in self.element_vars] + [self.index_var]
            )

    def __init__(self, collection, expr, filter=collection_expr_none,
                 do_concat=False, take_while_pred=collection_expr_none):
        """
        See CollectionExpression for the other parameters.

        :param filter: If provided, a function that takes an induction variable
            and that returns a boolean expression which says whether to include
            or exclude an item from the collection.
        :type filter: None|(AbstractExpression) -> AbstractExpression

        :param bool do_concat: If true, "expr" must return arrays, and this
            expression returns the concatenation of all the arrays "expr"
            returns.

        :param take_while_pred: If provided, a function that takes an
            induction variable and that returns a boolean expression which says
            whether to continue the map or not.
        :type take_while_pred: None|(AbstractExpression) -> AbstractExpression
        """
        super().__init__(collection, expr)

        self.filter_fn = filter

        self.take_while_pred = take_while_pred
        self.do_concat = do_concat
        self.filter_expr = None
        self.take_while_expr = None

    @classmethod
    def create_expanded(
        cls,
        collection: AbstractExpression,
        expr: AbstractExpression,
        element_var: AbstractVariable,
        index_var: Optional[AbstractVariable] = None,
        filter_expr: Optional[AbstractExpression] = None,
    ) -> Map:
        result = cls(collection, None)
        result.initialize(expr, element_var, index_var)
        result.filter_expr = filter_expr
        return result

    def do_prepare(self):
        super().do_prepare()

        if self.filter_expr is None:
            self.filter_expr = self.prepare_iter_function("filter expression",
                                                          self.filter_fn)
        if self.take_while_expr is None:
            self.take_while_expr = self.prepare_iter_function(
                "take while expression",
                self.take_while_pred,
            )

    def construct(self):
        """
        Construct a resolved expression for this map operation.

        :rtype: MapExpr
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

        return Map.Expr(r.element_vars, r.index_var, r.collection_expr,
                        r.inner_expr, r.inner_scope, filter_expr,
                        self.do_concat, take_while_expr, abstract_expr=self)

    @property
    def kind(self):
        """
        Identify the specific kind for this map expression: simple map, mapcat,
        filter or take_while.

        :rtype: str
        """
        if self.expr_fn == collection_expr_identity:
            return (
                'take_while'
                if self.filter_fn == collection_expr_none else 'filter'
            )
        if self.filter_fn != collection_expr_none:
            return 'filter_map'
        return 'mapcat' if self.do_concat else 'map'

    def __repr__(self):
        return '<{}>'.format(self.kind)


@auto_attr
def as_array(self, ast_list):
    """
    Turn an AST list node into an array for the same elements.

    This is basically a shortcut to a map operation with the identity function.
    """
    abstract_result = Map(ast_list, expr=collection_expr_identity)
    abstract_result.prepare()
    result = construct(abstract_result)
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

    class Expr(ComputingExpr):
        static_type = T.Bool
        pretty_class_name = 'Quantifier'

        serial_generator = iter(count(1))
        """
        Generator of unique IDs for this expression. Used to create unique
        labels in the generated code.
        """

        def __init__(self, kind, collection, expr, element_vars, index_var,
                     iter_scope, abstract_expr=None):
            """
            :param str kind: Kind for this quantifier expression. 'all' will
                check that all items in "collection" fullfill "expr" while
                'any' will check that at least one of them does.

            :param ResolvedExpression collection: Collection on which this map
                operation works.

            :param ResolvedExpression expr: A boolean expression to evaluate on
                the collection's items.

            :param element_vars: Variable to use in "expr".
            :type element_vars: list[(ResolvedExpression,
                                      ResolvedExpression|None)]

            :param index_var: Index variable to use in "expr".
            :type index_var: None|ResolvedExpression

            :param iter_scope: Scope for local variables internal to the
                iteration.
            :type iter_scope: langkit.expressions.base.LocalVars.Scope

            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.kind = kind
            self.collection = collection
            self.expr = expr
            self.element_vars = element_vars
            self.index_var = index_var
            self.iter_scope = iter_scope
            self.static_type = T.Bool

            self.exit_label = f"Exit_{next(self.serial_generator)}"
            """
            Exit label for early loop exits in the generated code.
            """

            with iter_scope.parent.use():
                super().__init__(
                    'Quantifier_Result', abstract_expr=abstract_expr
                )

        def _render_pre(self):
            return render(
                'properties/quantifier_ada', quantifier=self,
                ALL=Quantifier.ALL, ANY=Quantifier.ANY, Name=names.Name
            )

        @property
        def subexprs(self):
            return {
                'kind': self.kind,
                'collection': self.collection,
                'expr': self.expr,
                'element-vars-initalizers': [e for _, e in self.element_vars],
            }

        def _bindings(self):
            return filter(
                lambda v: v is not None,
                [v for v, _ in self.element_vars] + [self.index_var]
            )

        def __repr__(self):
            return '<Quantifier.Expr {}>'.format(self.kind)

    # Available quantifier kinds
    ALL = 'all'
    ANY = 'any'

    def __init__(self, collection, predicate, kind):
        """
        See CollectionExpression for the other parameters.

        :param AbstractExpression predicate: Boolean expression to evaluate on
            elements in "collection".
        :param str kind: Quantifier kind. ALL that checks "predicate" holds on
            all elements in "collection" while ANY checks that it holds on at
            least one of them.
        """
        super().__init__(collection, predicate)
        assert kind in (self.ALL, self.ANY)
        self.kind = kind

    def construct(self):
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

        return Quantifier.Expr(self.kind, r.collection_expr, r.inner_expr,
                               r.element_vars, r.index_var, r.inner_scope)

    def __repr__(self):
        return '<{}Quantifier>'.format(self.kind.capitalize())


@auto_attr_custom('at', or_null=True)
@auto_attr_custom('at_or_raise', or_null=False,
                  doc='Like :dsl:`at`, but raise a property error when the'
                      ' index is out of bounds.')
def collection_get(self, collection, index, or_null):
    """
    Get the `index`\\ -th element from `collection`.

    Indexes are 0-based. As in Python, `index` can be negative, to retrieve
    elements in reverse order. For instance, ``expr.at(-1)`` will return the
    last element.

    :param bool or_null: If true, the expression will return null if the
        index is not valid for the collection. If False, it will raise an
        exception.
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

    or_null = construct(or_null)
    result = CallExpr('Get_Result', 'Get', element_type,
                      [coll_expr, index_expr, or_null])

    if as_entity:
        result = SequenceExpr(saved_coll_expr,
                              make_as_entity(result, entity_info))

    result.abstract_expr = self
    return result


@auto_attr
def length(self, collection):
    """
    Compute the length of `collection`.
    """
    coll_expr = construct(collection)
    orig_type = coll_expr.type

    # Automatically unwrap entities
    if coll_expr.type.is_entity_type:
        coll_expr = FieldAccessExpr(coll_expr, 'Node', coll_expr.type.astnode,
                                    do_explicit_incref=False)

    check_source_language(
        coll_expr.type.is_collection,
        'Collection expected but got {} instead'.format(orig_type.dsl_name))

    coll_expr, _ = canonicalize_list(coll_expr)
    return CallExpr('Len', 'Length', T.Int, [coll_expr], abstract_expr=self)


@auto_attr
def unique(self, array):
    """
    Return a copy of `array` with duplicated elements removed.
    """
    from langkit.compile_context import AdaSourceKind

    array_expr = construct(array)
    array_type = array_expr.type
    check_source_language(
        array_type.is_array_type,
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

        def __init__(self, expr, abstract_expr=None):
            """
            :type expr: ResolvedExpression
            """
            self.expr = expr
            self.static_type = self.expr.type.array

            super().__init__('Singleton', abstract_expr=abstract_expr)

        def _render_pre(self):
            result_var = self.result_var.name
            return self.expr.render_pre() + """
                {result_var} := {constructor} (Items_Count => 1);
                {result_var}.Items (1) := {item};
                {inc_ref}
            """.format(
                constructor=self.type.constructor_name,
                result_var=result_var,
                item=self.expr.render_expr(),
                inc_ref=('Inc_Ref ({}.Items (1));'.format(result_var)
                         if self.expr.type.is_refcounted else '')
            )

        @property
        def subexprs(self):
            return [self.expr]

    def __init__(self, expr):
        """
        :param AbstractExpression expr: The expression representing the
            single element to create the collection from.
        :param bool coerce_null: If False, always return a 1-sized array.
            Otherwise, return an empty array when `expr` is null.
        """
        super().__init__()
        self.expr = expr

    def construct(self):
        return CollectionSingleton.Expr(construct(self.expr))

    def __repr__(self):
        return '<CollectionSingleton>'


@attr_call('concat')
class Concat(AbstractExpression):
    """
    Return the concatenation of `array_1` and `array_2`. Both must be arrays of
    the same type, or both must be strings.
    """

    def __init__(self, array_1, array_2):
        """
        :param AbstractExpression array_1: The first array expression.
        :param AbstractExpression array_2: The second array expression.
        """
        super().__init__()
        self.array_1 = array_1
        self.array_2 = array_2

    def construct(self):
        array_1 = construct(self.array_1)
        array_2 = construct(self.array_2)

        # Handle strings as a special case
        if array_1.type.is_string_type:
            check_source_language(
                array_2.type.is_string_type,
                "String type expected, got {}".format(array_2.type.dsl_name)
            )
            return CallExpr(
                "Concat_Result", "Concat_String", T.String, [array_1, array_2],
                abstract_expr=self,
            )

        def check_array(typ):
            check_source_language(
                typ.is_array_type,
                "Expected array type, got {}".format(typ.dsl_name)
            )

        check_array(array_1.type)
        check_array(array_2.type)

        check_multiple([
            (array_1.type == array_2.type,
             "Got different array element types in concat: {} and {}".format(
                 array_1.type.element_type.dsl_name,
                 array_2.type.element_type.dsl_name
             )),
        ])

        return CallExpr('Concat_Result', 'Concat', array_1.type,
                        [array_1, array_2],
                        abstract_expr=self)

    def __repr__(self):
        return '<Concat>'


@attr_call("join")
class Join(AbstractExpression):
    """
    Return the concatenation of all strings in an array, with a separator
    between each.
    """

    def __init__(self, separator, strings):
        super().__init__()
        self.separator = separator
        self.strings = strings

    def __repr__(self):
        return '<Join>'

    def construct(self):
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
