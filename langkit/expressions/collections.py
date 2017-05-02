from __future__ import absolute_import, division, print_function

import inspect
from itertools import count

import types

from langkit import names
from langkit.compiled_types import BoolType, LongType, ArrayType, get_context
from langkit.diagnostics import (
    check_multiple, check_source_language, check_type
)
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, BuiltinCallExpr, PropertyDef,
    ResolvedExpression, attr_expr, attr_call, auto_attr_custom, auto_attr,
    construct, render, unsugar
)


def collection_expr_identity(x):
    return x


def collection_expr_none(x):
    return None


builtin_collection_functions = (collection_expr_identity, collection_expr_none)


class CollectionExpression(AbstractExpression):
    """
    Base class to provide common code for abstract expressions working on
    collections.
    """

    _counter = count()

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
        super(CollectionExpression, self).__init__()
        self.collection = collection
        self.expr_fn = expr
        self.expr = None
        self.list_element_var = None
        self.element_var = None
        self.requires_index = False
        self.index_var = None

    def do_prepare(self):
        argspec = inspect.getargspec(self.expr_fn)

        check_multiple([
            (len(argspec.args) in (1, 2),
             'Invalid collection iteration lambda: only one '
             'or two parameters expected'),
            (not argspec.varargs and not argspec.keywords,
             'Invalid collection iteration lambda: no *args or **kwargs'),
            (not argspec.defaults,
             'Invalid collection iteration lambda: No default values allowed '
             'for arguments')
        ])

        # Get the name of the loop variable from the DSL. But don't when we
        # have a "default" one, such as for using the ".filter" combinator. In
        # this case, it's up to ".filter"'s special implementation to get the
        # name from the filter function.
        source_name = (None if self.expr_fn == collection_expr_identity else
                       names.Name.from_lower(argspec.args[0]))

        self.requires_index = len(argspec.args) == 2
        self.element_var = AbstractVariable(
            names.Name("Item_{}".format(next(CollectionExpression._counter))),
            source_name=source_name
        )
        if self.requires_index:
            self.index_var = AbstractVariable(
                names.Name('I'), type=LongType, create_local=True,
                source_name=names.Name.from_lower(argspec.args[1])
            )
            expr = self.expr_fn(self.index_var, self.element_var)
        else:
            expr = self.expr_fn(self.element_var)
        self.expr = unsugar(expr)

    def construct_common(self):
        """
        Construct the expressions commonly needed by collection expression
        subclasses, and return them as a tuple constituted of:

        1. The resolved collection expression.
        2. The resolved expression function passed to CollectionExpression's
           constructor.
        3. If the collection is an AST list, the iteration variable, whose type
           is the root grammar type. None otherwise.
        4. The element variable as a resolved expression. In the case of an AST
           list collection, this is just 3. converted to the specific type.
        5. The index variable as a resolved expression.
        6. The inner scope for the iteration.

        :rtype: (ResolvedExpression,
                 ResolvedExpression,
                 ResolvedExpression|None,
                 ResolvedExpression,
                 ResolvedExpression,
                 langkit.expressions.base.LocalVars.Scope)
        """
        collection_expr = construct(
            self.collection, lambda t: t.is_collection(),
            'Map cannot iterate on {expr_type}, which is not a collection'
        )
        self.element_var.set_type(collection_expr.type.element_type())

        current_scope = PropertyDef.get_scope()

        # If we are iterating over an AST list, then we get root grammar typed
        # values. We need to convert them to the more specific type to get the
        # rest of the expression machinery work. For this, create a new
        # variable.
        if collection_expr.type.is_list_type:
            self.list_element_var = AbstractVariable(
                names.Name("List_Item_{}".format(
                    next(CollectionExpression._counter)
                )),
                type=get_context().root_grammar_class
            )
            self.element_var.add_to_scope(current_scope)

        with current_scope.new_child() as iter_scope:
            if self.index_var:
                PropertyDef.get_scope().add(self.index_var.local_var)

            return (collection_expr,
                    construct(self.expr),
                    (construct(self.list_element_var)
                     if self.list_element_var else
                     None),
                    construct(self.element_var),
                    construct(self.index_var) if self.index_var else None,
                    iter_scope)


@attr_expr("contains")
class Contains(CollectionExpression):
    """
    Abstract expression for a membership test expression.
    """

    def __init__(self, collection, item):
        """
        :param AbstractExpression collection: The collection of which to check
            membership.
        :param AbstractExpression item: The item to check in "collection".
        """
        self.item = item
        super(Contains, self).__init__(
            collection,
            lambda item: item.equals(self.item),
        )

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: QuantifierExpr
        """
        (collection,
         predicate,
         list_element_var,
         element_var,
         index_var,
         iter_scope) = self.construct_common()
        assert index_var is None

        # "collection" contains "item" if at least one element in
        # "collection" is equal to "item".
        return Quantifier.Expr(Quantifier.ANY, collection, predicate,
                               list_element_var, element_var, index_var,
                               iter_scope)


@attr_call('filter', collection_expr_identity)
@attr_call('filtermap')
@attr_call('map', filter_expr=collection_expr_none)
@attr_call('mapcat', filter_expr=collection_expr_none, concat=True)
@attr_call('take_while', collection_expr_identity, collection_expr_none, False)
class Map(CollectionExpression):
    """
    Abstract expression that is the result of a map expression evaluation.
    """

    class Expr(ResolvedExpression):
        """
        Resolved expression that represents a map expression in the generated
        code.
        """
        pretty_class_name = 'Map'

        def __init__(self, list_element_var, element_var, index_var,
                     collection, expr, iter_scope, filter=None, concat=False,
                     take_while=None, abstract_expr=None):
            """
            :type list_element_var: VarExpr|None
            :type element_var: VarExpr
            :type index_var: None|VarExpr
            :type collection: ResolvedExpression
            :type expr: ResolvedExpression
            :type iter_scope: langkit.expressions.base.LocalVars.Scope
            :type filter: ResolvedExpression
            :type concat: bool
            :type take_while: ResolvedExpression
            :type abstract_expr: AbstractExpression|None
            """
            self.take_while = take_while
            self.list_element_var = list_element_var
            self.element_var = element_var
            self.index_var = index_var
            self.collection = collection
            self.expr = expr
            self.filter = filter
            self.concat = concat
            self.iter_scope = iter_scope

            element_type = (self.expr.type.element_type()
                            if self.concat else
                            self.expr.type)
            self.static_type = element_type.array_type()
            self.static_type.add_to_context()

            super(Map.Expr, self).__init__(result_var_name='Map',
                                           scopeless_result_var=True,
                                           abstract_expr=abstract_expr)
            iter_scope.parent.add(self.result_var)

        def __repr__(self):
            return "<MapExpr {}: {} -> {}{}>".format(
                self.collection,
                self.element_var,
                self.expr,
                " (if {})".format(self.filter) if self.filter else ""
            )

        def _render_pre(self):
            return render('properties/map_ada', map=self, Name=names.Name)

        def _render_expr(self):
            return self.result_var.name.camel_with_underscores

        @property
        def subexprs(self):
            result = {'collection': self.collection, 'expr': self.expr}
            if self.take_while:
                result['take_while'] = self.take_while
            if self.filter:
                result['filter'] = self.filter
            return result

        def _bindings(self):
            return [var for var in [self.element_var, self.index_var]
                    if var is not None]

    def __init__(self, collection, expr, filter_expr=collection_expr_none,
                 concat=False, take_while_pred=collection_expr_none):
        """
        See CollectionExpression for the other parameters.

        :param filter_expr: If provided, a function that takes an induction
            variable and that returns a boolean expression which says whether
            to include or exclude an item from the collection.
        :type filter_expr: None|(AbstractExpression) -> AbstractExpression

        :param bool concat: If true, "expr" must return arrays, and this
            expression returns the concatenation of all the arrays "expr"
            returns.

        :param take_while_pred: If provided, a function that takes an
            induction variable and that returns a boolean expression which says
            whether to continue the map or not.
        :type take_while_pred: None|(AbstractExpression) -> AbstractExpression
        """
        super(Map, self).__init__(collection, expr)

        self.filter_fn = filter_expr

        self.take_while_pred = take_while_pred
        self.concat = concat
        self.filter_expr = None
        self.take_while_expr = None

    def do_prepare(self):
        super(Map, self).do_prepare()

        self.filter_expr = check_type(
            self.filter_fn, types.FunctionType,
            "Filter expression passed to a collection expression must be a "
            "lambda or a function"
        )(self.element_var)

        self.take_while_expr = check_type(
            self.take_while_pred, types.FunctionType,
            "Take while expression passed to a collection expression must be a"
            " lambda or a function"
        )(self.element_var)

        # If the element transformation function is actually the built-in
        # identity function, try instead to get the source name of the loop
        # variable from the other functions.
        if self.element_var.source_name is None:
            for fn in (self.filter_fn, self.take_while_pred):
                if fn not in builtin_collection_functions:
                    argspec = inspect.getargspec(fn)
                    self.element_var.source_name = names.Name.from_lower(
                        argspec.args[0]
                    )
                    break

    def construct(self):
        """
        Construct a resolved expression for this map operation.

        :rtype: MapExpr
        """
        (collection_expr,
         expr,
         list_element_var,
         element_var,
         index_var,
         iter_scope) = self.construct_common()

        check_source_language(
            not self.concat or expr.type.is_collection(),
            'Cannot mapcat with expressions returning {} values (collections'
            ' expected instead)'.format(expr.type.name())
        )

        with iter_scope.use():
            filter_expr = (construct(self.filter_expr, BoolType)
                           if self.filter_expr else None)

            take_while_expr = (construct(self.take_while_expr, BoolType)
                               if self.take_while_expr else None)

        return Map.Expr(list_element_var, element_var, index_var,
                        collection_expr, expr, iter_scope, filter_expr,
                        self.concat, take_while_expr, abstract_expr=self)

    def __repr__(self):
        name = None
        if self.expr_fn == collection_expr_identity:
            name = ('TakeWhile'
                    if self.filter_fn == collection_expr_none else
                    'Filter')
        if not name:
            name = 'MapCat' if self.concat else 'Map'
        return '<{}>'.format(name)


@auto_attr
def as_array(self, list_expr):
    """
    Turn an AST list node into an array for the same elements.

    This is basically a shortcut to a map operation with the identity function.

    :param AbstractExpression list_expr: The AST list to convert.
    :rtype: ResolvedExpression
    """
    abstract_result = Map(list_expr, expr=collection_expr_identity)
    abstract_result.prepare()
    result = construct(abstract_result)
    root_list_type = get_context().generic_list_type
    check_source_language(
        issubclass(result.collection.type, root_list_type),
        '.as_array input must be an AST list (here: {})'.format(
            result.collection.type.name().camel
        )
    )
    return result


@attr_call('all', kind='all')
@attr_call('any', kind='any')
class Quantifier(CollectionExpression):
    """
    Expression that tests a predicate over the items of a collection.
    """

    class Expr(ResolvedExpression):
        static_type = BoolType
        pretty_class_name = 'Quantifier'

        def __init__(self, kind, collection, expr, list_element_var,
                     element_var, index_var, iter_scope, abstract_expr=None):
            """
            :param str kind: Kind for this quantifier expression. 'all' will
                check that all items in "collection" fullfill "expr" while
                'any' will check that at least one of them does.

            :param ResolvedExpression expr: Expression to evaluate for each
                item in "collection".

            :param ResolvedExpression collection: Collection on which this map
                operation works.

            :param ResolvedExpression expr: A boolean expression to evaluate on
                the collection's items.

            :param list_element_var: When the collection is an AST list,
                variable that holds the element we are currently processing
                during the iteration, typed as root grammar type.  None
                otherwise.
            :type: ResolvedExpression|None

            :param element_var: Variable to use in "expr".
            :type element_var: ResolvedExpression

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
            self.list_element_var = list_element_var
            self.element_var = element_var
            self.index_var = index_var
            self.iter_scope = iter_scope
            self.static_type = BoolType

            super(Quantifier.Expr, self).__init__('Quantifier_Result',
                                                  scopeless_result_var=True,
                                                  abstract_expr=abstract_expr)
            iter_scope.parent.add(self.result_var)

        def _render_pre(self):
            return render(
                'properties/quantifier_ada', quantifier=self,
                ALL=Quantifier.ALL, ANY=Quantifier.ANY, Name=names.Name
            )

        def _render_expr(self):
            return self.result_var.name.camel_with_underscores

        @property
        def subexprs(self):
            return {'kind': self.kind,
                    'collection': self.collection,
                    'expr': self.expr}

        def _bindings(self):
            return [var for var in [self.element_var, self.index_var]
                    if var is not None]

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
        super(Quantifier, self).__init__(collection, predicate)
        assert kind in (self.ALL, self.ANY)
        self.kind = kind

    def construct(self):
        """
        Construct a resolved expression for this quantifier expression.

        :rtype: QuantifierExpr
        """
        (collection_expr,
         expr,
         list_element_var,
         element_var,
         index_var,
         iter_scope) = self.construct_common()

        check_source_language(
            expr.type.matches(BoolType),
            "Wrong type for expression in quantifier: expected bool, "
            "got {}".format(expr.type.name().camel)
        )

        return Quantifier.Expr(self.kind, collection_expr, expr,
                               list_element_var, element_var, index_var,
                               iter_scope)


@auto_attr_custom("at")
@auto_attr_custom("at_or_raise", or_null=False)
def collection_get(self, coll_expr, index_expr, or_null=True):
    """
    Expression that will get an element from a collection.

    :param AbstractExpression coll_expr: The expression representing the
        collection to get from.
    :param AbstractExpression index_expr: The expression representing the
        index of the element to get.
    :param bool or_null: If true, the expression will return null if the
        index is not valid for the collection. If False, it will raise an
        exception.
    """
    # index_expr yields a 0-based index and all the Get primitives expect
    # 0-based indexes, so there is no need to fiddle indexes here.
    index_expr = construct(index_expr, LongType)

    coll_expr = construct(coll_expr, lambda t: t.is_collection())
    or_null = construct(or_null)
    return BuiltinCallExpr(
        'Get', coll_expr.type.element_type(),
        [coll_expr, index_expr, or_null],
        'Get_Result',
        abstract_expr=self,
    )


@auto_attr
def length(self, coll_expr):
    """
    Expression that will return the length of a collection.

    :param AbstractExpression coll_expr: The expression representing the
        collection to get from.
    """
    return BuiltinCallExpr(
        "Length", LongType,
        [construct(coll_expr, lambda t: t.is_collection())],
        abstract_expr=self,
    )


@attr_expr('singleton')
@attr_expr('to_array', coerce_null=True)
class CollectionSingleton(AbstractExpression):
    """
    Expression that will return a collection of a single element, given the
    single element.
    """

    class Expr(ResolvedExpression):
        pretty_class_name = 'ArraySingleton'

        def __init__(self, expr, abstract_expr=None):
            """
            :type expr: ResolvedExpression
            """
            self.expr = expr

            self.expr.type.array_type().add_to_context()
            self.static_type = self.expr.type.array_type()

            super(CollectionSingleton.Expr, self).__init__(
                result_var_name='Singleton',
                abstract_expr=abstract_expr
            )

        def _render_pre(self):
            return self.expr.render_pre() + """
            {result_var} := Create (Items_Count => 1);
            {result_var}.Items (1) := {item};
            """.format(result_var=self.result_var.name,
                       item=self.expr.render_expr())

        def _render_expr(self):
            return self.result_var.name

        @property
        def subexprs(self):
            return [self.expr]

    def __init__(self, expr, coerce_null=False):
        """
        :param AbstractExpression expr: The expression representing the
            single element to create the collection from.
        """
        super(CollectionSingleton, self).__init__()
        self.expr = expr
        self.coerce_null = coerce_null

    def construct(self):
        from langkit.expressions import If, IsNull, EmptyArray

        expr = construct(self.expr)
        expr_var = expr.create_result_var('To_Array_Prefix')

        # Use "expr" only for first evaluation, and then use expr_var to refer
        # to the result. We do this to avoid resolved expression sharing in the
        # expression tree.
        ret = CollectionSingleton.Expr(expr_var if self.coerce_null else expr)
        if self.coerce_null:
            return If.Expr(
                IsNull.construct_static(expr),
                EmptyArray.construct_static(expr.type.array_type()),
                ret,
                ret.type
            )
        else:
            return ret


@attr_call('concat')
class Concat(AbstractExpression):
    """
    Expression that will concatenate two arrays.
    """

    def __init__(self, array_1, array_2):
        """
        :param AbstractExpression array_1: The first array expression.
        :param AbstractExpression array_2: The second array expression.
        """
        super(Concat, self).__init__()
        self.array_1 = array_1
        self.array_2 = array_2

    def construct(self):
        array_1 = construct(self.array_1)
        array_2 = construct(self.array_2)

        # TODO: We don't use the type param to construct because construct will
        # try to cast arrays to the base array type. Would be better if
        # construct handled that correctly.
        check_type(array_1.type, ArrayType)
        check_type(array_2.type, ArrayType)

        check_multiple([
            (array_1.type == array_2.type,
             "Got different array element types in concat: {} and {}".format(
                 array_1.type.element_type().name(),
                 array_2.type.element_type().name()
             )),
        ])

        return BuiltinCallExpr(
            "Concat", array_1.type, [array_1, array_2], "Concat_Result"
        )
