import inspect
from itertools import count

from langkit import names
from langkit.compiled_types import BoolType, LongType
from langkit.diagnostics import check_multiple, check_source_language
from langkit.expressions.base import (
    AbstractExpression, construct, ResolvedExpression, AbstractVariable,
    render, PropertyDef, BuiltinCallExpr
)
from langkit.utils import assert_type


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

        self.requires_index = len(argspec.args) == 2
        self.element_var = AbstractVariable(
            names.Name("Item_{}".format(next(CollectionExpression._counter))),
        )
        if self.requires_index:
            self.index_var = AbstractVariable(
                names.Name('I'), type=LongType,
                create_local=True
            )
            expr = self.expr_fn(self.index_var, self.element_var)
        else:
            expr = self.expr_fn(self.element_var)
        self.expr = assert_type(expr, AbstractExpression)

    def construct_common(self):
        """
        Construct the expressions commonly needed by collection expression
        subclasses, and return them as a tuple constituted of:

        1. The resolved collection expression.
        2. The resolved expression function passed to CollectionExpression's
           constructor.
        3. The element variable as a resolved expression.
        4. The index variable as a resolved expression.
        5. The inner scope for the iteration.

        :rtype: (ResolvedExpression, ResolvedExpression,
                 ResolvedExpression, ResolvedExpression,
                 langkit.expressions.base.LocalVars.Scope)
        """
        collection_expr = construct(
            self.collection, lambda t: t.is_collection(),
            'Map cannot iterate on {expr_type}, which is not a collection'
        )

        with PropertyDef.get_scope().new_child() as iter_scope:
            if self.index_var:
                PropertyDef.get_scope().add(self.index_var.local_var)
            self.element_var.set_type(collection_expr.type.element_type())

            return (collection_expr,
                    construct(self.expr),
                    construct(self.element_var),
                    construct(self.index_var) if self.index_var else None,
                    iter_scope)


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
         element_var,
         index_var,
         iter_scope) = self.construct_common()
        assert index_var is None

        # "collection" contains "item" if at least one element in
        # "collection" is equal to "item".
        return Quantifier.Expr(Quantifier.ANY, collection, predicate,
                               element_var, index_var, iter_scope)


class Map(CollectionExpression):
    """
    Abstract expression that is the result of a map expression evaluation.
    """

    class Expr(ResolvedExpression):
        """
        Resolved expression that represents a map expression in the generated
        code.
        """

        def __init__(self, element_var, index_var, collection, expr,
                     iter_scope, filter=None, concat=False, take_while=None):
            """
            :type element_var: VarExpr
            :type index_var: None|VarExpr
            :type collection: ResolvedExpression
            :type expr: ResolvedExpression
            :type iter_scope: langkit.expressions.base.LocalVars.Scope
            :type filter: ResolvedExpression
            :type concat: bool
            :type take_while: ResolvedExpression
            """
            self.take_while = take_while
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

            self.array_var = PropertyDef.get().vars.create_scopeless(
                'Map', self.type
            )
            iter_scope.parent.add(self.array_var)

            super(Map.Expr, self).__init__()

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
            return self.array_var.name.camel_with_underscores

    def __init__(self, collection, expr, filter_expr=lambda x: None,
                 concat=False, take_while_pred=lambda x: None):
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
        self.filter_expr = self.filter_fn(self.element_var)
        self.take_while_expr = self.take_while_pred(self.element_var)

    def construct(self):
        """
        Construct a resolved expression for this map operation.

        :rtype: MapExpr
        """
        (collection_expr,
         expr,
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

        return Map.Expr(element_var, index_var, collection_expr, expr,
                        iter_scope, filter_expr, self.concat,
                        take_while_expr)


class Quantifier(CollectionExpression):
    """
    Expression that tests a predicate over the items of a collection.
    """

    class Expr(ResolvedExpression):
        static_type = BoolType

        def __init__(self, kind, collection, expr, element_var, index_var,
                     iter_scope):
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

            :param element_var: Variable to use in "expr".
            :type element_var: ResolvedExpression

            :param index_var: Index variable to use in "expr".
            :type index_var: None|ResolvedExpression

            :param iter_scope: Scope for local variables internal to the
                iteration.
            :type iter_scope: langkit.expressions.base.LocalVars.Scope
            """
            self.kind = kind
            self.collection = collection
            self.expr = expr
            self.element_var = element_var
            self.index_var = index_var
            self.iter_scope = iter_scope
            self.result_var = PropertyDef.get().vars.create_scopeless(
                'Quantifier_Result', BoolType
            )
            iter_scope.parent.add(self.result_var)

            super(Quantifier.Expr, self).__init__()

        def _render_pre(self):
            return render(
                'properties/quantifier_ada', quantifier=self,
                ALL=Quantifier.ALL, ANY=Quantifier.ANY, Name=names.Name
            )

        def _render_expr(self):
            return self.result_var.name.camel_with_underscores

        def __repr__(self):
            return '<Quantifier.Expr {}>'.format(self.kind)

    # Available quantifier kinds
    ALL = 'all'
    ANY = 'any'

    def __init__(self, kind, collection, predicate):
        """
        See CollectionExpression for the other parameters.

        :param str kind: Quantifier kind. ALL that checks "predicate" holds on
            all elements in "collection" while ANY checks that it holds on at
            least one of them.
        :param AbstractExpression predicate: Boolean expression to evaluate on
            elements in "collection".
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
         element_var,
         index_var,
         iter_scope) = self.construct_common()

        check_source_language(
            expr.type.matches(BoolType),
            "Wrong type for expression in quantifier: expected bool, "
            "got {}".format(expr.type.name().camel)
        )

        return Quantifier.Expr(self.kind, collection_expr, expr,
                               element_var, index_var, iter_scope)


class CollectionGet(AbstractExpression):
    """
    Expression that will get an element from a collection.
    """

    class Expr(ResolvedExpression):
        def __init__(self, coll_expr, index_expr, or_null):
            """
            :type coll_expr: ResolvedExpression
            :type index_expr: ResolvedExpression
            :type or_null: ResolvedExpression
            """
            res_type = coll_expr.type.element_type()
            self.result_var = PropertyDef.get().vars.create('Get_Result',
                                                            res_type)
            self.call_expr = BuiltinCallExpr(
                'Get', res_type,
                [coll_expr, index_expr, or_null]
            )

            super(CollectionGet.Expr, self).__init__()

        @property
        def type(self):
            return self.call_expr.type

        def _render_pre(self):
            return ('{}\n'
                    '{} := {};').format(
                self.call_expr.render_pre(),
                self.result_var.name,
                self.call_expr.render_expr(),
            )

        def _render_expr(self):
            return self.result_var.name

    def __init__(self, coll_expr, index_expr, or_null=True):
        """
        :param AbstractExpression coll_expr: The expression representing the
            collection to get from.
        :param AbstractExpression index_expr: The expression representing the
            index of the element to get.
        :param bool or_null: If true, the expression will return null if the
            index is not valid for the collection. If False, it will raise an
            exception.
        """
        super(CollectionGet, self).__init__()
        self.coll_expr = coll_expr
        self.index_expr = index_expr
        self.or_null = or_null

    def construct(self):
        return CollectionGet.Expr(
            construct(self.coll_expr, lambda t: t.is_collection()),
            construct(self.index_expr, LongType),
            construct(self.or_null)
        )


class CollectionLength(AbstractExpression):
    """
    Expression that will return the length of a collection.
    """

    def __init__(self, coll_expr):
        """
        :param AbstractExpression coll_expr: The expression representing the
            collection to get from.
        """
        super(CollectionLength, self).__init__()
        self.coll_expr = coll_expr

    def construct(self):
        return BuiltinCallExpr(
            "Length", LongType,
            [construct(self.coll_expr, lambda t: t.is_collection())]
        )


class CollectionSingleton(AbstractExpression):
    """
    Expression that will return a collection of a single element, given the
    single element.
    """

    class Expr(ResolvedExpression):
        def __init__(self, expr):
            """
            :type expr: ResolvedExpression
            """
            self.expr = expr

            self.expr.type.array_type().add_to_context()
            self.static_type = self.expr.type.array_type()

            self.array_var = PropertyDef.get().vars.create('Singleton',
                                                           self.type)

            super(CollectionSingleton.Expr, self).__init__()

        def _render_pre(self):
            return self.expr.render_pre() + """
            {array_var} := Create (Items_Count => 1);
            {array_var}.Items (1) := {item};
            """.format(array_var=self.array_var.name,
                       array_type=self.static_type.pointed(),
                       item=self.expr.render_expr())

        def _render_expr(self):
            return self.array_var.name

    def __init__(self, expr):
        """
        :param AbstractExpression expr: The expression representing the
            single element to create the collection from.
        """
        super(CollectionSingleton, self).__init__()
        self.expr = expr

    def construct(self):
        return CollectionSingleton.Expr(construct(self.expr))
