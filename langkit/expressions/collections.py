import inspect
from itertools import count

from langkit import names
from langkit.compiled_types import BoolType, LongType
from langkit.expressions.base import (
    AbstractExpression, construct, ResolvedExpression, AbstractVariable,
    render, Property, BuiltinCallExpr
)
from langkit.utils import assert_type, user_assert


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
        self.ind_var = None

        argspec = inspect.getargspec(self.expr_fn)
        user_assert(len(argspec.args) in (1, 2) and
                    not argspec.varargs and
                    not argspec.keywords and
                    not argspec.defaults,
                    'Invalid collection iteration lambda: only one or two'
                    ' parameters expected')
        self.requires_index = len(argspec.args) == 2
        self.index_var = None

    def do_prepare(self):
        self.ind_var = AbstractVariable(
            names.Name("Item_{}".format(next(CollectionExpression._counter))),
        )
        if self.requires_index:
            self.index_var = AbstractVariable(
                names.Name('I'), type=LongType,
                create_local=True
            )
            expr = self.expr_fn(self.index_var, self.ind_var)
        else:
            expr = self.expr_fn(self.ind_var)
        self.expr = assert_type(expr, AbstractExpression)

    def construct_common(self):
        """
        Construct the expressions commonly needed by collection expression
        subclasses, and return them as a tuple constituted of:

        1. The resolved collection expression.
        2. The resolved expression function passed to CollectionExpression's
           constructor.
        3. The induction variable, still in the AbstractExpression form,
           so that it can be reused in subclasses (for example for filter).
        4. The index induction variable, also in the AbstractExpression form,
           or None if no such variable is needed for iteration.

        :rtype: ResolvedExpression
        """
        collection_expr = construct(self.collection)
        assert collection_expr.type.is_collection(), (
            'Map cannot iterate on {}, which is not a collection'
        ).format(collection_expr.type.name().camel)
        self.ind_var.set_type(collection_expr.type.element_type())

        return (collection_expr,
                construct(self.expr),
                construct(self.ind_var),
                construct(self.index_var) if self.index_var else None)


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
        collection, predicate, ind_var, index_var = self.construct_common()
        assert index_var is None

        # "collection" contains "item" if at least one element in "collection"
        # is equal to "item".
        return Quantifier.Expr(Quantifier.ANY, collection, predicate, ind_var,
                               index_var)


class Map(CollectionExpression):
    """
    Abstract expression that is the result of a map expression evaluation.
    """

    class Expr(ResolvedExpression):
        """
        Resolved expression that represents a map expression in the generated
        code.
        """

        def __init__(self, induction_var, index_var, collection, expr,
                     filter=None, concat=False, take_while=None):
            """
            :type induction_var: VarExpr
            :type index_var: None|VarExpr
            :type collection: ResolvedExpression
            :type expr: ResolvedExpression
            :type filter: ResolvedExpression
            :type concat: bool
            :type take_while: ResolvedExpression
            """
            self.take_while = take_while
            self.induction_var = induction_var
            self.index_var = index_var
            self.collection = collection
            self.expr = expr
            self.filter = filter
            self.concat = concat

            element_type = (self.expr.type.element_type()
                            if self.concat else
                            self.expr.type)
            self._type = element_type.array_type()
            self._type.add_to_context()

            p = Property.get()
            self.array_var = p.vars.create('Map', self.type)

        @property
        def type(self):
            return self._type

        def __repr__(self):
            return "<MapExpr {}: {} -> {}{}>".format(
                self.collection,
                self.induction_var,
                self.expr,
                " (if {})".format(self.filter) if self.filter else ""
            )

        def render_pre(self):
            return render('properties/map_ada', map=self,
                          Name=names.Name)

        def render_expr(self):
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
        self.filter_expr = self.filter_fn(self.ind_var)
        self.take_while_expr = self.take_while_pred(self.ind_var)

    def construct(self):
        """
        Construct a resolved expression for this map operation.

        :rtype: MapExpr
        """
        collection_expr, expr, ind_var, index_var = self.construct_common()

        assert (not self.concat or expr.type.is_collection()), (
            'Cannot mapcat with expressions returning {} values'
            ' (collections expected instead)'
        ).format(expr.type.name())

        filter_expr = (construct(self.filter_expr, BoolType)
                       if self.filter_expr else None)

        take_while_expr = (construct(self.take_while_expr, BoolType)
                           if self.take_while_expr else None)

        return Map.Expr(ind_var, index_var, collection_expr, expr,
                        filter_expr, self.concat, take_while_expr)


class Quantifier(CollectionExpression):
    """
    Expression that tests a predicate over the items of a collection.
    """

    class Expr(ResolvedExpression):
        def __init__(self, kind, collection, expr, induction_var, index_var):
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

            :param induction_var: Variable to use in "expr".
            :type induction_var: ResolvedExpression

            :param index_var: Index variable to use in "expr".
            :type index_var: None|ResolvedExpression
            """
            self.kind = kind
            self.collection = collection
            self.expr = expr
            self.induction_var = induction_var
            self.index_var = index_var

            self.result_var = Property.get().vars.create('Result', BoolType)

        @property
        def type(self):
            return BoolType

        def render_pre(self):
            return render(
                'properties/quantifier_ada', quantifier=self,
                ALL=Quantifier.ALL, ANY=Quantifier.ANY, Name=names.Name
            )

        def render_expr(self):
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
        collection_expr, expr, ind_var, index_var = self.construct_common()
        assert expr.type.matches(BoolType)
        return Quantifier.Expr(self.kind, collection_expr, expr, ind_var,
                               index_var)


class CollectionGet(AbstractExpression):
    """
    Expression that will get an element from a collection.
    """

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
        self.coll_expr = coll_expr
        self.index_expr = index_expr
        self.or_null = or_null

    def construct(self):
        coll_expr = construct(self.coll_expr, lambda t: t.is_collection())
        return BuiltinCallExpr(
            "Get", coll_expr.type.element_type(),
            [coll_expr, construct(self.index_expr, LongType),
             construct(self.or_null)]
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
        self.coll_expr = coll_expr

    def construct(self):
        return BuiltinCallExpr(
            "Length", LongType,
            [construct(self.coll_expr, lambda t: t.is_collection())]
        )
