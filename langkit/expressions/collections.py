from __future__ import absolute_import, division, print_function

import inspect
from itertools import count

import types

from langkit import names
from langkit.compiled_types import (ArrayType, get_context, bool_type,
                                    long_type)
from langkit.diagnostics import (
    check_multiple, check_source_language, check_type
)
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, CallExpr, ComputingExpr, PropertyDef,
    SequenceExpr, UncheckedCastExpr, attr_expr, attr_call, auto_attr_custom,
    auto_attr, construct, render, unsugar
)
from langkit.expressions.envs import make_as_entity


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

    class ConstructCommonResult(object):
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
                names.Name('I'), type=long_type,
                source_name=names.Name.from_lower(argspec.args[1])
            )
            expr = self.expr_fn(self.index_var, self.element_var)
        else:
            expr = self.expr_fn(self.element_var)
        self.expr = unsugar(expr)

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
                collection_expr.type.name.camel
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
                type=elt_type.el_type
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
            zip(elt_vars, elt_var_inits),
            construct(self.index_var) if self.index_var else None,
            inner_expr,
            inner_scope
        )


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
        r = self.construct_common()
        assert r.index_var is None

        # "collection" contains "item" if at least one element in
        # "collection" is equal to "item".
        return Quantifier.Expr(Quantifier.ANY, r.collection_expr, r.inner_expr,
                               r.element_vars, r.index_var, r.inner_scope)

    def __repr__(self):
        return '<Contains>'


@attr_call('filter', collection_expr_identity)
@attr_call('filtermap')
@attr_call('map', filter_expr=collection_expr_none)
@attr_call('mapcat', filter_expr=collection_expr_none, do_concat=True)
@attr_call('take_while', collection_expr_identity, collection_expr_none, False)
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

            with iter_scope.parent.use():
                super(Map.Expr, self).__init__('Map_Result',
                                               abstract_expr=abstract_expr)

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

    def __init__(self, collection, expr, filter_expr=collection_expr_none,
                 do_concat=False, take_while_pred=collection_expr_none):
        """
        See CollectionExpression for the other parameters.

        :param filter_expr: If provided, a function that takes an induction
            variable and that returns a boolean expression which says whether
            to include or exclude an item from the collection.
        :type filter_expr: None|(AbstractExpression) -> AbstractExpression

        :param bool do_concat: If true, "expr" must return arrays, and this
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
        self.do_concat = do_concat
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
        r = self.construct_common()

        check_source_language(
            not self.do_concat or r.inner_expr.type.is_collection,
            'Cannot mapcat with expressions returning {} values (collections'
            ' expected instead)'.format(r.inner_expr.type.name)
        )

        with r.inner_scope.use():
            filter_expr = (construct(self.filter_expr, bool_type)
                           if self.filter_expr else None)

            take_while_expr = (construct(self.take_while_expr, bool_type)
                               if self.take_while_expr else None)

        return Map.Expr(r.element_vars, r.index_var, r.collection_expr,
                        r.inner_expr, r.inner_scope, filter_expr,
                        self.do_concat, take_while_expr, abstract_expr=self)

    def __repr__(self):
        name = None
        if self.expr_fn == collection_expr_identity:
            name = ('TakeWhile'
                    if self.filter_fn == collection_expr_none else
                    'Filter')
        if not name:
            name = 'MapCat' if self.do_concat else 'Map'
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
    check_source_language(
        result.collection.type.is_list_type,
        '.as_array input must be an AST list (here: {})'.format(
            result.collection.type.name.camel
        )
    )
    return result


@attr_call('all', kind='all')
@attr_call('any', kind='any')
class Quantifier(CollectionExpression):
    """
    Expression that tests a predicate over the items of a collection.
    """

    class Expr(ComputingExpr):
        static_type = bool_type
        pretty_class_name = 'Quantifier'

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
            self.static_type = bool_type

            with iter_scope.parent.use():
                super(Quantifier.Expr, self).__init__(
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
        super(Quantifier, self).__init__(collection, predicate)
        assert kind in (self.ALL, self.ANY)
        self.kind = kind

    def construct(self):
        """
        Construct a resolved expression for this quantifier expression.

        :rtype: QuantifierExpr
        """
        r = self.construct_common()

        check_source_language(
            r.inner_expr.type.matches(bool_type),
            "Wrong type for expression in quantifier: expected bool, "
            "got {}".format(r.inner_expr.type.name.camel)
        )

        return Quantifier.Expr(self.kind, r.collection_expr, r.inner_expr,
                               r.element_vars, r.index_var, r.inner_scope)

    def __repr__(self):
        return '<{}Quantifier>'.format(self.kind.capitalize())


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
    index_expr = construct(index_expr, long_type)

    coll_expr = construct(coll_expr, lambda t: t.is_collection)
    or_null = construct(or_null)
    return CallExpr('Get_Result', 'Get', coll_expr.type.element_type,
                    [coll_expr, index_expr, or_null],
                    abstract_expr=self)


@auto_attr
def length(self, coll_expr):
    """
    Expression that will return the length of a collection.

    :param AbstractExpression coll_expr: The expression representing the
        collection to get from.
    """
    return CallExpr('Len', 'Length', long_type,
                    [construct(coll_expr, lambda t: t.is_collection)],
                    abstract_expr=self)


@attr_expr('singleton')
@attr_expr('to_array', coerce_null=True)
class CollectionSingleton(AbstractExpression):
    """
    Expression that will return a collection of a single element, given the
    single element.
    """

    class Expr(ComputingExpr):
        pretty_class_name = 'ArraySingleton'

        def __init__(self, expr, abstract_expr=None):
            """
            :type expr: ResolvedExpression
            """
            self.expr = expr
            self.static_type = self.expr.type.array

            super(CollectionSingleton.Expr, self).__init__(
                'Singleton', abstract_expr=abstract_expr
            )

        def _render_pre(self):
            return self.expr.render_pre() + """
            {result_var} := Create (Items_Count => 1);
            {result_var}.Items (1) := {item};
            """.format(result_var=self.result_var.name,
                       item=self.expr.render_expr())

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
                EmptyArray.construct_static(expr.type.array),
                ret,
                ret.type
            )
        else:
            return ret

    def __repr__(self):
        return '<CollectionSingleton>'


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
                 array_1.type.element_type.name,
                 array_2.type.element_type.name
             )),
        ])

        return CallExpr('Concat_Result', 'Concat', array_1.type,
                        [array_1, array_2],
                        abstract_expr=self)

    def __repr__(self):
        return '<Concat>'
