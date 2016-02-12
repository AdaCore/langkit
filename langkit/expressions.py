"""
This represents the core of the expressions parts of the DSL, that will be used
notably to define properties on AST nodes.

- Users create trees of AbstractExpression subclasses instances, and wrap them
  in Property instances in ASTNode subclasses.
- Code generation (Property.render) is done in two steps. First,
  AbstractExpression.construct returns ResolvedExpression trees which are bound
  to specific ASTNode and Field classes.
- Finally, those ResolvedExpression trees are then used to generate concrete
  code for properties in the generated library.
"""

from __future__ import absolute_import

from contextlib import contextmanager
from copy import copy
from functools import partial
from itertools import count

from langkit import names
from langkit.compiled_types import (
    render as ct_render, LongType, LexicalEnvType, Token, BoolType,
    ASTNode, Struct, CompiledType, AbstractNodeData, EnvElement
)
from langkit.utils import Colors, col, assert_type, memoized


def construct(expr, expected_type_or_pred=None):
    """
    Construct a ResolvedExpression from an object that is a valid expression in
    the Property DSL.

    :param expected_type_or_pred: A type or a predicate. If a type, it will
        be checked against the ResolvedExpression's type to see if it
        corresponds. If a predicate, expects the type of the
        ResolvedExpression as a parameter, and returns a boolean, to allow
        checking properties of the type.
    :type expected_type_or_pred: type|(CompiledType) -> bool

    :param AbstractExpression|bool|int expr: The expression to resolve.
    :rtype: ResolvedExpression
    """

    if isinstance(expr, AbstractExpression):
        ret = expr.construct()
    elif isinstance(expr, int):
        ret = LiteralExpr(str(expr), LongType)
    elif isinstance(expr, bool):
        ret = LiteralExpr(str(expr), LongType)
    else:
        raise TypeError('Invalid abstract expression: {}'.format(type(expr)))

    if expected_type_or_pred:
        if isinstance(expected_type_or_pred, type):
            assert ret.type.matches(expected_type_or_pred), (
                "Expected type {}, got {}".format(
                    expected_type_or_pred, ret.type
                )
            )
        else:
            assert callable(expected_type_or_pred), (
                "Expected_type_or_pred must either be a type, or a predicate"
                " of type (ResolvedExpression) -> bool"
            )
            assert expected_type_or_pred(ret.type), (
                "Evaluating predicate on {} failed".format(ret.type)
            )

    return ret


class Frozable(object):
    """
    Trait class that defines:

    - A frozen read-only property, False by default;
    - A freeze method that sets the property to True.

    The idea is that classes can then derive from this trait and define a
    special behavior for when the object is frozen. This is used by the
    Expression classes to make sure that the user of those classes does not
    accidentally create new expressions while trying to rely on the classes's
    non magic behavior.

    For example, for an object that implements the FieldTrait trait, you might
    want to access regular fields on the object in the implementation part::

        a = Self.some_field
        assert isinstance(a, FieldAccess)
        a.wrong_spellled_field

    If the object is not frozen, this will generate a new FieldAccess object.
    If it is frozen, this will throw an exception.
    """

    @property
    def frozen(self):
        """
        Returns wether the object is frozen.

        :rtype: bool
        """
        return self.__dict__.get('_frozen', False)

    def freeze(self):
        """
        Freeze the object and all its frozable components recursively.
        """

        # AbstractExpression instances can appear in more than one place in
        # expression "trees" (which are more DAGs actually), so avoid
        # unnecessary processing.
        if self.frozen:
            return

        # Deactivate this inspection because we don't want to force every
        # implementer of frozable to call super.

        # noinspection PyAttributeOutsideInit
        self._frozen = True

        for _, val in self.__dict__.items():
            if isinstance(val, Frozable):
                val.freeze()

    @staticmethod
    def protect(func):
        """
        Decorator for subclasses methods to prevent invokation after freeze.

        :param func: Unbound method to protect.
        :rtype: function
        """
        def wrapper(self, *args, **kwargs):
            if self.__dict__.get('_frozen', False):
                raise Exception("Illegal field access")
            return func(self, *args, **kwargs)
        return wrapper


class AbstractExpression(Frozable):
    """
    An abstract expression is an expression that is not yet resolved (think:
    typed and bound to some AST node context). To be able to emulate lexical
    scope in expressions, the expression trees produced by initial python
    evaluation of the expressions will be a tree of AbstractExpression objects.

    You can then call construct on the root of the expression tree to get back
    a resolved tree of ResolvedExpression objects.
    """

    def construct(self):
        """
        Returns a resolved tree of resolved expressions.

        :rtype: ResolvedExpression
        """
        raise NotImplementedError()

    @memoized
    def attrs(self):
        # Using partial allows the user to be able to use keyword arguments
        # defined on the expressions constructors.
        return {
            'all':            partial(Quantifier, Quantifier.ALL, self),
            'any':            partial(Quantifier, Quantifier.ANY, self),
            'cast':           partial(Cast, self),
            'contains':       partial(Contains, self),
            'equals':         partial(Eq, self),
            'filter':         partial(Map, self, lambda x: x),
            'is_a':           partial(IsA, self),
            'map':            partial(Map, self),
            'mapcat':         partial(Map, self, concat=True),
            'get':            partial(EnvGet, self),
            'resolve_unique': partial(EnvGet, self, resolve_unique=True),
            'at':             partial(CollectionGet, self),
            'eval_in_env':    partial(EnvBind, self),
            'is_null':        IsNull(self)
        }

    @Frozable.protect
    def __getattr__(self, attr):
        """
        Depending on "attr", return either an AbstractExpression or an
        AbstractExpression constructor.

        :param str attr: Name of the field to access.
        :rtype: AbstractExpression|function
        """
        return self.attrs().get(attr, FieldAccess(self, attr))

    @Frozable.protect
    def __call__(self, *args, **kwargs):
        """
        Returns a OpCall expression object when the user uses the call notation
        on self.

        :rtype: OpCall
        """
        return OpCall(self, args, kwargs)

    @Frozable.protect
    def __or__(self, other):
        """
        Returns a OrExpr expression object when the user uses the binary or
        notation on self.

        :type other: AbstractExpression
        :rtype: BinaryBooleanOperator
        """
        return BinaryBooleanOperator(BinaryBooleanOperator.OR, self, other)

    @Frozable.protect
    def __and__(self, other):
        """
        Returns a AndExpr expression object when the user uses the binary and
        notation on self.

        :type other: AbstractExpression
        :rtype: BinaryBooleanOperator
        """
        return BinaryBooleanOperator(BinaryBooleanOperator.AND, self, other)


class ResolvedExpression(object):
    """
    Resolved expressions are expressions that can be readily rendered to code
    that will correspond to the initial expression, depending on the bound
    lexical scope.
    """

    def render_expr(self):
        """
        Renders the expression itself.

        :rtype: basestring
        """
        raise NotImplementedError()

    def render_pre(self):
        """
        Renders initial statements that might be needed to the expression.

        :rtype: basestring
        """
        return ""

    def render(self):
        """
        Render both the initial statements and the expression itself. This is
        basically a wrapper that calls render_pre and render_expr in turn.

        :rtype: basestring
        """
        return "{}\n{}".format(self.render_pre(), self.render_expr())

    @property
    def type(self):
        """
        Returns the type of the resolved expression.

        :rtype: langkit.compiled_types.CompiledType
        """
        raise NotImplementedError()


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
            expression to evaluate for each item in "collection".
        :type collection: (InductionVariable) -> AbstractExpression
        """
        super(CollectionExpression, self).__init__()
        self.collection = collection
        self.expr_fn = expr

    def construct_common(self):
        """
        Construct the expressions commonly needed by collection expression
        subclasses, and return them as a tuple constituted of:

        1. The resolved collection expression.
        2. The resolved expression function passed to CollectionExpression's
           constructor.
        3. The induction variable, still in the AbstractExpression form,
           so that it can be reused in subclasses (for example for filter).

        :rtype: (ResolvedExpression, ResolvedExpression, AbstractExpression)
        """
        collection_expr = construct(self.collection)
        assert collection_expr.type.is_collection(), (
            'Map cannot iterate on {}, which is not a collection'
        ).format(collection_expr.type.name().camel)

        induction_var = AbstractVariable(
            names.Name("Item_{}".format(next(CollectionExpression._counter))),
            collection_expr.type.element_type()
        )

        expr = construct(assert_type(self.expr_fn(induction_var),
                                     AbstractExpression))

        return collection_expr, expr, induction_var


class OpCall(AbstractExpression):
    """
    Abstract expression that is the result of a call expression evaluation.

    TODO: Not implemented yet!
    """
    def __init__(self, called, args, kwargs):
        self.called = called
        self.args = args
        self.kwargs = kwargs

    def __repr__(self):
        return "<OpCall {} {} {}>".format(self.called, self.args, self.kwargs)


class BinaryBooleanOperator(AbstractExpression):
    """
    Abstract expression for binary boolean expressions.
    """

    AND = 'and'
    OR = 'or'

    def __init__(self, kind, lhs, rhs):
        """
        :param str kind: Kind for this binary boolean operator
            (short-circuiting).
        :param AbstractExpression lhs: Left operand.
        :param AbstractExpression rhs: Right operand.
        """
        assert kind in (self.AND, self.OR)
        self.kind = kind
        self.lhs = lhs
        self.rhs = rhs

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: IfExpr
        """
        lhs = construct(self.lhs)
        rhs = construct(self.rhs)
        assert lhs.type.matches(BoolType)
        assert rhs.type.matches(BoolType)

        if self.kind == self.AND:
            then = rhs
            else_then = LiteralExpr('False', BoolType)
        else:
            then = LiteralExpr('True', BoolType)
            else_then = rhs
        return IfExpr(lhs, then, else_then, BoolType)


class Cast(AbstractExpression):
    """
    Abstract expression that is the result of casting an ASTNode subclass value
    to another subclass.
    """

    def __init__(self, expr, astnode):
        """
        :param AbstractExpression expr: Expression on which the cast is
            performed.
        :param ASTNode astnode: ASTNode subclass to use for the cast.
        """
        assert astnode.matches(ASTNode)
        self.expr = expr
        self.astnode = astnode

    def construct(self):
        """
        Construct a resolved expression that is the result of casting a AST
        node.

        :rtype: CastExpr
        """
        expr = self.expr.construct()
        assert self.astnode.matches(expr.type), (
            'Cannot cast {} to {}: only downcasting is allowed'.format(
                expr.type.name().camel,
                self.astnode.name().camel
            )
        )
        return CastExpr(expr, self.astnode)


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
        collection, predicate, ind_var = self.construct_common()
        # "collection" contains "item" if at least one element in "collection"
        # is equal to "item".
        return Quantifier.QuantifierExpr(Quantifier.ANY, collection, predicate,
                                         construct(ind_var))


class Eq(AbstractExpression):
    """
    Abstract expression for equality test expression.
    """

    class EqExpr(ResolvedExpression):
        """
        Resolved expression for an equality test expression.
        """

        def __init__(self, lhs, rhs):
            self.lhs = lhs
            self.rhs = rhs

        @property
        def type(self):
            return BoolType

        def render_pre(self):
            return '{}\n{}'.format(
                self.lhs.render_pre(),
                self.rhs.render_pre()
            )

        def render_expr(self):
            return '{} = {}'.format(self.lhs.render_expr(),
                                    self.rhs.render_expr())

    def __init__(self, lhs, rhs):
        """
        :param AbstractExpression lhs: Left operand.
        :param AbstractExpression rhs: Right operand.
        """
        self.lhs = lhs
        self.rhs = rhs

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: EqExpr
        """
        lhs = construct(self.lhs)
        rhs = construct(self.rhs)

        # Don't use CompiledType.matches since in the generated code, we need
        # both operands to be *exactly* the same types, so handle specifically
        # each case.
        if issubclass(lhs.type, ASTNode):
            # Handle checks between two subclasses without explicit casts. In
            # order to help users to detect dubious checks, forbid operands
            # that can never be equal because they have no subclass in common.
            if issubclass(lhs.type, rhs.type):
                lhs = CastExpr(lhs, assert_type(rhs.type, ASTNode))
            elif issubclass(rhs.type, lhs.type):
                rhs = CastExpr(rhs, assert_type(lhs.type, ASTNode))
            else:
                assert False, '{} and {} values are never equal'.format(
                    lhs.type.name().camel, rhs.type.name().camel
                )
        else:
            assert lhs.type == rhs.type, (
                'Incompatible types for equality: {} and {}'
            ).format(lhs.type.name().camel, rhs.type.name().camel)

        return Eq.EqExpr(lhs, rhs)


class If(AbstractExpression):
    """
    Abstract expression for a conditional expression.
    """

    def __init__(self, cond, then, else_then):
        """
        :param AbstractExpression cond: A boolean expression.
        :param AbstractExpression then: If "cond" is evaluated to true, this
            part is returned.
        :param AbstractExpression else_then: If "cond" is evaluated to false,
            this part is returned.
        """
        self.cond = cond
        self.then = then
        self.else_then = else_then

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: IfExpr
        """
        cond = self.cond.construct()
        assert cond.type.matches(BoolType)

        then = self.then.construct()
        else_then = self.else_then.construct()

        rtype = then.type.unify(else_then.type)
        return IfExpr(cond, then, else_then, rtype)


class IsNull(AbstractExpression):
    """
    Abstract expression to test whether an AST node is null.
    """

    def __init__(self, expr):
        """
        :param AbstractExpression expr: Expression on which the test is
            performed.
        """
        self.expr = expr

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: EqExpr
        """
        expr = construct(self.expr)
        assert issubclass(expr.type, ASTNode)
        return Eq.EqExpr(expr, LiteralExpr('null', ASTNode))


class Map(CollectionExpression):
    """
    Abstract expression that is the result of a map expression evaluation.
    """

    def __init__(self, collection, expr, filter_expr=None, concat=False):
        """
        See CollectionExpression for the other parameters.

        :param filter_expr: If provided, a function that takes an induction
            variable and that returns a boolean expression which says whether
            to include or exclude an item from the collection.
        :type filter_expr: None|(AbstractExpression) -> AbstractExpression

        :param bool concat: If true, "expr" must return arrays, and this
            expression returns the concatenation of all the arrays "expr"
            returns.
        """
        super(Map, self).__init__(collection, expr)
        self.filter_fn = filter_expr
        self.concat = concat

    def construct(self):
        """
        Construct a resolved expression for this map operation.

        :rtype: MapExpr
        """
        collection_expr, expr, ind_var = self.construct_common()

        assert (not self.concat or expr.type.is_collection()), (
            'Cannot mapcat with expressions returning {} values'
            ' (collections expected instead)'
        ).format(expr.type.name())

        filter_expr = (construct(self.filter_fn(ind_var), BoolType)
                       if self.filter_fn else None)

        return MapExpr(construct(ind_var), collection_expr, expr, filter_expr,
                       self.concat)


class New(AbstractExpression):
    """
    Abstract expression to create Struct values.
    """

    def __init__(self, struct_type, **field_values):
        """
        :param langkit.compiled_types.Struct struct_type: Struct subclass (but
            not an ASTNode subclass) for the struct type this expression must
            create.
        :param dict[str, AbstractExpression] fields: Values to assign to the
            fields for the created struct value.
        """
        assert (issubclass(struct_type, Struct) and
                not issubclass(struct_type, ASTNode))
        self.struct_type = struct_type
        self.field_values = field_values

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: NewExpr
        """
        provided_fields = {
            names.Name.from_lower('f_' + name): value.construct()
            for name, value in self.field_values.iteritems()
        }
        required_fields = {
            f.name: f
            for f in self.struct_type.get_fields()
        }

        # Make sure the provided set of fields matches the one the struct
        # needs.
        def complain_if_not_empty(name_set, message):
            assert not name_set, '{}: {}'.format(
                message,
                ', '.join(name.lower for name in name_set)
            )

        complain_if_not_empty(
            set(required_fields) - set(provided_fields),
            'Values are missing for {} fields'.format(
                self.struct_type.name().camel
            )
        )
        complain_if_not_empty(
            set(provided_fields) - set(required_fields),
            'Unknown {} fields'.format(
                self.struct_type.name().camel
            )
        )

        # And make sure we have the proper types
        for name, value in provided_fields.iteritems():
            field = required_fields[name]
            assert value.type.matches(field.type), (
                'Invalid value for field {}: got {} but expected {}'.format(
                    name,
                    value.type.name().camel,
                    field.type.name().camel
                )
            )

        return NewExpr(self.struct_type, provided_fields)


class Not(AbstractExpression):
    """
    Abstract expression for "not" boolean expressions.
    """

    def __init__(self, expr):
        """
        :param AbstractExpression expr: Operand for the "not" expression.
        """
        self.expr = expr

    def construct(self):
        """
        Consrtuct a resolved expression for this.
        :rtype: NotExpr
        """
        expr = construct(self.expr)
        assert expr.type.matches(BoolType)
        return NotExpr(expr)


class Quantifier(CollectionExpression):
    """
    Expression that tests a predicate over the items of a collection.
    """

    class QuantifierExpr(ResolvedExpression):
        def __init__(self, kind, collection, expr, induction_var):
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
            """
            self.kind = kind
            self.collection = collection
            self.expr = expr
            self.induction_var = induction_var

            self.result_var = Property.get().vars(names.Name('Result'),
                                                  BoolType,
                                                  create_unique=False)

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
        collection_expr, expr, ind_var = self.construct_common()
        assert expr.type.matches(BoolType)
        return Quantifier.QuantifierExpr(self.kind, collection_expr, expr,
                                         construct(ind_var))


class FieldAccess(AbstractExpression):
    """
    Abstract expression that is the result of a field access expression
    evaluation.
    """

    def __init__(self, receiver, field):
        """
        :param AbstractExpression receiver: Expression on which the field
               access was done.
        :param str field: The name of the field that is accessed.
        """
        self.receiver = receiver
        self.field = field

    def construct(self):
        """
        Constructs a resolved expression that is the result of:

        - Resolving the receiver;
        - Getting its corresponding field.

        :rtype: FieldAccessExpr
        """

        receiver_expr = construct(self.receiver)

        to_get = assert_type(
            receiver_expr.type, Struct
        ).get_abstract_fields_dict().get(self.field, None)
        ":type: AbstractNodeField"

        # If still not found, there's a problem
        assert to_get, col("Type {} has no '{}' field or property".format(
            receiver_expr.type.__name__, self.field
        ), Colors.FAIL)

        ret = FieldAccessExpr(receiver_expr, to_get)
        return ret

    def __repr__(self):
        return "<FieldAccess {} {}>".format(self.receiver, self.field)


class AbstractVariable(AbstractExpression):
    """
    Abstract expression that is an entry point into the expression DSL.

    If you have an instance of a PlaceHolder, you can use it to construct
    abstract expressions.

    You can then resolve the constructed expressions by:
    - Binding the type of the PlaceHolder instance via a call to the bind_type
      context manager.
    - Calling construct on the PlaceHolder.
    """

    class VarExpr(ResolvedExpression):
        """
        Resolved expression that represents a variable in generated code.
        """

        def __init__(self, type, name):
            """
            Create a variable reference expression.

            :param langkit.compiled_types.CompiledType type: Type for the
                referenced variable.
            :param names.Name name: Name of the referenced variable.
            """
            self._type = assert_type(type, CompiledType)
            self.name = name

        @property
        def type(self):
            return self._type

        def render_expr(self):
            return self.name.camel_with_underscores

    def __init__(self, name, type=None):
        """
        :param names.Name name: The name of the PlaceHolder variable.
        """
        self._name = name
        self._type = type

    @contextmanager
    def bind_name(self, name):
        """
        Bind the name of this placeholder.

        :param name: The new name.
        """
        _old_name = self._name
        self._name = name
        yield
        self._name = _old_name

    @contextmanager
    def bind_type(self, type):
        """
        Bind the type of this placeholder.

        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of this placeholder.
        """
        _old_type = self._type
        self._type = type
        yield
        self._type = _old_type

    def construct(self):
        return AbstractVariable.VarExpr(self._type, self._name)

    @property
    def type(self):
        return self._type

    def __repr__(self):
        return "<PlaceHolder {}>".format(self._name)


Self = AbstractVariable(names.Name("Self"))
Env = AbstractVariable(names.Name("Current_Env"), type=LexicalEnvType)


def render(*args, **kwargs):
    return ct_render(*args, property=Property.get(), Self=Self, **kwargs)


class IsA(AbstractExpression):
    """
    Expression that is the result of testing the kind of a node.
    """

    class IsAExpr(ResolvedExpression):
        def __init__(self, expr, astnodes):
            """
            :param ResolvedExpr expr: Expression on which the test is
                performed.
            :param [ASTNode] astnodes: ASTNode subclasses to use for the test.
            """
            self.expr = expr
            self.astnodes = astnodes

        @property
        def type(self):
            return BoolType

        def render_pre(self):
            return self.expr.render_pre()

        def render_expr(self):
            return "{}.all in {}".format(
                self.expr.render_expr(),
                " | ".join(
                    "{}_Type'Class".format(a.name().camel_with_underscores)
                    for a in self.astnodes
                )
            )

    def __init__(self, expr, *astnodes):
        """
        :param AbstractExpression astnode: Expression on which the test is
            performed.
        :param ASTNode astnode: ASTNode subclass to use for the test.
        """
        self.expr = expr
        self.astnodes = [assert_type(a, ASTNode) for a in astnodes]

    def construct(self):
        """
        Construct a resolved expression that is the result of testing the kind
        of a node.

        :rtype: IsAExpr
        """
        expr = construct(self.expr)
        for a in self.astnodes:
            assert a.matches(expr.type), (
                'When testing the dynamic subtype of an AST node, the type to'
                ' check must be a subclass of the value static type.'
            )
        return IsA.IsAExpr(expr, self.astnodes)


class EnvGet(AbstractExpression):
    """
    Expression for lexical environment get operation.
    """

    class EnvGetExpr(ResolvedExpression):
        def __init__(self, env_expr, token_expr, resolve_unique):
            """
            :param ResolvedExpression env_expr: The expression representing the
                env to get from.
            :param ResolvedExpression token_expr: The expression representing
                the token key.
            """
            self.env_expr = env_expr
            self.token_expr = token_expr
            self.resolve_unique = resolve_unique
            self.type.add_to_context()

        @property
        def type(self):
            """
            :rtype: compiled_types.ArrayType
            """
            return (
                EnvElement if self.resolve_unique else EnvElement.array_type()
            )

        def render_pre(self):
            return "{}\n{}".format(self.env_expr.render_pre(),
                                   self.token_expr.render_pre())

        def render_expr(self):
            return (
                "{} (0)" if self.resolve_unique else "Create ({})"
            ).format("AST_Envs.Get ({}, Symbol_Type ({}.Text))".format(
                self.env_expr.render_expr(), self.token_expr.render_expr()
            ))

    def __init__(self, env_expr, token_expr,
                 resolve_unique=False):
        """
        :param AbstractExpression env_expr: Expression that will yield
            the env to get the element from.
        :param AbstractExpression token_expr: Expression that will yield the
            token to use as a key on the env.
        :param bool resolve_unique: Wether we want an unique result or not.
            NOTE: For the moment, nothing will be done to ensure that only one
            result is available. The implementation will just take the first
            result.
        """
        self.env_expr = env_expr
        self.token_expr = token_expr
        self.resolve_unique = resolve_unique
        # TODO: Add a filter here. This will wait further developments in the
        # array machinery.

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: EnvGetExpr
        """
        return EnvGet.EnvGetExpr(construct(self.env_expr, LexicalEnvType),
                                 construct(self.token_expr, Token),
                                 self.resolve_unique)


class EnvBind(AbstractExpression):
    """
    Expression that will evaluate a subexpression in the context of a
    particular lexical environment. Not meant to be used directly,
    but instead via the eval_in_env shortcut.
    """

    class EnvBindExpr(ResolvedExpression):
        def __init__(self, env_expr, to_eval_expr):
            self.to_eval_expr = to_eval_expr
            self.env_expr = env_expr

            # Declare a variable that will hold the value of the
            # bound environment.
            self.env_var = Property.get().vars(
                names.Name("New_Env"), LexicalEnvType, create_unique=True
            )

        def render_pre(self):
            # We assign to our environment variable the value of the result
            # of the environment expression.
            return "{}\n{}\n{} := {};".format(
                self.to_eval_expr.render_pre(), self.env_expr.render_pre(),
                self.env_var.name, self.env_expr.render_expr()
            )

        def render_expr(self):
            # We just bind the name of the environment placeholder to our
            # variable.
            with Env.bind_name(self.env_var.name):
                return self.to_eval_expr.render_expr()

        @property
        def type(self):
            return self.to_eval_expr.type

    def __init__(self, env_expr, to_eval_expr):
        """

        :param AbstractExpression env_expr: An expression that will return a
            lexical environment in which we will eval to_eval_expr.
        :param AbstractExpression to_eval_expr: The expression to eval.
        """
        self.env_expr = env_expr
        self.to_eval_expr = to_eval_expr

    def construct(self):
        return EnvBind.EnvBindExpr(construct(self.env_expr, LexicalEnvType),
                                   construct(self.to_eval_expr))


class CollectionGet(AbstractExpression):
    """
    Expression that will get an element from a collection.
    """

    class CollectionGetExpr(ResolvedExpression):
        def __init__(self, coll_expr, index_expr, or_null):
            """
            :type coll_expr: ResolvedExpression
            :type index_expr: ResolvedExpression
            :type or_null: bool
            """
            self.coll_expr = coll_expr
            self.index_expr = index_expr
            self.or_null = or_null

        @property
        def type(self):
            return self.coll_expr.type.element_type()

        def render_pre(self):
            return "{}\n{}".format(
                self.coll_expr.render_pre(),
                self.index_expr.render_pre()
            )

        def render_expr(self):
            return "Get ({}, {}, Or_Null => {})".format(
                self.coll_expr.render_expr(), self.index_expr.render_expr(),
                self.or_null
            )

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
        return CollectionGet.CollectionGetExpr(
            coll_expr=construct(self.coll_expr, lambda t: t.is_collection()),
            index_expr=construct(self.index_expr, LongType),
            or_null=self.or_null)


class FieldAccessExpr(ResolvedExpression):
    """
    Resolved expression that represents a field access in generated code.
    """

    def __init__(self, receiver_expr, property):
        """
        :param ResolvedExpression receiver_expr: The receiver of the field
               access.
        :param Property|Field property: The accessed property or field.
        """
        self.receiver_expr = receiver_expr
        self.property = property
        self.simple_field_access = False

        # TODO: For the moment we use field accesses in the environments
        # code, which doesn't have a property context and hence local
        # variables instance. At a later stage we'll want to get rid of that
        #  limitation by binding the local variables separately from the
        # current property.

        p = Property.get()
        if p:
            self.result_var = p.vars(names.Name('Internal_Pfx'),
                                     self.receiver_expr.type,
                                     create_unique=False)
        else:
            self.simple_field_access = True

    @property
    def type(self):
        return self.property.type

    def __repr__(self):
        return "<FieldAccessExpr {} {} {}>".format(
            self.receiver_expr, self.property, self.type
        )

    def render_pre(self):
        # Before accessing the field of a record through an access, we must
        # check that whether this access is null in order to raise a
        # Property_Error in the case it is.
        return render('properties/null_safety_check_ada',
                      expr=self.receiver_expr,
                      result_var=self.result_var)

    def render_expr(self):
        if self.simple_field_access:
            prefix = self.receiver_expr.render()
        else:
            prefix = self.result_var.name
        ret = "{}.{}".format(prefix, self.property.name)

        # If we're calling a property, then pass the currently bound lexical
        # environment as parameter.
        if isinstance(self.property, Property):
            ret += " ({})".format(Env._name)

        return ret


class CastExpr(ResolvedExpression):
    """
    Resolved expression that is the result of casting an ASTNode subclass value
    to another subclass.
    """

    def __init__(self, expr, astnode):
        """
        :param ResolvedExpr expr: Expression on which the cast is performed.
        :param ASTNode astnode: ASTNode subclass to use for the cast.
        """
        self.expr = expr
        self.astnode = astnode

        p = Property.get()
        self.result_var = p.vars(names.Name('Base'),
                                 self.expr.type,
                                 create_unique=False)

    @property
    def type(self):
        return self.astnode

    def render_pre(self):
        # Before actually downcasting an access to an AST node, add a type
        # check so that we raise a Property_Error if it's wrong.
        return render('properties/type_safety_check_ada',
                      expr=self.expr,
                      astnode=self.astnode,
                      result_var=self.result_var)

    def render_expr(self):
        return "{} ({})".format(
            self.astnode.name().camel_with_underscores,
            self.expr.render_expr()
        )


class IfExpr(ResolvedExpression):
    """
    Resolved expression for a conditional expression.
    """

    def __init__(self, cond, then, else_then, rtype):
        """
        :param ResolvedExpression cond: A boolean expression.
        :param ResolvedExpression then: If "cond" is evaluated to true, this
            part is returned.
        :param ResolvedExpression else_then: If "cond" is evaluated to false,
            this part is returned.
        :param langkit.compiled_types.CompiledType rtype: Type parameter. The
            type that is returned by then and else_then.
        """
        self.cond = cond
        self.then = then
        self.else_then = else_then
        self.rtype = rtype

        self.result_var = Property.get().vars(names.Name('Result'), rtype,
                                              create_unique=False)

    @property
    def type(self):
        return self.rtype

    def render_pre(self):
        return render('properties/if_ada', expr=self)

    def render_expr(self):
        return self.result_var.name.camel_with_underscores


class LiteralExpr(ResolvedExpression):
    """
    Resolved expression for literals of any type.
    """

    def __init__(self, literal, type):
        self.literal = literal
        self._type = type

    @property
    def type(self):
        return self._type

    def render_expr(self):
        return self.literal


class MapExpr(ResolvedExpression):
    """
    Resolved expression that represents a map expression in the generated code.
    """

    def __init__(self, induction_var, collection, expr, filter=None,
                 concat=False):
        """
        :param VarExpr induction_var: Variable to use in "expr".
        :param ResolvedExpression collection: Collection on which this map
            operation works.
        :param ResolvedExpression expr: Expression to evaluate for each item in
            "collection".
        :param filter: If provided, a boolean expression that says whether to
            include or exclude an item from the collection.
        :type filter: None|ResolvedExpression
        :param bool concat: If true, "expr" must return arrays, and this
            expression returns the concatenation of all the arrays "expr"
            returns.
        :return:
        """
        self.induction_var = induction_var
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
        self.array_var = p.vars(names.Name('Map'), self.type,
                                create_unique=False)

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


class NewExpr(ResolvedExpression):
    """
    Resolved expression to create Struct values.
    """

    def __init__(self, struct_type, field_values):
        self.struct_type = struct_type
        self.field_values = field_values

    @property
    def type(self):
        return self.struct_type

    def _iter_ordered(self):
        keys = sorted(self.field_values)
        for k in keys:
            yield (k, self.field_values[k])

    def render_pre(self):
        return '\n'.join(
            expr.render_pre()
            for _, expr in self._iter_ordered()
        )

    def render_expr(self):
        return '({})'.format(', '.join(
            '{} => {}'.format(
                name.camel_with_underscores,
                expr.render_expr()
            )
            for name, expr in self._iter_ordered()
        ))


class NotExpr(ResolvedExpression):
    """
    Resolved expression for "not" boolean expressions.
    """

    def __init__(self, expr):
        self.expr = expr

    @property
    def type(self):
        return BoolType

    def render_pre(self):
        return self.expr.render_pre()

    def render_expr(self):
        return 'not ({})'.format(self.expr.render_expr())


class LocalVars(object):
    """
    Represents the state of local variables in a property definition.
    """

    def __init__(self):
        self.local_vars = {}

    class LocalVar(object):
        """
        Represents one local variable in a property definition.
        """
        def __init__(self, vars, name, type):
            """

            :param LocalVars vars: The LocalVars instance to which this
                local variable is bound.
            :param langkit.names.Name name: The name of this local variable.
            :param langkit.compiled_types.CompiledType type: Type parameter.
                The type of this local variable.
            """
            self.vars = vars
            self.name = name
            self.type = type

        def render(self):
            return "{} : {};".format(
                self.name.camel_with_underscores,
                self.type.name().camel_with_underscores
            )

    def __call__(self, name, type, create_unique=True):
        """
        This getattr override allows you to declare local variables in
        templates via the syntax::

            import langkit.compiled_types
            vars = LocalVars()
            var = vars('Index', langkit.compiled_types.LongType)

        :param names.Name name: The name of the variable.
        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of the local variable.
        :param bool create_unique: If true and "name" is already associated to
            a variable, raise an error. Otherwise, add a suffix to create a new
            variable.
        """
        assert isinstance(name, names.Name)
        assert not create_unique or (name not in self.local_vars), (
            "Already declared local variable {}".format(name)
        )
        i = 0
        orig_name = name
        while name in self.local_vars:
            i += 1
            name = orig_name + names.Name(str(i))
        ret = LocalVars.LocalVar(self, name, type)
        self.local_vars[name] = ret
        return ret

    def __getattr__(self, name):
        """
        Returns existing instance of variable called name, so that you can use
        existing variables via the syntax::

            ivar = var.Index

        :param str name: The name of the variable.
        """
        return self.local_vars[name]

    def render(self):
        return "\n".join(lv.render() for lv in self.local_vars.values())

    def __copy__(self):
        """
        When copying local variables, we want to make sure they don't share
        the underlying dictionnary, so we copy it.

        :rtype: LocalVars
        """
        new = LocalVars()
        new.local_vars = copy(self.local_vars)
        return new


class Property(AbstractNodeData):
    """
    This is the public class via which you'll create properties in the DSL.

    You can declare your properties on your ast node subclasses directly, like
    this::

        class SubNode(ASTNode):
            my_field = Field()
            my_property = Property(Self.my_field)

    and functions will be generated in the resulting library.
    """

    __current_property__ = None

    is_property = True
    prefix = names.Name("P")

    def __init__(self, expr, doc=None, private=False, abstract=False,
                 type=None, abstract_runtime_check=False):
        """
        :param expr: The expression for the property. It can be either an
            expression, or a function that will take the Self placeholder as
            parameter and return the constructed AbstractExpression. This is
            useful to reference classes that are not yet defined.
        :type expr:
            None|AbstractExpression|(AbstractExpression) -> AbstractExpression

        :param str|None doc: User documentation for this property.
        :param bool private: Whether this property is private or not.
        :param bool abstract: Whether this property is abstract or not. If this
            is True, then expr can be None.
        :param CompiledType|None type: The optional type annotation for this
            property. If supplied, it will be used to check the validity of
            inferred types for this propery, and eventually for overriding
            properties in sub classes. NOTE: The type is mandatory for abstract
            base properties.
        :param abstract_runtime_check: If the property is abstract, whether the
            implementation by subclasses requirement must be checked at compile
            time, or at runtime. If true, you can have an abstract property
            that is not implemented by all subclasses. In the absence of
            interface types in Langkit, this is helpful to develop features
            faster, because first you don't have to make every implementation
            at once, and second you don't have to find a typing scheme with
            current langkit capabilities in which the parser generate the right
            types for the functionality you want.
        """

        super(Property, self).__init__(private=private)

        assert ((expr is None and abstract) or (expr and not abstract)), (
            "Property can either be abstract, either have an expression, "
            "not both"
        )

        self.expr = expr
        if expr:
            assert isinstance(self.expr,
                              AbstractExpression) or callable(expr), (
                "Invalid object passed for expression of property: {}".format(
                    expr
                )
            )

        self.constructed_expr = None
        self.vars = LocalVars()
        self.expected_type = type
        self.abstract = abstract
        self.abstract_runtime_check = abstract_runtime_check

        self.overriding = False
        """
        Whether this property is overriding or not. This is put to False by
        default, and the information is inferred during the compute phase.
        """

        self.dispatching = self.abstract
        """
        Whether this property is dispatching or not. Initial value of that is
        self.abstract, because every abstract property is dispatching. For
        other dispatching properties (non abstract base properties, overriding
        properties), this information is inferred during the compute phase.
        """

        self.prop_decl = None
        """
        The emitted code for this property declaration.
        :type: str
        """

        self.prop_def = None
        """
        The emitted code for this property definition.
        :type: str
        """

        self._name = None
        ":type: names.Name"

        self._doc = doc
        ":type: str|None"

        self.ast_node = None
        ":type: ASTNode|None"

        if self.abstract:
            # TODO: We could also at a later stage add a check to see that the
            # abstract property definition doesn't override another property
            # definition on a base class.
            assert self.expected_type, (
                "Abstract properties need an explicit type annotation"
            )

    def __copy__(self):
        """
        When copying properties, we want to make sure they don't share local
        variables, so we implement a custom copier that duplicates the
        LocalVars instance.

        :rtype: Property
        """
        new = Property(self.expr, self._doc, self.is_private, self.abstract,
                       self.expected_type)
        new.vars = copy(self.vars)
        return new

    @classmethod
    def get(cls):
        """
        Return the currently bound property. Used by the rendering context to
        get the current property.

        :rtype: Property
        """
        return cls.__current_property__

    @contextmanager
    def bind(self):
        """
        Bind the current property to self, so that it is accessible in the
        expression templates.
        """
        assert self.__current_property__ is None, (
            "You cannot nest calls to Property.bind_type context manager"
        )
        self.__class__.__current_property__ = self
        yield
        self.__class__.__current_property__ = None

    @property
    def type(self):
        """
        Returns the type of the underlying expression after resolution.

        :rtype: langkit.compiled_types.CompiledType
        """
        if self.abstract:
            return self.expected_type
        else:
            return self.constructed_expr.type

    def base_property(self, owner_type):
        """
        Get the base property for this property, if it exists.

        :param ASTNode owner_type: The type on which this property was
            declared.
        :rtype: Property|None
        """
        return owner_type.base().get_abstract_fields_dict(
            field_class=Property
        ).get(self._name.lower, None)

    def compute(self, owner_type):
        """
        Compute information related to dispatching for properties.

        :param ASTNode owner_type: The type on which this property was
            declared.
        """

        # If the user passed a lambda or function for the expression,
        # now is the moment to transform it into an abstract expression by
        # calling it.
        if self.expr and not isinstance(self.expr, AbstractExpression):
            self.expr = assert_type(self.expr(Self), AbstractExpression)

        base_prop = self.base_property(owner_type)

        if base_prop:
            # If we have a base property, then this property is dispatching and
            # overriding, and the base property is dispatching (This
            # information can be missing at this stage for non abstract base
            # properties).
            self.overriding = True
            self.dispatching = True
            base_prop.dispatching = True

            # We then want to check the consistency of type annotations if they
            # exist.
            if base_prop.expected_type:
                if self.expected_type:
                    assert self.expected_type.matches(
                        base_prop.expected_type
                    ), (
                        "Property type does not match the type of the parent"
                        " property"
                    )
                else:
                    # If base has a type annotation and not self, then
                    # propagate it.
                    self.expected_type = base_prop.expected_type

    def render(self, owner_type):
        """
        Render the given property to generated code.

        :param langkit.compiled_types.ASTNode owner_type: The ast node
            subclass to which this property is bound.
        :rtype: basestring
        """
        with self.bind():
            with Self.bind_type(owner_type):
                if self.abstract:
                    self.prop_decl = render('properties/decl_ada')
                    self.prop_def = ""
                    return

                self.expr.freeze()
                self.constructed_expr = construct(self.expr)

                if self.expected_type:
                    assert self.expected_type == self.constructed_expr.type, (
                        "Property's expession doesn't have the expected type"
                    )

                with names.camel_with_underscores:
                    self.prop_decl = render('properties/decl_ada')
                    self.prop_def = render('properties/def_ada')

        base_prop = self.base_property(owner_type)
        if base_prop and base_prop.type:
            # TODO: We need to make sure Properties are rendered in the proper
            # order (base classes first), to make sure that this check is
            # always effectful.
            assert self.type == base_prop.type, (
                "Overriding property doesn't have the same type as base"
                "property !"
            )

    def doc(self):
        return self._doc


# noinspection PyPep8Naming
def AbstractProperty(type, doc="", runtime_check=False, **kwargs):
    """
    Shortcut for abstract properties, where you can pass no expression but
    must pass a type. See Property for further documentation.

    :type type: CompiledType
    :type doc: str
    :type runtime_check: bool
    :rtype: Property
    """
    return Property(expr=None, type=type, doc=doc, abstract=True,
                    abstract_runtime_check=runtime_check, **kwargs)
