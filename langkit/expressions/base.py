from contextlib import contextmanager
from copy import copy
from functools import partial
import inspect
from itertools import count
import types

from enum import Enum
import funcy

from langkit import names
from langkit.compiled_types import (
    AbstractNodeData, ASTNode, BoolType, CompiledType, LexicalEnvType,
    LongType, render as ct_render, Symbol, Token, resolve_type, Argument, T
)
from langkit.diagnostics import (
    extract_library_location, check_source_language, check_multiple,
    Severity, DiagnosticError, check_type, Context
)
from langkit.utils import (
    assert_type, memoized, TypeSet, dispatch_on_type
)


def unsugar(expr, ignore_errors=False):
    """
    Given a Python expession that can be unsugared to an AbstractExpression,
    return a valid AbstractExpression.

    :param expr: The expression to unsugar.
    :type expr: None|AbstractExpression|bool|int|() -> AbstractExpression

    :param bool ignore_errors: If True, invalid abstract expressions are
        returned as-is. Raise a diagnostic error for them otherwise.

    :rtype: AbstractExpression
    """
    if expr is None:
        return None

    # WARNING: Since bools are ints in python, bool needs to be before int
    if isinstance(expr, (bool, int)):
        expr = Literal(expr)

    check_source_language(
        ignore_errors or isinstance(expr, AbstractExpression),
        'Invalid abstract expression: {}'.format(type(expr))
    )

    return expr


def construct(expr, expected_type_or_pred=None, custom_msg=None):
    """
    Construct a ResolvedExpression from an object that is a valid expression in
    the Property DSL.

    :param expected_type_or_pred: A type or a predicate. If a type, it will
        be checked against the ResolvedExpression's type to see if it
        corresponds. If a predicate, expects the type of the
        ResolvedExpression as a parameter, and returns a boolean, to allow
        checking properties of the type.
    :type expected_type_or_pred: CompiledType|(CompiledType) -> bool

    :param AbstractExpression|bool|int expr: The expression to resolve.

    :param custom_msg: A string for the error messages. It can contain the
        format-like template holes {expected} and {expr_type}, which will be
        substituted with the expected type, and the obtained expression type
        respectively.If expected_type_or_pred is a predicate, only {expr_type}
        will be provided, and putting an {expected} template hole will result
        in an error.

    :rtype: ResolvedExpression
    """

    expr = unsugar(expr)
    with expr.diagnostic_context():

        ret = expr.construct()
        ret.location = expr.location

        if expected_type_or_pred:
            if isinstance(expected_type_or_pred, type):
                if not custom_msg:
                    custom_msg = "Expected type {expected}, got {expr_type}"
                expected_type = assert_type(expected_type_or_pred,
                                            CompiledType)

                if expected_type == ASTNode:
                    # ASTNode does not exist in the generated code: we use it
                    # as a shortcut for the actual root grammar class instead.
                    expected_type = T.root_node

                check_source_language(ret.type.matches(expected_type), (
                    custom_msg.format(expected=expected_type.name().camel,
                                      expr_type=ret.type.name().camel)
                ))

                # If the type matches expectation but is incompatible in the
                # generated code, generate a conversion. This is needed for the
                # various ASTNode subclasses.
                if expected_type != ret.type:
                    from langkit.expressions import Cast
                    return Cast.Expr(ret, expected_type)
            else:
                if not custom_msg:
                    custom_msg = "Evaluating predicate on {expr_type} failed"
                assert callable(expected_type_or_pred), (
                    "Expected_type_or_pred must either be a type, or a "
                    "predicate of type (ResolvedExpression) -> bool"
                )
                check_source_language(expected_type_or_pred(ret.type), (
                    custom_msg.format(expr_type=ret.type.name().camel)
                ))

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

    attrs_dict = {}

    def diagnostic_context(self):
        ctx_message = 'in {} expression'.format(self.__class__.__name__)
        return Context(ctx_message, self.location, "abstract_expr")

    def __init__(self):
        self.location = extract_library_location()

    def do_prepare(self):
        """
        This method will automatically be called before construct on every
        node of a property's AbstractExpression. If you have stuff that
        needs to be done before construct, such as constructing new
        AbstractExpression instances, this is the place to do it.

        :rtype: None
        """
        pass

    def prepare(self):
        """
        This method will be called in the top-level construct function, for
        expressions that have not been prepared yet. When prepare is called,
        the idea is that AbstractExpressions are not yet frozen so you can
        still construct new AbstractExpressions, which is not necessarily
        possible in construct.
        """
        def explore_objs(objs):
            for v in objs:
                if isinstance(v, AbstractExpression):
                    # Prepare the AbstractExpression direct attributes
                    v.prepare()

                elif isinstance(v, (list, tuple)):
                    explore_objs(v)

                elif isinstance(v, (dict)):
                    explore_objs(v.items())

        if not self.__dict__.get("_is_prepared", False):
            self.do_prepare()
            self.__dict__['_is_prepared'] = True
            explore_objs(self.__dict__.items())

    def construct(self):
        """
        Returns a resolved tree of resolved expressions.

        :rtype: ResolvedExpression
        """
        raise NotImplementedError()

    @memoized
    def attrs(self):
        from langkit.expressions.structs import Match
        from langkit.expressions.boolean import BinaryBooleanOperator, Then
        from langkit.expressions.collections import CollectionSingleton, Concat
        from langkit.expressions.logic import (
            GetLogicValue, SolveEquation
        )

        # Using partial allows the user to be able to use keyword arguments
        # defined on the expressions constructors.
        return {
            'concat':         partial(Concat, self),

            'singleton':      CollectionSingleton(self),

            # Control flow handling
            'and_then':       partial(BinaryBooleanOperator,
                                      BinaryBooleanOperator.AND, self),
            'match':          partial(Match, self),
            'or_else':        partial(BinaryBooleanOperator,
                                      BinaryBooleanOperator.OR, self),
            'then':           partial(Then, self),

            # Logic handling
            "get_value":      GetLogicValue(self),
            "solve":          SolveEquation(self)
        }

    @memoized
    def composed_attrs(self):
        """
        Helper memoized dict for attributes that are composed on top of
        built-in ones. Since they're built on regular attrs, we cannot put
        them in attrs or it would cause infinite recursion.
        """
        from langkit.expressions.structs import IsNull
        from langkit.expressions.boolean import Not

        return {
            'empty': self.length.equals(0),
            'find': lambda filter_expr:
                self.filter(filter_expr).at(0),
            'exists': lambda filter_expr:
                Not(IsNull(self.filter(filter_expr).at(0))),
            'find_or_raise': lambda filter_expr:
                self.filter(filter_expr).at_or_raise(0),
        }

    @Frozable.protect
    def __getattr__(self, attr):
        """
        Depending on "attr", return either an AbstractExpression or an
        AbstractExpression constructor.

        :param str attr: Name of the field to access.
        :rtype: AbstractExpression|function
        """
        from langkit.expressions.structs import FieldAccess

        try:
            return self.attrs()[attr]
        except KeyError:
            try:
                klass, args, kwargs, paramless = (
                    AbstractExpression.attrs_dict[attr]
                )
                if paramless:
                    # Automatically instantiate paramless attributes
                    return klass(self, *args, **kwargs)
                else:
                    # For attributes with parameters, return a partial
                    # instantiation.
                    return partial(klass, self, *args, **kwargs)
            except KeyError:
                return self.composed_attrs().get(attr, FieldAccess(self, attr))

    @Frozable.protect
    def __or__(self, other):
        """
        Returns a OrExpr expression object when the user uses the binary or
        notation on self.

        :type other: AbstractExpression
        :rtype: BinaryBooleanOperator
        """
        from langkit.expressions.boolean import BinaryBooleanOperator
        return BinaryBooleanOperator(BinaryBooleanOperator.OR, self, other)

    @Frozable.protect
    def __and__(self, other):
        """
        Returns a AndExpr expression object when the user uses the binary and
        notation on self.

        :type other: AbstractExpression
        :rtype: BinaryBooleanOperator
        """
        from langkit.expressions.boolean import BinaryBooleanOperator
        return BinaryBooleanOperator(BinaryBooleanOperator.AND, self, other)

    @Frozable.protect
    def __lt__(self, other):
        """
        Return an OrderingTest expression to compare two values with the "less
        than" test.

        :param AbstractExpression other: Right-hand side expression for the
            test.
        :rtype: OrderingTest
        """
        from langkit.expressions.boolean import OrderingTest
        return OrderingTest(OrderingTest.LT, self, other)

    @Frozable.protect
    def __le__(self, other):
        """
        Return an OrderingTest expression to compare two values with the "less
        than or equal" test.

        :param AbstractExpression other: Right-hand side expression for the
            test.
        :rtype: OrderingTest
        """
        from langkit.expressions.boolean import OrderingTest
        return OrderingTest(OrderingTest.LE, self, other)

    @Frozable.protect
    def __sub__(self, other):
        """
        Return an Arithmetic expression to substract two values.

        :param AbstractExpression other: Right-hand side expression.
        :rtype: Arithmetic
        """
        return Arithmetic(self, other, "-")

    @Frozable.protect
    def __add__(self, other):
        """
        Return an Arithmetic expression to add two values.

        :param AbstractExpression other: Right-hand side expression.
        :rtype: Arithmetic
        """
        return Arithmetic(self, other, "+")

    @Frozable.protect
    def __gt__(self, other):
        """
        Return an OrderingTest expression to compare two values with the
        "greater than" test.

        :param AbstractExpression other: Right-hand side expression for the
            test.
        :rtype: OrderingTest
        """
        from langkit.expressions.boolean import OrderingTest
        return OrderingTest(OrderingTest.GT, self, other)

    @Frozable.protect
    def __ge__(self, other):
        """
        Return an OrderingTest expression to compare two values with the
        "greater than or equal" test.

        :param AbstractExpression other: Right-hand side expression for the
            test.
        :rtype: OrderingTest
        """
        from langkit.expressions.boolean import OrderingTest
        return OrderingTest(OrderingTest.GE, self, other)

    @Frozable.protect
    def __eq__(self, other):
        """
        Return an Eq expression. Be careful when using this because the '=='
        operator priority in python is lower than the '&' and '|' operators
        priority that we use for logic. So it means that::

            A == B | B == C

        is actually interpreted as::

            A == (B | B) == C

        and not as what you would expect::

            (A == B) | (B == C)

        So be careful to parenthesize your expressions, or use non operator
        overloaded boolean operators.
        """
        from langkit.expressions.boolean import Eq
        return Eq(self, other)


def attr_call(name, *args, **kwargs):
    """
    Decorator to create  expression accessible through an attribute call on an
    AbstractExpression instance, from an abstract expression class. See
    attr_expr_impl for more details.

    :param name: The name of the attribute.
    :param args: additional arguments to pass to the class.
    :param kwargs: additional arguments to pass to the class.
    """
    return attr_expr_impl(name, args, kwargs)


def attr_expr(name, *args, **kwargs):
    """
    Decorator to create  expression accessible through a parameterless
    attribute on an AbstractExpression instance, from an abstract expression
    class. See attr_expr_impl for more details.

    :param name: The name of the attribute.
    :param args: additional arguments to pass to the class.
    :param kwargs: additional arguments to pass to the class.
    """
    return attr_expr_impl(name, args, kwargs, parameterless=True)


def attr_expr_impl(name, args, kwargs, parameterless=False):
    """
    Implementation for attr_expr and attr_call.

    :param name: The name of the attribute.
    :param bool parameterless: Whether the attribute should take parameters
        or not.
    :param args: additional arguments to pass to the class.
    :param kwargs: additional arguments to pass to the class.
    """

    def internal(decorated_class):
        AbstractExpression.attrs_dict[name] = (decorated_class, args, kwargs,
                                               parameterless)
        return decorated_class

    return internal


class ResolvedExpression(object):
    """
    Resolved expressions are expressions that can be readily rendered to code
    that will correspond to the initial expression, depending on the bound
    lexical scope.

    Code generation for resolved expression happens in two steps:

    * render_pre, which yields a list of statements to "prepare" the value the
      expression produces;
    * render_expr, which yields an expression that evaluates to this value.

    Subclasses must override the _render_pre method to implement the first
    step and override the _render_expr method to implement the second one.
    This base classe provides wrappers to these method, these create a local
    variable and make it contain the resulting value.
    """

    static_type = None
    """
    If subclasses redefine this, then the type property will return this
    static type value.

    :type: CompiledType
    """

    def __init__(self, result_var_name=None):
        """
        Create a resolve expression.

        :param None|str result_var_name: If provided, create a local variable
            using this as a base name to hold the result of this expression.
            In this case, the "type" property must be ready.
        """
        self._result_var = (
            PropertyDef.get().vars.create(result_var_name, self.type)
            if result_var_name else
            None
        )

    def render_pre(self):
        """
        Render initial statements that might be needed to the expression.

        :rtype: str
        """
        result = self._render_pre()
        if self._result_var:
            return '{}\n{} := {};'.format(
                result,
                self._result_var.name.camel_with_underscores,
                self._render_expr()
            )
        else:
            return result

    def render_expr(self):
        """
        Render the expression itself.

        :rtype: str
        """
        return (self._result_var.name.camel_with_underscores
                if self._result_var else
                self._render_expr())

    def _render_pre(self):
        """
        Per-expression kind implementation for render_pre. The default
        implementation returns no statement.

        :rtype: str
        """
        return ''

    def _render_expr(self):
        """
        Per-expression kind implementation for render_expr. To be overriden in
        subclasses.

        :rtype: str
        """
        raise NotImplementedError()

    def render(self):
        """
        Render both the initial statements and the expression itself. This is
        basically a wrapper that calls render_pre and render_expr in turn.

        :rtype: str
        """
        return "{}\n{}".format(self.render_pre(), self.render_expr())

    @property
    def type(self):
        """
        Returns the type of the resolved expression.

        :rtype: langkit.compiled_types.CompiledType
        """
        if not self.static_type:
            raise NotImplementedError(
                "You need to redefine the type property, or to fill the "
                "static_type class field"
            )
        return self.static_type


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

    class Expr(ResolvedExpression):
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
            self.static_type = assert_type(type, CompiledType)
            self.name = name

            super(AbstractVariable.Expr, self).__init__()

        def _render_expr(self):
            return self.name.camel_with_underscores

        def __repr__(self):
            return '<AbstractVariable.Expression {}>'.format(
                self.name.lower
            )

    def __init__(self, name, type=None, create_local=False):
        """
        :param names.Name name: The name of the PlaceHolder variable.
        :param CompiledType type: The type of the variable. Optional for
            global abstract variables where you will use bind_type. Mandatory
            if create_local is True.
        :param bool create_local: Whether to create a corresponding local
            variable in the current property.
        """
        super(AbstractVariable, self).__init__()
        self.local_var = None
        if create_local:
            self.local_var = PropertyDef.get().vars.create_scopeless(name,
                                                                     type)
            self._name = self.local_var.name
        else:
            self._name = name

        self._type = type

    @contextmanager
    def bind_name(self, name):
        """
        Bind the name of this var.

        :param name: The new name.
        """
        _old_name = self._name
        self._name = name
        yield
        self._name = _old_name

    @contextmanager
    def bind_type(self, type):
        """
        Bind the type of this var.

        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of this placeholder.
        """
        _old_type = self._type
        self._type = type
        yield
        self._type = _old_type

    def construct(self):
        return AbstractVariable.Expr(self._type, self._name)

    @property
    def type(self):
        return self._type

    def set_type(self, type):
        assert self._type is None, ("You cannot change the type of a "
                                    "variable that already has one")
        self._type = type
        if self.local_var:
            self.local_var.type = type

    def __repr__(self):
        return "<AbstractVariable {}>".format(
            self._name.camel_with_underscores
        )


Self = AbstractVariable(names.Name("Self"))


@attr_expr("symbol")
class GetSymbol(AbstractExpression):
    """
    Abstract expression that gets a symbol out of a token.
    """

    def __init__(self, token_expr):
        """
        :param AbstractExpression token_expr: Expression returning a token.
        """
        super(GetSymbol, self).__init__()
        self.token_expr = token_expr

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: BuiltinCallExpr
        """
        token = construct(self.token_expr, Token)
        return BuiltinCallExpr("Get_Symbol", Symbol, [token])


class Let(AbstractExpression):
    """
    Abstract expressions that associates names to values from other abstract
    expressions and that evaluates yet another abstract expressions with these
    names available.
    """

    class Expr(ResolvedExpression):
        def __init__(self, vars, var_exprs, expr):
            self.vars = vars
            self.var_exprs = var_exprs
            self.expr = expr
            self.static_type = self.expr.type

            super(Let.Expr, self).__init__()

        def _render_pre(self):
            result = []
            for var, expr in zip(self.vars, self.var_exprs):
                result.append(expr.render_pre())
                result.append('{} := {};'.format(var.name, expr.render_expr()))
                if var.type.is_refcounted():
                    result.append('Inc_Ref ({});'.format(var.name))
            result.append(self.expr.render_pre())
            return '\n'.join(result)

        def _render_expr(self):
            return self.expr.render_expr()

        def __repr__(self):
            return '<Let.Expr (vars: {})>'.format(
                ', '.join(var.name.lower for var in self.vars)
            )

    def __init__(self, lambda_fn):
        """
        :param lambda_fn: Function that take an arbitrary number of arguments
            with default values (AbstractExpression instances) and that returns
            another AbstractExpression.
        """
        super(Let, self).__init__()
        argspec = inspect.getargspec(lambda_fn)

        self.vars = None
        ":type: list[AbstractVariable]"

        self.var_names = argspec.args

        self.var_exprs = argspec.defaults or []
        ":type: list[AbstractExpression]"

        self.expr = None
        self.lambda_fn = lambda_fn

    def do_prepare(self):
        argspec = inspect.getargspec(self.lambda_fn)

        check_multiple([
            (not argspec.varargs and not argspec.keywords,
             'Invalid function for Let expression (*args and **kwargs '
             'not accepted)'),

            (len(self.var_names) == len(self.var_exprs),
             'All Let expression function arguments must have default values')
        ])

        # Create the variables this Let expression binds and expand the result
        # expression using them.
        self.vars = [
            AbstractVariable(names.Name.from_lower(arg), create_local=True)
            for arg in self.var_names
        ]
        self.expr = self.lambda_fn(*self.vars)

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: LetExpr
        """
        scope = PropertyDef.get_scope()
        var_exprs = []
        for var, abs_expr in zip(self.vars, self.var_exprs):
            # First we construct the expression
            var_expr = construct(abs_expr)

            # Then we bind the type of this variable immediately, so that it is
            # available to subsequent variable declarations in this let block.
            var.set_type(var_expr.type)
            scope.add(var.local_var)
            var_exprs.append(var_expr)

        vars = map(construct, self.vars)

        return Let.Expr(vars, var_exprs, construct(self.expr))


class Block(Let):
    """
    Block is a helper class around let, that is not meant to be used directly,
    but is instead implicitly created when a property is given a function as an
    expression, so that you can do stuff like::

        @langkit_property()
        def my_prop():
            a = Var(1)
            b = Var(2)
            ...
    """

    blocks = []

    @classmethod
    @contextmanager
    def set_block(cls, block):
        cls.blocks.append(block)
        yield
        cls.blocks.pop()

    @classmethod
    def get(cls):
        return cls.blocks[-1]

    def __init__(self):
        # We bypass the let constructor, because we have a different
        # construction mode. However, we still want to call
        # AbstractExpression's __init__.
        AbstractExpression.__init__(self)

        self.vars = []
        self.var_exprs = []

    def add_var(self, var, expr):
        self.vars.append(var)
        self.var_exprs.append(expr)

    def do_prepare(self):
        pass


class Var(AbstractVariable):
    """
    Var allows you to declare local variable bound to expressions in the body
    of Properties, when those are defined through a function. See Block's
    documentation for more details.
    """

    def __init__(self, expr):
        super(Var, self).__init__(names.Name("Block_Var"), create_local=True)
        Block.get().add_var(self, expr)


class No(AbstractExpression):
    """
    Expression that returns a null value for the given type.
    """

    def __init__(self, expr_type):
        """
        :param CompiledType expr_type: Type parameter. Type for the value this
            expression creates.
        """
        super(No, self).__init__()
        self.expr_type = expr_type

    def do_prepare(self):
        self.expr_type = resolve_type(self.expr_type)
        check_source_language(
            self.expr_type.null_allowed,
            "Invalid type for Null expression: {}".format(
                self.expr_type.name().camel
            )
        )

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: LiteralExpr
        """
        return LiteralExpr(self.expr_type.nullexpr(), self.expr_type,

                           # We want to create a tmp in pointer cases,
                           # so that overloading resolution always works.
                           result_var_name="Null_Value"
                           if self.expr_type.is_ptr
                           else None)


class EmptyArray(AbstractExpression):
    """
    Expression that returns an empty array.
    """

    def __init__(self, element_type):
        """
        :param CompiledType element_type: Type for array items.
        """
        super(EmptyArray, self).__init__()
        self.element_type = element_type
        self.array_type = None

    def do_prepare(self):
        self.array_type = resolve_type(self.element_type).array_type()
        self.array_type.add_to_context()

    def construct(self):
        return LiteralExpr('Create (Items_Count => 0)', self.array_type,
                           result_var_name='Empty_Array')


def render(*args, **kwargs):
    return ct_render(*args, property=PropertyDef.get(), Self=Self, **kwargs)


class PropertyDef(AbstractNodeData):
    """
    This is the underlying class that is used to represent properties in the
    DSL. You are not supposed to use it directly, but instead use one of
    Property/AbstractProperty proxy constructors that will ensure the
    consistency of the passed arguments.
    """

    __current_properties__ = []
    """
    Stack for the properties that are currently bound.

    See the "bind" method.

    :type: list[Property|None]
    """

    # Overridings for AbstractNodeData class attributes
    is_property = True

    # Reserved names for arguments in generated subprograms
    self_arg_name = names.Name('Node')
    env_arg_name = names.Name('Lex_Env')

    # Collections for these
    reserved_arg_names = (self_arg_name, env_arg_name)
    reserved_arg_lower_names = [n.lower for n in reserved_arg_names]

    def __init__(self, expr, prefix, name=None, doc=None, private=None,
                 abstract=False, type=None, abstract_runtime_check=False):
        """
        :param expr: The expression for the property. It can be either:
            * An expression.
            * A function that will take the Self placeholder as parameter and
              return the constructed AbstractExpression. This is useful to
              reference classes that are not yet defined.
            * A function that takes one or more arguments with default values
              which are CompiledType subclasses. This is the way one can write
              properties that take parameters.
        :type expr:
            None
          | AbstractExpression
          | (AbstractExpression) -> AbstractExpression
          | () -> AbstractExpression

        :param names.Name prefix: Prefix to use for the name of the subprogram
            that implements this property in code generation.
        :param names.Name|None name: See AbstractNodeData's constructor.
        :param str|None doc: User documentation for this property.
        :param bool|None private: See AbstractNodeData's constructor.
        :param bool abstract: Whether this property is abstract or not. If this
            is True, then expr can be None.

        :param type: The optional type annotation for this property. If
            supplied, it will be used to check the validity of inferred types
            for this propery, and eventually for overriding properties in sub
            classes. NOTE: The type is mandatory for abstract base properties
            and for properties that take parameters. If the type itself is not
            available when creating the property, a lambda function that
            returns it is available.
        :type type: CompiledType|langkit.compiled_types.TypeRepo.Defer|None

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

        super(PropertyDef, self).__init__(name=name, private=private)

        self.in_type = False
        "Recursion guard for the type property"

        self.needs_logic_binder = False
        self.logic_predicates = []
        """
        The list of logic predicates to generate. First element of the tuple is
        a list of the args types, second is the unique identifier for this
        predicate.

        :type: [([CompiledType], str)]
        """

        self.prefix = prefix

        self.expr = expr
        ":type: AbstractExpression"

        self.constructed_expr = None

        self.vars = LocalVars()
        ":type: LocalVars"

        self.expected_type = type
        self.abstract = abstract
        self.abstract_runtime_check = abstract_runtime_check

        self.argument_vars = []
        """
        For each argument additional to Self, this is the AbstractVariable
        corresponding to this argument. Note that this is computed in the
        "prepare" pass.
        """

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

        self._doc = doc
        ":type: str|None"

    def __copy__(self):
        """
        When copying properties, we want to make sure they don't share local
        variables, so we implement a custom copier that duplicates the
        LocalVars instance.

        :rtype: Property
        """
        new = PropertyDef(self.expr, self.prefix, self._name, self._doc,
                          self._is_private, self.abstract, self.expected_type)
        new.vars = copy(self.vars)

        # Copy is used in the context of macros. In macros, we want to copy
        # the original Property's source location for error diagnostics,
        # rather than use the copied stack trace that will reference the new
        # class.
        new.location = self.location
        return new

    @classmethod
    def get(cls):
        """
        Return the currently bound property. Used by the rendering context to
        get the current property.

        :rtype: PropertyDef
        """
        return (cls.__current_properties__[-1]
                if cls.__current_properties__ else
                None)

    @classmethod
    def get_scope(cls):
        """
        Return the current local variable scope for the currently bound
        property.

        :rtype: LocalVars.Scope
        """
        return cls.get().vars.current_scope

    @contextmanager
    def bind(self):
        """
        Bind the current property to "Self", so that it is accessible in the
        expression templates.
        """
        self.__current_properties__.append(self)
        yield
        self.__current_properties__.pop()

    @classmethod
    @contextmanager
    def bind_none(cls):
        """
        Unbind "Self", so that compilation no longer see the current property.

        This is needed to compile Property-less expressions such as environment
        specifications.
        """
        cls.__current_properties__.append(None)
        yield
        cls.__current_properties__.pop()

    @property
    def type(self):
        """
        Returns the type of the underlying expression after resolution.

        :rtype: langkit.compiled_types.CompiledType
        """
        # If the user has provided a type, we'll return it for clients wanting
        # to know the type of the Property. Internal consistency with the
        # constructed_expr is checked when we emit the Property.
        if self.expected_type:
            return resolve_type(self.expected_type)

        check_source_language(
            not self.in_type,
            'Recursion loop in type inference for property {}. Try to '
            'specify its return type explicitly.'.format(self.qualname)
        )

        self.in_type = True
        try:
            # If the expr has not yet been constructed, try to construct it
            if not self.constructed_expr:
                self.construct_and_type_expression()

            ret = self.constructed_expr.type
        finally:
            self.in_type = False

        return resolve_type(ret)

    def _add_argument(self, name, type, default_value=None):
        """
        Helper to add an argument to this property.

        This basically just fills the .arguments and the .argument_vars lists.

        :param str names.Name: Name for this argument.
        :param CompiledType type: Type argument. Type for this argument.
        :param None|str default_value: Default value for this argument, if any.
        """
        self.arguments.append(Argument(name, type, default_value))
        self.argument_vars.append(AbstractVariable(name, type))

    def base_property(self):
        """
        Get the base property for this property, if it exists.

        :rtype: Property|None
        """
        if issubclass(self.struct, ASTNode):
            struct = assert_type(self.struct, ASTNode)
            return struct.base().get_abstract_fields_dict(
                field_class=PropertyDef
            ).get(self._name.lower, None)
        else:
            return None

    def prepare_abstract_expression(self):
        """
        Run the "prepare" pass on the expression associated to this property.

        This pass will:

        * Handle expansion of the toplevel function, and of property
          arguments, if there are some.

        * Call the prepare pass on the AbstractExpression tree. It will expand
          the abstract expression tree where needed, and perform some checks on
          it that cannot be done in the constructors. Notably, it will expand
          all lambda functions there into AbstractExpression nodes (which are
          then prepared themselves).

        After this pass, the expression tree is ready for the "construct" pass,
        which can yield a ResolvedExpression tree.

        :rtype: None
        """

        # TODO: We could at a later stage add a check to see that the abstract
        # property definition doesn't override another property definition on a
        # base class.

        # If the expected type is not a CompiledType, then it's a Defer.
        # Resolve it.
        self.expected_type = resolve_type(self.expected_type)

        if not self.expr:
            # Add the implicit lexical env. parameter
            self._add_argument(PropertyDef.env_arg_name,
                               LexicalEnvType,
                               LexicalEnvType.nullexpr())
            return

        self.expr = unsugar(self.expr, ignore_errors=True)
        check_source_language(
            isinstance(self.expr, AbstractExpression) or callable(self.expr),
            "Invalid object passed for expression of property: {}".format(
                self.expr
            )
        )

        # If the user passed a lambda or function for the expression,
        # now is the moment to transform it into an abstract expression by
        # calling it.
        if not isinstance(self.expr, AbstractExpression):

            check_source_language(callable(self.expr), 'Expected either an'
                                  ' expression or a function')

            argspec = inspect.getargspec(self.expr)
            defaults = argspec.defaults or []

            check_multiple([
                (not argspec.varargs or not argspec.keywords, 'Invalid'
                 ' function signature: no *args nor **kwargs allowed'),

                (len(argspec.args) == len(defaults), 'All parameters '
                 'must have an associated type as a default value')
            ])

            # This is a function for a property that takes parameters: check
            # that all parameters have declared types in default arguments.
            for kw, default in zip(argspec.args, defaults):
                # The type could be an early reference to a not yet declared
                # type, resolve it.
                default = resolve_type(default)

                check_source_language(
                    kw.lower() not in PropertyDef.reserved_arg_lower_names,
                    'Cannot define reserved arguments ({})'.format(
                        ', '.join(PropertyDef.reserved_arg_lower_names)
                    )
                )
                check_source_language(
                    issubclass(default, CompiledType),
                    'A valid langkit CompiledType is required for '
                    'parameter {} (got {})'.format(kw, default)
                )

                self._add_argument(names.Name.from_lower(kw), default)

            # Now that we have placeholder for all explicit arguments (i.e.
            # only the ones the user defined), we can expand the lambda
            # into a real AbstractExpression.

            # Wrap the expression in a Let block, so that the user can
            # declare local variables via the Var helper.
            if self.abstract:
                self.expr = None
            else:
                with self.bind():
                    function_block = Block()
                    with Block.set_block(function_block):
                        fn = assert_type(self.expr, types.FunctionType)
                        expr = check_type(
                            fn(*self.argument_vars), AbstractExpression,
                            "Properties return value should be an expression"
                        )
                        function_block.expr = expr
                        self.expr = function_block

        if not self.abstract:
            with self.bind():
                self.expr.prepare()

        # Add the implicit lexical env. parameter
        self._add_argument(PropertyDef.env_arg_name,
                           LexicalEnvType,
                           LexicalEnvType.nullexpr())

    def freeze_abstract_expression(self):
        """
        Run the "freeze" pass on the expression associated to this property.

        Afterwards, it will not be possible anymore to build
        AbstractExpressions trees out of the overloaded operators of the
        AbstractExpression instances in self.expr. See Frozable for more
        details.
        """
        if self.expr:
            self.expr.freeze()

    def compute_property_attributes(self):
        """
        Compute various property attributes, notably:
        * Information related to dispatching for properties.
        * Inheritance based information generally, like inheriting return
          type or privacy, consistency of annotations between base property
          and inherited properties.
        * Property overriding completeness checking.
        """
        base_prop = self.base_property()

        type_set = TypeSet()

        def check_overriding_props(klass):
            """
            Recursive helper. Checks wether klass and its subclasses override
            self.

            :param langkit.compiled_types.ASTNode klass: The class to check.
            """
            for subclass in klass.subclasses:
                for prop in subclass.get_properties(include_inherited=False):
                    if prop._name == self._name:
                        type_set.include(subclass)
                check_overriding_props(subclass)

        if self.abstract and not self.abstract_runtime_check:
            check_overriding_props(assert_type(self.struct, ASTNode))

            unmatched_types = sorted(type_set.unmatched_types(self.struct),
                                     key=lambda cls: cls.hierarchical_name())

            check_source_language(
                not unmatched_types,
                "Abstract property {} is not overriden in all subclasses. "
                "Missing overriding properties on classes: {}".format(
                    self.name.lower, ", ".join([t.name().camel for t in
                                                unmatched_types])
                ),
                severity=Severity.non_blocking_error
            )

        if base_prop:
            # If we have a base property, then this property is dispatching and
            # overriding, and the base property is dispatching (This
            # information can be missing at this stage for non abstract base
            # properties).
            self.overriding = True
            self.dispatching = True
            base_prop.dispatching = True

            # Inherit the privacy level or check that it's consistent with the
            # base property.
            if self._is_private is None:
                self._is_private = base_prop.is_private
            else:
                check_source_language(
                    self._is_private == base_prop.is_private,
                    "{} is {}, so should be {}".format(
                        base_prop.qualname,
                        'private' if base_prop.is_private else 'public',
                        self.qualname,
                    )
                )

            # We then want to check the consistency of type annotations if they
            # exist.
            if base_prop.expected_type:
                if self.expected_type:
                    check_source_language(
                        self.expected_type.matches(base_prop.expected_type),
                        '{} returns {} whereas it overrides {}, which returns'
                        ' {}. The former should match the latter.'.format(
                            self.qualname, self.expected_type.name().camel,
                            base_prop.qualname, base_prop.type.name().camel
                        )
                    )
                else:
                    # If base has a type annotation and not self, then
                    # propagate it.
                    self.expected_type = base_prop.expected_type
        elif self._is_private is None:
            # By default, properties are public
            self._is_private = False

    def construct_and_type_expression(self):
        """
        This pass will construct the resolved expression from the abstract
        expression, and get type information at the same time.
        """
        # If expr has already been constructed, return
        if self.constructed_expr or self.abstract:
            return

        with self.bind(), Self.bind_type(self.struct):
            base_prop = self.base_property()
            message = (
                'expected type {{expected}}, got'
                ' {{expr_type}} instead (expected type comes from'
                ' overridden base property in {base_prop})'.format(
                    base_prop=base_prop.struct.name().camel
                )
            ) if base_prop else None
            self.constructed_expr = construct(self.expr, self.expected_type,
                                              message)

        # Make sure that all the created local variables are associated to a
        # scope.
        self.vars.check_scopes()

    def render_property(self):
        """
        Render the given property to generated code.

        :rtype: basestring
        """
        with self.bind(), Self.bind_type(self.struct):
            with names.camel_with_underscores:
                self.prop_decl = render('properties/decl_ada')
                self.prop_def = render('properties/def_ada')

        base_prop = self.base_property()
        if base_prop and base_prop.type:
            # TODO: We need to make sure Properties are rendered in the proper
            # order (base classes first), to make sure that this check is
            # always effectful.
            check_source_language(
                self.type.matches(base_prop.type),
                "{} returns {} whereas it overrides {}, which returns {}."
                " The former should match the latter.".format(
                    self.qualname, self.type.name().camel,
                    base_prop.qualname, base_prop.type.name().camel
                )
            )

    def doc(self):
        return self._doc

    @property
    def explicit_arguments(self):
        """
        Return the subset of "self.arguments" that are to be passed explicitely
        when invoking this property.

        :rtype: list[Argument]
        """
        # Strip the implicit "Lex_Env" argument
        return self.arguments[:-1]

    @classmethod
    def compilation_passes(cls):
        """
        Return the list of compilation passes to run for properties.

        :rtype: list[(PropertyDef) -> None]
        """
        return [cls.prepare_abstract_expression,
                cls.freeze_abstract_expression,
                cls.compute_property_attributes,
                cls.construct_and_type_expression,
                cls.render_property]

    def do_generate_logic_binder(self):
        """
        Helper method, will trigger the emission of a logic binder object
        for the property.
        """
        self.needs_logic_binder = True

    @memoized
    def do_generate_logic_predicate(self, *partial_args_types):
        """
        Helper method, will trigger the emission of a logic predicate object
        for the property for the given partial argument types.

        :param [CompiledType] partial_args_types: The type of partially applied
            arguments passed to the logic predicate.

        :return: The identifier for the logic predicate, to be used as a prefix
            in code generation for every entity related to it.
        :rtype: str
        """
        # We use the length of the list as an id for the logic predicate. If
        # the method is called again with the same arg types, the same id
        # will be returned thanks to memoization.
        pred_num = len(self.logic_predicates)

        # This id will uniquely identify both the generic package and the
        # closure data structure.
        pred_id = "{}_{}_{}".format(self.struct.name(), self.name, pred_num)

        # We can use a list because the method is memoized, eg. this won't
        # be executed twice for the same partial_args_types tuple.
        self.logic_predicates.append((partial_args_types, pred_id))

        return pred_id

    def get_concrete_node_types(self, partial_args_types):
        """
        Helper for emission of logic predicate wrappers. Given partial
        argument types for trailing arguments that do not correspond to logic
        variables bound by the predicate, this helper will return the
        concrete node type for leading arguments that correspond to logic
        variables bound by the predicate.

        :param [CompiledType] partial_args_types: The type of partially applied
            arguments passed to the logic predicate.
        """
        return [self.struct] + [
            a.type for a in self.explicit_arguments[:-len(partial_args_types)]
        ]


# noinspection PyPep8Naming
def AbstractProperty(type, doc="", runtime_check=False, **kwargs):
    """
    Public constructor for abstract properties, where you can pass no
    expression but must pass a type. See _Property for further documentation.

    :type type: CompiledType
    :type doc: str
    :type runtime_check: bool
    :rtype: PropertyDef
    """
    return PropertyDef(expr=None, prefix=AbstractNodeData.PREFIX_PROPERTY,
                       type=type, doc=doc, abstract=True,
                       abstract_runtime_check=runtime_check, **kwargs)


# noinspection PyPep8Naming
def Property(expr, doc=None, private=None, type=None):
    """
    Public constructor for concrete properties. You can declare your properties
    on your ast node subclasses directly, like this::

        class SubNode(ASTNode):
            my_field = Field()
            my_property = Property(Self.my_field)

    and functions will be generated in the resulting library.

    :type expr: AbstractExpression|function
    :type type: CompiledType
    :type doc: str
    :type private: bool|None
    :rtype: PropertyDef
    """
    return PropertyDef(expr, AbstractNodeData.PREFIX_PROPERTY, doc=doc,
                       private=private, type=type)


class AbstractKind(Enum):
    concrete = 1
    abstract = 2
    abstract_runtime_check = 3


def langkit_property(private=None, return_type=None,
                     kind=AbstractKind.concrete):
    """
    Decorator to create properties from real python methods. See Property for
    more details.

    :type private: bool|None
    :type return_type: CompiledType
    """
    def decorator(expr_fn):
        return PropertyDef(
            expr_fn, AbstractNodeData.PREFIX_PROPERTY,
            type=return_type,
            private=private,
            doc=expr_fn.__doc__,
            abstract=kind in [AbstractKind.abstract,
                              AbstractKind.abstract_runtime_check],
            abstract_runtime_check=kind == AbstractKind.abstract_runtime_check
        )
    return decorator


class Literal(AbstractExpression):
    """
    Expression for literals of any type.
    """

    def __init__(self, literal):
        super(Literal, self).__init__()
        self.literal = literal

    def construct(self):
        # WARNING: Since bools are ints in python, bool needs to be before int
        # in the following table.
        return dispatch_on_type(type(self.literal), [
            (bool, lambda _: LiteralExpr(str(self.literal), BoolType)),
            (int, lambda _:  LiteralExpr(str(self.literal), LongType)),
        ], exception=DiagnosticError('Invalid abstract expression type: {}'))


class BasicExpr(ResolvedExpression):
    """
    A basic resolved expression template, that automatically handles:

    - Passing a list of sub expressions to the constructor, and a type
    - Doing the pre render of those expressions automatically
    - Rendering the sub expressions passed as parameters into the holes that
      are in the template.
    """

    def __init__(self, template, type, sub_exprs, result_var_name=None):
        """
        :param str template: The template string.
        :param None|CompiledType type: The return type of the expression.
        :param None|str result_var_name: See ResolvedExpression's constructor.
        """
        self.sub_exprs = sub_exprs
        self.static_type = type
        self.template = template
        super(BasicExpr, self).__init__(result_var_name)

    def _render_expr(self):
        return self.template.format(*map(ResolvedExpression.render_expr,
                                         self.sub_exprs))

    def _render_pre(self):
        return '\n'.join(expr.render_pre() for expr in self.sub_exprs)


class LiteralExpr(BasicExpr):
    """
    Resolved expression for literals of any type.
    """

    def __init__(self, literal, type, result_var_name=None):
        """
        :param str literal: The literal expression.
        :param CompiledType|None type: The return type of the expression.
        :param None|str result_var_name: See ResolvedExpression's constructor.
        """
        super(LiteralExpr, self).__init__(literal, type, [], result_var_name)

    def __repr__(self):
        return '<LiteralExpr {} ({})>'.format(self.template,
                                              self.type.name().camel)


class ArrayExpr(BasicExpr):
    """
    Resolved expression for an aggregate expression for any type of array.
    """

    def __init__(self, exprs, element_type):
        """
        :param list[ResolvedExpression] exprs: List of resolved expression
            whose types match element_type.
        :param CompiledType element_type: Type for elements in this array.
        """
        super(ArrayExpr, self).__init__(
            '({})'.format(', '.join(['{}'] * len(exprs)))
            if exprs else '(1 .. 0 => <>)',
            element_type.array_type(), exprs
        )


class UnreachableExpr(ResolvedExpression):
    """
    Resolved expression that just raises an error.

    This is useful to use as a placeholder for unreachable code.
    """

    def __init__(self, expr_type):
        """
        :param CompiledType expr_type: Type parameter. Type that a usual
            expression would return in this case.
        """
        self.static_type = expr_type
        super(UnreachableExpr, self).__init__()

    def _render_expr(self):
        return ('raise Program_Error with'
                ' "Executing supposedly unreachable code"')

    def __repr__(self):
        return '<UnreachableExpr (for {} expr)>'.format(
            self.type.name().camel
        )


class LocalVars(object):
    """
    Represents the state of local variables in a property definition.
    """

    def __init__(self):
        self.local_vars = {}
        self.root_scope = LocalVars.Scope(self, None)
        self.current_scope = self.root_scope

    class Scope(object):
        """
        Local variables are organized in a traditional scope hierarchy.

        During properties compilation, scopes are created and variables are put
        in a specific scope. This will help memory management: when execution
        goes out of a scope, the ref-count for all the variables is
        decremented.
        """

        COUNT = count(0)

        def __init__(self, vars, parent):
            """
            :param LocalVars vars: LocalVars instance for this scope.
            :param LocalVars.Scope|None parent: Parent scope.
            """
            self.index = next(self.COUNT)
            self.vars = vars
            self.parent = parent
            self.sub_scopes = []
            self.variables = []

        @property
        def name(self):
            return names.Name('Scope_{}'.format(self.index))

        @property
        def finalizer_name(self):
            """
            Return the name of the finalization procedure for this scope.

            :rtype: names.Name
            """
            return names.Name('Finalizer') + self.name

        def has_refcounted_vars(self, include_children=False):
            """
            Return whether this scope contains at least one variable that
            matters for reference counting.

            :param bool include_children: Whether to account for children in
                the computation.
            :rtype: bool
            """
            for var in self.variables:
                if var.type.is_refcounted():
                    return True

            return include_children and any(s.has_refcounted_vars(True)
                                            for s in self.sub_scopes)

        def add(self, var):
            """
            Associate "var" to this scope. Doing so twice for the same variable
            is an error.

            :param LocalVars.LocalVar var: Variable to associate.
            """
            assert var._scope is None, (
                'Trying to associate {} to some scope whereas it already has'
                ' one'.format(var)
            )
            self.variables.append(var)
            var._scope = self
            import traceback
            var._scope_tb = ''.join(traceback.format_stack())

        def push(self):
            """
            Create a new scope that is a child for the current scope, make it
            the current scope and return it.

            :rtype: LocalVars.Scope
            """
            result = LocalVars.Scope(self.vars, self)
            self.sub_scopes.append(result)
            self.vars.current_scope = result
            return result

        def pop(self):
            """
            Set the current scope to the parent of the current scope. Return
            this parent scope. Doing so when the current scope is the root one
            is an error.

            :rtype: LocalVars.Scope
            """
            parent = self.vars.current_scope.parent
            assert parent, 'Trying to pop the root scope'
            self.vars.current_scope = parent
            return parent

        @contextmanager
        def new_child(self):
            """
            Create a child scope for this block and return a context manager to
            make it the current scope temporarily.
            """
            yield self.push()
            self.pop()

        @contextmanager
        def use(self):
            """
            Return a context manager to make self the current scope
            temporarily.
            """
            old_scope = self.vars.current_scope
            self.vars.current_scope = self
            yield self
            self.vars.current_scope = old_scope

    class LocalVar(object):
        """
        Represents one local variable in a property definition.
        """
        def __init__(self, vars, name, type=None):
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

            self._scope = None
            """
            The scope this variable lives in. During the construct phase, all
            resolved expressions that create local variables must initialize
            this using LocalVars.Scope.add.

            :type: LocalVars.Scope
            """

        def render(self):
            assert self.type, "Local var must have type before it is rendered"
            return "{} : {};".format(
                self.name.camel_with_underscores,
                self.type.name().camel_with_underscores
            )

        def __repr__(self):
            return '<LocalVar {} : {}>'.format(
                self.name.camel_with_underscores,
                self.type.name().camel if self.type else '<none>'
            )

    def create(self, name, type):
        """
        Create a local variables in templates::

            from langkit.compiled_types import LocalVars
            vars = LocalVars()
            var = vars.create('Index', langkit.compiled_types.LongType)

        The names are *always* unique, so you can pass several time the same
        string as a name, and create will handle creating a name that is unique
        in the scope.

        The new local variable is automatically associated to the current
        scope.

        :param str|names.Name name: The name of the variable.
        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of the local variable.
        """
        result = self.create_scopeless(name, type)
        PropertyDef.get_scope().add(result)
        return result

    def create_scopeless(self, name, type):
        """
        Like "create", but do not assign a scope for the new local variable.
        The scope will have to be initialized later.

        :param str|names.Name name: The name of the variable.
        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of the local variable.
        """
        name = names.Name.get(name)

        i = 0
        orig_name = name
        while name in self.local_vars:
            i += 1
            name = orig_name + names.Name(str(i))
        ret = LocalVars.LocalVar(self, name, type)
        self.local_vars[name] = ret
        return ret

    def check_scopes(self):
        """
        Check that all variables are associated to a scope. Raise an
        AssertionError if it is not the case.
        """
        for var in self.local_vars.values():
            assert var._scope, '{} has no scope'.format(var)

    @property
    def all_scopes(self):
        """
        Return the list of all scopes in this repository.

        :rtype: list[LocalVars.Scope]
        """
        def children(s):
            return s.sub_scopes
        return funcy.tree_nodes(self.root_scope, children, children)

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


class BuiltinCallExpr(BasicExpr):
    """
    Convenience resolved expression that models a call to a function on the
    Ada side of things.
    """

    def __init__(self, name, type, exprs, result_var_name=None):
        """
        :param names.Name|str name: The name of the procedure to call.
        :param CompiledType|None type: The return type of the function call.
        :param [ResolvedExpression] exprs: A list of expressions that
            represents the arguments to the function call.
        :param None|str result_var_name: See ResolvedExpression's constructor.
        """
        self.name = names.Name.get(name)
        super(BuiltinCallExpr, self).__init__(
            '{} ({})'.format(self.name.camel_with_underscores,
                             ', '.join(['{}'] * len(exprs))),
            type, exprs, result_var_name
        )

    def __repr__(self):
        return '<BuiltinCallExpr {}>'.format(self.name.camel_with_underscores)


def is_simple_expr(expr):
    """
    Helper method to check that the expression is a simple expression,
    that can be evaluated outside of a property context.

    :param AbstractExpression expr: The expression to check.
    :rtype: bool
    """
    from langkit.expressions.structs import FieldAccess

    # Only accept FieldAccess. If the designated field is actually a property,
    # only allow argument-less ones.
    return expr is Self or (
        isinstance(expr, FieldAccess)
        and expr.receiver is Self and not expr.arguments
    )


def check_simple_expr(expr):
    """
    Helper method to check that the expression is a simple expression,
    that can be evaluated outside of a property context, and to raise an
    AssertionError otherwise.

    :param AbstractExpression expr: The expression to check.
    """
    assert is_simple_expr(expr), (
        "Only simple expressions consisting of a reference to"
        " Self, or a Field/Property access on Self, are allowed in"
        " the expressions in a lexical environment specification"
    )


class Arithmetic(AbstractExpression):
    """
    Arithmetic abstract expression. Used for emission of simple operator
    expressions like +, -, /, *, ..
    """

    def __init__(self, l, r, op):
        """
        :param AbstractExpression l: Left operand.
        :param AbstractExpression r: Right operand.
        :param str op: The operator to use, as a string.
        """
        super(Arithmetic, self).__init__()
        self.l, self.r, self.op = l, r, op

    def construct(self):
        return BasicExpr("({} %s {})" % self.op,
                         LongType, [construct(self.l, LongType),
                                    construct(self.r, LongType)])
