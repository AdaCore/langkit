from __future__ import absolute_import, division, print_function

from contextlib import contextmanager, nested
from functools import partial
import inspect
from itertools import count

from enum import Enum
import funcy

from langkit import names
from langkit.common import string_repr
from langkit.compiled_types import (
    AbstractNodeData, Argument, ASTNodeType, CompiledType, T, TypeRepo,
    gdb_helper, get_context, no_compiled_type, render as ct_render,
    resolve_type
)
from langkit.diagnostics import (
    Context, DiagnosticError, Severity, WarningSet, check_multiple,
    check_source_language, check_type, extract_library_location
)
from langkit.expressions.utils import assign_var
from langkit.utils import (
    TypeSet, assert_type, dispatch_on_type, inherited_property,
    not_implemented_error, memoized, self_memoized
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
    elif isinstance(expr, basestring):
        expr = SymbolLiteral(expr)
    elif isinstance(expr, TypeRepo.Defer):
        expr = expr.get()
    elif isinstance(expr, (list, tuple)):
        expr = ArrayLiteral(expr, None)

    check_source_language(
        ignore_errors or isinstance(expr, AbstractExpression),
        'Invalid abstract expression: {}'.format(expr)
    )

    return expr


def expand_abstract_fn(fn):
    """
    Expand a function used to describe a Langkit properties into an
    AbstractExpression tree with arguments substitued with AbstractVariable
    instances.

    Return a couple (fn_arguments, fn_expr) where fn_arguments is a list of
    Argument instances (for the properties arguments) and fn_expr is an
    AbstractExpression for the body of the property, or None if there is no
    such body.
    """
    fn_arguments = []
    fn_expr = None

    argspec = inspect.getargspec(fn)
    defaults = argspec.defaults or []

    check_multiple([
        (not argspec.varargs or not argspec.keywords, 'Invalid'
         ' function signature: no *args nor **kwargs allowed'),

        (len(argspec.args) == len(defaults), 'All parameters '
         'must have an associated type as a default value')
    ])

    # Check that all parameters have declared types in default arguments
    for kw, default in zip(argspec.args, defaults):
        check_source_language(
            kw.lower() not in PropertyDef.reserved_arg_lower_names,
            'Cannot define reserved arguments ({})'.format(
                ', '.join(PropertyDef.reserved_arg_lower_names)
            )
        )

        # Expect either a single value (the argument type) or a couple (the
        # argument type and an expression for the default value).
        if isinstance(default, tuple) and len(default) == 2:
            type_ref, default_value = default
            default_value = unsugar(default_value)
            default_value.prepare()
        else:
            type_ref = default
            default_value = None

        # The type could be an early reference to a not yet declared type,
        # resolve it.
        type_ref = resolve_type(type_ref)
        check_source_language(
            isinstance(type_ref, CompiledType),
            'A valid Langkit DSLType subclass is required for parameter {}'
            ' (got {})'.format(kw, type_ref)
        )

        # Only check that the expression is valid: we'll re-generate one tree
        # of resolved expression per call site.
        if default_value is not None:
            defval_expr = construct(default_value, type_ref)
            check_source_language(
                isinstance(defval_expr, BindableLiteralExpr),
                'Default value must be a compile-time known constant'
                ' (got {})'.format(default_value)
            )

        fn_arguments.append(Argument(names.Name.from_lower(kw), type_ref,
                                     default_value=default_value))

    # Now that we have placeholder for all arguments, we can expand the lambda
    # into a real AbstractExpression.

    # Wrap the expression in a Block, so that the user can declare local
    # variables via the Var helper.
    function_block = Block()
    with Block.set_block(function_block):
        expr = fn(*[arg.var for arg in fn_arguments])
        if expr is not None:
            expr = check_type(
                unsugar(expr), AbstractExpression,
                'Expected an abstract expression, but got instead a'
                ' {expr_type}'
            )
            function_block.expr = expr
            fn_expr = function_block

    return (fn_arguments, fn_expr)


def construct(expr, expected_type_or_pred=None, custom_msg=None,
              downcast=True):
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

    :param bool downcast: If the type of expr is a subtype of the passed
        expected_type, and this param is True, then generate a downcast.

    :rtype: ResolvedExpression
    """

    expr = unsugar(expr)
    with expr.diagnostic_context:

        ret = expr.construct()
        ret.location = expr.location

        if expected_type_or_pred:
            if isinstance(expected_type_or_pred, CompiledType):
                expected_type = expected_type_or_pred
                if not custom_msg:
                    custom_msg = "Expected type {expected}, got {expr_type}"

                check_source_language(ret.type.matches(expected_type), (
                    custom_msg.format(expected=expected_type.dsl_name,
                                      expr_type=ret.type.dsl_name)
                ))

                # If the type matches expectation but is incompatible in the
                # generated code, generate a conversion. This is needed for the
                # various ASTNodeType instances.
                if downcast and expected_type != ret.type:
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
                    custom_msg.format(expr_type=ret.type.dsl_name)
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

    def trigger_freeze(self, value=True):
        """
        Freeze the object and all its frozable components recursively.
        """

        # AbstractExpression instances can appear in more than one place in
        # expression "trees" (which are more DAGs actually), so avoid
        # unnecessary processing.
        if self.frozen and value:
            return

        # Deactivate this inspection because we don't want to force every
        # implementer of frozable to call super.

        # noinspection PyAttributeOutsideInit
        self._frozen = value

        for _, val in self.__dict__.items():
            if isinstance(val, Frozable):
                val.freeze()

    def freeze(self):
        self.trigger_freeze()

    def unfreeze(self):
        self.trigger_freeze(False)

    @staticmethod
    def protect(func):
        """
        Decorator for subclasses methods to prevent invokation after freeze.

        :param func: Unbound method to protect.
        :rtype: function
        """
        def wrapper(self, *args, **kwargs):
            if self.__dict__.get('_frozen', False):
                func_name = func.__name__
                if func_name == '__getattr__':
                    error_msg = 'Illegal field access: {}'.format(*args)
                else:
                    error_msg = ' Illegal method call: {}'.format(func_name)
                raise Exception(error_msg)
            return func(self, *args, **kwargs)
        return wrapper


class DocumentedExpression(object):
    """
    Holder for documentation data associated to a property DSL constructor
    (attribute or class constructor).
    """

    def __init__(self, is_attribute, name, constructor, args, kwargs,
                 parameterless, doc=None):
        """
        :param bool is_attribute: Whether this constructor is a mere class
            constructor or an attribute constructor.
        :param str name: Unique string to use as the name for this in generated
            documentation. This is the attribute name or the class name.
        :param constructor: Callable that builds the expression.
        :param args: Partial list of positional arguments to pass to
            `constructor`.
        :param kwargs: Partial keyword arguments to pass to `constructor`.
        :param bool parameterless: False if this ".attribute" requires
            arguments, true otherwise.
        :param str|None doc: If provided, must be a string to use as the
            documentation for this attribute expression.
        """
        self.is_attribute = is_attribute
        self.name = name
        self.constructor = constructor
        self.args = args
        self.kwargs = kwargs
        self.parameterless = parameterless

        self.doc = doc or constructor.__doc__
        self._prefix_name, self._argspec = self._build_argspec()

    @property
    def prefix_name(self):
        """
        Only valid for attribute constructors.  Name of the prefix for this
        attribute expression. This is used to generate documentation.

        :rtype: str
        """
        assert self.is_attribute
        return self._prefix_name

    @property
    def argspec(self):
        """
        Return a user-level argument specification for this construct.

        This returns None for parameter-less attributes and a list of strings
        for the others.

        :rtype: None|list[str]
        """
        return self._argspec

    def _build_argspec(self):
        func = self.constructor
        first_arg_is_self = False

        if inspect.isclass(func):
            func = getattr(func, '_wrapped_function', func.__init__)
            first_arg_is_self = True
        elif not inspect.isfunction(func):
            return 'expr', ['???']

        args, varargs, keywords, defaults = inspect.getargspec(func)

        # If present, discard the first argument (self), which is irrelevant
        # for documentation purposes. The second argument is the prefix for the
        # attribute expression.
        argspec = list(args)
        if first_arg_is_self:
            argspec.pop(0)
        prefix_name = argspec.pop(0) if self.is_attribute else None

        # Remove positional and keyword arguments which are already provided by
        # partial evaluation.
        argspec = argspec[len(self.args):]
        for kw in self.kwargs:
            argspec.remove(kw)

        # Describe variadic constructors as such
        if varargs:
            argspec.append('\*' + varargs)
        if keywords:
            argspec.append('\*\*' + keywords)

        if self.parameterless:
            argspec = None
        return prefix_name, argspec

    def build(self, prefix):
        assert self.is_attribute
        return (self.constructor(prefix, *self.args, **self.kwargs)
                if self.parameterless else
                partial(self.constructor, prefix, *self.args, **self.kwargs))

    def __repr__(self):
        return '<DocumentedExpression for {}, args={}, kwargs={}>'.format(
            self.constructor, self.args, self.kwargs
        )


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
    constructors = []

    @property
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

    def expand_underscores_1(self):
        """
        First pass for underscore expansion. This allows a user to write::

            A._.b

        instead of::

            A.then(lambda real_a: real_a.b)
        """
        from langkit.expressions import AbstractVariable, Then, FieldAccess

        for k, v in self.__dict__.items():
            if isinstance(v, FieldAccess) and v.field == "_":
                var_expr = AbstractVariable(names.Name("Var_Expr"),
                                            create_local=True)
                setattr(self, k, var_expr)
                t = Then.create_from_exprs(v.receiver, self, var_expr)
                t.underscore_then = True
                return t

    def expand_underscores_2(self):
        """
        Second pass for underscore expansion. This will hoist further field
        accesses on an underscore expression, so that a user can write::

            A._.b.c
        """
        from langkit.expressions import Then, FieldAccess
        if (isinstance(self, FieldAccess)
                and isinstance(self.receiver, Then)
                and self.receiver.underscore_then):
            then = self.receiver
            self.receiver = then.then_expr
            then.then_expr = self
            return then

    def prepare(self):
        """
        This method will be called in the top-level construct function, for
        expressions that have not been prepared yet. It will run a certain
        number of passes on AbstractExpression trees, before they are frozen.

        This means that if you want to add custom expansions to expression
        trees, this is a good moment to do it. You can register new passes that
        will be called on every node, and decide if the pass is called on
        children first or on the parent first.

        The current passes are:
        * prepare_pass: A pass that will run the custom do_prepare method on
          every AbstractExpression in the expression tree, aswell as the first
          part of the expand_underscores transformation.

        * expand_underscores_2: Second part of the expand_underscores
          transformation.
        """

        def prepare_pass(expr):
            expr = expr.expand_underscores_1() or expr
            expr.do_prepare()
            return expr

        passes = [
            (prepare_pass, True),
            (lambda expr: expr.expand_underscores_2(), False)
        ]

        def explore(obj, fn, pre=True):
            """
            Traversal function. Will traverse the object graph, and call fn on
            every object that is an AbstractExpression. If fn returns a new
            AbstractExpression, it will replace the old one in the tree.

            :param obj: The object to visit.
            :param fn: The fn to apply.
            :param pre: Whether to explore tree before or after calling fn.
            """
            if isinstance(obj, AbstractExpression):
                if pre:
                    obj = fn(obj) or obj
                for k, v in obj.__dict__.items():
                    new_v = explore(v, fn)
                    if new_v:
                        obj.__dict__[k] = new_v
                if not pre:
                    obj = fn(obj) or obj
                return obj
            elif isinstance(obj, (list, tuple)):
                for v in obj:
                    explore(v, fn)
            elif isinstance(obj, (dict)):
                for v in obj.items():
                    explore(v, fn)
            elif (not isinstance(obj, (PropertyDef, TypeRepo.Defer)) and
                    hasattr(obj, 'prepare')):
                explore(obj.prepare(), fn)

        ret = self
        for p, order in passes:
            ret = explore(ret, p, order) or ret
        return ret

    def construct(self):
        """
        Returns a resolved tree of resolved expressions.

        :rtype: ResolvedExpression
        """
        raise NotImplementedError()

    @memoized
    def composed_attrs(self):
        """
        Helper memoized dict for attributes that are composed on top of
        built-in ones. Since they're built on regular attrs, we cannot put
        them in attrs or it would cause infinite recursion.
        """
        from langkit.expressions.structs import IsNull
        from langkit.expressions.boolean import Not, Or
        from langkit.expressions.logic import All, Any

        return {
            '_or': lambda alt: self.then(lambda e: e, default_val=alt),
            'any_of': lambda *els: Or(*[self == el for el in els]),
            'empty': self.length.equals(0),
            'find': lambda filter_expr:
                self.filter(filter_expr).at(0),
            'keep': lambda klass:
                self.filtermap(lambda e: e.cast(klass),
                               lambda e: e.is_a(klass)),
            'logic_all': lambda e: All(self.map(e)),
            'logic_any': lambda e: Any(self.map(e)),
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
            return AbstractExpression.attrs_dict[attr].build(self)
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


def dsl_document(cls):
    """
    Decorator for AbstractExpression subclasses to be described in the Langkit
    user documentation.
    """
    AbstractExpression.constructors.append(DocumentedExpression(
        False, cls.__name__, cls, (), {}, False
    ))
    return cls


def attr_call(name, *args, **kwargs):
    """
    Decorator to create an expression accessible through an attribute call on
    an AbstractExpression instance, from an abstract expression class. See
    attr_expr_impl for more details.

    :param name: The name of the attribute.
    :param str|None doc: If provided, must be a string to use as the
        documentation for this attribute expression.
    :param args: additional arguments to pass to the class.
    :param kwargs: additional arguments to pass to the class.
    """
    return attr_expr_impl(name, args, kwargs)


def attr_expr(name, *args, **kwargs):
    """
    Decorator to create an expression accessible through a parameterless
    attribute on an AbstractExpression instance, from an abstract expression
    class. See attr_expr_impl for more details.

    :param name: The name of the attribute.
    :param str|None doc: If provided, must be a string to use as the
        documentation for this attribute expression.
    :param args: additional arguments to pass to the class.
    :param kwargs: additional arguments to pass to the class.
    """
    return attr_expr_impl(name, args, kwargs, parameterless=True)


def attr_expr_impl(name, args, kwargs, parameterless=False):
    """
    Implementation for attr_expr and attr_call.

    :param name: The name of the attribute.
    :param args: additional arguments to pass to the class.
    :param kwargs: additional arguments to pass to the class.
    :param bool parameterless: Whether the attribute should take parameters
        or not.
    :param str|None doc: If provided, must be a string to use as the
        documentation for this attribute expression.
    """

    def internal(decorated_class):
        AbstractExpression.attrs_dict[name] = DocumentedExpression(
            True, name, decorated_class, args, kwargs, parameterless,
            kwargs.pop('doc', None)
        )
        return decorated_class

    return internal


def auto_attr_custom(name, *partial_args, **partial_kwargs):
    """
    Helper decorator, that will automatically register an AbstractExpression
    subclass accessible as an attribute. Exposes more options than auto_attr.
    See auto_attr for more detail.

    :param str|None name: The name of the attribute. If None, the name of the
        function will be taken.
    :param str|None doc: If provided, must be a string to use as the
        documentation for this attribute expression.
    :param [object] partial_args: Arguments to partially apply to the function.
    :param [object] partial_kwargs: Keyword arguments to partially apply to the
        function.
    """
    doc = partial_kwargs.pop('doc', None)

    def internal(fn):
        attr_name = name or fn.__name__

        def __init__(self, *sub_expressions, **kwargs):
            AbstractExpression.__init__(self)
            self.nb_exprs = len(sub_expressions)
            for i, expr in enumerate(sub_expressions):
                setattr(self, "expr_{}".format(i), expr)
            self.kwargs = kwargs

        @property
        def sub_expressions(self):
            return tuple(getattr(self, "expr_{}".format(i))
                         for i in range(self.nb_exprs))

        def construct(self):
            return fn(self, *self.sub_expressions, **self.kwargs)

        def __repr__(self):
            return "<{}{}>".format(
                self.__class__.__name__,
                "({})".format(", ".join(str(e) for e in self.sub_expressions))
                if self.sub_expressions else ""
            )

        nb_args = len(inspect.getargspec(fn).args)

        assert nb_args > 1

        decorator = (attr_expr if nb_args == 2 else attr_call)

        decorator(attr_name, *partial_args, doc=doc, **partial_kwargs)(type(
            b'{}'.format(attr_name),
            (AbstractExpression, ), {
                'construct': construct,
                '__init__': __init__,
                '__repr__': __repr__,
                'sub_expressions': sub_expressions,
                '__doc__': fn.__doc__,
                '_wrapped_function': fn,
            }
        ))

        # We're returning the function because we want to be able to chain
        # those decorators calls.
        return fn

    return internal


def auto_attr(fn):
    """
    Helper decorator, that will automatically register an AbstractExpression
    subclass accessible as an attribute, from a function that takes a number of
    abstract expressions. This decorator will automatically infer whether
    it's parameterless or not.

    :param (*[AbstractExpression]) -> ResolvedExpression fn: A function
        taking a number of abstract expressions as parameters, and returning a
        resolved expression.
    """
    return auto_attr_custom(None)(fn)


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

    expr_count = iter(count(1))
    """
    Generator of unique identifiers for expressions in GDB helpers. See
    render_pre.
    """

    def __init__(self, result_var_name=None, skippable_refcount=False,
                 abstract_expr=None):
        """
        Create a resolve expression.

        :param None|str result_var_name: If provided, create a local variable
            using this as a base name to hold the result of this expression.
            In this case, the "type" property must be ready.
        :param bool skippable_refcount: If True, this resolved expression can
            omit having a result variable even though its result is
            ref-counted. This makes it possible to simplify the generated code.
        :param AbstractExpression|None abstract_expr: For resolved expressions
            that implement an abstract expression, this must be the original
            abstract expression.
        """
        if result_var_name:
            self._result_var = PropertyDef.get().vars.create(result_var_name,
                                                             self.type)
        else:
            self._result_var = None

        self.skippable_refcount = skippable_refcount

        self.abstract_expr = abstract_expr

        self._render_pre_called = False
        """
        Safety guard: except for variables, it is highly suspicious for
        render_pre to be called more than once for a given resolved expression.
        This will happen if we start using such an expression multiple times in
        the expression tree.
        """

    @property
    def result_var(self):
        """
        Return the local variable used to store the result of this expression,
        if any. Note that if the result is not null, the caller can assume that
        the "render_expr" method only returns the result variable name.

        :rtype: LocalVars.LocalVar|None
        """
        return self._result_var

    def create_result_var(self, name):
        """
        If this property already has a result variable, return it as a resolved
        expression. Otherwise, create one and return it.

        :param str name: Camel with underscores-formatted name for the result
            variable.
        :rtype: VariableExpr
        """
        assert not self._render_pre_called, (
            'Trying to create a result variable while the expression has been'
            ' rendered'
        )

        # If this is already a variable, we don't need to create another
        # variable to hold the same value: just return it.
        if isinstance(self, VariableExpr):
            return self

        # Otherwise, create a result variable if it does not exist yet
        elif not self._result_var:
            self._result_var = PropertyDef.get().vars.create(name, self.type)

        return self.result_var.ref_expr

    def render_pre(self):
        """
        Render initial statements that might be needed to the expression.

        :rtype: str
        """
        assert not self._render_pre_called, (
            '{}.render_pre can be called only once'.format(type(self).__name__)
        )
        self._render_pre_called = not isinstance(self, VariableExpr)

        assert (self.skippable_refcount
                or self.type is no_compiled_type
                or not self.type.is_refcounted
                or self._result_var), (
            'ResolvedExpression instances that return ref-counted values must'
            ' store their result in a local variable (this {} does'
            ' not).'.format(self)
        )

        pre = self._render_pre()
        expr = str(self._render_expr())

        # Some expressions build their result directly inside the result
        # variable, and thus their _render_pre() method will only return the
        # name of the result variable. In such cases, there is no need to
        # add a tautological assignment (X:=X), which would hamper generated
        # code reading anyway.
        if self.result_var and expr != str(self.result_var.name):
            result = '{}\n{} := {};'.format(
                pre, self.result_var.name.camel_with_underscores, expr,
            )
        else:
            result = pre

        # If this resolved expression materialize the computation of an
        # abstract expression and its result is stored in a variable, make it
        # visible to the GDB helpers.
        if (PropertyDef.get() and
                PropertyDef.get().has_debug_info and
                self.abstract_expr and
                self.result_var):
            unique_id = str(next(self.expr_count))

            loc = self.abstract_expr.location
            loc_str = '{}:{}'.format(loc.file, loc.line) if loc else 'None'

            result = '{}\n{}\n{}'.format(
                gdb_helper('expr-start', unique_id,
                           str(self.abstract_expr),
                           self.result_var.name.camel_with_underscores,
                           loc_str),
                result,
                gdb_helper('expr-done', unique_id),
            )

        return result

    def render_expr(self):
        """
        Render the expression itself.

        :rtype: str
        """
        return (self.result_var.name.camel_with_underscores
                if self.result_var else
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

        Note that the returned expression must be idempotent: each evaluation
        must return the exact same result for the exact same context.

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
                '{} must redefine the type property, or to fill the'
                ' static_type class field'.format(self)
            )
        return self.static_type

    @property
    def ir_dump(self):
        """
        Return a textual representation of this resolved expression tree.

        :rtype: str
        """
        return '\n'.join(self._ir_dump(self.subexprs))

    @classmethod
    def _ir_dump(cls, json_like):
        """
        Helper for "ir_dump". Return text representation as a list of lines.

        :rtype: list[str]
        """
        max_cols = 72
        result = []

        def one_line_subdumps(subdumps):
            """
            Return whether all dumps in "subdumps" are one-line long.
            """
            return all(len(d) == 1 for d in subdumps)

        # Adopt a specific dump format depending on the type of "json_like".
        # In each case below, first try to return a one-line dump that fits in
        # the column limit. If it does not, fall back to a multi-line dump.

        if isinstance(json_like, list):
            subdumps = [cls._ir_dump(elt) for elt in json_like]

            # One-line format: [A, B, ...]
            if one_line_subdumps(subdumps):
                one_liner = '[{}]'.format(', '.join(
                    d[0] for d in subdumps
                ))
                if len(one_liner) <= max_cols:
                    return [one_liner]

            # Multi-line format::
            #
            #     * Aaaaaa...
            #     | aaaa
            #     * Bbbbbbbbbbb
            #     ...
            for elt in json_like:
                subdump = cls._ir_dump(elt)
                result.append('*  {}'.format(subdump[0]))
                result.extend('|  {}'.format(line) for line in subdump[1:])

        elif isinstance(json_like, dict):
            keys = sorted(json_like)
            subdumps = [cls._ir_dump(json_like[key]) for key in keys]
            items = zip(keys, subdumps)

            # One-line format: {A=a, B=b, ...}
            if one_line_subdumps(subdumps):
                one_liner = '{{{}}}'.format(
                    ', '.join('{}={}'.format(key, d[0]) for key, d in items)
                )
                if len(one_liner) <= max_cols:
                    return [one_liner]

            # Multi-line format::
            #
            #     A: aaaa
            #     B:
            #     |  bbbbbbbb...
            #     |  bbbbb
            for key, d in zip(keys, subdumps):
                if len(d) == 1 and len(d[0]) <= max_cols:
                    result.append('{}: {}'.format(key, d[0]))
                else:
                    result.append('{}:'.format(key))
                    result.extend('|  {}'.format(line) for line in d)

        elif isinstance(json_like, ResolvedExpression):
            class_name = getattr(json_like, 'pretty_class_name',
                                 type(json_like).__name__)
            subdump = cls._ir_dump(json_like.subexprs)

            # One-line format: ResolvedExpressionName(...)
            if len(subdump) == 1:
                one_liner = '{}{}'.format(
                    class_name, subdump[0]
                )
                if len(one_liner) <= max_cols:
                    return [one_liner]

            # Multi-line format::
            #
            #     ResolvedExpressionName(
            #     |  ...
            #     )
            result.append('{}('.format(class_name))
            result.extend('|  {}'.format(line) for line in subdump)
            result.append(')')

        elif isinstance(json_like, CompiledType):
            return cls._ir_dump(json_like.name)

        elif isinstance(json_like, names.Name):
            result.append(json_like.camel_with_underscores)

        else:
            result.append(str(json_like))

        return result

    @property
    def subexprs(self):
        """
        A JSON-like datastructure to describe this expression.

        Leaves of this datastructure are: strings, CompiledType instances,
        AbtsractNodeData instances and ResolvedExpression instances (for
        operands). This is used both for expression tree traversal and for IR
        dump.

        Subclasses must override this property if they have operands.
        """
        return []

    def flat_subexprs(
        self, filter=lambda expr: isinstance(expr, ResolvedExpression)
    ):
        """
        Wrapper around "subexprs" to return a flat list of items matching
        "filter". By default, get all ResolvedExpressions.

        :param filter: Predicate to test whether a subexpression should be
            returned.
        :type filter: (T) -> bool

        :rtype: list[ResolvedExpression]
        """
        def explore(values):
            if values is None:
                return []
            elif isinstance(values, (list, tuple)):
                return funcy.mapcat(explore, values)
            elif isinstance(values, dict):
                return funcy.mapcat(explore, values.values())
            elif filter(values):
                return [values]
            else:
                return []

        return explore(self.subexprs)

    @property
    def bindings(self):
        """
        Return the list of variables defined in "self", including in subexprs.

        Subclasses must override the "_bindings" method.

        :rtype: list[VariableExpr]
        """
        # Do a copy to avoid mutating the expression own's data structures
        result = list(self._bindings())
        for expr in self.flat_subexprs():
            result.extend(expr.bindings)
        return result

    def _bindings(self):
        """
        Return the list of variables "self" defines.

        Subclasses must override this method if they define variables.

        :rtype: list[VariableExpr]
        """
        return []

    def destructure_entity(self):
        """
        Must be called only on expressions that evaluate to entities.  Return
        3 expressions:

          1. A SavedExpr wrapper for self, so its result can be used multiple
             times.
          2. An expression that evaluates the entity node.
          3. An expression that evaluates the entity info.

        The SavedExpr (1) must be evaluated before any of (2) and (3) are
        evaluated themselves.

        :rtype: (ResolvedExpression, ResolvedExpression, ResolvedExpression).
        """
        from langkit.expressions.structs import FieldAccess
        assert self.type.is_entity_type
        fields = self.type.get_abstract_fields_dict()
        saved = SavedExpr('Saved', self)
        return (
            saved,
            FieldAccess.Expr(saved.result_var_expr, fields['el'], []),
            FieldAccess.Expr(saved.result_var_expr, fields['info'], []),
        )


class VariableExpr(ResolvedExpression):
    """
    Resolved expression that is just a reference to an already computed value.
    """

    pretty_class_name = 'Var'

    def __init__(self, type, name, local_var=None, abstract_var=None):
        """
        Create a variable reference expression.

        :param langkit.compiled_types.CompiledType type: Type for the
            referenced variable.
        :param names.Name name: Name of the referenced variable.
        :param LocalVars.LocalVar|None local_var: The corresponding local
            variable, if there is one.
        :param AbstractVariable|None abstract_var: AbstractVariable that
            compiled to this resolved expression, if any.
        """
        self.static_type = assert_type(type, CompiledType)
        self.name = name
        self.local_var = local_var
        self.abstract_var = abstract_var
        self._ignored = False

        super(VariableExpr, self).__init__(skippable_refcount=True)

    @property
    def result_var(self):
        return self.local_var

    def _render_expr(self):
        return self.name.camel_with_underscores

    @property
    def source_name(self):
        """
        If it comes from the language specification, return the original
        source name for this variable. Return None otherwise.

        :rtype: names.Name|None
        """
        return (self.abstract_var.source_name
                if self.abstract_var and self.abstract_var.source_name else
                None)

    @property
    def ignored(self):
        """
        If this comes from the language specification, return whether it is
        supposed to be ignored. Return False otherwise.
        """
        return self._ignored or (self.abstract_var.ignored
                                 if self.abstract_var else False)

    def set_ignored(self):
        """
        Ignore this resolved variable in the context of the unused bindings
        warning machinery.
        """
        self._ignored = True

    def __repr__(self):
        src_name = self.source_name
        return '<VariableExpr {}{}>'.format(
            self.name.lower,
            ' ({})'.format(src_name.lower) if src_name else '')

    @property
    def is_self(self):
        """
        Return whether this correspond to the Self singleton.

        :rtype: bool
        """
        return self.abstract_var and self.abstract_var is Self

    @property
    def subexprs(self):
        result = {'name': self.name.lower}
        if self.source_name:
            result['source-name'] = self.source_name.lower
        return result


class ErrorExpr(ResolvedExpression):
    """
    Resolved expression that just raises an error.
    """

    def __init__(self, expr_type, exception_name, message=None):
        """
        :param CompiledType expr_type: Placeholder type for this expression, as
            if this expression would return a value.
        :param names.Name exception_name: Name of the Ada exception to raise.
        :param str|None message: Optional error message.
        """
        self.static_type = expr_type
        self.exception_name = exception_name
        self.message = message
        super(ErrorExpr, self).__init__(skippable_refcount=True)

    def _render_expr(self):
        result = 'raise {}'.format(self.exception_name)
        if self.message:
            result += ' with {}'.format(string_repr(self.message))
        return result

    def __repr__(self):
        return '<ErrorExpr {} with {}>'.format(self.exception_name,
                                               repr(self.message))


class UnreachableExpr(ErrorExpr):
    """
    Placeholder resolved expression for unreachable code.
    """

    def __init__(self, expr_type):
        super(UnreachableExpr, self).__init__(
            expr_type, names.Name('Program_Error'),
            'Executing supposedly unreachable code'
        )


@dsl_document
class PropertyError(AbstractExpression):
    """
    Expression to raise a Property_Error. `expr_type` is the type this
    expression would have if it computed a value. `message` is an optional
    error message (it can be left to None).
    """

    def __init__(self, expr_type, message=None):
        self.expr_type = expr_type
        self.message = message
        super(PropertyError, self).__init__()

    def construct(self):
        check_source_language(
            self.message is None or isinstance(self.message, str),
            'Invalid error message: {}'.format(repr(self.message))
        )
        return ErrorExpr(resolve_type(self.expr_type),
                         names.Name('Property_Error'),
                         self.message)


class LiteralExpr(ResolvedExpression):
    """
    Resolved expression for literals of any type.

    The pecularity of literals is that they are not required to live in local
    variables. Because of this, if the type at hand is ref-counted, then the
    literal must be a ref-counting "insensitive" value, for instance a null
    value or an Ada aggregate.
    """

    def __init__(self, template, expr_type, operands=[], abstract_expr=None):
        """
        :param str template: String template for the expression. Rendering will
            interpolate it with the operands' render_expr methods evaluation.
        :param CompiledType type: The return type of the expression.
        :param list[ResolvedExpression] operand: Operands for this expression.
        """
        self.static_type = expr_type
        self.template = template
        self.operands = operands

        super(LiteralExpr, self).__init__(skippable_refcount=True,
                                          abstract_expr=abstract_expr)

    def _render_pre(self):
        return '\n'.join(o.render_pre() for o in self.operands)

    def _render_expr(self):
        return self.template.format(*[o.render_expr() for o in self.operands])

    def render_python_constant(self):
        """
        Assuming this expression is a valid constant, return Python code to
        materialize it in the generated binding.

        :rtype: str
        """
        raise not_implemented_error(self, self.rendrer_python_constant)

    @property
    def subexprs(self):
        return {'0-type': self.static_type,
                '1-template': self.template,
                '2-operands': self.operands}

    def __repr__(self):
        return '<LiteralExpr {} ({})>'.format(
            self.template,
            self.static_type.name.camel if self.static_type else '<no type>'
        )


class BindableLiteralExpr(LiteralExpr):
    """
    Resolved expression for literals that can be expressed in all bindings.
    """

    def render_private_ada_constant(self):
        """
        Assuming this expression is a valid constant, return Ada code to
        materialize it in the private API ($.Analysis.Implementation).

        :rtype: str
        """
        raise not_implemented_error(self, self.render_private_ada_constant)

    def render_public_ada_constant(self):
        """
        Assuming this expression is a valid constant, return Ada code to
        materialize it in the public API ($.Analysis).

        :rtype: str
        """
        raise not_implemented_error(self, self.render_public_ada_constant)

    def render_python_constant(self):
        """
        Assuming this expression is a valid constant, return Python code to
        materialize it in the generated binding.

        :rtype: str
        """
        raise not_implemented_error(self, self.rendrer_python_constant)


class BooleanLiteralExpr(BindableLiteralExpr):

    def __init__(self, value, abstract_expr=None):
        self.value = value
        super(BooleanLiteralExpr, self).__init__(
            str(value), T.BoolType, abstract_expr=abstract_expr
        )

    def render_private_ada_constant(self):
        return str(self.value)

    def render_public_ada_constant(self):
        return str(self.value)

    def render_python_constant(self):
        return str(self.value)


class IntegerLiteralExpr(BindableLiteralExpr):

    def __init__(self, value, abstract_expr=None):
        self.value = value
        super(IntegerLiteralExpr, self).__init__(
            str(value), T.LongType, abstract_expr=abstract_expr
        )

    def render_private_ada_constant(self):
        return str(self.value)

    def render_public_ada_constant(self):
        return str(self.value)

    def render_python_constant(self):
        return str(self.value)


class NullExpr(BindableLiteralExpr):
    """
    Resolved expression for the null expression corresponding to some type.
    """

    def __init__(self, type, abstract_expr=None):
        expr = type.nullexpr
        if type.is_ptr:
            expr = "{}'({})".format(type.name.camel_with_underscores,
                                    expr)
        super(NullExpr, self).__init__(expr, type, abstract_expr=abstract_expr)

    def render_private_ada_constant(self):
        return self._render_expr()

    def render_public_ada_constant(self):
        if self.type.is_entity_type:
            return 'No_{}'.format(self.type.api_name.camel_with_underscores)

        assert self.type.api_name == self.type.name, (
            'Cannot generate a public Ada constant for type {}'.format(
                self.type.dsl_name
            )
        )
        return self._render_expr()

    def render_python_constant(self):
        return 'None' if self.type.is_entity_type else self.type.py_nullexpr


class UncheckedCastExpr(ResolvedExpression):
    """
    Resolved expression for unchecked casts.

    These casts will not raise a Property_Error if they fail. We must use them
    in code generation only when we know they cannot fail.
    """

    def __init__(self, expr, dest_type):
        self.expr = expr
        self.dest_type = dest_type
        self.static_type = dest_type
        super(UncheckedCastExpr, self).__init__()

    def _render_pre(self):
        return self.expr.render_pre()

    def _render_expr(self):
        return '{} ({})'.format(self.dest_type.name, self.expr.render_expr())

    @property
    def subexprs(self):
        return {'0-type': self.dest_type, '1-expr': self.expr}

    def __repr__(self):
        return '<UncheckedCastExpr {}>'.format(
            self.dest_type.name.camel_with_underscores
        )


class ComputingExpr(ResolvedExpression):
    """
    Base class for resolved expressions that do computations.

    These expressions are the only ones visible for GDB helpers. As such, they
    are required to store their result into a result variable, and thus
    subclasses only need to override the "_render_pre" method, which is
    supposed to initialize the result variable with the expression evaluation.
    """

    def __init__(self, result_var_name, abstract_expr=None):
        super(ComputingExpr, self).__init__(result_var_name,
                                            abstract_expr=abstract_expr)

    def _render_expr(self):
        return self.result_var.name.camel_with_underscores


class SavedExpr(ResolvedExpression):
    """
    Wrapper expression that will make sure we have a result variable for the
    input expression. This makes it easier to re-use the result of an
    expression multiple times, as we forbid tree sharing.

    If the input expression has no result variable, we create one for it,
    otherwise we re-use it.
    """

    def __init__(self, result_var_name, expr, abstract_expr=None):
        self.expr = expr
        self.static_type = expr.type

        if expr.result_var:
            self.exposed_result_var = expr.result_var
            result_var_name = None

        super(SavedExpr, self).__init__(result_var_name,
                                        skippable_refcount=True,
                                        abstract_expr=abstract_expr)

        if result_var_name:
            self.exposed_result_var = self._result_var

    @property
    def result_var(self):
        """
        Return the LocalVar instance corresponding to the result of this
        expression.

        :rtype: LocalVars.LocalVar
        """
        return self.exposed_result_var

    @property
    def result_var_expr(self):
        """
        Return a reference to the variable that contains the result of this
        expression.

        :rtype: VariableExpr
        """
        return self.result_var.ref_expr

    def _render_pre(self):
        result = [self.expr.render_pre()]
        if self._result_var:
            result.append(assign_var(self._result_var.ref_expr,
                                     self.expr.render_expr()))
        return '\n'.join(result)

    def _render_expr(self):
        return self.exposed_result_var.name.camel_with_underscores

    @property
    def subexprs(self):
        return {'expr': self.expr}

    def __repr__(self):
        return '<SavedExpr>'


class SequenceExpr(ResolvedExpression):
    """
    Expression to evaluate a first expression, then a second one.

    The result of this compound expression is the result of the second one.
    This makes it easier to express computations where an expression needs to
    be repeated multiple times later on (as we forbid tree sharing).
    """

    def __init__(self, pre_expr, post_expr, abstract_expr=None):
        """
        This expression will evaluate `pre_expr`, then `post_expr`, and will
        then return the result of `post_expr`.
        """
        self.pre_expr = pre_expr
        self.post_expr = post_expr
        self.static_type = post_expr.type

        # This expression completely delegates the work of managing the result
        # value to `post_expr`, so we can safely avoid all ref-counting
        # activity here.
        super(SequenceExpr, self).__init__(skippable_refcount=True,
                                           abstract_expr=abstract_expr)

    def _render_pre(self):
        return '{}\n{}'.format(self.pre_expr.render_pre(),
                               self.post_expr.render_pre())

    def _render_expr(self):
        return self.post_expr.render_expr()

    @property
    def subexprs(self):
        return {'0-pre': self.pre_expr,
                '1-post': self.post_expr}

    def __repr__(self):
        return '<SequenceExpr>'

    class _ForwardExpr(ResolvedExpression):
        def __init__(self, dest_var, expr):
            self.dest_var = dest_var
            self.expr = expr
            self.static_type = dest_var.type
            super(SequenceExpr._ForwardExpr, self).__init__()

        def _render_pre(self):
            result = [self.expr.render_pre()]

            # If the destination variable comes from the sources, emit debug
            # info for it: the end of our inner expression is its definition
            # point.
            if (
                PropertyDef.get().has_debug_info and
                self.dest_var.abstract_var and
                self.dest_var.abstract_var.source_name
            ):
                result.append(gdb_helper(
                    'bind',
                    self.dest_var.abstract_var.source_name.lower,
                    self.dest_var.name.camel_with_underscores
                ))

            result.append(assign_var(self.dest_var, self.expr.render_expr()))
            return '\n'.join(result)

        def _render_expr(self):
            return self.dest_var.render_expr()

        @property
        def subexprs(self):
            return {'0-var': self.dest_var, '1-expr': self.expr}

        def __repr__(self):
            return '<ForwardExpr {}>'.format(self.dest_var)

    @classmethod
    def make_forward(cls, dest_var, pre_expr, post_expr, abstract_expr=None):
        """
        Create a sequence expression that:

          * evaluates `pre_expr`;
          * forward its value to `dest_var`;
          * evaluates `post_expr` and return its value.

        :param VariableExpr dest_var: Variable to forward `pre_expr` to.
        :param ResolvedExpression pre_expr: First expression to evaluate.
        :param ResolvedExpression post_expr: Second expression to evaluate.
        :rtype: SequenceExpr
        """
        assert pre_expr.type.matches(dest_var.type)

        return cls(cls._ForwardExpr(dest_var, pre_expr), post_expr,
                   abstract_expr=abstract_expr)


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

    unused_count = count(1)

    def __init__(self, name, type=None, create_local=False, source_name=None):
        """
        :param names.Name name: The name of the PlaceHolder variable.
        :param CompiledType type: The type of the variable. Optional for
            global abstract variables where you will use bind_type. Mandatory
            if create_local is True.
        :param bool create_local: Whether to create a corresponding local
            variable in the current property. If True, the variable is created
            scopeless.
        :param names.Name|None source_name: If this variables comes from the
            language specification, hold its original name.
        """
        super(AbstractVariable, self).__init__()

        # Kludge: in DynamicVariable and only there, name can be None
        if name is not None and name.lower == '_':
            i = next(self.unused_count)
            name = names.Name('Unused_{}'.format(i))

        self._type = type
        self.local_var = None
        self._name = name
        if create_local:
            self.create_local_variable()

        self.source_name = source_name

        self.construct_cache = {}
        """
        :type: dict[(str, CompiledType), VariableExpr]

        Cache used to memoize the "construct" method.
        """

        self._ignored = False
        """
        Whether this variable was explicitely ignored
        """

    def create_local_variable(self, scope=None):
        """
        Create a local variable to correspond to this variable reference.
        Update its name, if needed. This must not be called if a local variable
        was already created for `self`.

        :param LocalVars.Scope|None scope: If left to None, the variable is
            created scope-less. Otherwise, it is added to `scope`.
        """
        assert self.local_var is None

        self.local_var = PropertyDef.get().vars.create_scopeless(self._name,
                                                                 self._type)
        self._name = self.local_var.name
        if scope:
            scope.add(self.local_var)

    def add_to_scope(self, scope):
        """
        Add this already existing variable to `scope`.

        This is allowed iff this variable is not registered as a local variable
        yet. `type` must be None iff this variable is already typed.
        """
        self.create_local_variable()
        scope.add(self.local_var)

    def construct(self):
        typ = self.type
        key = (self._name, typ)
        try:
            expr = self.construct_cache[key]
        except KeyError:
            expr = VariableExpr(typ, self._name, abstract_var=self)
            self.construct_cache[key] = expr
        return expr

    @property
    def type(self):
        return resolve_type(self._type)

    def set_type(self, type):
        assert self._type is None, 'Variable type cannot be set twice'
        self._type = type
        if self.local_var:
            self.local_var.type = type

    @property
    def ignored(self):
        return self._ignored or self.source_name == names.Name.from_lower('_')

    def tag_ignored(self):
        self._ignored = True

    def __repr__(self):
        return "<Var {}>".format(self.source_name.lower
                                 if self.source_name else
                                 self._name.camel_with_underscores)


class DynamicVariable(AbstractVariable):
    """
    Reference to a dynamic property variable.
    """

    def __init__(self, name, type):
        """
        Create a dynamic variable.

        These are implemented as optional arguments in properties.

        :param str name: Lower-case name for this variable.
        :param CompiledType type: Variable type.
        """
        self.argument_name = names.Name.from_lower(name)
        super(DynamicVariable, self).__init__(None, type)

    def is_accepted_in(self, prop):
        """
        Return whether `self` is accepted as an optional argument in the given
        property.

        :param PropertyDef|None prop: Property to test. If None, this returns
            False.
        :rtype: bool
        """
        return prop is not None and any(self is dyn_var
                                        for dyn_var in prop.dynamic_vars)

    @contextmanager
    def _bind(self, name):
        """
        Bind this variable to the given name.

        :param names.Name name: The new name.
        """
        p = PropertyDef.get()
        saved = self._name

        self._name = name
        p.dynvar_binding_stack.append(self)
        yield
        self._name = saved
        assert p.dynvar_binding_stack.pop() is self

    @contextmanager
    def bind_default(self, prop):
        """
        Context manager to setup the default binding for this dynamic variable
        in `prop`.

        This means: no binding if this property does not accept `self` as an
        implicit argument, and the default one if it does.

        :type prop: PropertyDef
        """
        with self._bind(self.argument_name
                        if self.is_accepted_in(prop) else None):
            yield

    @property
    def is_bound(self):
        """
        Return whether this dynamic variable is bound.

        This returns true iff at least one of the following conditions is True:

          * the current property accepts this implicit argument;
          * it is currently bound, through the DynamicVariable.bind construct.

        :rtype: bool
        """
        return (self.is_accepted_in(PropertyDef.get())
                or self._name is not None)

    def construct(self):
        check_source_language(
            PropertyDef.get()._dynamic_vars is not None,
            'Dynamic variables cannot be bound in this context'
        )
        check_source_language(
            self.is_bound,
            '{} is not bound in this context: please use the .bind construct'
            ' to bind it first.'.format(
                self.argument_name.lower
            )
        )
        return super(DynamicVariable, self).construct()

    def __repr__(self):
        return '<DynamicVariable {}>'.format(self.argument_name.lower)

    @staticmethod
    def check_call_bindings(prop, context_msg):
        """
        Emit an error diagnostic of there is at least one dynamic variable in
        `prop` that is not currently bound. `context_msg` is used to format the
        error message.

        :param PropertyDef prop: Property "to call".
        :param str context_msg: String to describe how this property is used.
            This helps formatting the error message. It is formatted with
            "prop", being the name of the property. For instance:

                "In call to {prop}".
        """
        unbound_dynvars = [
            dynvar for dynvar in prop.dynamic_vars
            if not dynvar.is_bound
        ]
        check_source_language(
            not unbound_dynvars,
            '{}, some dynamic variables need to be bound: {}'.format(
                context_msg.format(prop=prop.qualname),
                ', '.join(dynvar.argument_name.lower
                          for dynvar in unbound_dynvars)
            )
        )


class DynamicVariableBindExpr(ComputingExpr):

    def __init__(self, dynvar, value_var, value, to_eval_expr,
                 abstract_expr=None):
        self.dynvar = dynvar
        self.value_var = value_var
        self.value = value
        self.to_eval_expr = to_eval_expr
        self.static_type = self.to_eval_expr.type

        super(DynamicVariableBindExpr, self).__init__(
            'Dyn_Var_Bind_Result',
            abstract_expr=abstract_expr
        )

    def _render_pre(self):
        return '\n'.join([
            # First, compute the value to bind
            self.value.render_pre(),
            assign_var(self.value_var.ref_expr, self.value.render_expr()),

            # Then we can compute the nested expression with the bound variable
            self.to_eval_expr.render_pre(),
            assign_var(self.result_var.ref_expr,
                       self.to_eval_expr.render_expr())
        ])

    @property
    def subexprs(self):
        return {'var': self.dynvar,
                'value': self.value,
                'expr': self.to_eval_expr}

    def __repr__(self):
        return '<DynamicVariableBindExpr>'


@auto_attr
def bind(self, dynvar, value, expr):
    """
    Bind `value` to the `dynvar` dynamic variable in order to evaluate `expr`.

    :param DynamicVariable dynvar: Dynamic variable to bind.
    :param AbstractExpression value: Value to bind.
    :param AbstractExpression expr: Expression to evaluate with the binding.
    :rtype: ResolvedExpression
    """
    check_source_language(
        isinstance(dynvar, DynamicVariable),
        '.bind must be called on dynamic variables only'
        ' (here, got: {})'.format(dynvar)
    )
    resolved_value = construct(value, dynvar.type)
    value_var = PropertyDef.get().vars.create(
        'Bound_{}'.format(dynvar.argument_name.camel_with_underscores),
        dynvar.type
    )
    with dynvar._bind(value_var.name):
        return DynamicVariableBindExpr(dynvar, value_var, resolved_value,
                                       construct(expr),
                                       abstract_expr=self)


@auto_attr
def can_reach(self, node, from_node):
    """
    Return whether `node` can reach `from_node` (two AST nodes), from a
    sequential viewpoint.  If elements are declared in different units, it will
    always return True, eg this does not handle general visibility issues, just
    sequentiality of declarations.
    """
    node_expr = construct(node, T.root_node)
    from_node_expr = construct(from_node, T.root_node)
    return CallExpr('Node_Can_Reach', 'Can_Reach', T.BoolType,
                    [node_expr, from_node_expr], abstract_expr=self)


class SelfVariable(AbstractVariable):

    _singleton = None

    def __init__(self):
        assert SelfVariable._singleton is None
        SelfVariable._singleton = self
        self._type = None
        super(SelfVariable, self).__init__(names.Name('Self'))

    @contextmanager
    def bind_type(self, type):
        """
        Bind the type of this variable.

        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of this placeholder.
        """
        _old_type = self._type
        _old_entity_type = Entity._type
        self._type = type
        Entity._type = self._type.entity
        yield
        self._type = _old_type
        Entity._type = _old_entity_type

    def construct(self):
        check_source_language(self._type is not None,
                              'Self is not bound in this context')
        return super(SelfVariable, self).construct()


Self = SelfVariable()


class EntityVariable(AbstractVariable):
    _singleton = None

    def __init__(self):
        assert EntityVariable._singleton is None
        EntityVariable._singleton = self
        super(EntityVariable, self).__init__(names.Name('Ent'))

    def construct(self):
        PropertyDef.get().set_uses_entity_info()
        PropertyDef.get()._has_self_entity = True
        return super(EntityVariable, self).construct()

Entity = EntityVariable()


@attr_expr('symbol')
class GetSymbol(AbstractExpression):
    """
    Return the symbol associated to `node`. `token` must be an AST node that is
    a token node.
    """

    def __init__(self, node):
        super(GetSymbol, self).__init__()
        self.node_expr = node

    def construct(self):
        node = construct(self.node_expr)
        if node.type.is_entity_type:
            node = FieldAccessExpr(node, 'Node', node.type.astnode,
                                   do_explicit_incref=False)
        check_source_language(
            node.type.is_ast_node,
            'Token node expected, but got instead {}'
            .format(node.type.dsl_name)
        )
        check_source_language(
            node.type.is_token_node,
            'Token node expected, but the input {} node is not a token node'
            .format(node.type.dsl_name)
        )
        return self.construct_static(node, abstract_expr=self)

    @staticmethod
    def construct_static(node_expr, abstract_expr=None):
        return CallExpr('Sym', 'Get_Symbol', T.SymbolType, [node_expr],
                        abstract_expr=abstract_expr)

    def __repr__(self):
        return '<GetSymbol>'


class SymbolLiteral(AbstractExpression):
    """
    Abstract expression that returns a symbol from a string literal.
    """

    class Expr(ComputingExpr):

        def __init__(self, name, abstract_expr=None):
            self.static_type = T.SymbolType
            self.name = name
            get_context().add_symbol_literal(self.name)

            super(SymbolLiteral.Expr, self).__init__(
                'Sym', abstract_expr=abstract_expr
            )

        def _render_pre(self):
            return assign_var(
                self.result_var,
                'Self.Unit.Context.Symbol_Literals ({})'.format(
                    get_context().symbol_literals[self.name]
                )
            )

        @property
        def subexprs(self):
            return {'name': self.name}

    def __init__(self, name):
        """
        :type name: str
        """
        super(SymbolLiteral, self).__init__()
        self.name = name

    def construct(self):
        return self.Expr(self.name, abstract_expr=self)

    def __repr__(self):
        return '<Symbol {}>'.format(self.name)


class BindingScope(ComputingExpr):
    """
    Resolved expression that materializes new bindings.

    This resolved expression is just an annotation: it is useless from a code
    generation point of view. It makes it possible to describe the creation of
    new bindings for some scope.
    """

    def __init__(self, expr, bindings, scope=None, abstract_expr=None):
        """
        :type expr: ResolvedExpression
        :type bindings: list[VariableExpr]
        :type abstract_expr: None|AbstractExpression

        :param LocalVars.Scope|None scope: If provided, this BindingScope
            instance will materialize scope entry/finalization in the generated
            code.
        """
        self.expr = expr
        self.expr_bindings = bindings
        self.static_type = self.expr.type
        self.scope = scope

        # Create a local variable that belong to the outer scope so that at
        # finalization time, our result is still live.
        super(BindingScope, self).__init__('Scope_Result',
                                           abstract_expr=abstract_expr)

    def _render_pre(self):
        return render('properties/binding_scope', expr=self)

    @property
    def subexprs(self):
        return {'0-bindings': self.expr_bindings,
                '1-expr': self.expr}

    def _bindings(self):
        return self.expr_bindings

    def __repr__(self):
        return '<BindingScope ({}): {}>'.format(
            ', '.join(repr(b) for b in self.expr_bindings),
            repr(self.expr))


@dsl_document
class Let(AbstractExpression):
    """
    Define bindings in order to evaluate an expression.

    `lambda_fn` is a lambda function that takes one argument per binding. Each
    argument must have a default value that is the expression to compute the
    value associated to the binding. The body for this lambda function is the
    expression to evaluate with the bindings and computes the result of this
    Let expression.

    This is similar to the ``let ... in ...`` constructs in traditional
    functional languages. For instance::

        Let(lambda collection=node.some_property:
            If(collection.length > 0,
               collection.at(0),
               node))
    """

    class Expr(ComputingExpr):
        pretty_class_name = 'Let'

        def __init__(self, vars, var_exprs, expr, abstract_expr=None):
            """
            :type vars: list[VariableExpr]
            :type vars_exprs: list[ResolvedExpression]
            :type expr: ResolvedExpression
            """
            self.vars = vars
            self.var_exprs = var_exprs
            self.expr = expr
            self.static_type = self.expr.type

            # This expression does not create itself the result value: expr
            # does. Hence, relying on expr's result variable to make sure there
            # is no ref-counting issue is fine.
            super(Let.Expr, self).__init__('Let_Result',
                                           abstract_expr=abstract_expr)

        def _render_pre(self):
            prop = PropertyDef.get()
            debug_info = prop.has_debug_info

            # Start and end a debug info scope around the whole expression so
            # that the bindings we create in this Let expression die when
            # leaving its evaluation in a debugger.
            result = []
            if debug_info:
                result.append(gdb_helper('scope-start'))

            for var, expr in zip(self.vars, self.var_exprs):
                result.extend([expr.render_pre(),
                               assign_var(var, expr.render_expr())])
                if debug_info:
                    result.append(gdb_bind_var(var))

            result.extend([
                self.expr.render_pre(),
                assign_var(self.result_var.ref_expr, self.expr.render_expr())
            ])

            if debug_info:
                result.append(gdb_helper('end'))
            return '\n'.join(result)

        @property
        def subexprs(self):
            return {'vars': {v.name: e
                             for v, e in zip(self.vars, self.var_exprs)},
                    'expr': self.expr}

        def _bindings(self):
            return self.vars

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
            AbstractVariable(names.Name.from_lower(arg), create_local=True,
                             source_name=names.Name.from_lower(arg))
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

        return Let.Expr(vars, var_exprs, construct(self.expr),
                        abstract_expr=self)

    def __repr__(self):
        return '<Let {}>'.format(', '.join(self.var_names))


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

    def __repr__(self):
        return '<Block>'


class Var(AbstractVariable):
    """
    Var allows you to declare local variable bound to expressions in the body
    of Properties, when those are defined through a function. See Block's
    documentation for more details.
    """

    def __init__(self, expr):
        super(Var, self).__init__(names.Name("Block_Var"), create_local=True)

        # For debug purposes, preserve a link to the block that contains this
        # variable. We can't store the block itself as an attribute or we'll
        # get an infinite recursion in AbstractExpression.explore because of
        # the reference loop between this variable and the block.
        block = Block.get()
        self.get_block = lambda: block

        block.add_var(self, expr)

        # TODO: the following is a hack, that will likely only disappear when
        # we'll move the DSL from Python to a true DSL.
        #
        # The source name of block variable is available only as the name of
        # the local variable that will hold this instance in the caller's stack
        # frame. So for now, keep this stack frame in memory so we can find
        # which local variable holds it at prepare time.
        stack = inspect.getouterframes(inspect.currentframe())
        self._creator_stack_frame = (stack[1][0]
                                     if stack and len(stack) > 1 else None)

        # Break the reference loop for this stack frame
        del stack

    def do_prepare(self):
        super(Var, self).do_prepare()

        # If the information is available, find the source name for this
        # variable from the creator's stack frame.
        if self._creator_stack_frame:
            local_names = set(name for name, value
                              in self._creator_stack_frame.f_locals.iteritems()
                              if value is self)

            # If we have multiple local variables that point to self, take the
            # first one in sorted order to keep our output stable across runs.
            if local_names:
                self.source_name = names.Name.from_lower(
                    sorted(local_names)[0]
                )

            # Let the frame object be reclaimed
            self._creator_stack_frame = None


@dsl_document
class ArrayLiteral(AbstractExpression):
    """
    Return an array literal that contains `elements`, a list of expressions for
    array components.

    If `element_type` is provided, the type of all components is checked
    against it, otherwise it is inferred from sub-expressions. Because of this,
    `element_type` is mandatory when `elements` is empty.
    """

    def __init__(self, elements=[], element_type=None):
        super(ArrayLiteral, self).__init__()
        self.element_type = element_type
        self.array_type = None
        self.elements = list(elements)

    @staticmethod
    def construct_static(elements, array_type, abstract_expr=None):
        if len(elements) == 0:
            return CallExpr('Array_Lit', 'Create', array_type,
                            ['Items_Count => 0'],
                            abstract_expr=abstract_expr)
        else:
            return CallExpr(
                'Array_Lit', 'Create', array_type,
                [aggregate_expr(str(array_type.array_type_name), [
                    (i, el) for i, el in enumerate(elements, 1)
                ])],
                abstract_expr=abstract_expr
            )

    def construct(self):
        resolved_elements = []
        if self.elements:
            resolved_elements = [construct(el) for el in self.elements]
            for el in resolved_elements:
                if self.element_type is None:
                    self.element_type = el.static_type
                else:
                    check_source_language(
                        self.element_type == el.static_type,
                        'In Array literal, expected element of type {},'
                        ' got {}'.format(self.element_type.dsl_name,
                                         el.static_type.dsl_name)
                    )
        else:
            check_source_language(
                self.element_type is not None,
                'Missing element type for empty array literal'
            )

        self.array_type = resolve_type(self.element_type).array

        return self.construct_static(
            resolved_elements, self.array_type, abstract_expr=self
        )

    def __repr__(self):
        return '<ArrayLiteral>'


def gdb_property_start(prop):
    if prop.is_dispatcher:
        return gdb_helper('property-start', prop.debug_name, 'dispatcher')
    else:
        return gdb_helper('property-start', prop.debug_name,
                          '{}:{}'.format(prop.location.file,
                                         prop.location.line))


def gdb_property_body_start():
    return gdb_helper('property-body-start')


def gdb_memoization_lookup():
    return gdb_helper('memoization-lookup')


def gdb_memoization_return():
    return gdb_helper('memoization-return')


def gdb_scope_start():
    return gdb_helper('scope-start')


def gdb_property_call_start(prop):
    return gdb_helper('property-call-start', prop.debug_name)


def gdb_end():
    return gdb_helper('end')


def gdb_bind(dsl_name, var_name):
    return gdb_helper('bind', dsl_name, var_name)


def gdb_bind_var(var):
    """
    Output a GDB helper directive to bind a variable. This does nothing if the
    variable has no source name.

    :param LocalVars.LocalVar|VariableExpr var: The variable to bind.
    :rtype: str
    """
    gen_name = var.name
    if isinstance(var, VariableExpr):
        var = var.abstract_var
    else:
        assert isinstance(var, AbstractVariable)

    if not (var and var.source_name):
        return ''

    return gdb_bind(var.source_name.lower,
                    gen_name.camel_with_underscores)


def render(*args, **kwargs):
    return ct_render(
        *args,
        property=PropertyDef.get(),
        Self=Self,
        assign_var=assign_var,
        gdb_property_start=gdb_property_start,
        gdb_property_body_start=gdb_property_body_start,
        gdb_memoization_lookup=gdb_memoization_lookup,
        gdb_memoization_return=gdb_memoization_return,
        gdb_scope_start=gdb_scope_start,
        gdb_end=gdb_end,
        gdb_bind=gdb_bind,
        gdb_bind_var=gdb_bind_var,
        **kwargs
    )


inherited_information = inherited_property(lambda s: s.base_property)


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
    env_arg_name = names.Name('Bound_Env')
    env_rebinding_name = names.Name('Envs_Rebindings')

    # Collections for these
    reserved_arg_names = (self_arg_name, env_arg_name)
    reserved_arg_lower_names = [n.lower for n in reserved_arg_names]

    def __init__(self, expr, prefix, name=None, doc=None, public=None,
                 abstract=False, type=None, abstract_runtime_check=False,
                 dynamic_vars=None, memoized=False, call_memoizable=False,
                 memoize_in_populate=False, external=False,
                 uses_entity_info=None, uses_envs=None,
                 optional_entity_info=False, warn_on_unused=True,
                 ignore_warn_on_node=None,
                 call_non_memoizable_because=None):
        """
        :param expr: The expression for the property. It can be either:
            * An expression.
            * A function that takes one or more arguments with default values
              which are CompiledType instances. This is the way one can write
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
        :param bool|None public: See AbstractNodeData's constructor.
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
            that is not implemented by all subclasses.

            In the absence of interface types in Langkit, this is helpful to
            develop features faster, because first you don't have to make every
            implementation at once, and second you don't have to find a typing
            scheme with current langkit capabilities in which the parser
            generate the right types for the functionality you want.

        :param None|list[DynamicVariable] dynamic_vars: List of dynamically
            bound variables for this property. If left to None, inherit from
            the overriden property, or the empty list if these is no property
            to override. Just like `public`, it must always be consistent with
            base classes.

        :param bool memoized: Whether this property must be memoized. Disabled
            by default.

        :param bool call_memoizable: If true, allow memoization for this
            property or its callers even when it is unsafe to do so, for
            instance when using equation resolution constructs, which are
            memoization-unfriendly as they use side-effects. This should be
            used when the side is contained inside the call to this property.

        :param bool memoize_in_populate: Whether to memoize the property during
            the populate lexical environment pass. It is disabled by default as
            the hash of lexical environments changes during this pass.

        :param bool external: Whether this property's implementation is
            provided by the language specification. If true, `expr` must be
            None and the implementation must be provided in the
            extensions/nodes/{node_name}/bodies extension file. Note that the
            engines always generate the public declaration part.

        :param bool uses_entity_info: Whether this property requires entity
            information to be passed for Self. If left to None, this will be
            computed using the properties call graph. If false, uses of entity
            info will be rejected. Note that this must be non-None for external
            property, as they escape call graph analysis.

        :param bool|None uses_envs: Whether this property makes a lexical
            environment lookup, or calls a property that does one
            (transitively). If left to None, this will be computed using the
            properties call graph. If false, lookups will be rejected. Note
            that this must be non-None for external property, as they escape
            call graph analysis.

        :param bool optional_entity_info: If `uses_entity_info` is True,
            whether the entity info is optional. This allows properties to be
            called on 1) bare AST nodes, in which case the default entity info
            is passed, and 2) on entities, in which case the entity info from
            the prefix is passed.

        :param bool warn_on_unused: Wether to warn on unused or not. Defaults
            to True.

        :param bool|None ignore_warn_on_node: Wether to ignore warn_on_node
            warnings for this property. Defaults to None, which means inherit.

        :param str|None call_non_memoizable_because: If not-None, makes the use
            of this property in a memoized context impossible. Must be used for
            external properties that do side effects (such as loading an
            analysis unit), as this conflicts with the memoization machinery.
        """

        self.prefix = prefix

        super(PropertyDef, self).__init__(name=name, public=public)

        self._original_is_public = None
        """
        Original privacy for this property. Can be different from `is_public`
        after properties expansion. Computed right after `is_public` itself in
        the `prepare` pass.

        :type: bool
        """

        self.is_dispatcher = False
        """
        Whether this property is just a wrapper that, based on the kind of
        Self, dispatches to specific properties.

        :type: bool
        """

        self.in_type = False
        "Recursion guard for the construct pass"

        self.logic_predicates = []
        """
        The list of logic predicates to generate. First element of the tuple is
        a list of the args types, second is the unique identifier for this
        predicate.

        :type: [([CompiledType], str)]
        """

        self.expr = expr
        ":type: AbstractExpression"

        self.constructed_expr = None

        self.vars = LocalVars()
        ":type: LocalVars"

        self.expected_type = type

        self.abstract = abstract
        """
        Whether this property is declared as abstract in the DSL.

        Note that this being true does not imply that the function that
        implements this property in the generated code is abstract: if
        `abstract_runtime_check` is True, the function will be concrete (and
        will just raise an exception).

        :type: bool
        """

        self.abstract_runtime_check = abstract_runtime_check
        """
        Assuming this property is abstract, whether AST node concrete subclass
        are allowed not to override it. If true, this means that we will
        generate a concrete function to implement this property, and this
        function will just raise a runtime error when called.

        :type: bool
        """

        assert not self.abstract_runtime_check or self.abstract

        self._dynamic_vars = dynamic_vars

        self.overriding_properties = set()
        """
        Set of properties that override "self".

        This is inferred during the "compute" pass.
        :type: set[PropertyDef]|None
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

        self.memoized = memoized
        self.call_memoizable = call_memoizable
        self.memoize_in_populate = memoize_in_populate

        self.external = external

        self._uses_entity_info = uses_entity_info
        self._uses_envs = uses_envs

        self.optional_entity_info = optional_entity_info

        self._requires_untyped_wrapper = False
        self.warn_on_unused = warn_on_unused
        self._ignore_warn_on_node = ignore_warn_on_node

        self._call_non_memoizable_because = call_non_memoizable_because

        self.dynvar_binding_stack = []
        """
        Stack of dynamic variable bindings. This is used to determine the set
        of dynamic variables to reset when recursing on the construction of
        properties.

        :type: list[DynamicVariable]
        """

        self._solves_equation = False
        """
        Whether this property uses the ".solve" operation on a logic equation.
        """

        self._gets_logic_var_value = False
        """
        Whether this property uses the ".get_value" operation on a logic
        variable.
        """

    @property
    def has_debug_info(self):
        """
        Return whether we should emit debug information for this property.

        :rtype: bool
        """
        return self.location is not None

    @property
    def debug_name(self):
        """
        Return the name for this property to use in debug info.
        """
        return ('[dispatcher]{}'.format(self.qualname)
                if self.is_dispatcher else self.qualname)

    @inherited_information
    def ignore_warn_on_node(self):
        return self._ignore_warn_on_node

    @property
    @self_memoized
    def all_overriding_properties(self):
        """
        Return self's overriding properties and all their own overriding ones,
        recursively.

        :rtype: list[PropertyDef]
        """
        def helper(prop, except_self=False):
            return sum((helper(p) for p in prop.overriding_properties),
                       [] if except_self else [prop])
        return helper(self, except_self=True)

    def property_set(self):
        """
        Return all properties associated with this property in terms of
        overriding hierarchy.

        :rtype: list[PropertyDef]
        """
        return (
            self.base_property.property_set()
            if self.base_property else [self] + self.all_overriding_properties
        )

    @property
    def overriding(self):
        """
        Whether this property is overriding or not.

        This the information is inferred during the compute phase.

        :rtype: bool
        """
        return self.base_property is not None

    @property
    def dispatching(self):
        """
        Whether this property is dispatching or not.  This is True as soon as
        the property is abstract or the property is overriden in AST node
        subclasses or the property overrides another one.

        This is inferred during the "compute" pass.

        :rtype: bool
        """
        return (self.abstract
                or self.base_property
                or self.overriding_properties)

    @property
    def uid(self):
        """
        Returns a string that uniquely identifies this property.

        :rtype: str
        """
        return str(self._serial)

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
    def bind(self, bind_dynamic_vars=False):
        """
        Bind the current property to `self`, so that it is accessible in the
        expression templates.

        :param bool bind_dynamic_vars: Whether to bind dynamic variables.
        """
        previous_property = self.get()
        self.__current_properties__.append(self)

        context_managers = []

        # Reset bindings for dynamically bound variables so that they don't
        # leak through this property.  Also provide default bindings for self's
        # dynamically bound variables.
        if bind_dynamic_vars:
            to_reset = ([] if previous_property is None else
                        previous_property.dynvar_binding_stack)
            context_managers.extend(dynvar.bind_default(self)
                                    for dynvar in to_reset + self.dynamic_vars)

        with nested(*context_managers):
            yield

        self.__current_properties__.pop()

    @classmethod
    @contextmanager
    def bind_none(cls):
        """
        Unbind `self`, so that compilation no longer see the current property.

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

        # If the expr has not yet been constructed, try to construct it
        if not self.constructed_expr:
            with self.diagnostic_context:
                self.construct_and_type_expression(get_context())

        return resolve_type(self.constructed_expr.type)

    def _add_argument(self, name, type, is_artificial=False,
                      abstract_var=None):
        """
        Helper to add an argument to this property.

        This basically just fills the .arguments list. See Argument's
        constructor for parameters documentation.
        """
        self.arguments.append(Argument(name, type, is_artificial=is_artificial,
                                       abstract_var=abstract_var))

    @property
    @self_memoized
    def base_property(self):
        """
        Get the base property for this property, if it exists.

        :rtype: PropertyDef|None
        """
        if self.struct.is_ast_node and self.struct.base:
            result = self.struct.base.get_abstract_fields_dict(
                field_class=PropertyDef
            ).get(self._name.lower, None)

            if result:
                check_source_language(
                    not self.abstract or self.abstract_runtime_check,
                    'Abstract properties with no runtime check cannot'
                    ' override another property. Here, {} is abstract and'
                    ' overrides {}.'.format(
                        self.qualname, result.qualname
                    )
                )
            return result

        return None

    @property
    @self_memoized
    def root_property(self):
        """
        Return the ultimate base property for "self", or "self" is it has no
        base property.

        :rtype: PropertyDef
        """
        result = self
        while result.base_property:
            result = result.base_property
        return result

    def reset_inheritance_info(self):
        """
        Reset memoization caches inheritance-related information.

        Must be called when modifying a tree of inherited properties.
        """
        for prop in (PropertyDef.base_property, PropertyDef.root_property,
                     PropertyDef.all_overriding_properties):
            prop.fget.reset(self)
        self.overriding_properties = set()

    @property
    def dynamic_vars(self):
        """
        Return the list of dynamically bound variables for this property.

        :rtype: list[DynamicVariable]
        """
        assert self._dynamic_vars is not None
        return self._dynamic_vars

    def prepare_abstract_expression(self, context):
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

        :type context: langkit.compile_context.CompileCtx
        :rtype: None
        """

        # TODO: We could at a later stage add a check to see that the abstract
        # property definition doesn't override another property definition on a
        # base class.

        # If the expected type is not a CompiledType, then it's a Defer.
        # Resolve it.
        self.expected_type = resolve_type(self.expected_type)

        if not self.expr:
            return

        # If the user passed a lambda or function for the expression,
        # now is the moment to transform it into an abstract expression by
        # calling it.
        if (not isinstance(self.expr, AbstractExpression)
                and callable(self.expr)):
            with self.bind():
                fn_arguments, fn_expr = expand_abstract_fn(self.expr)
                check_source_language(
                    fn_expr is not None or self.external or self.abstract,
                    'Unless a property is external or abstract, it must'
                    ' have an expression'
                )
                self.expr = fn_expr
                for arg in fn_arguments:
                    self.arguments.append(arg)

        elif not(callable(self.expr)):
            self.expr = unsugar(self.expr)

        if self.expr:
            with self.bind():
                self.expr = self.expr.prepare() or self.expr

    def freeze_abstract_expression(self, context):
        """
        Run the "freeze" pass on the expression associated to this property.

        Afterwards, it will not be possible anymore to build
        AbstractExpressions trees out of the overloaded operators of the
        AbstractExpression instances in self.expr. See Frozable for more
        details.

        :type context: langkit.compile_context.CompileCtx
        """
        if self.expr:
            self.expr.freeze()

    def compute_property_attributes(self, context):
        """
        Compute various property attributes, notably:
        * Information related to dispatching for properties.
        * Inheritance based information generally, like inheriting return
          type or privacy, consistency of annotations between base property
          and inherited properties.
        * Property overriding completeness checking.

        :type context: langkit.compile_context.CompileCtx
        """
        type_set = TypeSet()

        def check_overriding_props(node):
            """
            Recursive helper. Checks wether klass and its subclasses override
            self.

            :param langkit.compiled_types.ASTNodeType node: The AST node to
                check.
            """
            for subclass in node.subclasses:
                for prop in subclass.get_properties(include_inherited=False):
                    if prop._name == self._name:
                        type_set.include(subclass)
                check_overriding_props(subclass)

        if self.abstract and not self.abstract_runtime_check:
            check_overriding_props(assert_type(self.struct, ASTNodeType))

            # Don't consider abstract types for unmatched types as it's safe to
            # have a propert that is left abstract on an abstract AST node.
            unmatched_types = sorted(
                [t for t in type_set.unmatched_types(self.struct)
                 if not t.abstract],
                key=lambda cls: cls.hierarchical_name
            )

            check_source_language(
                not unmatched_types,
                'Abstract property {} is not overriden in all subclasses.'
                ' Missing overriding properties on classes: {}'.format(
                    self.name.lower, ", ".join([t.dsl_name for t in
                                                unmatched_types])
                ),
                severity=Severity.non_blocking_error
            )

        if self.base_property:
            # If we have a base property, then this property is dispatching and
            # overriding, and the base property is dispatching (This
            # information can be missing at this stage for non abstract base
            # properties).
            self.base_property.overriding_properties.add(self)

            # Inherit the privacy level or check that it's consistent with the
            # base property.
            if self._is_public is None:
                self._is_public = self.base_property.is_public
            else:
                check_source_language(
                    self._is_public == self.base_property.is_public,
                    "{} is {}, so should be {}".format(
                        self.base_property.qualname,
                        ('public'
                            if self.base_property.is_public else
                            'private'),
                        self.qualname,
                    )
                )

            # Likewise for dynamically bound variables
            self_dynvars = self._dynamic_vars
            base_dynvars = self.base_property.dynamic_vars
            if self_dynvars is not None:
                # Don't use the equality operator on DynamicVariable, as it
                # returns a new AbstractExpression.
                check_source_language(
                    len(self_dynvars) == len(base_dynvars)
                    and all(sd is bd
                            for sd, bd in zip(self_dynvars, base_dynvars)),
                    'Requested set of dynamically bound variables is not'
                    ' consistent with the property to override: {}'.format(
                        self.base_property.qualname
                    )
                )
            self._dynamic_vars = base_dynvars

            # We then want to check the consistency of type annotations if they
            # exist.
            if self.base_property.expected_type:
                if self.expected_type:
                    check_source_language(
                        self.expected_type.matches(
                            self.base_property.expected_type),
                        '{} returns {} whereas it overrides {}, which returns'
                        ' {}. The former should match the latter.'.format(
                            self.qualname,
                            self.expected_type.dsl_name,
                            self.base_property.qualname,
                            self.base_property.type.dsl_name
                        )
                    )
                else:
                    # If base has a type annotation and not self, then
                    # propagate it.
                    self.expected_type = self.base_property.expected_type

            args = self.natural_arguments
            base_args = self.base_property.natural_arguments
            check_source_language(
                len(args) == len(base_args),
                "Derived and base properties don't have the same number"
                " of arguments, base has {}, derived has {}".format(
                    len(base_args), len(args)
                )
            )

            for i, (arg, base_arg) in enumerate(zip(args, base_args)):
                # Check that argument names and types are consistent with the
                # base property.
                check_source_language(
                    arg.name == base_arg.name,
                    'Argument #{} does not have the same name here ({}) as in'
                    ' base property ({})'.format(
                        i + 1, arg.name.lower, base_arg.name.lower
                    )
                )
                check_source_language(
                    arg.var.type == base_arg.var.type,
                    'Argument "{}" does not have the same type as in base'
                    ' property. Base has {}, derived has {}'.format(
                        arg.name,
                        arg.var.type.dsl_name,
                        base_arg.var.type.dsl_name
                    )
                )

                # First check that the presence of a default argument value is
                # consistent with the base property.
                if arg.default_value is None:
                    check_source_language(
                        base_arg.default_value is None,
                        'Argument "{}" must have the same default value as in'
                        ' base property ({})'.format(
                            arg.name, self.base_property.qualname
                        )
                    )
                else:
                    check_source_language(
                        base_arg.default_value is not None,
                        'Argument "{}" cannot have a default value, to be'
                        ' consistent with its base property ({})'.format(
                            arg.name, self.base_property.qualname
                        )
                    )

                # Then check that if there is a default value, it is the same
                if arg.default_value is not None:
                    val = construct(arg.default_value)
                    base_val = construct(base_arg.default_value)
                    check_source_language(
                        val.ir_dump == base_val.ir_dump,
                        'Argument "{}" does not have the same default value'
                        ' ({}) as in base property ({})'.format(
                            arg.name, arg.default_value, base_arg.default_value
                        )
                    )

        else:
            # By default, properties are private and they have no dynamically
            # bound variable.
            self._is_public = bool(self._is_public)
            if self._dynamic_vars is None:
                self._dynamic_vars = []

        self._original_is_public = self.is_public

        if self.external:
            check_source_language(
                self.expr is None,
                'An external property cannot have a DSL implementation'
            )
            check_source_language(
                not self.abstract,
                'An external property cannot be abstract'
            )

            check_source_language(
                self._uses_entity_info is not None,
                'uses_entity_info is required for external properties'
            )
            check_source_language(
                self._uses_envs is not None,
                'uses_envs is required for external properties'
            )
        else:
            check_source_language(
                self._uses_entity_info in (None, False),
                'Cannot specify uses_entity_info=True for internal'
                ' properties'
            )
            check_source_language(
                self._uses_envs is None,
                'Cannot explicitely pass uses_envs for internal'
                ' properties'
            )

        # Add dynamically bound variables as arguments
        check_source_language(
            self.is_private or not self._dynamic_vars,
            'A public property cannot have dynamically bound variables'
        )
        self.build_dynamic_var_arguments()

        # At this point, we assume the list of argument has reached its final
        # state.

        if self.base_property:
            args = len(self.arguments)
            base_args = len(self.base_property.arguments)
            assert args == base_args, (
                '{} has {} arguments, whereas its base property {} has {}'
                ' ones'.format(self.qualname, args,
                               self.base_property.qualname, base_args)
            )

    @property
    def original_is_public(self):
        return self._original_is_public

    def build_dynamic_var_arguments(self):
        """
        Append arguments for each dynamic variable in this property.
        """
        for dynvar in self._dynamic_vars:
            self._add_argument(dynvar.argument_name, dynvar.type,
                               is_artificial=True, abstract_var=dynvar)

    @property
    @memoized
    def entity_info_arg(self):
        """
        Return an abstract expression to yield the entity information passed as
        argument.

        :rtype: AbstractExpression
        """
        assert self._uses_entity_info
        return AbstractVariable(self.entity_info_name, T.entity_info,
                                source_name=self.entity_info_name)

    def set_uses_entity_info(self):
        """
        Set this property as using entity information for Self.

        This triggers the addition of an implicit parameter (Entity_Info).
        """
        check_source_language(
            self._uses_entity_info is not False,
            'Cannot use entity info, as explicitely forbidden'
        )
        self._uses_entity_info = True

    @property
    def uses_envs(self):
        """
        Return whether the proper evaluation of this property requires
        Populate_Lexical_Env to be called.

        :rtype: bool
        """
        assert self._uses_envs is not None
        return self._uses_envs

    def set_uses_envs(self):
        """
        Set this property as using lexical environment lookups.

        If this property is public, this will trigger an automatical call to
        Populate_Lexical_Env.
        """
        check_source_language(
            self._uses_envs is not False,
            'Cannot use lexical environments, as explicitely forbidden'
        )
        self._uses_envs = True

    def require_untyped_wrapper(self):
        """
        Tag this property as requiring an untyped wrapper function. This
        wrapper is a function that takes Self and an Entity_Info record as
        parameters and that return an entity.
        """
        self._requires_untyped_wrapper = True

    @property
    def requires_untyped_wrapper(self):
        return self._requires_untyped_wrapper

    @property
    def untyped_wrapper_rtype(self):
        """
        Assuming this property requires an untyped wrapper, return the return
        type of this wrapper.
        """
        assert self.requires_untyped_wrapper
        if self.type.is_entity_type:
            return T.entity
        elif self.type.is_ast_node:
            return T.root_node
        else:
            return self.type

    def construct_and_type_expression(self, context):
        """
        This pass will construct the resolved expression from the abstract
        expression, and get type information at the same time.

        :type context: langkit.compile_context.CompileCtx
        """
        # If expr has already been constructed, return
        if self.constructed_expr:
            return

        check_source_language(
            not self.in_type,
            'Recursion loop in type inference for property {}. Try to '
            'specify its return type explicitly.'.format(self.qualname)
        )

        # If we don't have an expression, this has to be an abstract/external
        # property. In this case, try to get the type from the base property.
        if self.expr is None:
            assert self.abstract or self.external
            if not self.expected_type:
                check_source_language(
                    self.base_property,
                    'This property requires an explicit return type'
                )
                self.expected_type = self.base_property.type
            return

        with self.bind(bind_dynamic_vars=True), Self.bind_type(self.struct):
            message = (
                'expected type {{expected}}, got'
                ' {{expr_type}} instead (expected type comes from'
                ' overridden base property in {base_prop})'.format(
                    base_prop=self.base_property.struct.dsl_name
                )
            ) if self.base_property else None

            self.in_type = True
            try:
                self.constructed_expr = construct(self.expr,
                                                  self.expected_type,
                                                  message)
            finally:
                self.in_type = False

        # Make sure that all the created local variables are associated to a
        # scope.
        self.vars.check_scopes()

        # Warn on unused bindings
        self.warn_on_unused_bindings()

    def check_overriding_types(self, context):
        """
        Check that the return type of this property and the return type of the
        base property that self overrides are the same, if applicable.
        """
        if self.base_property and self.base_property.type:
            check_source_language(
                self.type.matches(self.base_property.type),
                "{} returns {} whereas it overrides {}, which returns {}."
                " The former should match the latter.".format(
                    self.qualname, self.type.dsl_name,
                    self.base_property.qualname,
                    self.base_property.type.dsl_name
                )
            )

    def check_return_types(self, context):
        if (self.struct.annotations.warn_on_node
                and not self.ignore_warn_on_node):
            WarningSet.prop_only_entities.warn_if(
                self.type.matches(T.root_node),
                '{} returns a node type'.format(self.qualname),
            )

    def render_property(self, context):
        """
        Render the given property to generated code.

        :type context: langkit.compile_context.CompileCtx
        :rtype: basestring
        """
        with self.bind(), Self.bind_type(self.struct):
            with names.camel_with_underscores:
                self.prop_decl = render('properties/decl_ada')
                self.prop_def = render('properties/def_ada')

                if self.requires_untyped_wrapper:
                    self.untyped_wrapper_decl = render(
                        'properties/untyped_wrapper_decl_ada'
                    )
                    self.untyped_wrapper_def = render(
                        'properties/untyped_wrapper_def_ada'
                    )
                else:
                    self.untyped_wrapper_decl = self.untyped_wrapper_def = ''

    @property
    def doc(self):
        return self._doc

    @property
    def natural_arguments(self):
        non_art, art = funcy.split_by(lambda a: not a.is_artificial,
                                      self.arguments)
        assert all(a.is_artificial for a in art), (
            'All artificial arguments must come after all the other ones'
        )
        return non_art

    @memoized
    def do_generate_logic_predicate(self, partial_args_types,
                                    default_passed_args):
        """
        Helper method, will trigger the emission of a logic predicate object
        for the property for the given partial argument types.

        :param [CompiledType] partial_args_types: The type of partially applied
            arguments passed to the logic predicate.
        :param int default_passed_args: Number of arguments passed by default
            value.

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
        pred_id = "{}_{}_{}".format(self.struct.name, self.name, pred_num)

        # We can use a list because the method is memoized, eg. this won't
        # be executed twice for the same partial_args_types tuple.
        self.logic_predicates.append((partial_args_types,
                                      default_passed_args,
                                      pred_id))

        return pred_id

    def get_concrete_node_types(self, partial_args_types, default_passed_args):
        """
        Helper for emission of logic predicate wrappers. Given partial
        argument types for trailing arguments that do not correspond to logic
        variables bound by the predicate, this helper will return the
        concrete node type for leading arguments that correspond to logic
        variables bound by the predicate.

        :param [CompiledType] partial_args_types: The type of partially applied
            arguments passed to the logic predicate.
        :param int default_passed_args: Number of arguments passed by default
            value.
        """
        logic_vars = (len(self.arguments) -
                      len(partial_args_types) -
                      default_passed_args)
        return [self.struct] + [a.type for a in self.arguments[:logic_vars]]

    @property
    def memoization_enum(self):
        """
        Return the enumerator name to materialize references to this property
        in the memoization engine.

        :rtype: str
        """
        return (names.Name('Mmz') +
                self.struct.name +
                self.name).camel_with_underscores

    @property
    def reason_for_no_memoization(self):
        """
        Return whether this property is a valid candidate for memoization.

        If it is memoizable, return None, otherwise return a message that
        describes why it is not memoizable.

        This predicate ignores callgraph considerations and focuses on
        characteristics specific to `self`: whether it contains side-effects
        (equation solving), whether it is external, or abstract. The
        `CompileCtx.check_memoized` pass will take care of doing call-graph
        analysis on top of this.

        :rtype: None|str
        """
        if self.abstract:
            return ('A memoized property cannot be abstract: memoization is'
                    ' not an inherited behavior')

        if self.external:
            return 'An external property cannot be memoized'

        return None

    @property
    def transitive_reason_for_no_memoization(self):
        """
        Determine if there is a reason that this property cannot be memoized.
        If so, this reason is considered to be transitive and will propagate to
        properties that all this one.

        If there is no such reason (i.e. if this property can be memoized,
        assuming that `self.reason_for_no_memoization` returns True), return
        None. Otherwise, return the reason as a string.

        As for `reason_for_no_memoization`, this does not do callgraph
        propagation itself and relies on `CompileCtx.check_memoized` to do so.

        :rtype: str|None
        """
        if self._call_non_memoizable_because:
            return self._call_non_memoizable_because
        elif self._solves_equation:
            return 'Cannot memoize equation solving'
        elif self._gets_logic_var_value:
            return 'Cannot memoize extracting the value of a logic variable'
        else:
            return None

    def warn_on_unused_bindings(self):
        """
        Emit warnings for bindings such as variables or arguments, that are not
        used. Also emit warnings for bindings that are used whereas they have
        been tagged as ignored.
        """
        # Mapping to tell for each variable if it is referenced at least once
        all_vars = {
            arg: False
            for arg in (self.constructed_expr.bindings
                        + [construct(arg.var)
                           for arg in self.natural_arguments])
        }

        def mark_vars(expr):
            if isinstance(expr, BindingScope):
                # BindingScope has bindings themselves as operands, but they
                # must not be considered as uses for this analysis: skip them.
                expr = expr.expr

            if isinstance(expr, VariableExpr):
                all_vars[expr] = True

            for sub in expr.flat_subexprs():
                mark_vars(sub)

        mark_vars(self.constructed_expr)
        unused_vars = [var for var, is_used in all_vars.items()
                       if not is_used and not var.ignored]
        wrongly_used_vars = [var for var, is_used in all_vars.items()
                             if is_used and var.ignored]

        unused_vars.sort(key=lambda var: var.name)
        wrongly_used_vars.sort(key=lambda var: var.name)

        def format_list(vars):
            return ', '.join(
                (var.source_name or var.name).lower
                for var in vars
            )

        WarningSet.unused_bindings.warn_if(
            unused_vars,
            'The following bindings are not used: {}'.format(
                format_list(unused_vars)),
        )
        WarningSet.unused_bindings.warn_if(
            wrongly_used_vars,
            'The following bindings are used even though they are supposed to'
            ' be ignored: {}'.format(format_list(wrongly_used_vars)),
        )

    def warn_on_undocumented_public_property(self, context):
        del context
        # For public properties only, warn undocumented ones. Only warn for
        # base properties: no need to repeat for the other ones.
        WarningSet.undocumented_public_properties.warn_if(
            self.is_public and not self.overriding and not self.doc,
            'This property is public but it lacks documentation'
        )


def ExternalProperty(type=None, doc="", **kwargs):
    """
    Public constructor for properties whose implementation is provided by the
    language specification. See PropertyDef for further documentation.
    :type type: CompiledType
    :type doc: str
    :rtype: PropertyDef
    """
    return PropertyDef(expr=None, prefix=AbstractNodeData.PREFIX_PROPERTY,
                       type=type, doc=doc, external=True, **kwargs)


# noinspection PyPep8Naming
def AbstractProperty(type, doc="", runtime_check=False, **kwargs):
    """
    Public constructor for abstract properties, where you can pass no
    expression but must pass a type. See PropertyDef for further documentation.

    :type type: CompiledType
    :type doc: str
    :type runtime_check: bool
    :rtype: PropertyDef
    """
    return PropertyDef(expr=None, prefix=AbstractNodeData.PREFIX_PROPERTY,
                       type=type, doc=doc, abstract=True,
                       abstract_runtime_check=runtime_check, **kwargs)


# noinspection PyPep8Naming
def Property(expr, doc=None, public=None, type=None, dynamic_vars=None,
             memoized=False, warn_on_unused=True, uses_entity_info=None,
             ignore_warn_on_node=None, call_non_memoizable_because=None):
    """
    Public constructor for concrete properties. You can declare your properties
    on your AST node subclasses directly, like this::

        class SubNode(ASTNode):
            my_field = Field()
            my_property = Property(Self.my_field)

    and functions will be generated in the resulting library.

    :type expr: AbstractExpression|function
    :type type: CompiledType
    :type doc: str
    :type public: bool|None
    :rtype: PropertyDef
    """
    return PropertyDef(
        expr, AbstractNodeData.PREFIX_PROPERTY, doc=doc, public=public,
        type=type, dynamic_vars=dynamic_vars, memoized=memoized,
        warn_on_unused=warn_on_unused, ignore_warn_on_node=ignore_warn_on_node,
        uses_entity_info=uses_entity_info,
        call_non_memoizable_because=call_non_memoizable_because
    )


class AbstractKind(Enum):
    concrete = 1
    abstract = 2
    abstract_runtime_check = 3


def langkit_property(public=None, return_type=None, kind=AbstractKind.concrete,
                     dynamic_vars=None, memoized=False,
                     call_memoizable=False, memoize_in_populate=False,
                     external=False, uses_entity_info=None, uses_envs=None,
                     warn_on_unused=True, ignore_warn_on_node=None,
                     call_non_memoizable_because=None):
    """
    Decorator to create properties from real Python methods. See Property for
    more details.

    :type public: bool|None
    :type return_type: CompiledType
    :type kind: int
    """
    def decorator(expr_fn):
        return PropertyDef(
            expr_fn, AbstractNodeData.PREFIX_PROPERTY,
            type=return_type,
            public=public,
            doc=expr_fn.__doc__,
            abstract=kind in [AbstractKind.abstract,
                              AbstractKind.abstract_runtime_check],
            abstract_runtime_check=kind == AbstractKind.abstract_runtime_check,
            dynamic_vars=dynamic_vars,
            memoized=memoized,
            call_memoizable=call_memoizable,
            memoize_in_populate=memoize_in_populate,
            external=external,
            uses_entity_info=uses_entity_info,
            uses_envs=uses_envs,
            warn_on_unused=warn_on_unused,
            ignore_warn_on_node=ignore_warn_on_node,
            call_non_memoizable_because=call_non_memoizable_because,
        )
    return decorator


@dsl_document
class Literal(AbstractExpression):
    """
    Turn the Python `literal` into the corresponding DSL literal. This is
    sometimes necessary to disambiguate the DSL.

    For instance::

        Literal(0)
    """

    def __init__(self, literal):
        super(Literal, self).__init__()
        self.literal = literal

    def construct(self):
        # WARNING: Since bools are ints in Python, bool needs to be before int
        # in the following table.
        cls = dispatch_on_type(type(self.literal), [
            (bool, lambda _: BooleanLiteralExpr),
            (int, lambda _:  IntegerLiteralExpr),
        ], exception=DiagnosticError('Invalid abstract expression type: {}'))
        return cls(self.literal, abstract_expr=self)

    def __repr__(self):
        return '<Literal {}>'.format(self.literal)


def aggregate_expr(type, assocs):
    """
    Create a LiteralExpr instance for an Ada aggregate.

    :param None|str|CompiledType type: Type of the aggregate.

        If None, generate a mere Ada aggregate. For instance: `(A, B, C)`.

        If it's a string, use it as a type name to generate a qualified
        expression. For instance, with `type='Foo'`: `Foo'(A, B, C)`.

        Otherwise, use the given CompileType to generate a qualified
        expression, unless it's no_compiled_type.

        Unless a true CompiledType instance is provided, the result will get
        the no_compiled_type type annotation.

    :param list[(str|names.Name, ResolvedExpression)] assocs: List of
        associations for the aggregate.

    :rtype: LiteralExpr
    """
    if type is None or type is no_compiled_type:
        meta_template = '({operands})'
        type_name = None
        type = no_compiled_type
    elif isinstance(type, str):
        meta_template = "{type}'({operands})"
        type_name = type
        type = no_compiled_type
    else:
        assert issubclass(type, no_compiled_type)
        meta_template = "{type}'({operands})"
        type_name = type.name.camel

    template = meta_template.format(
        type=type_name,
        operands=(', '.join('{} => {{}}'.format(n) for n, _ in assocs)
                  or 'null record')
    )
    return LiteralExpr(template, type, [e for _, e in assocs])


class BasicExpr(ComputingExpr):
    """
    A basic resolved expression template, that automatically handles:

    - Passing a list of sub expressions to the constructor, and a type
    - Doing the pre render of those expressions automatically
    - Rendering the sub expressions passed as parameters into the holes that
      are in the template.
    """

    def __init__(self, result_var_name, template, type, operands,
                 requires_incref=True, abstract_expr=None):
        """
        :param str result_var_name: See ResolvedExpression's constructor.
        :param str template: The template string.
        :param None|CompiledType type: The return type of the expression.
        :param bool requires_incref: Whether the computation in `template`
            returns a value that must be inc-ref'd to be stored in the result
            variable.
        :param AbstractExpression|None abstract_expr: See ResolvedExpression's
            constructor.
        """
        self.template = template
        self.static_type = type
        self.operands = operands
        self.requires_incref = requires_incref
        super(BasicExpr, self).__init__(result_var_name,
                                        abstract_expr=abstract_expr)

    def _render_pre(self):
        expr = self.template.format(*[
            (e if isinstance(e, basestring) else e.render_expr())
            for e in self.operands
        ])
        return '\n'.join(
            [e.render_pre()
             for e in self.operands
             if not isinstance(e, basestring)]
            + [assign_var(self.result_var.ref_expr, expr,
                          self.requires_incref)]
        )

    @property
    def subexprs(self):
        return [op for op in self.operands
                if isinstance(op, ResolvedExpression)]


@dsl_document
class No(AbstractExpression):
    """
    Return a null value of type `expr_type`.
    """

    def __init__(self, expr_type):
        """
        :param CompiledType expr_type: Type for the value this expression
            creates.
        """
        super(No, self).__init__()
        self.expr_type = expr_type

    def do_prepare(self):
        self.expr_type = resolve_type(self.expr_type)
        check_source_language(
            self.expr_type.null_allowed,
            "Invalid type for No expression: {}".format(
                self.expr_type.dsl_name
            )
        )

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: LiteralExpr
        """
        if self.expr_type.is_array:
            return ArrayLiteral.construct_static([], self.expr_type,
                                                 abstract_expr=self)
        else:
            return NullExpr(self.expr_type, abstract_expr=self)

    def __repr__(self):
        expr_type = resolve_type(self.expr_type)
        return '<No {}>'.format(expr_type.name.camel)


class FieldAccessExpr(BasicExpr):
    """
    Resolved expression for anything that compiles to "{prefix}.{field}" in the
    generated code.

    Note that this automatically generates a null safety check if prefix is
    allowed to be null.
    """

    def __init__(self, prefix_expr, field_name, result_type,
                 do_explicit_incref, abstract_expr=None):
        """
        :param ResolvedExpression prefix_expr: The prefix corresponding to this
            expression.
        :param str field_name: The name of the field to access.
        :param CompiledType type: The type of the result.
        :param bool do_explicit_incref: If True, perform an inc-ref on the
            result of the field access. This must be True for field accesses
            that do not automatically perform it.
        :param AbstractExpression|None abstract_expr: See ResolvedExpression's
            constructor.
        """
        super(FieldAccessExpr, self).__init__(
            'Fld', '{}.{}', resolve_type(result_type),
            [NullCheckExpr(prefix_expr), field_name],
            requires_incref=do_explicit_incref,
            abstract_expr=abstract_expr,
        )
        self.prefix_expr = prefix_expr
        self.field_name = field_name

    @property
    def subexprs(self):
        return {'prefix': self.prefix_expr, 'field': self.field_name}

    def __repr__(self):
        return '<FieldAccessExpr {} ({})>'.format(self.field_name,
                                                  self.type.name.camel)


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
                if var.type.is_refcounted:
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
            :param langkit.compiled_types.CompiledType type: The type of this
                local variable.
            """
            self.vars = vars
            self.name = name
            self.type = type
            assert self.type is None or isinstance(self.type, CompiledType)

            self._scope = None
            """
            The scope this variable lives in. During the construct phase, all
            resolved expressions that create local variables must initialize
            this using LocalVars.Scope.add.

            :type: LocalVars.Scope
            """

        def render(self):
            assert self.type, "Local var must have type before it is rendered"
            return "{} : {}{};".format(
                self.name.camel_with_underscores,
                self.type.name.camel_with_underscores,
                (' := {}'.format(self.type.nullexpr)
                 if self.type.is_refcounted and not self.type.is_ptr else '')
            )

        @property
        def ref_expr(self):
            """
            Return a resolved expression that references "self".
            :rtype: VariableExpr
            """
            assert self.type, ('Local variables must have a type before turned'
                               ' into a resolved expression.')
            return VariableExpr(self.type, self.name, local_var=self)

        def __repr__(self):
            return '<LocalVar {} : {}>'.format(
                self.name.camel_with_underscores,
                self.type.name.camel if self.type else '<none>'
            )

    def create(self, name, type):
        """
        Create a local variables in templates::

            from langkit.compiled_types import LocalVars, T
            vars = LocalVars()
            var = vars.create('Index', T.LongType)

        The names are *always* unique, so you can pass several time the same
        string as a name, and create will handle creating a name that is unique
        in the scope.

        The new local variable is automatically associated to the current
        scope.

        :param str|names.Name name: The name of the variable.
        :param langkit.compiled_types.CompiledType type: The type of the local
            variable.
        :rtype: LocalVars.LocalVar
        """
        result = self.create_scopeless(name, type)
        PropertyDef.get_scope().add(result)
        return result

    def create_scopeless(self, name, type):
        """
        Like "create", but do not assign a scope for the new local variable.
        The scope will have to be initialized later.

        :param str|names.Name name: The name of the variable.
        :param langkit.compiled_types.CompiledType type: The type of the local
            variable.
        :rtype: LocalVars.LocalVar
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


class CallExpr(BasicExpr):
    """
    Convenience resolved expression that models a call to a function on the Ada
    side of things. This assumes that for ref-counted types, function calls
    return a new ownership share to the caller.
    """

    def __init__(self, result_var_name, name, type, exprs, abstract_expr=None):
        """
        :param None|str result_var_name: See ResolvedExpression's constructor.
        :param names.Name|str name: The name of the procedure to call.
        :param CompiledType type: The return type of the function call.
        :param [ResolvedExpression] exprs: A list of expressions that
            represents the arguments to the function call.
        :param AbstractExpression|None abstract_expr: See ResolvedExpression's
            constructor.
        """
        self.name = names.Name.get(name)

        template = ('{name} ({args})' if exprs else '{name}').format(
            name=self.name.camel_with_underscores,
            args=', '.join(['{}'] * len(exprs))
        )

        super(CallExpr, self).__init__(result_var_name, template, type, exprs,
                                       requires_incref=False,
                                       abstract_expr=abstract_expr)

    @property
    def subexprs(self):
        return {'0-type': self.type,
                '1-name': self.name,
                '2-args': self.operands}

    def __repr__(self):
        return '<CallExpr {}>'.format(self.name.camel_with_underscores)


class NullCheckExpr(ResolvedExpression):
    """
    Expression that raises a PropertyError when the input is a null pointer.
    Just return the input otherwise.

    Note that the check is not performed at all when property checks are
    disabled context-wide.
    """

    def __init__(self, expr, implicit_deref=False):
        """
        :param ResolvedExpression expr: Expression to evaluate.
        :param bool implicit_deref: If expr is an entity, perform the
            check on the embedded AST node instead.
        """
        self.expr = expr
        self.implicit_deref = implicit_deref

        # There is no need for ref-counting handling because this expression
        # only forwards the result of the "expr" operand to the user, without
        # storing it in a local variable.
        super(NullCheckExpr, self).__init__(skippable_refcount=True)

    @property
    def type(self):
        return self.expr.type

    def _render_pre(self):
        return render('properties/null_check_ada', expr=self)

    def _render_expr(self):
        return self.expr.render_expr()

    @property
    def subexprs(self):
        return {'expr': self.expr}

    def __repr__(self):
        return '<NullCheckExpr>'


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
        l = construct(self.l)
        r = construct(self.r)

        if l.type == T.SymbolType and r.type == T.SymbolType:
            assert self.op == '&'
            return BasicExpr(
                'Sym_Concat',
                'Find (Self.Unit.TDH.Symbols, ({}.all & {}.all))',
                T.SymbolType, [l, r]
            )

        check_source_language(
            l.type == r.type, "Incompatible types for {}: {} and {}".format(
                self.op, l.type.dsl_name, r.type.dsl_name
            )
        )

        check_source_language(
            l.type in (T.SymbolType, T.LongType),
            "Invalid type for {}: {}".format(self.op, l.type.dsl_name)
        )

        return BasicExpr('Arith_Result', '({} %s {})' % self.op, T.LongType,
                         [l, r],
                         abstract_expr=self)

    def __repr__(self):
        return '<Op {}>'.format(self.op)


def ignore(*vars):
    """
    Annotate variables in "var" as being intentionally unused.

    This disables the warning on unused bindings for them.

    :type vars: list[AbstractVariable]
    """
    for var in vars:
        var.tag_ignored()


def resolve_property(propref):
    """
    Resolve a property reference to the actual PropertyDef instance.

    :param propref: Property reference to resolve. It can be either:

        * None: it is directly returned;
        * a PropertyDef instance: it is directly returned;
        * a TypeRepo.Defer instance: it is deferred.

    :rtype: PropertyDef
    """
    if propref is None or isinstance(propref, PropertyDef):
        result = propref

    elif isinstance(propref, TypeRepo.Defer):
        result = propref.get()

    else:
        check_source_language(False, 'Invalid property reference: {}'.format(
            propref
        ))

    assert result is None or isinstance(result, PropertyDef)
    return result
