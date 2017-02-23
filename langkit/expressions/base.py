from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from contextlib import contextmanager
from copy import copy
from functools import partial
import inspect
from itertools import count

from enum import Enum
import funcy

from langkit import names
from langkit.compiled_types import (
    AbstractNodeData, Argument, ASTNode, BoolType, CompiledType,
    LexicalEnvType, LongType, Symbol, T, Token, get_context,
    render as ct_render, resolve_type
)
from langkit.diagnostics import (
    Context, DiagnosticError, Severity, check_multiple, check_source_language,
    check_type, extract_library_location, warn_if
)
from langkit.utils import TypeSet, assert_type, dispatch_on_type, memoized


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
    elif isinstance(expr, str):
        expr = SymbolLiteral(expr)

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
        # The type could be an early reference to a not yet declared type,
        # resolve it.
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

        fn_arguments.append(Argument(names.Name.from_lower(kw), default, None))

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
        from langkit.expressions.boolean import Not
        from langkit.expressions.logic import All, Any

        return {
            '_or': lambda alt: self.then(lambda e: e, default_val=alt),
            'empty': self.length.equals(0),
            'find': lambda filter_expr:
                self.filter(filter_expr).at(0),
            'keep': lambda klass:
                self.filtermap(filter_expr=lambda e: e.is_a(klass),
                               expr=lambda e: e.cast(klass)),
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
    Decorator to create an expression accessible through an attribute call on
    an AbstractExpression instance, from an abstract expression class. See
    attr_expr_impl for more details.

    :param name: The name of the attribute.
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


def auto_attr_custom(name, *partial_args, **partial_kwargs):
    """
    Helper decorator, that will automatically register an AbstractExpression
    subclass accessible as an attribute. Exposes more options than auto_attr.
    See auto_attr for more detail.

    :param str|None name: The name of the attribute. If None, the name of the
        function will be taken.
    :param [object] partial_args: Arguments to partially apply to the function.
    :param [object] partial_kwargs: Keyword arguments to partially apply to the
        function.
    """

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
            kwargs = dict(partial_kwargs)
            kwargs.update(self.kwargs)
            return fn(*(self.sub_expressions + partial_args), **kwargs)

        def __repr__(self):
            return "<{}{}>".format(
                self.__class__.__name__,
                "({})".format(", ".join(str(e) for e in self.sub_expressions))
                if self.sub_expressions else ""
            )

        nb_args = len(inspect.getargspec(fn).args)

        assert nb_args > 0

        decorator = (attr_expr if nb_args == 1 else attr_call)

        decorator(attr_name)(type(
            b'{}'.format(attr_name),
            (AbstractExpression, ), {
                'construct': construct,
                '__init__': __init__,
                '__repr__': __repr__,
                'sub_expressions': sub_expressions,
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

    def __init__(self, result_var_name=None, scopeless_result_var=False):
        """
        Create a resolve expression.

        :param None|str result_var_name: If provided, create a local variable
            using this as a base name to hold the result of this expression.
            In this case, the "type" property must be ready.
        :param bool scopeless_result_var: Whether the result variable must be
            scopeless. This has no effect if "result_var_name" is None.
        """
        if result_var_name:
            create_var = (PropertyDef.get().vars.create_scopeless
                          if scopeless_result_var else
                          PropertyDef.get().vars.create)
            self._result_var = create_var(result_var_name, self.type)
        else:
            self._result_var = None

    @property
    def result_var(self):
        """
        Return the local variable used to store the result of this expression,
        if any. Note that if the result is not null, the caller can assume that
        the "render_expr" method only returns the result variable name.

        :rtype: LocalVars.LocalVar|None
        """
        return self._result_var

    def render_pre(self):
        """
        Render initial statements that might be needed to the expression.

        :rtype: str
        """
        result = self._render_pre()
        if self.result_var:
            return '{}\n{} := {};'.format(
                result,
                self.result_var.name.camel_with_underscores,
                self._render_expr()
            )
        else:
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
            return all(len(d) == 1 for d in subdumps)

        if isinstance(json_like, list):
            subdumps = [cls._ir_dump(elt) for elt in json_like]
            if one_line_subdumps(subdumps):
                one_liner = '[{}]'.format(', '.join(
                    d[0] for d in subdumps
                ))
                if len(one_liner) <= max_cols:
                    return [one_liner]
            for elt in json_like:
                subdump = cls._ir_dump(elt)
                result.append('*  {}'.format(subdump[0]))
                result.extend('|  {}'.format(line) for line in subdump[1:])

        elif isinstance(json_like, dict):
            keys = sorted(json_like)
            subdumps = [cls._ir_dump(json_like[key])
                        for key in sorted(json_like)]
            items = zip(keys, subdumps)
            if one_line_subdumps(subdumps):
                one_liner = '{{{}}}'.format(
                    ', '.join('{}={}'.format(key, d[0]) for key, d in items)
                )
                if len(one_liner) <= max_cols:
                    return [one_liner]
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

            if len(subdump) == 1:
                one_liner = '{}{}'.format(
                    class_name, subdump[0]
                )
                if len(one_liner) <= max_cols:
                    return [one_liner]
            result.append('{}('.format(class_name))
            result.extend('|  {}'.format(line) for line in subdump)
            result.append(')')

        elif (issubclass(type(json_like), type)
                and issubclass(json_like, CompiledType)):
            return cls._ir_dump(json_like.name())

        elif isinstance(json_like, names.Name):
            result.append(json_like.camel_with_underscores)

        else:
            result.append(str(json_like))

        return result

    @property
    def subexprs(self):
        """
        A JSON-like datastructure to describe this expression.

        Leaves of this datastructure are: strings, CompiledType subclasses,
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
        def mapcat(seq, map_fn):
            return sum([map_fn(v) for v in seq], [])

        def explore(values):
            if values is None:
                return []
            elif isinstance(values, list):
                return mapcat(values, explore)
            elif isinstance(values, dict):
                return mapcat(values.values(), explore)
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

        :rtype: list[AbstractVariable.Expr]
        """
        result = self._bindings()
        for expr in self.flat_subexprs():
            result.extend(expr.bindings)
        return result

    def _bindings(self):
        """
        Return the list of variables "self" defines.

        Subclasses must override this method if they define variables.

        :rtype: list[AbstractVariable.Expr]
        """
        return []


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

    class Expr(ResolvedExpression):
        """
        Resolved expression that represents a variable in generated code.
        """

        pretty_class_name = 'Var'

        def __init__(self, type, name, abstract_var=None):
            """
            Create a variable reference expression.

            :param langkit.compiled_types.CompiledType type: Type for the
                referenced variable.
            :param names.Name name: Name of the referenced variable.
            :param AbstractVariable|None source_name: AbstractVariable that
                compiled to this resolved expression, if any.
            """
            self.static_type = assert_type(type, CompiledType)
            self.name = name
            self.abstract_var = abstract_var

            super(AbstractVariable.Expr, self).__init__()

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
            return self.abstract_var.ignored if self.abstract_var else False

        def __repr__(self):
            src_name = self.source_name
            return '<AbstractVariable.Expr {}{}>'.format(
                self.name.lower,
                ' ({})'.format(src_name.lower) if src_name else '')

    def __init__(self, name, type=None, create_local=False, source_name=None):
        """
        :param names.Name name: The name of the PlaceHolder variable.
        :param CompiledType type: The type of the variable. Optional for
            global abstract variables where you will use bind_type. Mandatory
            if create_local is True.
        :param bool create_local: Whether to create a corresponding local
            variable in the current property.
        :param names.Name|None source_name: If this variables comes from the
            language specification, hold its original name.
        """
        super(AbstractVariable, self).__init__()

        if name.lower == '_':
            i = next(self.unused_count)
            name = names.Name('Unused_{}'.format(i))

        self.local_var = None
        if create_local:
            self.local_var = PropertyDef.get().vars.create_scopeless(name,
                                                                     type)
            self._name = self.local_var.name
        else:
            self._name = name

        self._type = type
        self.source_name = source_name

        self.construct_cache = {}
        """
        :type: dict[(str, CompiledType), AbstractVariable.Expr]

        Cache used to memoize the "construct" method.
        """

        self.ignored = source_name == names.Name.from_lower('_')

    def add_to_scope(self, scope):
        """
        Add this already existing variable to "scope".

        This is allowed iff this variable is not registered as a local variable
        yet and it must already be typed.
        """
        assert self.local_var is None
        assert self._type is not None
        self.local_var = PropertyDef.get().vars.create_scopeless(
            self._name,
            self._type
        )
        scope.add(self.local_var)
        self._name = self.local_var.name

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
        key = (self._name, self._type)
        try:
            expr = self.construct_cache[key]
        except KeyError:
            expr = AbstractVariable.Expr(self._type, self._name, self)
            self.construct_cache[key] = expr
        return expr

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
        return self.construct_static(token)

    @staticmethod
    def construct_static(token_expr):
        return BuiltinCallExpr("Get_Symbol", Symbol, [token_expr])


class SymbolLiteral(AbstractExpression):
    """
    Abstract expression that returns a symbol from a string literal.
    """

    class Expr(ResolvedExpression):

        def __init__(self, name):
            super(SymbolLiteral.Expr, self).__init__()

            self.name = name
            self.static_type = Symbol

            get_context().add_symbol_literal(self.name)

        def _render_expr(self):
            return 'Self.Unit.Context.Symbol_Literals ({})'.format(
                get_context().symbol_literals[self.name]
            )

        def _render_pre(self):
            return ''

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
        return self.Expr(self.name)


class BindingScope(ResolvedExpression):
    """
    Resolved expression that materializes new bindings.

    This resolved expression is just an annotation: it is useless from a code
    generation point of view. It makes it possible to describe the creation of
    new bindings for some scope.
    """

    def __init__(self, expr, bindings):
        """
        :type expr: ResolvedExpression
        :type bindings: list[AbstractExpression.Expr]
        """
        super(BindingScope, self).__init__()
        self.expr = expr
        self.expr_bindings = bindings
        self.static_type = self.expr.type

    def _render_pre(self):
        return self.expr._render_pre()

    def _render_expr(self):
        return self.expr._render_expr()

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


class Let(AbstractExpression):
    """
    Abstract expressions that associates names to values from other abstract
    expressions and that evaluates yet another abstract expressions with these
    names available.
    """

    class Expr(ResolvedExpression):
        pretty_class_name = 'Let'

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
        # TODO: get the original source name. This is unfortunately difficult,
        # as this information is hidden in the source code of the caller.
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

    @staticmethod
    def construct_static(array_type):
        return LiteralExpr('Create (Items_Count => 0)', array_type,
                           result_var_name='Empty_Array')

    def construct(self):
        return self.construct_static(self.array_type)


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

    def __init__(self, expr, prefix, name=None, doc=None, public=None,
                 abstract=False, type=None, abstract_runtime_check=False,
                 has_implicit_env=None, memoized=False, external=False):
        """
        :param expr: The expression for the property. It can be either:
            * An expression.
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

            Note that for abstract properties that are private, this is
            automatically enabled, as abstract private primitives are not
            allowed in Ada.

        :param bool|None has_implicit_env: Whether this property is passed an
            implicit "current environment" parameter.  If None, inherit from
            the overriden property, or False in there is no property to
            override. Just like `public`, it must always be consistent with
            base classes.

        :param bool memoized: Whether this property must be memoized. Disabled
            by default.

        :param bool external: Whether this property's implementation is
            provided by the language specification. If true, `expr` must be
            None and the implementation must be provided in the
            extensions/nodes/{node_name}/bodies extension file. Note that the
            engines always generate the public declaration part.
        """

        super(PropertyDef, self).__init__(name=name, public=public)

        self.in_type = False
        "Recursion guard for the construct pass"

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
        self._has_implicit_env = has_implicit_env

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
        self.external = external

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
        return str(self._index)

    def __copy__(self):
        """
        When copying properties, we want to make sure they don't share local
        variables, so we implement a custom copier that duplicates the
        LocalVars instance.

        :rtype: Property
        """
        new = PropertyDef(
            expr=self.expr,
            prefix=self.prefix,
            name=self._name,
            doc=self._doc,
            public=self._is_public,
            abstract=self.abstract,
            type=self.expected_type,
            has_implicit_env=self._has_implicit_env,
            external=self.external,
        )
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

        # If the expr has not yet been constructed, try to construct it
        if not self.constructed_expr:
            with self.diagnostic_context():
                self.construct_and_type_expression(get_context())

        return resolve_type(self.constructed_expr.type)

    def _add_argument(self, name, type, default_value=None):
        """
        Helper to add an argument to this property.

        This basically just fills the .arguments list.

        :param str names.Name: Name for this argument.
        :param CompiledType type: Type argument. Type for this argument.
        :param None|str default_value: Default value for this argument, if any.
        """
        self.arguments.append(Argument(name, type, default_value))

    @property
    @memoized
    def base_property(self):
        """
        Get the base property for this property, if it exists.

        :rtype: PropertyDef|None
        """
        if issubclass(self.struct, ASTNode):
            struct = assert_type(self.struct, ASTNode)
            return struct.base().get_abstract_fields_dict(
                field_class=PropertyDef
            ).get(self._name.lower, None)
        else:
            return None

    @property
    @memoized
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

    @property
    def has_implicit_env(self):
        """
        Return whether this property is passed an implicit environment param.

        :rtype: bool
        """
        assert self._has_implicit_env is not None
        return self._has_implicit_env

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

            # Likewise for accepting an implicit environment parameter
            if self._has_implicit_env is None:
                self._has_implicit_env = self.base_property._has_implicit_env
            else:
                check_source_language(
                    (self._has_implicit_env
                        == self.base_property.has_implicit_env),
                    '{} has {} implicit environment parameter, so should have'
                    ' {}'.format(
                        self.base_property.qualname,
                        'an' if self.base_property._has_implicit_env else 'no',
                        self.qualname,
                    )
                )

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
                            self.expected_type.name().camel,
                            self.base_property.qualname,
                            self.base_property.type.name().camel
                        )
                    )
                else:
                    # If base has a type annotation and not self, then
                    # propagate it.
                    self.expected_type = self.base_property.expected_type
        else:
            # By default, properties are private and they accept an implicit
            # environment parameter.
            def with_default(value, default_value):
                return default_value if value is None else value
            self._is_public = with_default(self._is_public, False)
            self._has_implicit_env = with_default(
                self._has_implicit_env, False
            )

        if self.memoized:
            check_source_language(
                not self.abstract,
                'A memoized property cannot be abstract: memoization is not an'
                ' inheritted behavior'
            )
            check_source_language(
                not self.has_implicit_env,
                'A memoized property is not allowed to take an implicit env'
                ' arguments'
            )
            check_source_language(
                not self.explicit_arguments,
                'A memoized property is not allowed to take explicit arguments'
            )

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
                not self.memoized,
                'An external property cannot be memoized'
            )

        # See abstract_runtime_check documentation in __init__
        if self.is_private and self.abstract:
            self.abstract_runtime_check = True

        # Add the implicit lexical env. parameter if required
        if self.has_implicit_env:
            self._add_argument(PropertyDef.env_arg_name,
                               LexicalEnvType,
                               LexicalEnvType.nullexpr())

    def construct_and_type_expression(self, context):
        """
        This pass will construct the resolved expression from the abstract
        expression, and get type information at the same time.

        :type context: langkit.compile_context.CompileCtx
        """
        from langkit.expressions.envs import Env

        # If expr has already been constructed, return
        if self.constructed_expr:
            return

        check_source_language(
            not self.in_type,
            'Recursion loop in type inference for property {}. Try to '
            'specify its return type explicitly.'.format(self.qualname)
        )

        # If we don't have an expression, this have to be an abstract/external
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

        with self.bind(), Self.bind_type(self.struct):
            message = (
                'expected type {{expected}}, got'
                ' {{expr_type}} instead (expected type comes from'
                ' overridden base property in {base_prop})'.format(
                    base_prop=self.base_property.struct.name().camel
                )
            ) if self.base_property else None

            # Reset the Env binding so that this construction does not use a
            # caller's binding.
            with Env.bind_default(self):
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

        if self.base_property and self.base_property.type:
            # TODO: We need to make sure Properties are rendered in the proper
            # order (base classes first), to make sure that this check is
            # always effectful.
            check_source_language(
                self.type.matches(self.base_property.type),
                "{} returns {} whereas it overrides {}, which returns {}."
                " The former should match the latter.".format(
                    self.qualname, self.type.name().camel,
                    self.base_property.qualname,
                    self.base_property.type.name().camel
                )
            )

    def doc(self):
        return self._doc

    def _explicit_arguments_slice(self, arg_array):
        """
        Return the subset of "arg_array" corresponding to arguments that must
        be passed explicitely when invoking this property.

        :type arg_array: list[T]
        :rtype: list[T]
        """
        # Strip the implicit "Lex_Env" argument
        return (arg_array[:-1]
                if self.is_property and self.has_implicit_env else
                arg_array)

    @property
    def explicit_arguments(self):
        """
        Return the subset of "self.arguments" that are to be passed explicitely
        when invoking this property.

        :rtype: list[Argument]
        """
        return self._explicit_arguments_slice(self.arguments)

    @property
    def argument_vars(self):
        """
        For each argument additional to Self, return the AbstractVariable
        corresponding to this argument. Note that this is available only after
        the "prepare" pass.

        :rtype: list[AbstractVariable]
        """
        return [arg.var for arg in self.arguments]

    @property
    def explicit_argument_vars(self):
        """
        Like "explicit_arguments", but for AbstractVariable instances.

        :rtype: list[AbstractVariable]
        """
        return self._explicit_arguments_slice(self.argument_vars)

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
        exp_args = self.explicit_arguments[:len(self.explicit_arguments)
                                           - len(partial_args_types)]

        ret = [self.struct] + [a.type for a in exp_args]
        return ret

    @property
    def memoization_state_field_name(self):
        """
        Assuming this property is memoized, return the name of the boolean
        field that indicates whether there is already a value that has been
        computed for this property.

        :rtype: names.Name
        """
        assert self.memoized
        return names.Name('In_Cache') + self.name

    @property
    def memoization_value_field_name(self):
        """
        Assuming this property is memoized, return the name of the field that
        is used to store the cached value for this property.

        :rtype: names.Name
        """
        assert self.memoized
        return names.Name('Cached') + self.name

    def warn_on_unused_bindings(self):
        """
        Emit warnings for bindings such as variables or arguments, that are not
        used. Also emit warnings for bindings that are used whereas they have
        been tagged as ignored.
        """
        # Mapping to tell for each variable if it is referenced at least once
        all_vars = {
            var: False
            for var in (self.constructed_expr.bindings
                        + [construct(arg)
                           for arg in self.explicit_argument_vars])
        }

        def mark_vars(expr):
            if isinstance(expr, AbstractVariable.Expr):
                all_vars[expr] = True

            elif isinstance(expr, BindingScope):
                # BindingScope has bindings themselves as operands, but they
                # must not be considered as uses for this analysis: skip them.
                expr = expr.expr

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

        warn_if(
            unused_vars,
            'The following bindings are not used: {}'.format(
                format_list(unused_vars)))
        warn_if(
            wrongly_used_vars,
            'The following bindings are used even though they are supposed to'
            ' be ignored: {}'.format(format_list(wrongly_used_vars)))


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
def Property(expr, doc=None, public=None, type=None, has_implicit_env=None,
             memoized=False):
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
    return PropertyDef(expr, AbstractNodeData.PREFIX_PROPERTY, doc=doc,
                       public=public, type=type,
                       has_implicit_env=has_implicit_env, memoized=memoized)


class AbstractKind(Enum):
    concrete = 1
    abstract = 2
    abstract_runtime_check = 3


def langkit_property(public=None, return_type=None,
                     kind=AbstractKind.concrete, has_implicit_env=None,
                     memoized=False, external=False):
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
            has_implicit_env=has_implicit_env,
            memoized=memoized,
            external=external,
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

    def __init__(self, template, type, operands, result_var_name=None):
        """
        :param str template: The template string.
        :param None|CompiledType type: The return type of the expression.
        :param None|str result_var_name: See ResolvedExpression's constructor.
        """
        self.operands = operands
        self.static_type = type
        self.template = template
        super(BasicExpr, self).__init__(result_var_name)

    def _render_expr(self):
        return self.template.format(*[
            (expr if isinstance(expr, basestring) else expr.render_expr())
            for expr in self.operands
        ])

    def _render_pre(self):
        return '\n'.join(expr.render_pre()
                         for expr in self.operands
                         if not isinstance(expr, basestring))

    @property
    def subexprs(self):
        return [op for op in self.operands
                if isinstance(op, ResolvedExpression)]


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
        self.literal = literal

    @property
    def subexprs(self):
        return {'0-type': self.static_type,
                '1-literal': self.literal}

    def __repr__(self):
        return '<LiteralExpr {} ({})>'.format(self.template,
                                              self.type.name().camel)


class FieldAccessExpr(BasicExpr):
    """
    Resolved expression for anything that compiles to "{prefix}.{field}" in the
    generated code.

    Note that this automatically generates a null safety check if prefix is
    allowed to be null.
    """

    def __init__(self, prefix_expr, field_name, result_type):
        """
        :param ResolvedExpression prefix_expr: The prefix corresponding to this
            expression.
        :param str field_name: The name of the field to access.
        :param CompiledType type: The type of the result.
        """
        super(FieldAccessExpr, self).__init__(
            '{}.{}', result_type, [NullCheckExpr(prefix_expr), field_name]
        )
        self.prefix_expr = prefix_expr
        self.field_name = field_name

    @property
    def subexprs(self):
        return {'prefix': self.prefix_expr, 'field': self.field_name}


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
        self.element_type = element_type

    @property
    def subexprs(self):
        return {'0-elt_type': self.element_type,
                '2-elements': self.operands}


class TokenTextEq(BasicExpr):
    """
    Resolved expression to test equality of the text of two tokens.
    """
    def __init__(self, left, right):
        """
        :type left: ResolvedExpression
        :type right: ResolvedExpression
        """
        super(TokenTextEq, self).__init__(
            template="Text_Type'(Text ({})) = Text_Type'(Text ({}))",
            type=BoolType,
            operands=[left, right])


@auto_attr
def text_equals(left, right):
    """
    Expression to test equality of the text of two tokens.

    :param AbstractExpression left: Expression that must resolve to a token,
        whose text will be used for the test.
    :param AbstractExpression right: Likewise.
    """
    return TokenTextEq(construct(left, Token), construct(right, Token))


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

        @property
        def ref_expr(self):
            """
            Return a resolved expression that references "self".
            :rtype: AbstractVariable.Expr
            """
            assert self.type, ('Local variables must have a type before turned'
                               ' into a resolved expression.')
            return AbstractVariable.Expr(self.type, self.name)

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
        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of the local variable.
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

    @property
    def subexprs(self):
        return {'0-type': self.type,
                '1-name': self.name,
                '2-args': self.operands}

    def __repr__(self):
        return '<BuiltinCallExpr {}>'.format(self.name.camel_with_underscores)


class NullCheckExpr(ResolvedExpression):
    """
    Expression that raises a PropertyError when the input is a null pointer.
    Just return the input otherwise.

    Note that the check is not performed at all when property checks are
    disabled context-wide.
    """

    def __init__(self, expr, implicit_deref=False, result_var_name=None):
        """
        :param ResolvedExpression expr: Expression to evaluate.
        :param bool implicit_deref: If expr is an env element, perform the
            check on the embedded AST node instead.
        """
        self.expr = expr
        self.implicit_deref = implicit_deref
        super(NullCheckExpr, self).__init__(result_var_name)

    @property
    def type(self):
        return self.expr.type

    def _render_pre(self):
        return '{}\n{}'.format(self.expr.render_pre(),
                               render('properties/null_check_ada',
                                      expr=self.expr,
                                      implicit_deref=self.implicit_deref))

    def _render_expr(self):
        return self.expr.render_expr()

    @property
    def subexprs(self):
        return {'expr': self.expr}

    def __repr__(self):
        return '<NullCheckExpr>'


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
        l = construct(self.l)
        r = construct(self.r)

        if l.type == Symbol and r.type == Symbol:
            return BasicExpr(
                'Find (Self.Unit.TDH.Symbols, ({}.all & {}.all))',
                Symbol,
                [l, r]
            )

        check_source_language(
            l.type == r.type, "Incompatible types for {}: {} and {}".format(
                self.op, l.type.name().camel, r.type.name().camel
            )
        )

        check_source_language(
            l.type in (Symbol, LongType), "Invalid type for {}: {}".format(
                self.op, l.type.name().camel
            )
        )

        return BasicExpr("({} %s {})" % self.op, LongType, [l, r])


def ignore(*vars):
    """
    Annotate variables in "var" as being intentionally unused.

    This disables the warning on unused bindings for them.

    :type vars: list[AbstractVariable]
    """
    for var in vars:
        var.ignored = True
