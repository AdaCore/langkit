from contextlib import contextmanager
from copy import copy
from functools import partial
import inspect

from langkit import names
from langkit.compiled_types import (
    AbstractNodeData, BoolType, CompiledType, LexicalEnvType, LongType,
    render as ct_render, Symbol, Token
)
from langkit.utils import assert_type, memoized, user_assert


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

    :param custom_msg: A string for the error messages, containing
        format-like template holes "{}". If expected_type_or_pred is a type,
        the message must contain two holes for the names of the types, the
        expected one first, and the obtained type second. If it is a
        predicate, it must contain one hole for the expr type.

    :rtype: ResolvedExpression
    """

    if not custom_msg:
        custom_msg = "Expected type {}, got {}"

    if isinstance(expr, AbstractExpression):
        ret = expr.construct()

    # WARNING: Since bools are ints in python, this check needs to be before
    # the "is int" check.
    elif isinstance(expr, bool):
        ret = LiteralExpr(str(expr), BoolType)
    elif isinstance(expr, int):
        ret = LiteralExpr(str(expr), LongType)
    else:
        raise TypeError('Invalid abstract expression: {}'.format(type(expr)))

    if expected_type_or_pred:
        if isinstance(expected_type_or_pred, type):
            assert ret.type.matches(expected_type_or_pred), (
                custom_msg.format(expected_type_or_pred, ret.type)
            )
        else:
            assert callable(expected_type_or_pred), (
                "Expected_type_or_pred must either be a type, or a predicate"
                " of type (ResolvedExpression) -> bool"
            )
            assert expected_type_or_pred(ret.type), (
                "Evaluating predicate on {} failed".format(
                    ret.type.name().camel
                )
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
        if not self.__dict__.get("_is_prepared", False):
            self.do_prepare()
            self.__dict__['_is_prepared'] = True
            for _, v in self.__dict__.items():
                if isinstance(v, AbstractExpression):
                    v.prepare()

    def construct(self):
        """
        Returns a resolved tree of resolved expressions.

        :rtype: ResolvedExpression
        """
        raise NotImplementedError()

    @memoized
    def attrs(self):

        from langkit.expressions.collections import (
            Quantifier, Map, Contains
        )
        from langkit.expressions.structs import Cast, IsA, IsNull
        from langkit.expressions.envs import EnvGet, EnvBind
        from langkit.expressions.boolean import Eq, BinaryBooleanOperator, Then
        from langkit.expressions.collections import (
            CollectionGet, CollectionLength
        )

        # Using partial allows the user to be able to use keyword arguments
        # defined on the expressions constructors.
        return {
            # Quantifiers
            'all':            partial(Quantifier, Quantifier.ALL, self),
            'any':            partial(Quantifier, Quantifier.ANY, self),

            # Type handling
            'cast':           partial(Cast, self, do_raise=False),
            'cast_or_raise':  partial(Cast, self, do_raise=True),
            'is_a':           partial(IsA, self),
            'symbol':         GetSymbol(self),

            # Other predicate combinators
            'equals':         partial(Eq, self),
            'is_null':        IsNull(self),

            # Other containers handling
            'at':             partial(CollectionGet, self),
            'at_or_raise':    partial(CollectionGet, self, or_null=False),
            'contains':       partial(Contains, self),
            'filter':         partial(Map, self, lambda x: x),
            'length':         CollectionLength(self),
            'map':            partial(Map, self),
            'mapcat':         partial(Map, self, concat=True),
            'take_while':     partial(Map, self, lambda x: x, lambda x: None,
                                      False),

            # Control flow handling
            'and_then':       partial(BinaryBooleanOperator,
                                      BinaryBooleanOperator.AND, self),
            'or_else':        partial(BinaryBooleanOperator,
                                      BinaryBooleanOperator.OR, self),
            'then':           partial(Then, self),

            # Environments handling
            'eval_in_env':    partial(EnvBind, self),
            'get':            partial(EnvGet, self),
            'resolve_unique': partial(EnvGet, self, resolve_unique=True),
        }

    @memoized
    def composed_attrs(self):
        """
        Helper memoized dict for attributes that are composed on top of
        built-in ones. Since they're built on regular attrs, we cannot put
        them in attrs or it would cause infinite recursion.
        """
        return {
            'empty': self.length.equals(0),
            'find': lambda filter_expr:
                self.filter(filter_expr).at(0),
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
            self._type = assert_type(type, CompiledType)
            self.name = name

        @property
        def type(self):
            return self._type

        def render_expr(self):
            return self.name.camel_with_underscores

    def __init__(self, name, type=None, create_local=False):
        """
        :param names.Name name: The name of the PlaceHolder variable.
        :param CompiledType type: The type of the variable. Optional for
            global abstract variables where you will use bind_type. Mandatory
            if create_local is True.
        :param bool create_local: Whether to create a corresponding local
            variable in the current property.
        """
        self.local_var = None
        if create_local:
            self.local_var = Property.get().vars.create(name, type)
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


class GetSymbol(AbstractExpression):
    """
    Abstract expression that gets a symbol out of a token.
    """

    def __init__(self, token_expr):
        """
        :param AbstractExpression token_expr: Expression returning a token.
        """
        self.token_expr = token_expr

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: BuiltinCallExpr
        """
        return BuiltinCallExpr("Get_Symbol", Symbol,
                               [construct(self.token_expr, Token)])


def render(*args, **kwargs):
    return ct_render(*args, property=Property.get(), Self=Self, **kwargs)


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

    self_arg_name = names.Name('Node')
    env_arg_name = names.Name('Lex_Env')

    reserved_arg_names = (self_arg_name, env_arg_name)
    reserved_arg_lower_names = [n.lower for n in reserved_arg_names]

    def __init__(self, expr, doc=None, private=False, abstract=False,
                 type=None, abstract_runtime_check=False):
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

        :param str|None doc: User documentation for this property.
        :param bool private: Whether this property is private or not.
        :param bool abstract: Whether this property is abstract or not. If this
            is True, then expr can be None.
        :param CompiledType|None type: The optional type annotation for this
            property. If supplied, it will be used to check the validity of
            inferred types for this propery, and eventually for overriding
            properties in sub classes. NOTE: The type is mandatory for abstract
            base properties and for properties that take parameters.
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
        # If the user has provided a type, we'll return it for clients wanting
        # to know the type of the Property. Internal consistency with the
        # constructed_expr is checked when we emit the Property.
        if self.expected_type:
            return self.expected_type
        # In other cases, let's rely on the constructed expr's type. TODO: We
        # need to add a proper error message for the cases when the type is
        # asked too early, or when there is a circular dep.
        else:
            return self.constructed_expr.type

    def _add_argument(self, name, type, default_value=None):
        """
        Helper to add an argument to this property.

        This basically just fills the .arguments and the .argument_vars lists.

        :param str names.Name: Name for this argument.
        :param CompiledType type: Type argument. Type for this argument.
        :param None|str default_value: Default value for this argument, if any.
        """
        self.arguments.append((name, type, default_value))
        self.argument_vars.append(AbstractVariable(name, type))

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

    def prepare(self):
        """
        Run the "prepare" pass on the expression associated to this property.

        This pass, which just invokes the "prepare" method on all
        AbstractExpression nodes in the expression, expands all lambda
        functions there into AbstractExpression nodes (which are then prepared
        themselves).

        After this pass, the expression tree is ready for the "construct" pass,
        which can yield a ResolvedExpression tree.

        :rtype: None
        """

        # Add the implicit lexical env. parameter
        self._add_argument(Property.env_arg_name,
                           LexicalEnvType,
                           LexicalEnvType.nullexpr())

        if not self.expr:
            return

        # If the user passed a lambda or function for the expression,
        # now is the moment to transform it into an abstract expression by
        # calling it.
        if self.expr and not isinstance(self.expr, AbstractExpression):
            user_assert(callable(self.expr),
                        'Expression should be either AbstractExpression'
                        ' instances, either functions that return these')
            argspec = inspect.getargspec(self.expr)

            if (len(argspec.args) == 1 and
                    not argspec.varargs and
                    not argspec.keywords and
                    not argspec.defaults):
                # This is a mere: lambda self: <expression>
                self.expr = assert_type(self.expr(Self), AbstractExpression)

            else:
                user_assert(
                    not argspec.varargs or not argspec.keywords,
                    'Invalid lamda signature: no *args nor **kwargs allowed'
                )
                user_assert(
                    len(argspec.args) > 0,
                    'Invalid lambda signature: at least one parameter expected'
                )
                user_assert(
                    len(argspec.args) == len(argspec.defaults),
                    'All types must have an associated type as a default value'
                )

                # This is a lambda for a property that takes parameters: check
                # that all parameters have declared types in default arguments.
                for kw, default in zip(argspec.args, argspec.defaults):
                    # Because there's no forward definition mechanism, it is
                    # sometimes not possible to annotate an argument with a
                    # type because the type does not exist yet. In this case,
                    # we allow lambda functions that take no argument just to
                    # delay the evaluation of the type itself.
                    if not inspect.isclass(default):
                        default = default()

                    user_assert(
                        kw.lower not in Property.reserved_arg_lower_names,
                        'Cannot define reserved arguments ({})'.format(
                            ', '.join(Property.reserved_arg_lower_names)
                        )
                    )
                    user_assert(
                        issubclass(default, CompiledType),
                        'A type is required for parameter {} (got {})'.format(
                            kw, default
                        )
                    )

                    self._add_argument(names.Name.from_lower(kw), default)

                # Now that we have placeholder for all explicit arguments (i.e.
                # only the ones the user defined), we can expand the lambda
                # into a real AbstractExpression.
                explicit_args = self.argument_vars[1:]
                self.expr = assert_type(self.expr(*explicit_args),
                                        AbstractExpression)

        with self.bind():
            self.expr.prepare()

    def freeze(self):
        """
        Run the "freeze" pass on the expression associated to this property.

        Afterwards, it will not be possible anymore to build
        AbstractExpressions trees out of the overloaded operators of the
        AbstractExpression instances in self.expr. See Frozable for more
        details.
        """
        if self.expr:
            self.expr.freeze()

    def compute(self, owner_type):
        """
        Compute information related to dispatching for properties.

        :param ASTNode owner_type: The type on which this property was
            declared.
        """
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

    @property
    def explicit_arguments(self):
        """
        Return the subset of "self.arguments" that are to be passed explicitely
        when invoking this property.

        :rtype: list[(names.Name, CompiledType, None|str)]
        """
        # Strip the implicit "Lex_Env" argument
        return self.arguments[1:]


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

        def render(self):
            assert self.type, "Local var must have type before it is rendered"
            return "{} : {};".format(
                self.name.camel_with_underscores,
                self.type.name().camel_with_underscores
            )

    def create(self, name, type):
        """
        This getattr override allows you to declare local variables in
        templates via the syntax::

            import langkit.compiled_types
            vars = LocalVars()
            var = vars('Index', langkit.compiled_types.LongType)

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


class BuiltinCallExpr(ResolvedExpression):
    """
    Convenience resolved expression that models a call to a function on the
    Ada side of things.
    """

    def __init__(self, name, type, exprs):
        """
        :param names.Name|str name: The name of the procedure to call.
        :param CompiledType type: The return type of the function call.
        :param [ResolvedExpression] exprs: A list of expressions that
            represents the arguments to the function call.
        """
        self.name = names.Name.get(name)
        self.exprs = exprs
        self._type = type

    @property
    def type(self):
        return self._type

    def render_pre(self):
        return "\n".join(expr.render_pre() for expr in self.exprs)

    def render_expr(self):
        return "{} ({})".format(
            self.name.camel_with_underscores, ", ".join(
                expr.render_expr() for expr in self.exprs
            )
        )


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
    return (
        expr == Self or (isinstance(expr, FieldAccess) and
                         expr.receiver == Self and
                         not expr.arguments)
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
