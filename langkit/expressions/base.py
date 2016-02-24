from contextlib import contextmanager
from copy import copy
from functools import partial

from langkit import names
from langkit.compiled_types import (
    LongType, CompiledType, render as ct_render, AbstractNodeData, BoolType
)
from langkit.utils import memoized, assert_type


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

        from langkit.expressions.collections import (
            Quantifier, Map, Contains
        )
        from langkit.expressions.structs import Cast, IsA, IsNull
        from langkit.expressions.envs import EnvGet, EnvBind
        from langkit.expressions.boolean import Eq, BinaryBooleanOperator
        from langkit.expressions.collections import CollectionGet

        # Using partial allows the user to be able to use keyword arguments
        # defined on the expressions constructors.
        return {
            'all':            partial(Quantifier, Quantifier.ALL, self),
            'any':            partial(Quantifier, Quantifier.ANY, self),
            'cast':           partial(Cast, self, do_raise=False),
            'cast_or_raise':  partial(Cast, self, do_raise=True),
            'contains':       partial(Contains, self),
            'equals':         partial(Eq, self),
            'filter':         partial(Map, self, lambda x: x),
            'take_while':     partial(Map, self, lambda x: x, None, False),
            'is_a':           partial(IsA, self),
            'map':            partial(Map, self),
            'mapcat':         partial(Map, self, concat=True),
            'get':            partial(EnvGet, self),
            'resolve_unique': partial(EnvGet, self, resolve_unique=True),
            'at':             partial(CollectionGet, self),
            'eval_in_env':    partial(EnvBind, self),
            'is_null':        IsNull(self),
            'or_else':        partial(BinaryBooleanOperator,
                                      BinaryBooleanOperator.OR, self),
            'and_then':       partial(BinaryBooleanOperator,
                                      BinaryBooleanOperator.AND, self)
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
        return self.attrs().get(attr, FieldAccess(self, attr))

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

    def __init__(self, name, type=None):
        """
        :param names.Name name: The name of the PlaceHolder variable.
        """
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

    def __repr__(self):
        return "<PlaceHolder {}>".format(self._name)


Self = AbstractVariable(names.Name("Self"))


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

    def __call__(self, name, type):
        """
        This getattr override allows you to declare local variables in
        templates via the syntax::

            import langkit.compiled_types
            vars = LocalVars()
            var = vars('Index', langkit.compiled_types.LongType)

        :param names.Name name: The name of the variable.
        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of the local variable.
        """
        assert isinstance(name, names.Name)
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
