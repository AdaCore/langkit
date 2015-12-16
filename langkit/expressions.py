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

from contextlib import contextmanager
from itertools import count

import compiled_types
import names
from utils import Colors, col, common_ancestor


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

    @Frozable.protect
    def __getattr__(self, attr):
        """
        Depending on "attr", return either an AbstractExpression or an
        AbstractExpression constructor.

        :param str attr: Name of the field to access.
        :rtype: AbstractExpression|function
        """

        # Constructors for operations with attribute-like syntax

        def _build_all(predicate, var=None):
            return Quantifier(Quantifier.ALL, self, predicate, var)

        def _build_any(predicate, var=None):
            return Quantifier(Quantifier.ANY, self, predicate, var)

        def _build_cast(astnode):
            return Cast(self, astnode)

        def _build_filter(filter):
            return Map(None, Vars, self, filter)

        def _build_is_a(astnode):
            return IsA(self, astnode)

        def _build_mapcat(expr, filter=None, var=None):
            return Map(var, expr, self, filter, concat=True)

        def _build_map(expr, filter=None, var=None):
            return Map(var, expr, self, filter)

        CONSTRUCTORS = {
            'all':    _build_all,
            'any':    _build_any,
            'cast':   _build_cast,
            'filter': _build_filter,
            'is_a':   _build_is_a,
            'map':    _build_map,
            'mapcat': _build_mapcat,
        }

        try:
            return CONSTRUCTORS[attr]
        except KeyError:
            return FieldAccess(self, attr)

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
        lhs = self.lhs.construct()
        rhs = self.rhs.construct()
        assert lhs.type == compiled_types.BoolType
        assert rhs.type == compiled_types.BoolType

        if self.kind == self.AND:
            then = rhs
            else_then = LiteralExpr('False', compiled_types.BoolType)
        else:
            then = LiteralExpr('True', compiled_types.BoolType)
            else_then = rhs
        return IfExpr(lhs, then, else_then, compiled_types.BoolType)


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
        assert issubclass(astnode, compiled_types.ASTNode)
        self.expr = expr
        self.astnode = astnode

    def construct(self):
        """
        Construct a resolved expression that is the result of casting a AST
        node.

        :rtype: CastExpr
        """
        expr = self.expr.construct()
        assert issubclass(expr.type, compiled_types.ASTNode)
        return CastExpr(expr, self.astnode)


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
        assert cond.type == compiled_types.BoolType

        then = self.then.construct()
        else_then = self.else_then.construct()

        if issubclass(then.type, compiled_types.ASTNode):
            assert issubclass(else_then.type, compiled_types.ASTNode)
            rtype = common_ancestor(then.type, else_then.type)
        else:
            assert then.type == else_then.type
            rtype = then.type

        return IfExpr(cond, then, else_then, rtype)


class IsA(AbstractExpression):
    """
    Abstract expression that is the result of testing the kind of a node.
    """

    def __init__(self, expr, astnode):
        """
        :param AbstractExpression astnode: Expression on which the test is
            performed.
        :param ASTNode astnode: ASTNode subclass to use for the test.
        """
        assert issubclass(astnode, compiled_types.ASTNode)
        self.expr = expr
        self.astnode = astnode

    def construct(self):
        """
        Construct a resolved expression that is the result of testing the kind
        of a node.

        :rtype: IsAExpr
        """
        expr = self.expr.construct()
        assert issubclass(expr.type, compiled_types.ASTNode)
        return IsAExpr(expr, self.astnode)


class CollectionExpression(AbstractExpression):
    """
    Base class to provide common code for abstract expressions working on
    collections.
    """

    def __init__(self, expr, collection, induction_var=None):
        """
        :param induction_var: Variable to use in "expr".
        :type induction_var: None|InductionVariable
        :param AbstractExpression expr: Expression to evaluate for each item in
            "collection".
        :param AbstractExpression collection: Collection on which this map
            operation works.
        """
        super(CollectionExpression, self).__init__()
        self.expr = expr
        self.collection = collection
        self.induction_var = induction_var

    def construct_collection(self):
        """
        Construct the collection expression and determine the type of the
        induction variable.

        :rtype: (ResolvedExpression, langkit.compiled_types.CompiledType)
        """
        collection_expr = self.collection.construct()
        assert issubclass(collection_expr.type,
                          (compiled_types.ASTList,
                           compiled_types.ArrayType)), (
            'Map cannot iterate on {}, which is not a collection'
        ).format(collection_expr.type)

        return (collection_expr, collection_expr.type.element_type)

    def bind_induction_var(self, type):
        """
        Return a context manager to bind temporarily the induction variable to
        a specific type. Also create the induction variable first if there is
        none.

        :param langkit.compiled_types.CompiledType type: Type for the induction
            variable.
        """
        default_induction_var = not self.induction_var
        if default_induction_var:
            self.induction_var = Vars.create_default()
        return self.induction_var.vars.bind(self.induction_var, type)


class Map(CollectionExpression):
    """
    Abstract expression that is the result of a map expression evaluation.
    """

    def __init__(self, induction_var, expr, collection, filter_expr=None,
                 concat=False):
        """
        See CollectionExpression for the other parameters.

        :param filter_expr: If provided, a boolean expression that says whether
            to include or exclude an item from the collection.
        :type filter: None|AbstractExpression
        :param bool concat: If true, "expr" must return arrays, and this
            expression returns the concatenation of all the arrays "expr"
            returns.
        """
        super(Map, self).__init__(expr, collection, induction_var)
        self.filter_expr = filter_expr
        self.concat = concat

    def construct(self):
        """
        Construct a resolved expression for this map operation.

        :rtype: MapExpr
        """
        collection_expr, elt_type = self.construct_collection()

        # Now construct the expressions that rely on the induction variable
        with self.bind_induction_var(elt_type):
            ind_var = self.induction_var.construct()
            expr = self.expr.construct()
            assert (not self.concat or
                    issubclass(expr.type, (compiled_types.ASTList,
                                           compiled_types.ArrayType))), (
                'Cannot mapcat with expressions returning {} values'
                ' (collections expected instead)'
            ).format(expr.name())

            if self.filter_expr:
                filter_expr = self.filter_expr.construct()
                assert filter_expr.type == compiled_types.BoolType
            else:
                filter_expr = None

        return MapExpr(ind_var, expr, collection_expr, filter_expr,
                       self.concat)


class Quantifier(CollectionExpression):
    """
    Abstract expression that tests a predicate over the items of a collection.
    """

    # Available quantifier kinds
    ALL = 'all'
    ANY = 'any'

    def __init__(self, kind, collection, predicate, induction_var):
        """
        See CollectionExpression for the other parameters.

        :param str kind: Quantifier kind. ALL that checks "predicate" holds on
            all elements in "collection" while ANY checks that it holds on at
            least one of them.
        :param AbstractExpression predicate: Boolean expression to evaluate on
            elements in "collection".
        """
        super(Quantifier, self).__init__(predicate, collection, induction_var)
        assert kind in (self.ALL, self.ANY)
        self.kind = kind

    def construct(self):
        """
        Construct a resolved expression for this quantifier expression.

        :rtype: QuantifierExpr
        """
        collection_expr, elt_type = self.construct_collection()

        # Now construct the expressions that rely on the induction variable
        with self.bind_induction_var(elt_type):
            ind_var = self.induction_var.construct()
            expr = self.expr.construct()

        return QuantifierExpr(self.kind, collection_expr, expr, ind_var)


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

        receiver_expr = self.receiver.construct()
        ":type: ResolvedExpression"

        # For the moment, this can work only on expressions deriving from
        # ASTNode.
        receiver_type = receiver_expr.type
        ":type: langkit.compiled_types.ASTNode"

        to_get = receiver_type.get_abstract_fields_dict().get(self.field, None)
        ":type: AbstractNodeField"

        # If still not found, there's a problem
        assert to_get, col("Type {} has no '{}' field or property".format(
            receiver_expr.type.__name__, self.field
        ), Colors.FAIL)

        ret = FieldAccessExpr(receiver_expr, to_get)
        return ret

    def __repr__(self):
        return "<FieldAccess {} {}>".format(self.receiver, self.field)


class PlaceHolderSingleton(AbstractExpression):
    """
    Abstract expression that is an entry point into the expression DSL.

    If you have an instance of a PlaceHolder, you can use it to construct
    abstract expressions.

    You can then resolve the constructed expressions by:
    - Binding the type of the PlaceHolder instance via a call to the bind
      context manager.
    - Calling construct on the PlaceHolder.
    """

    def __init__(self, name):
        """
        :param str name: The name of the PlaceHolder variable.
        """
        self.name = name
        self._type = None

    @contextmanager
    def bind(self, type):
        """
        Bind the type of this placeholder.

        :param langkit.compiled_types.CompiledType type: Type parameter. The
            type of this placeholder.
        """
        self._type = type
        yield
        self._type = None

    def construct(self):
        return VarExpr(self._type, self.name)

    @property
    def type(self):
        return self._type

    def __repr__(self):
        return "<PlaceHolder {}>".format(self.name)


class InductionVariable(AbstractExpression):
    """
    Abstract expression that represents an induction variable in loops exprs.
    """

    def __init__(self, vars, name):
        """
        :param InductionVariables vars: Factory from which "self" comes.
        :param names.Name name: The name of the induction variable.
        """
        self.vars = vars
        self.name = name

    def __repr__(self):
        return '<InductionVariable {}>'.format(self.name)

    def construct(self):
        return VarExpr(self.vars.get_type(self), self.name)


class DefaultInductionVariableSingleton(AbstractExpression):
    """
    Abstract expression that represents an anonymous induction variable.
    """

    def construct(self):
        return Vars.default.construct()


class InductionVariablesSingleton(object):
    """
    Factory for InductionVariable instances.
    """

    def __init__(self):
        self.vars = {}
        self.bind_stack = []

    def create_default(self):
        return self._create_var(names.Name('Item'))

    def _create_var(self, name):
        """
        :type name: names.Name
        :rtype: InductionVariable
        """
        try:
            return self.vars[name]
        except KeyError:
            var = InductionVariable(self, name)
            self.vars[name] = var
            return var

    def __getattr__(self, name):
        return self._create_var(names.Name('I') + names.Name.from_lower(name))

    @contextmanager
    def bind(self, var, type):
        """
        Bind the type of this placeholder.

        :param var: Variable to bind as the default induction variable.
        :type var: langkit.compiled_types.CompiledType
        """
        self.bind_stack.append((var, type))
        yield
        self.bind_stack.pop()

    @property
    def default(self):
        var, type = self.bind_stack[-1]
        return var

    def get_type(self, var):
        for v, t in reversed(self.bind_stack):
            if var == v:
                return t
        else:
            raise KeyError('Unknown induction variable: {}'.format(var))


Self = PlaceHolderSingleton("Self")
Vars = InductionVariablesSingleton()
Var = DefaultInductionVariableSingleton()


def render(*args, **kwargs):
    return compiled_types.render(*args, property=Property.get(), Self=Self,
                                 **kwargs)


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


class VarExpr(ResolvedExpression):
    """
    Resolved expression that represents a variable in generated code.
    """

    def __init__(self, type, name):
        assert issubclass(type, compiled_types.CompiledType)
        self._type = type
        self.name = name

    @property
    def type(self):
        return self._type

    def render_expr(self):
        return self.name


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

    @property
    def type(self):
        return self.property.type

    def __repr__(self):

        return "<FieldAccessExpr {} {} {}>".format(
            self.receiver_expr, self.property, self.type
        )

    def render_expr(self):
        return "{}.{}".format(self.receiver_expr.render(), self.property.name)


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

    @property
    def type(self):
        return self.astnode

    def render_pre(self):
        return self.expr.render_pre()

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
        return compiled_types.render('properties/if_ada', expr=self)

    def render_expr(self):
        return self.result_var.name.camel_with_underscores


class IsAExpr(ResolvedExpression):
    """
    Resolved expression that represents a kind test of a node.
    """

    def __init__(self, expr, astnode):
        """
        :param ResolvedExpr expr: Expression on which the test is performed.
        :param ASTNode astnode: ASTNode subclass to use for the test.
        """
        self.expr = expr
        self.astnode = astnode

    @property
    def type(self):
        return compiled_types.BoolType

    def render_pre(self):
        return self.expr.render_pre()

    def render_expr(self):
        return "{}.all in {}_Type'Class".format(
            self.expr.render_expr(),
            self.astnode.name().camel_with_underscores
        )


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

    def __init__(self, induction_var, expr, collection, filter=None,
                 concat=False):
        """
        :param InductionVariable induction_var: Variable to use in "expr".
        :param ResolvedExpression expr: Expression to evaluate for each item in
            "collection".
        :param ResolvedExpression collection: Collection on which this map
            operation works.
        :param filter: If provided, a boolean expression that says whether to
            include or exclude an item from the collection.
        :type filter: None|ResolvedExpression
        :param bool concat: If true, "expr" must return arrays, and this
            expression returns the concatenation of all the arrays "expr"
            returns.
        :return:
        """
        self.induction_var = induction_var
        self.expr = expr
        self.collection = collection
        self.filter = filter
        self.concat = concat

        element_type = (self.expr.type.element_type
                        if self.concat else
                        self.expr.type)
        self._type = compiled_types.array_type(element_type)
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
        return compiled_types.render('properties/map_ada', map=self,
                                     Name=names.Name)

    def render_expr(self):
        return self.array_var.name.camel_with_underscores


class QuantifierExpr(ResolvedExpression):
    """
    Resolved expression that represents a qualifier expression in the generated
    code.
    """

    def __init__(self, kind, collection, expr, induction_var):
        """
        :param str kind: Kind for this quantifier expression. 'all' will check
            that all items in "collection" fullfill "expr" while 'any' will
            check that at least one of them does.
        :param ResolvedExpression expr: Expression to evaluate for each item in
            "collection".
        :param ResolvedExpression collection: Collection on which this map
            operation works.
        :param ResolvedExpression expr: A boolean expression to evaluate on the
            collection's items.
        :param induction_var: Variable to use in "expr".
        :type induction_var: ResolvedExpression
        """
        self.kind = kind
        self.collection = collection
        self.expr = expr
        self.induction_var = induction_var

        self.result_var = Property.get().vars(names.Name('Result'),
                                              compiled_types.BoolType,
                                              create_unique=False)

    @property
    def type(self):
        return compiled_types.BoolType

    def render_pre(self):
        return compiled_types.render(
            'properties/quantifier_ada',
            quantifier=self,
            ALL=Quantifier.ALL,
            ANY=Quantifier.ANY,
            Name=names.Name
        )

    def render_expr(self):
        return self.result_var.name.camel_with_underscores


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
            assert isinstance(name, names.Name)
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


class AbstractNodeData(object):
    """
    This class defines an abstract base class for fields and properties on
    AST nodes.

    It defines the basis of what is needed to bind them in other languages
    bindings: a type and a name.
    """

    # Hack: the field declarations order in AST nodes matters.  The simple and
    # very handy syntax we use here for such declarations doesn't preserve this
    # order in Python2, however.  Waiting for the move to Python3, we use a
    # hack here: the following counter will help us to recover the declaration
    # order (assuming it is the same as the Field instantiation order).
    _counter = iter(count(0))

    def __init__(self):
        self._index = next(self._counter)

    @property
    def type(self):
        """
        Type of the abstract node field.
        :rtype: langkit.compiled_types.CompiledType
        """
        raise NotImplementedError()

    @type.setter
    def type(self, type):
        raise NotImplementedError()

    @property
    def name(self):
        """
        Name of the abstract node field.
        :rtype: names.Name
        """
        raise NotImplementedError()

    @name.setter
    def name(self, name):
        raise NotImplementedError()

    def doc(self):
        """
        Documentation for the abstract node field.
        :rtype: str
        """
        raise NotImplementedError()


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

    def __init__(self, expr, doc=None):
        """
        :param AbstractExpression expr: The expression for the property.
        :param str|None doc: User documentation for this property.
        """

        super(Property, self).__init__()

        self.expr = expr
        self.constructed_expr = None
        self.vars = LocalVars()

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
            "You cannot nest calls to Property.bind context manager"
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
        return self.constructed_expr.type

    def render(self, owner_type):
        """
        Render the given property to generated code.

        :param langkit.compiled_types.CompiledType owner_type: The ast node
            subclass to which this property is bound.
        :rtype: basestring
        """
        with Self.bind(owner_type):
            with self.bind():
                self.expr.freeze()
                self.constructed_expr = self.expr.construct()
                with names.camel_with_underscores:
                    self.prop_decl = render('properties/decl_ada')
                    self.prop_def = render('properties/def_ada')

    @property
    def name(self):
        """
        Return the name of the property, namely P_ + the name defined by the
        user.
        :rtype: names.Name
        """
        assert self._name
        from names import Name
        return Name("P") + self._name

    @name.setter
    def name(self, name):
        assert isinstance(name, names.Name)
        self._name = name

    def doc(self):
        return self._doc
