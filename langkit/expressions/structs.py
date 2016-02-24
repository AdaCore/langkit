from langkit import names
from langkit.compiled_types import ASTNode, Struct, BoolType
from langkit.expressions.base import (
    AbstractExpression, ResolvedExpression, construct, render, Property,
    LiteralExpr
)
from langkit.expressions.boolean import Eq
from langkit.expressions.envs import Env
from langkit.utils import assert_type, col, Colors


class Cast(AbstractExpression):
    """
    Expression that is the result of casting an ASTNode subclass value
    to another subclass.
    """

    class Expr(ResolvedExpression):
        def __init__(self, expr, astnode):
            """
            :param ResolvedExpr expr: Expression on which the cast is
                performed.
            :param ASTNode astnode: ASTNode subclass to use for the cast.
            """
            self.expr = expr
            self.astnode = astnode

            p = Property.get()
            self.result_var = p.vars(names.Name('Base'), self.expr.type)

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

    def __init__(self, expr, astnode):
        """
        :param langkit.expressions.base.AbstractExpression expr: Expression
            on which the cast is performed.
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
                expr.type.name().camel, self.astnode.name().camel
            )
        )
        return Cast.Expr(expr, self.astnode)


class IsNull(AbstractExpression):
    """
    Abstract expression to test whether an AST node is null.
    """

    def __init__(self, expr):
        """
        :param langkit.expressions.base.AbstractExpression expr: Expression on
            which the test is performed.
        """
        self.expr = expr

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: EqExpr
        """
        return Eq.Expr(
            construct(self.expr, ASTNode), LiteralExpr('null', ASTNode)
        )


class New(AbstractExpression):
    """
    Abstract expression to create Struct values.
    """

    class Expr(ResolvedExpression):
        """
        Resolved expression to create Struct values.
        """

        def __init__(self, struct_type, assocs):
            self.struct_type = struct_type
            self.assocs = assocs

        @property
        def type(self):
            return self.struct_type

        def _iter_ordered(self):
            return ((k, self.assocs[k]) for k in sorted(self.assocs))

        def render_pre(self):
            return '\n'.join(expr.render_pre()
                             for _, expr in self._iter_ordered())

        def render_expr(self):
            return '({})'.format(
                ', '.join('{} => {}'.format(name.camel_with_underscores,
                                            expr.render_expr())
                          for name, expr in self._iter_ordered())
            )

    def __init__(self, struct_type, **field_values):
        """
        :param langkit.compiled_types.Struct struct_type: Struct subclass (but
            not an ASTNode subclass) for the struct type this expression must
            create.
        :param dict[str, langkit.expressions.base.AbstractExpression] fields:
            Values to assign to the fields for the created struct value.
        """
        assert (issubclass(struct_type, Struct) and
                not issubclass(struct_type, ASTNode))
        self.struct_type = struct_type
        self.field_values = field_values

    def construct(self):
        """
        :rtype: NewExpr
        """
        provided_fields = {
            names.Name.from_lower('f_' + name): value.construct()
            for name, value in self.field_values.iteritems()
        }
        required_fields = {f.name: f for f in self.struct_type.get_fields()}

        # Make sure the provided set of fields matches the one the struct
        # needs.
        def complain_if_not_empty(name_set, message):
            assert not name_set, '{}: {}'.format(
                message, ', '.join(name.lower for name in name_set)
            )

        complain_if_not_empty(
            set(required_fields) - set(provided_fields),
            'Values are missing for {} fields'.format(
                self.struct_type.name().camel
            )
        )
        complain_if_not_empty(
            set(provided_fields) - set(required_fields),
            'Unknown {} fields'.format(self.struct_type.name().camel)
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

        return New.Expr(self.struct_type, provided_fields)


class FieldAccess(AbstractExpression):
    """
    Abstract expression that is the result of a field access expression
    evaluation.
    """

    class Expr(ResolvedExpression):
        """
        Resolved expression that represents a field access in generated code.
        """

        def __init__(self, receiver_expr, property):
            """
            :param langkit.expressions.base.ResolvedExpression receiver_expr:
                The receiver of the field access.
            :param Property|Field property: The accessed property or field.
            """
            self.receiver_expr = receiver_expr
            self.property = property
            self.simple_field_access = False

            # TODO: For the moment we use field accesses in the environments
            # code, which doesn't have a property context and hence local
            # variables instance. At a later stage we'll want to get rid of
            # that limitation by binding the local variables separately from
            # the current property.

            p = Property.get()
            if p:
                self.result_var = p.vars(names.Name('Internal_Pfx'),
                                         self.receiver_expr.type)
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

            # If we're calling a property, then pass the currently bound
            # lexical environment as parameter.
            if isinstance(self.property, Property):
                ret += " ({})".format(Env._name)

            return ret

    def __init__(self, receiver, field):
        """
        :param langkit.expressions.base.AbstractExpression receiver: Expression
            on which the field access was done.
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

        ret = FieldAccess.Expr(receiver_expr, to_get)
        return ret

    def __repr__(self):
        return "<FieldAccess {} {}>".format(self.receiver, self.field)


class IsA(AbstractExpression):
    """
    Expression that is the result of testing the kind of a node.
    """

    class Expr(ResolvedExpression):
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
        return IsA.Expr(expr, self.astnodes)
