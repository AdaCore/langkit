import inspect

from langkit import names
from langkit.compiled_types import ASTNode, Struct, BoolType
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, ResolvedExpression, construct,
    render, Property, LiteralExpr, UnreachableExpr
)
from langkit.expressions.boolean import Eq, If, Not
from langkit.expressions.envs import Env
from langkit.utils import assert_type, col, user_assert, Colors


class Cast(AbstractExpression):
    """
    Expression that is the result of casting an ASTNode subclass value
    to another subclass.
    """

    class Expr(ResolvedExpression):
        def __init__(self, expr, astnode, do_raise=False, result_var=None):
            """
            :type expr: ResolvedExpr
            :type astnode: ASTNode
            :type do_raise: bool

            :param ResolvedExpr result_var: If provided, the cast will use it
                to store the cast result. Othewise, a dedicated variable is
                created for this.
            """
            self.do_raise = do_raise
            self.expr = expr
            self.astnode = astnode

            p = Property.get()
            self.expr_var = p.vars.create('Cast_Expr', self.expr.type)
            self.result_var = (
                result_var or p.vars.create('Cast_Result', astnode)
            )
            assert self.result_var.type == astnode, (
                'Cast temporaries must have exactly the cast type: {} expeced'
                ' but got {} instead'.format(
                    astnode.name().camel,
                    self.result_var.type.name().camel
                )
            )

        @property
        def type(self):
            return self.astnode

        def render_pre(self):
            # Before actually downcasting an access to an AST node, add a type
            # check so that we raise a Property_Error if it's wrong.
            return render('properties/type_safety_check_ada', expr=self)

        def render_expr(self):
            return self.result_var.name

        def __repr__(self):
            return '<Cast.Expr {}>'.format(self.astnode.name().camel)

    def __init__(self, expr, astnode, do_raise=False):
        """
        :param AbstractExpression expr: Expression on which the cast is
            performed.
        :param ASTNode astnode: ASTNode subclass to use for the cast.
        :param bool do_raise: Whether the exception should raise an
            exception or return null when the cast is invalid.
        """
        assert astnode.matches(ASTNode)
        self.expr = expr
        self.astnode = astnode
        self.do_raise = do_raise

    def construct(self):
        """
        Construct a resolved expression that is the result of casting a AST
        node.

        :rtype: CastExpr
        """
        return Cast.Expr(construct(
            self.expr, lambda t: self.astnode.matches(t),
            'Cannot cast {{}} to {}: only downcasting is allowed'.format(
                self.astnode
            )
        ), self.astnode)


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
            return '({}, Is_Null => False)'.format(
                ', '.join('{} => {}'.format(name.camel_with_underscores,
                                            expr.render_expr())
                          for name, expr in self._iter_ordered())
            )

        def __repr__(self):
            return '<New.Expr {}>'.format(self.struct_type.name().camel)

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
        :rtype: NewExpr
        """

        required_fields = {f.name: f for f in self.struct_type.get_fields()}

        # First construct a sequence of field names to values
        fields = [(names.Name.from_lower('f_' + n), v)
                  for n, v in self.field_values.items()]

        provided_fields = {name: construct(
            value, required_fields[name].type,
            'Wrong type for field {}: got {{}} but expected {{}}'.format(name)
        ) for name, value in fields}

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

        def __init__(self, receiver_expr, property, arguments):
            """
            :param ResolvedExpression receiver_expr: The receiver of the field
                access.
            :param Property|Field property: The accessed property or field.
            :type arguments: list[ResolvedExpression] arguments
            """
            self.receiver_expr = receiver_expr
            self.property = property
            self.arguments = arguments
            self.simple_field_access = False

            # TODO: For the moment we use field accesses in the environments
            # code, which doesn't have a property context and hence local
            # variables instance. At a later stage we'll want to get rid of
            # that limitation by binding the local variables separately from
            # the current property.

            p = Property.get()

            if p:
                self.result_var = p.vars.create('Internal_Pfx',
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
            result = [render('properties/null_safety_check_ada',
                             expr=self.receiver_expr,
                             result_var=self.result_var)]

            result.extend(arg.render_pre() for arg in self.arguments)
            return '\n'.join(result)

        def render_expr(self):
            if self.simple_field_access:
                prefix = self.receiver_expr.render()
            else:
                prefix = self.result_var.name
            ret = "{}.{}".format(prefix, self.property.name)

            # Sequence of tuples: (formal name, expression) for each argument
            # to pass.
            args = []

            # If we're calling a property, then pass the currently bound
            # lexical environment as parameter.
            if isinstance(self.property, Property):
                args.append((Property.env_arg_name, str(Env._name)))

            # Then add the explicit arguments
            if isinstance(self.property, Property):
                for actual, (formal_name, formal_type, _) in zip(
                    self.arguments, self.property.explicit_arguments
                ):
                    expr = actual.render_expr()

                    # The only case in which actual and formal types can be
                    # differents is when using object derivation. In this case,
                    # we have to introduce explicit view conversions in Ada.
                    if actual.type != formal_type:
                        expr = '{} ({})'.format(formal_type.name(), expr)

                    args.append((formal_name, expr))

            if args:
                ret += " ({})".format(', '.join(
                    '{} => {}'.format(name, value)
                    for name, value in args
                ))

            return ret

    def __init__(self, receiver, field, arguments=()):
        """
        :param AbstractExpression receiver: Expression on which the field
            access was done.

        :param str field: The name of the field that is accessed.

        :param arguments: Assuming field is a property that takes arguments,
            these are passed to it.
        :type arguments: list[AbstractExpression]
        """
        self.receiver = receiver
        self.field = field
        self.arguments = arguments

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

        # Check that this property actually accepts these arguments and that
        # they are correctly typed.
        user_assert(
            len(self.arguments) == len(to_get.explicit_arguments),
            'Invalid number of arguments in the call to {}:'
            ' {} expected but got {}'.format(
                to_get.qualname,
                len(to_get.explicit_arguments),
                len(self.arguments),
            )
        )
        arg_exprs = map(construct, self.arguments)
        exprs_and_formals = zip(arg_exprs, to_get.explicit_arguments)
        for i, (actual, formal) in enumerate(exprs_and_formals, 1):
            formal_name, formal_type, _ = formal
            user_assert(
                actual.type.matches(formal_type),
                'Invalid {} actual (#{}) for {}:'
                ' expected {} but got {}'.format(
                    formal_name, i,
                    to_get.qualname,
                    formal_type.name().camel,
                    actual.type.name().camel,
                )
            )

        ret = FieldAccess.Expr(receiver_expr, to_get, arg_exprs)
        return ret

    def __call__(self, *args):
        """
        Build a new FieldAccess instance with "args" as arguments.

        :param args: List of arguments for the call.
        :type args: list[AbstractExpression]
        :rtype: FieldAccess
        """
        # TODO: at some point, it could be useful to allow passing arguments by
        # keywords.

        assert not self.arguments, 'Cannot call the result of a property'
        return FieldAccess(self.receiver, self.field, args)

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

        def __repr__(self):
            return '<IsA.Expr {}>'.format(', '.join(
                astnode.name().camel for astnode in self.astnodes
            ))

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


class Match(AbstractExpression):
    """
    Expression that performs computations that depend on a AST node type match.

    For instance::

        expression.match(
            lambda n=SomeNodeType: t.foo,
            lambda n=SomeOtherType: t.bar,
        )

    Will return t.foo for a node "t" that is a SomeNodeType.
    """

    class Expr(ResolvedExpression):
        pass

    def __init__(self, expr, *matchers):
        """
        :param AbstractExpression expr: The expression to match.

        :param matchers: Sequence of functions that return the expressions to
            evaluate depending on the match. There are two cases.

            1.  Either they all must accept one optional argument whose default
                values are the types to match. In this case, the set of types
                to match must cover all possible values. For instance, given
                the following type tree::

                    Statement
                    Expr:
                        BinaryOp:
                            PlusOp
                            MinusOp
                        Call

                Then given an "expr" parameter that yields an Expr value, the
                follwing matchers are valid::

                    Match(expr,
                          lambda e=PlusOp: X,
                          lambda e=MinusOp: Y,
                          lambda e=Call: Z)
                    Match(expr,
                          lambda e=BinaryOp: X,
                          lambda e=Call: Y)

                But the following are not::

                    # MinusOp not handled:
                    Match(expr,
                          lambda e=PlusOp: X,
                          lambda e=Call: Z)

                    # Expr nodes can never be Statement
                    Match(expr,
                          lambda e=BinaryOp: X,
                          lambda e=Call: Y,
                          lambda e=Statement: Z)

            2.  Otherwise, all but one must accept such an optional argument.
                The only other one must accept a mandatory argument and will
                match the remaining cases. For
                instance::

                    Match(expr,
                          lambda e=BinaryOp: X,
                          lambda e: Y)
        :type matchers: list[() -> AbstractExpression]
        """
        self.matched_expr = expr
        self.matchers_functions = matchers

        self.matchers = None
        """
        Dictionary of matchers. Built in the "prepare" pass.
        :type: dict[CompiledType, (AbstractVariable, AbstractExpression)]
        """

    def do_prepare(self):
        # TODO: implement complete input validation
        self.matchers = {}

        for i, match_fn in enumerate(self.matchers_functions):
            argspec = inspect.getargspec(match_fn)
            user_assert(len(argspec.args) == 1 and
                        not argspec.varargs and
                        not argspec.keywords and
                        (not argspec.defaults or len(argspec.defaults) < 2),
                        'Invalid matcher lambda')

            if argspec.defaults:
                match_type = argspec.defaults[0]
                user_assert(inspect.isclass(match_type) and
                            issubclass(match_type, ASTNode) and
                            match_type != ASTNode,
                            'Invalid matching type: {}'.format(match_type))
            else:
                match_type = None

            match_var = AbstractVariable(
                names.Name('Match_{}'.format(i)),
                type=match_type,
                create_local=True
            )
            self.matchers[match_type] = (match_var, match_fn(match_var))

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: ResolvedExpression
        """
        matched_expr = construct(self.matched_expr)
        user_assert(issubclass(matched_expr.type, ASTNode),
                    'Match expressions can only work on AST nodes')

        # The default matcher (if any) matches the most general type, which is
        # the input type.
        default_matcher = self.matchers.get(None)
        if default_matcher:
            match_var, match_expr = default_matcher
            match_var.set_type(matched_expr.type)

        # We are going to expand this Match expression into a chain of if/else
        # constructs that will test the matchers one by one. We have to find an
        # other so that the more specific matchers go first. For now, sort by
        # ASTNode subclassing depth. In the list below, the most general
        # matchers will appear first.
        match_list = sorted(
            (
                (0 if match_var.type is None else
                 len(list(match_var.type.get_inheritance_chain()))),
                construct(match_var),
                construct(expr)
            )
            for match_var, expr in self.matchers.values()
        )

        # Compute the return type as the unification of all branches
        _, _, expr = match_list[-1]
        rtype = expr.type
        for _, _, expr in match_list:
            rtype = expr.type.unify(rtype)

        # This is the expression execution will reach if we have a bug in our
        # code (i.e. if matchers did not cover all cases).
        result = UnreachableExpr(rtype)

        # Wrap this "failing" expression with all the cases to match in the
        # appropriate order, so that in the end the most specific matchers are
        # tested first.
        for _, match_var, expr in match_list:
            casted = Cast.Expr(matched_expr,
                               match_var.type,
                               result_var=match_var)
            guard = Not.Expr(Eq.Expr(casted, LiteralExpr('null', casted.type)))
            result = If.Expr(guard, expr, result, rtype)

        return result
