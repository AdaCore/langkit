import inspect

from langkit import names
from langkit.compiled_types import ASTNode, Struct, BoolType, resolve_type
from langkit.diagnostics import Severity, check_source_language
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, LiteralExpr, PropertyDef,
    ResolvedExpression, Self, UnreachableExpr, construct, render
)
from langkit.expressions.boolean import Eq, If, Not
from langkit.expressions.envs import Env
from langkit.utils import assert_type, TypeSet


class Cast(AbstractExpression):
    """
    Expression that is the result of casting an ASTNode subclass value
    to another subclass.
    """

    class Expr(ResolvedExpression):
        def __init__(self, expr, astnode, do_raise=False, result_var=None):
            """
            :type expr: ResolvedExpression
            :type astnode: ASTNode
            :type do_raise: bool

            :param ResolvedExpr result_var: If provided, the cast will use it
                to store the cast result. Otherwise, a dedicated variable is
                created for this.
            """
            self.do_raise = do_raise
            self.expr = expr
            self.astnode = astnode

            p = PropertyDef.get()
            self.expr_var = p.vars.create('Cast_Expr', self.expr.type)
            self.result_var = (
                result_var or p.vars.create('Cast_Result', astnode)
            )
            assert self.result_var.type == astnode, (
                'Cast temporaries must have exactly the cast type: {} expected'
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
        super(Cast, self).__init__()
        self.expr = expr
        self.astnode = astnode
        self.do_raise = do_raise

    def do_prepare(self):
        self.astnode = resolve_type(self.astnode)
        check_source_language(self.astnode.matches(ASTNode), (
            "One can only cast to an ASTNode subtype"
        ))

    def construct(self):
        """
        Construct a resolved expression that is the result of casting a AST
        node.

        :rtype: CastExpr
        """
        expr = construct(
            self.expr,
            lambda t: self.astnode.matches(t) or t.matches(self.astnode),
            'Cannot cast {{expr_type}} to {}: only (up/down)casting is '
            'allowed'.format(
                self.astnode.name().camel
            )
        )
        check_source_language(
            expr.type != self.astnode,
            'Casting to the same type',
            severity=Severity.warning
        )
        return Cast.Expr(expr, self.astnode)


class IsNull(AbstractExpression):
    """
    Abstract expression to test whether an AST node is null.
    """

    def __init__(self, expr):
        """
        :param AbstractExpression expr: Expression on which the test is
            performed.
        """
        super(IsNull, self).__init__()
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
        super(New, self).__init__()
        self.struct_type = struct_type
        self.field_values = field_values

    def do_prepare(self):
        check_source_language(issubclass(self.struct_type, Struct), (
            "Invalid type, expected struct type, got {}".format(
                self.struct_type.name().camel
            )
        ))

        check_source_language(not issubclass(self.struct_type, ASTNode), (
            "Invalid type, expected struct type, got {} which is an "
            "ASTNode".format(
                self.struct_type.name().camel
            )
        ))

    def construct(self):
        """
        :rtype: NewExpr
        """
        # Make sure the provided set of fields matches the one the struct
        # needs.
        def error_if_not_empty(name_set, message):
            check_source_language(not name_set, ('{}: {}'.format(
                message, ', '.join(name for name in name_set)
            )))

        # Create a dict of field names to fields in the struct type
        required_fields = {f._name.lower: f for f in
                           self.struct_type.get_fields()}

        error_if_not_empty(
            set(required_fields) - set(self.field_values.keys()),
            'Values are missing for {} fields'.format(
                self.struct_type.name().camel
            )
        )
        error_if_not_empty(
            set(self.field_values.keys()) - set(required_fields),
            'Extraneous fields for {}'.format(self.struct_type.name().camel)
        )

        # At this stage, we know that the user has only provided fields that
        # are valid for the struct type.
        provided_fields = {names.Name.from_lower('f_' + name): construct(
            value, required_fields[name].type,
            'Wrong type for field {}: expected {{expected}}, '
            'got {{expr_type}}'.format(name)
        ) for name, value in self.field_values.items()}

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

        def __init__(self, receiver_expr, node_data, arguments):
            """
            :param ResolvedExpression receiver_expr: The receiver of the field
                access.

            :param langkit.compiled_types.AbstracNodeData node_data: The
                accessed property or field.

            :param list[ResolvedExpression] arguments: If non-empty, this field
                access will actually be a primitive call.
            """
            self.receiver_expr = receiver_expr
            self.node_data = node_data
            self.arguments = arguments
            self.simple_field_access = False

            # After EnvSpec.create_properties has been run, expressions in
            # environment specifications only allow field accesses. These are
            # not evaluated in a property context, so they cannot create local
            # variables.
            #
            # TODO: in this context, it would still be useful to emit a null
            # check so that we raise a special exception instead of a
            # Storage_Error.

            p = PropertyDef.get()

            if p:
                self.prefix_var = p.vars.create('Pfx', self.receiver_expr.type)
            else:
                self.simple_field_access = True

        @property
        def type(self):
            return self.node_data.type

        def __repr__(self):
            return "<FieldAccessExpr {} {} {}>".format(
                self.receiver_expr, self.node_data, self.type
            )

        def render_pre(self):
            # Before accessing the field of a record through an access, we must
            # check that whether this access is null in order to raise a
            # Property_Error in the case it is.
            return "\n".join([
                render('properties/null_safety_check_ada',
                       expr=self.receiver_expr, result_var=self.prefix_var)
            ] + [arg.render_pre() for arg in self.arguments])

        def render_expr(self):
            if self.simple_field_access:
                prefix = self.receiver_expr.render()
            else:
                prefix = self.prefix_var.name
            ret = "{}.{}".format(prefix, self.node_data.name)

            # If we're calling a property, then pass the currently bound
            # lexical environment as parameter.
            if isinstance(self.node_data, PropertyDef):
                # Sequence of tuples: (formal name, expression) for each
                # argument to pass.
                args = [(PropertyDef.env_arg_name, str(Env._name))]

                # Then add the explicit arguments
                for actual, (formal_name, formal_type, _) in zip(
                    self.arguments, self.node_data.explicit_arguments
                ):
                    expr = actual.render_expr()

                    # The only case in which actual and formal types can be
                    # differents is when using object derivation. In this case,
                    # we have to introduce explicit view conversions in Ada.
                    if actual.type != formal_type:
                        expr = '{} ({})'.format(formal_type.name(), expr)

                    args.append((formal_name, expr))

                ret += " ({})".format(', '.join(
                    '{} => {}'.format(name, value)
                    for name, value in args
                ))

            else:
                # If we reach this point, we know that we are accessing a
                # Struct field: make sure we return the public API type, which
                # may be different from the type thas is stored in the Struct.
                ret = self.node_data.type.extract_from_storage_expr(
                    Self._name.camel_with_underscores,
                    ret
                )

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
        super(FieldAccess, self).__init__()
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
        check_source_language(
            issubclass(receiver_expr.type, Struct),
            '{} values have no field (accessed field was {})'.format(
                receiver_expr.type.name().camel,
                self.field
            )
        )

        to_get = assert_type(
            receiver_expr.type, Struct
        ).get_abstract_fields_dict().get(self.field, None)
        ":type: AbstractNodeField"

        # If still not found, there's a problem
        check_source_language(
            to_get is not None, "Type {} has no '{}' field or property".format(
                receiver_expr.type.__name__, self.field
            )
        )

        check_source_language(
            not to_get.is_internal,
            '{} is for internal use only'.format(to_get.qualname)
        )

        # Check that this property actually accepts these arguments and that
        # they are correctly typed.
        check_source_language(
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
            check_source_language(
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
        super(IsA, self).__init__()
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
            check_source_language(a.matches(expr.type), (
                'When testing the dynamic subtype of an AST node, the type to'
                ' check must be a subclass of the value static type.'
            ))
        return IsA.Expr(expr, self.astnodes)


class Match(AbstractExpression):
    """
    Expression that performs computations that depend on a AST node type match.

    For instance::

        expression.match(
            lambda n=SomeNodeType: n.foo,
            lambda n=SomeOtherType: n.bar,
        )

    Will return n.foo for a node "n" that is a SomeNodeType.
    """

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
        super(Match, self).__init__()
        self.matched_expr = expr
        self.matchers_functions = matchers

        self.matchers = None
        """
        List of matchers. Built in the "prepare" pass.
        :type: list[(CompiledType, AbstractVariable, AbstractExpression)]
        """

    def do_prepare(self):
        self.matchers = []

        for i, match_fn in enumerate(self.matchers_functions):
            argspec = inspect.getargspec(match_fn)
            check_source_language(
                len(argspec.args) == 1 and
                not argspec.varargs and
                not argspec.keywords and
                (not argspec.defaults or len(argspec.defaults) < 2),
                'Invalid matcher lambda'
            )

            if argspec.defaults:
                match_type = resolve_type(argspec.defaults[0])
                check_source_language(
                    issubclass(match_type, ASTNode) and
                    match_type != ASTNode,
                    'Invalid matching type: {}'.format(
                        match_type.name().camel
                    )
                )
            else:
                match_type = None

            match_var = AbstractVariable(
                names.Name('Match_{}'.format(i)),
                type=match_type,
                create_local=True
            )
            self.matchers.append((match_type, match_var, match_fn(match_var)))

    def _check_match_coverage(self, input_type):
        """
        Given some input type for this match expression, make sure the set of
        matchers cover all cases. check_source_language will raise an error if
        it's not the case. Also emit warnings for unreachable matchers.

        :param ASTNode input_type: Type parameter.
        :rtype: None
        """

        type_set = TypeSet()

        for i, (t, _, _) in enumerate(self.matchers, 1):
            t_name = 'default one' if t is None else t.name().camel
            check_source_language(not type_set.include(t or input_type),
                                  'The #{} matcher ({}) is unreachable'
                                  ' as all previous matchers cover all the'
                                  ' nodes it can match'.format(i, t_name),
                                  Severity.warning)

        mm = sorted(type_set.unmatched_types(input_type),
                    key=lambda cls: cls.hierarchical_name())

        check_source_language(
            not mm,
            'The following AST nodes have no handler: {} (all {} subclasses'
            ' require one)'.format(
                ', '.join(t.name().camel for t in mm),
                input_type.name().camel
            )
        )

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: ResolvedExpression
        """
        matched_expr = construct(self.matched_expr)
        check_source_language(issubclass(matched_expr.type, ASTNode),
                              'Match expressions can only work on AST nodes')

        # Yes, the assertion below is what we just checked above, but unlike
        # check_source_language, assert_type provides type information to
        # PyCharm's static analyzer.
        matched_type = assert_type(matched_expr.type, ASTNode)

        constructed_matchers = []

        # Check (i.e. raise an error if no true) the set of matchers is valid:

        # * all matchers must target allowed types, i.e. input type subclasses;
        for t, v, e in self.matchers:
            if t is not None:
                check_source_language(
                    t.matches(matched_expr.type),
                    'Cannot match {} (input type is {})'.format(
                        t.name().camel,
                        matched_expr.type.name().camel
                    )
                )
            else:
                # The default matcher (if any) matches the most general type,
                # which is the input type.
                v.set_type(matched_expr.type)
            constructed_matchers.append((construct(v), construct(e)))

        # * all possible input types must have at least one matcher. Also warn
        #   if some matchers are unreachable.
        self._check_match_coverage(matched_type)

        # Compute the return type as the unification of all branches
        _, expr = constructed_matchers[-1]
        rtype = expr.type
        for _, expr in constructed_matchers:
            rtype = expr.type.unify(rtype)

        # This is the expression execution will reach if we have a bug in our
        # code (i.e. if matchers did not cover all cases).
        result = UnreachableExpr(rtype)

        # Wrap this "failing" expression with all the cases to match in the
        # appropriate order, so that in the end the first matchers are tested
        # first.
        for match_var, expr in reversed(constructed_matchers):
            casted = Cast.Expr(matched_expr,
                               match_var.type,
                               result_var=match_var)
            guard = Not.Expr(Eq.Expr(casted, LiteralExpr('null', casted.type)))
            result = If.Expr(guard, expr, result, rtype)

        return result
