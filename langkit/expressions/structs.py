import inspect

from langkit import names
from langkit.compiled_types import (
    AnalysisUnitType, ASTNode, BoolType, BuiltinField, Field, Struct,
    UserField, resolve_type, T
)
from langkit.diagnostics import Severity, check_source_language
from langkit.expressions.analysis_units import construct_analysis_unit_property
from langkit.expressions.base import (
    AbstractExpression, AbstractVariable, LiteralExpr, PropertyDef,
    ResolvedExpression, Self, UnreachableExpr, construct, render, attr_expr,
    attr_call
)
from langkit.expressions.boolean import Eq, If, Not
from langkit.expressions.envs import Env
from langkit.utils import TypeSet


@attr_call("cast", do_raise=False)
@attr_call("cast_or_raise", do_raise=True)
class Cast(AbstractExpression):
    """
    Expression that is the result of casting an ASTNode subclass value
    to another subclass.
    """

    class Expr(ResolvedExpression):
        def __init__(self, expr, dest_type, do_raise=False, result_var=None):
            """
            :type expr: ResolvedExpression
            :type dest_type: ASTNode
            :type do_raise: bool

            :param ResolvedExpr result_var: If provided, the cast will use it
                to store the cast result. Otherwise, a dedicated variable is
                created for this.
            """
            self.do_raise = do_raise
            self.expr = expr
            self.static_type = dest_type

            p = PropertyDef.get()
            self.expr_var = p.vars.create('Cast_Expr', self.expr.type)
            self.result_var = (result_var or
                               p.vars.create('Cast_Result', dest_type))
            assert self.result_var.type == dest_type, (
                'Cast temporaries must have exactly the cast type: {} expected'
                ' but got {} instead'.format(
                    dest_type.name().camel,
                    self.result_var.type.name().camel
                )
            )

            super(Cast.Expr, self).__init__()

        def _render_pre(self):
            # Before actually downcasting an access to an AST node, add a type
            # check so that we raise a Property_Error if it's wrong.
            return render('properties/type_safety_check_ada', expr=self)

        def _render_expr(self):
            return self.result_var.name

        @property
        def subexprs(self):
            return [self.expr]

        def __repr__(self):
            return '<Cast.Expr {}>'.format(self.static_type.name().camel)

    def __init__(self, expr, dest_type, do_raise=False):
        """
        :param AbstractExpression expr: Expression on which the cast is
            performed.
        :param ASTNode dest_type: ASTNode subclass to use for the cast.
        :param bool do_raise: Whether the exception should raise an
            exception or return null when the cast is invalid.
        """
        super(Cast, self).__init__()
        self.expr = expr
        self.dest_type = dest_type
        self.do_raise = do_raise

    def do_prepare(self):
        self.dest_type = resolve_type(self.dest_type)

        check_source_language(
            self.dest_type.matches(ASTNode)
            or self.dest_type.matches(T.root_node.env_el()),
            "One can only cast to an ASTNode subtype or to an env_element"
        )

    def construct(self):
        """
        Construct a resolved expression that is the result of casting a AST
        node.

        :rtype: CastExpr
        """
        expr = construct(
            self.expr,
            lambda t: self.dest_type.matches(t) or t.matches(self.dest_type),
            'Cannot cast {{expr_type}} to {}: only (up/down)casting is '
            'allowed'.format(
                self.dest_type.name().camel
            )
        )

        check_source_language(expr.type != self.dest_type, (
            'Casting to the same type'
        ), severity=Severity.warning)

        return Cast.Expr(expr, self.dest_type, do_raise=self.do_raise)


@attr_expr("is_null")
class IsNull(AbstractExpression):
    """
    Abstract expression to test the passed expression is null.
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
        cexpr = construct(self.expr)
        return Eq.make_expr(
            cexpr, LiteralExpr(cexpr.type.nullexpr(), cexpr.type)
        )


class New(AbstractExpression):
    """
    Abstract expression to create Struct or ASTNode values.
    """

    class StructExpr(ResolvedExpression):
        """
        Resolved expression to create Struct values.
        """

        def __init__(self, struct_type, assocs):
            """
            :type struct_type: CompiledType
            :type assocs: {names.Name: ResolvedExpression}
            """
            self.static_type = struct_type
            self.assocs = assocs

            super(New.StructExpr, self).__init__()

        def _iter_ordered(self):
            return ((k, self.assocs[k]) for k in sorted(self.assocs))

        def _render_pre(self):
            return '\n'.join(expr.render_pre()
                             for _, expr in self._iter_ordered())

        def _render_expr(self):
            return '({}, Is_Null => False)'.format(
                ', '.join('{} => {}'.format(name.camel_with_underscores,
                                            expr.render_expr())
                          for name, expr in self._iter_ordered())
            )

        @property
        def subexprs(self):
            return self.assocs.values()

        def __repr__(self):
            return '<New.{} {}>'.format(type(self).__name__,
                                        self.static_type.name().camel)

    class NodeExpr(StructExpr):
        """
        Resolved expression to create AST node values.
        """

        def __init__(self, astnode, assocs):
            p = PropertyDef.get()
            self.result_var = p.vars.create('New_Node', astnode)

            super(New.NodeExpr, self).__init__(astnode, assocs)

        def _render_pre(self):
            return (super(New.NodeExpr, self)._render_pre() +
                    render('properties/new_astnode_ada', expr=self))

        def _render_expr(self):
            return self.result_var.name

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
        self.struct_type = resolve_type(self.struct_type)

        check_source_language(issubclass(self.struct_type, Struct), (
            "Invalid type, expected struct type, got {}".format(
                self.struct_type.name().camel
            )
        ))

    def construct(self):
        """
        :rtype: StructExpr
        """
        if issubclass(self.struct_type, ASTNode):
            check_source_language(
                not self.struct_type.is_list_type,
                'List node synthetization is not supported for now'
            )
            check_source_language(
                PropertyDef.get().memoized,
                'Node synthetization can only happen inside a memoized'
                ' property'
            )

        # Make sure the provided set of fields matches the one the struct
        # needs.
        def error_if_not_empty(name_set, message):
            check_source_language(not name_set, ('{}: {}'.format(
                message, ', '.join(name for name in name_set)
            )))

        # Create a dict of field names to fields in the struct type
        required_fields = {
            f._name.lower: f
            for f in self.struct_type.get_abstract_fields()
            if isinstance(f, (Field, UserField))
            and not isinstance(f, BuiltinField)
        }

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
        provided_fields = {required_fields[name].name: construct(
            value, required_fields[name].type,
            'Wrong type for field {}: expected {{expected}}, '
            'got {{expr_type}}'.format(name)
        ) for name, value in self.field_values.items()}

        expr_cls = (New.NodeExpr
                    if self.struct_type.is_ast_node() else
                    New.StructExpr)
        return expr_cls(self.struct_type, provided_fields)


class FieldAccess(AbstractExpression):
    """
    Abstract expression that is the result of a field access expression
    evaluation.
    """

    class Expr(ResolvedExpression):
        """
        Resolved expression that represents a field access in generated code.
        """

        def __init__(self, receiver_expr, node_data, arguments,
                     implicit_deref=False):
            """
            :param ResolvedExpression receiver_expr: The receiver of the field
                access.

            :param langkit.compiled_types.AbstracNodeData node_data: The
                accessed property or field.

            :param list[ResolvedExpression] arguments: If non-empty, this field
                access will actually be a primitive call.

            :param bool implicit_deref: Whether the receiver is an env element,
                and we want to access a field or property of the stored node.
            """
            self.receiver_expr = receiver_expr
            self.node_data = node_data
            self.static_type = self.node_data.type
            self.arguments = arguments
            self.simple_field_access = False
            self.implicit_deref = implicit_deref

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

            # Create a variable for all field accesses in properties. This is
            # needed because the property will return an owning reference, so
            # we need it to be attached to the scope. In other cases, this can
            # make debugging easier.
            super(FieldAccess.Expr, self).__init__(
                'Field_Access_Result' if p else None
            )

        def __repr__(self):
            return "<FieldAccessExpr {} {} {}>".format(
                self.receiver_expr, self.node_data, self.type
            )

        def _render_pre(self):
            # Before accessing the field of a record through an access, we must
            # check whether this access is null in order to raise a
            # Property_Error in the case it is.
            return '{}\n{}'.format(
                render('properties/null_safety_check_ada',
                       expr=self.receiver_expr, result_var=self.prefix_var),
                '\n'.join(arg.render_pre() for arg in self.arguments)
            )

        def _render_expr(self):
            if self.simple_field_access:
                prefix = self.receiver_expr.render()
            else:
                prefix = self.prefix_var.name

            if self.implicit_deref:
                prefix = "{}.El".format(prefix)

            ret = "{}.{}".format(prefix, self.node_data.name)

            # If we're calling a property, then pass the arguments
            if isinstance(self.node_data, PropertyDef):

                # Create a collection of name => expression for parameters
                args = [
                    (formal.name, actual.render_expr())
                    for actual, formal in zip(
                        self.arguments, self.node_data.explicit_arguments
                    )
                ]
                if self.node_data.has_implicit_env:
                    args.append((PropertyDef.env_arg_name, str(Env._name)))

                if args:
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

        @property
        def subexprs(self):
            return [self.receiver_expr] + self.arguments

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

        is_deref = False
        receiver_expr = construct(self.receiver)

        if issubclass(receiver_expr.type, AnalysisUnitType):
            return construct_analysis_unit_property(receiver_expr, self.field,
                                                    self.arguments)

        check_source_language(
            issubclass(receiver_expr.type, Struct),
            '{} values have no field (accessed field was {})'.format(
                receiver_expr.type.name().camel,
                self.field
            )
        )

        to_get = receiver_expr.type.get_abstract_fields_dict().get(self.field,
                                                                   None)
        ":type: AbstractNodeField"

        # If still not found, maybe the receiver is an env el, in which case we
        # want to do implicit dereference.
        if not to_get and receiver_expr.type.is_env_element_type:
            to_get = receiver_expr.type.el_type.get_abstract_fields_dict().get(
                self.field, None
            )
            is_deref = bool(to_get)

        # If still not found, we have a problem
        check_source_language(
            to_get is not None, "Type {} has no '{}' field or property".format(
                receiver_expr.type.__name__, self.field
            )
        )

        check_source_language(
            not to_get.is_internal,
            '{} is for internal use only'.format(to_get.qualname)
        )

        # If the field is a property that take an implicit env argument, make
        # sure we have one to provide.
        check_source_language(
            not to_get.is_property or
            not to_get.has_implicit_env or
            Env.has_ambient_env,
            'This property has no implicit environment parameter whereas {}'
            ' expects one: please use the eval_in_env construct to bind an'
            ' environment first.'.format(to_get.qualname)
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

        arg_exprs = [
            construct(
                actual, formal.type,
                custom_msg='Invalid {} actual (#{}) for {}:'.format(
                    formal.name, i, to_get.qualname,
                ) + ' expected {expected} but got {expr_type}'
            ) for i, (actual, formal) in enumerate(
                zip(self.arguments, to_get.explicit_arguments), 1
            )
        ]

        ret = FieldAccess.Expr(receiver_expr, to_get, arg_exprs, is_deref)
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


@attr_call("is_a")
class IsA(AbstractExpression):
    """
    Expression that is the result of testing the kind of a node.
    """

    class Expr(ResolvedExpression):
        static_type = BoolType

        def __init__(self, expr, astnodes):
            """
            :param ResolvedExpr expr: Expression on which the test is
                performed.
            :param [ASTNode] astnodes: ASTNode subclasses to use for the test.
            """
            self.expr = expr
            self.astnodes = astnodes

            super(IsA.Expr, self).__init__()

        def _render_pre(self):
            return self.expr.render_pre()

        def _render_expr(self):
            return "{}.all in {}".format(
                self.expr.render_expr(),
                " | ".join(
                    "{}_Type'Class".format(a.name().camel_with_underscores)
                    for a in self.astnodes
                )
            )

        @property
        def subexprs(self):
            return [self.expr]

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
        self.astnodes = astnodes

    def construct(self):
        """
        Construct a resolved expression that is the result of testing the kind
        of a node.

        :rtype: IsAExpr
        """
        expr = construct(self.expr)
        astnodes = [resolve_type(a) for a in self.astnodes]
        for a in astnodes:
            check_source_language(
                issubclass(a, ASTNode),
                "Expected ASTNode subclass, got {}".format(a)
            )
            check_source_language(a.matches(expr.type), (
                'When testing the dynamic subtype of an AST node, the type to'
                ' check must be a subclass of the value static type.'
            ))
        return IsA.Expr(expr, astnodes)


@attr_call('match')
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
                    issubclass(match_type, T.root_node)
                    or match_type.is_env_element_type,
                    'Invalid matching type: {}'.format(
                        match_type.name().camel
                    )
                )
            else:
                match_type = None

            match_var = AbstractVariable(
                names.Name('Match_{}'.format(i)),
                type=match_type,
                create_local=True,
                source_name=names.Name.from_lower(argspec.args[0])
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
        env_el = input_type.is_env_element_type

        if env_el:
            input_type = input_type.el_type

        for i, (typ, _, _) in enumerate(self.matchers, 1):
            t_name = 'default one' if typ is None else typ.name().camel

            if env_el and typ:
                check_source_language(
                    typ.is_env_element_type,
                    "Match expression on an env element, should match env "
                    "element types"
                )
                typ = typ.el_type

            check_source_language(not type_set.include(typ or input_type),
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
                ', '.join(typ.name().camel for typ in mm),
                input_type.name().camel
            )
        )

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: ResolvedExpression
        """
        # Add the variables created for this expression to the current scope
        scope = PropertyDef.get_scope()
        for _, var, _ in self.matchers:
            scope.add(var.local_var)

        matched_expr = construct(self.matched_expr)
        check_source_language(issubclass(matched_expr.type, ASTNode)
                              or matched_expr.type.is_env_element_type,
                              'Match expressions can only work on AST nodes '
                              'or env elements')

        constructed_matchers = []

        # Check (i.e. raise an error if no true) the set of matchers is valid:

        # * all matchers must target allowed types, i.e. input type subclasses;
        for typ, var, expr in self.matchers:
            if typ is not None:
                check_source_language(
                    typ.matches(matched_expr.type),
                    'Cannot match {} (input type is {})'.format(
                        typ.name().camel,
                        matched_expr.type.name().camel
                    )
                )
            else:
                # The default matcher (if any) matches the most general type,
                # which is the input type.
                var.set_type(matched_expr.type)
            constructed_matchers.append((construct(var), construct(expr)))

        # * all possible input types must have at least one matcher. Also warn
        #   if some matchers are unreachable.
        self._check_match_coverage(matched_expr.type)

        # Compute the return type as the unification of all branches
        _, expr = constructed_matchers[-1]
        rtype = expr.type
        for _, expr in constructed_matchers:
            check_source_language(
                expr.type.matches(rtype), "Wrong type for match expression : "
                "{}, expected {} or sub/supertype".format(
                    expr.type.name().camel, rtype.name().camel
                )
            )
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
            guard = Not.make_expr(
                Eq.make_expr(
                    casted, LiteralExpr(casted.type.nullexpr(), casted.type)
                )
            )
            if expr.type != rtype:
                # We already checked that type matches, so only way this is
                # true is if expr.type is an ASTNode type derived from
                # rtype. In that case, we need an explicity upcast.
                expr = Cast.Expr(expr, rtype)

            result = If.Expr(guard, expr, result, rtype)

        return result
