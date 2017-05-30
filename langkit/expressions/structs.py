from __future__ import absolute_import, division, print_function

import inspect

from langkit import names
from langkit.compiled_types import (
    ASTNode, BoolType, BuiltinField, Field, Struct, UserField, resolve_type, T
)
from langkit.diagnostics import Severity, check_source_language
from langkit.expressions import (
    AbstractExpression, AbstractVariable, BasicExpr, BindingScope,
    ComputingExpr, Let, NullCheckExpr, NullExpr, PropertyDef,
    ResolvedExpression, SavedExpr, SequenceExpr, UnreachableExpr, attr_call,
    attr_expr, construct, render
)
from langkit.expressions.boolean import Eq, If, Not
from langkit.expressions.envs import Env
from langkit.expressions.utils import assign_var
from langkit.utils import TypeSet, memoized


@attr_call("cast", do_raise=False)
@attr_call("cast_or_raise", do_raise=True)
class Cast(AbstractExpression):
    """
    Expression that is the result of casting an ASTNode subclass value
    to another subclass.
    """

    class Expr(ComputingExpr):
        pretty_class_name = 'Cast'

        def __init__(self, expr, dest_type, do_raise=False,
                     abstract_expr=None):
            """
            :type expr: ResolvedExpression
            :type dest_type: ASTNode
            :type do_raise: bool

            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.do_raise = do_raise
            self.expr = SavedExpr('Cast_Expr', expr)
            self.static_type = dest_type
            super(Cast.Expr, self).__init__('Cast_Result',
                                            abstract_expr=abstract_expr)

        def _render_pre(self):
            return render('properties/cast_ada', expr=self)

        @property
        def subexprs(self):
            return {'expr': self.expr, 'type': self.static_type.name()}

        def __repr__(self):
            return '<Cast.Expr {}>'.format(self.static_type.name().camel)

        @property
        def is_downcast(self):
            return self.expr.type.matches(self.static_type)

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
            or self.dest_type.matches(T.root_node.entity()),
            "One can only cast to an ASTNode subtype or to an entity"
        )

    def construct(self):
        """
        Construct a resolved expression that is the result of casting a AST
        node.

        :rtype: Cast.Expr
        """
        expr = construct(self.expr)
        t = expr.type

        dest_type = (self.dest_type.entity()
                     if t.is_entity_type and not self.dest_type.is_entity_type
                     else self.dest_type)

        check_source_language(
            dest_type.matches(t) or t.matches(dest_type),
            'Cannot cast {} to {}: only (up/down)casting is '
            'allowed'.format(t.name().camel,
                             dest_type.name().camel)
        )

        check_source_language(expr.type != dest_type,
                              'Casting to the same type',
                              severity=Severity.warning)

        return Cast.Expr(expr, dest_type, do_raise=self.do_raise,
                         abstract_expr=self)

    def __repr__(self):
        return '<Cast to {}>'.format(resolve_type(self.dest_type).name().camel)


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

    @classmethod
    def construct_static(cls, cexpr, abstract_expr=None):
        result = (
            cls.construct_node(cexpr)
            if cexpr.type.is_ast_node or cexpr.type.is_entity_type else
            Eq.make_expr(cexpr, NullExpr(cexpr.type))
        )
        result.abstract_expr = abstract_expr
        return result

    @staticmethod
    def construct_node(cexpr):
        return BasicExpr(
            'Is_Null',
            '{} = null'.format('{}.El'
                               if cexpr.type.is_entity_type else '{}'),
            BoolType, [cexpr]
        )

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: EqExpr
        """
        cexpr = construct(self.expr)
        return self.construct_static(cexpr, abstract_expr=self)

    def __repr__(self):
        return '<IsNull>'


class New(AbstractExpression):
    """
    Abstract expression to create Struct or ASTNode values.
    """

    class StructExpr(ComputingExpr):
        """
        Resolved expression to create Struct values.
        """

        def __init__(self, struct_type, assocs, result_var_name=None,
                     abstract_expr=None):
            """
            :type struct_type: CompiledType
            :type assocs: {names.Name: ResolvedExpression}
            :type result_var_name: str|None
            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.static_type = struct_type
            self.assocs = assocs

            super(New.StructExpr, self).__init__(
                result_var_name or 'New_Struct',
                abstract_expr=abstract_expr
            )

        def _iter_ordered(self):
            return ((k, self.assocs[k]) for k in sorted(self.assocs))

        def _render_fields(self):
            """
            Helper to return the elaboration of structure fields plus the
            ref-counting adjustment to create new ownership shares.

            :rtype: str
            """
            fields = list(self._iter_ordered())

            return '\n'.join(
                # Evaluate expressions for all operands
                [expr.render_pre() for _, expr in fields]

                # Only then, create ownership shares for the returned record
                + ['Inc_Ref ({});'.format(expr.render_expr())
                   for _, expr in fields
                   if expr.type.is_refcounted()]
            )

        def _render_pre(self):
            record_expr = '({})'.format(', '.join(
                '{} => {}'.format(name.camel_with_underscores,
                                  expr.render_expr())
                for name, expr in self._iter_ordered()
            ))

            return '{}\n{}'.format(
                self._render_fields(),

                # We must not inc-ref the resulting record as we already
                # inc-ref'd manually all the ref-counted members.
                assign_var(self.result_var.ref_expr, record_expr,
                           requires_incref=False)
            )

        @property
        def subexprs(self):
            result = {str(k): v for k, v in self.assocs.items()}
            result['_type'] = self.static_type.name()
            return result

        def __repr__(self):
            return '<New.{} {}>'.format(type(self).__name__,
                                        self.static_type.name().camel)

    class NodeExpr(StructExpr):
        """
        Resolved expression to create AST node values.
        """

        def __init__(self, astnode, assocs, abstract_expr=None):
            super(New.NodeExpr, self).__init__(astnode, assocs, 'New_Node',
                                               abstract_expr=abstract_expr)

        def _render_pre(self):
            return (super(New.NodeExpr, self)._render_fields()
                    + render('properties/new_astnode_ada', expr=self))

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
                    if self.struct_type.is_ast_node else
                    New.StructExpr)
        return expr_cls(self.struct_type, provided_fields, abstract_expr=self)

    def __repr__(self):
        return '<New {}>'.format(resolve_type(self.struct_type).name().camel)


class FieldAccess(AbstractExpression):
    """
    Abstract expression that is the result of a field access expression
    evaluation.
    """

    class Expr(ResolvedExpression):
        """
        Resolved expression that represents a field access in generated code.

        Note that this automatically generates a check for null nodes, unless
        this is a simple field access.
        """
        pretty_class_name = 'FieldAccess'

        def __init__(self, receiver_expr, node_data, arguments,
                     implicit_deref=False, abstract_expr=None):
            """
            :param ResolvedExpression receiver_expr: The receiver of the field
                access.

            :param langkit.compiled_types.AbstracNodeData node_data: The
                accessed property or field.

            :param list[ResolvedExpression] arguments: If non-empty, this field
                access will actually be a primitive call.

            :param bool implicit_deref: Whether the receiver is an entity,
                and we want to access a field or property of the stored node.

            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            # When calling environment properties, the call itself happens are
            # outside a property. We cannot create a variable in this context,
            # and the field access is not supposed to require a "render_pre"
            # step.
            p = PropertyDef.get()
            self.simple_field_access = not p

            if not self.simple_field_access:
                self.receiver_expr = NullCheckExpr(receiver_expr,
                                                   implicit_deref)
            else:
                self.receiver_expr = receiver_expr

            self.node_data = node_data
            self.static_type = self.node_data.type
            self.arguments = arguments
            self.implicit_deref = implicit_deref

            # Create a variable for all field accesses in properties. This is
            # needed because the property will return an owning reference, so
            # we need it to be attached to the scope. In other cases, this can
            # make debugging easier.
            super(FieldAccess.Expr, self).__init__(
                None if self.simple_field_access else 'Fld',
                abstract_expr=abstract_expr,
            )

        def __repr__(self):
            return "<FieldAccessExpr {} {} {}>".format(
                self.receiver_expr, self.node_data, self.type
            )

        @property
        @memoized
        def prefix(self):
            """
            Compute the prefix expression, render it and return it.

            :rtype: str
            """
            if self.simple_field_access:
                prefix = self.receiver_expr.render()
            else:
                prefix = self.receiver_expr.render_expr()

            return prefix

        @property
        @memoized
        def entity_info_expr(self):
            """
            Return the value of the entity info parameter along, compute its
            value. Return None otherwise.

            :rtype: str|None
            """

            # Entity info can come from one of two sources:
            if self.implicit_deref:
                # From the entity, if we're calling the property on an entity
                return '{}.Info'.format(self.prefix)
            elif PropertyDef.get() and PropertyDef.get().uses_entity_info:
                # From the property context, if we are in a property that
                # uses_entity_info and calling the property on an AST node.
                return str(PropertyDef.entity_info_name)
            else:
                # Just use the default (empty) entity info if we have none of
                # the above.
                return None

        @property
        def field_access_expr(self):
            """
            Return the code for the expression that evaluates the actual field
            access.

            :rtype: str
            """
            prefix = self.prefix

            if self.implicit_deref:
                prefix = "{}.El".format(prefix)

            # If we're calling a property, then pass the arguments
            if isinstance(self.node_data, PropertyDef):

                # Create a collection of name => expression for parameters
                args = [
                    (formal.name, actual.render_expr())
                    for actual, formal in zip(
                        self.arguments, self.node_data.natural_arguments
                    )
                ]

                # If the property has an implicit env argument, then pass it
                # along.
                if self.node_data.has_implicit_env:
                    args.append((PropertyDef.env_arg_name, str(Env._name)))

                # If the called property uses environments, it will need and
                # env rebindings parameter.
                if self.node_data.uses_entity_info and self.entity_info_expr:
                    args.append((str(PropertyDef.entity_info_name),
                                 self.entity_info_expr))

                # Private non-dispatching properties are declared in
                # $.Analysis' body, so they are not genuine Ada primitives, so
                # dot notation is not available for them.
                dot_notation = (self.node_data.is_public
                                or self.node_data.dispatching)
                if dot_notation:
                    ret = '{}.{}'.format(prefix, self.node_data.name)
                else:
                    ret = str(self.node_data.name)
                    args.insert(0, ('Node', prefix))

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
                    prefix, '{}.{}'.format(prefix, self.node_data.name)
                )

            return ret

        def _render_pre(self):
            sub_exprs = [self.receiver_expr] + self.arguments
            result = [e.render_pre() for e in sub_exprs]

            if self.result_var:
                # If we return to a result var, we need to make sure we create
                # a new ownership share for the result of the field access.
                # Property calls already do that, but we must inc-ref ourselves
                # for other cases.
                result.append('{} := {};'.format(self.result_var.name,
                                                 self.field_access_expr))
                if (self.type.is_refcounted() and
                        self.node_data.access_needs_incref):
                    result.append('Inc_Ref ({});'.format(self.result_var.name))

            return '\n'.join(result)

        def _render_expr(self):
            return (self.result_var.name
                    if self.result_var else self.field_access_expr)

        @property
        def subexprs(self):
            result = {'0-prefix': self.receiver_expr,
                      '1-field': self.node_data}
            if self.arguments:
                result['2-args'] = self.arguments
            return result

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
        self.is_deref = False

    def resolve_field(self):
        """
        Resolve the field that should be accessed, by:

        - Constructing the receiver;
        - Getting its corresponding field.

        :rtype: AbstractNodeData
        """
        self.receiver_expr = construct(self.receiver)
        pfx_type = self.receiver_expr.type

        check_source_language(
            issubclass(pfx_type, Struct),
            '{} values have no field (accessed field was {})'.format(
                pfx_type.name().camel,
                self.field
            )
        )

        self.to_get = pfx_type.get_abstract_fields_dict().get(self.field, None)
        ":type: AbstractNodeField"

        # If still not found, maybe the receiver is an entity, in which case we
        # want to do implicit dereference.
        if not self.to_get and pfx_type.is_entity_type:
            self.to_get = pfx_type.el_type.get_abstract_fields_dict().get(
                self.field, None
            )
            self.is_deref = bool(self.to_get)

        return self.to_get

    def construct(self):
        """
        Constructs the resolved expression corresponding to this field access.
        It can be either a field access or a property call.

        :rtype: FieldAccessExpr
        """

        to_get = self.resolve_field()

        # If still not found, we have a problem
        check_source_language(
            to_get is not None, "Type {} has no '{}' field or property".format(
                self.receiver_expr.type.__name__, self.field
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
            Env.is_bound,
            'This property has no implicit environment parameter whereas {}'
            ' expects one: please use the eval_in_env construct to bind an'
            ' environment first.'.format(to_get.qualname)
        )

        # Check that this property actually accepts these arguments and that
        # they are correctly typed.
        check_source_language(
            len(self.arguments) == len(to_get.natural_arguments),
            'Invalid number of arguments in the call to {}:'
            ' {} expected but got {}'.format(
                to_get.qualname,
                len(to_get.natural_arguments),
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
                zip(self.arguments, to_get.natural_arguments), 1
            )
        ]

        ret = FieldAccess.Expr(
            self.receiver_expr, to_get, arg_exprs, self.is_deref,
            abstract_expr=self
        )
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
        return "<FieldAccess .{}{}>".format(self.field,
                                            '(...)' if self.arguments else '')


@attr_call("is_a")
class IsA(AbstractExpression):
    """
    Expression that is the result of testing the kind of a node.
    """

    class Expr(ComputingExpr):
        static_type = BoolType
        pretty_class_name = 'IsA'

        def __init__(self, expr, astnodes, abstract_expr=None):
            """
            :param ResolvedExpr expr: Expression on which the test is
                performed.
            :param [ASTNode] astnodes: ASTNode subclasses to use for the test.
            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.expr = expr
            self.astnodes = [a.el_type if a.is_entity_type else a
                             for a in astnodes]

            super(IsA.Expr, self).__init__('Is_A', abstract_expr=abstract_expr)

        def _render_pre(self):
            target = ('{}.El.all'
                      if self.expr.type.is_entity_type else
                      '{}.all').format(self.expr.render_expr())
            result_expr = '{} in {}'.format(
                target,
                ' | '.join(
                    "{}_Type'Class".format(a.name().camel_with_underscores)
                    for a in self.astnodes
                )
            )
            return '{}\n{}'.format(
                self.expr.render_pre(),
                assign_var(self.result_var.ref_expr, result_expr)
            )

        @property
        def subexprs(self):
            return {'expr': self.expr,
                    'types': [astnode.name() for astnode in self.astnodes]}

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
                issubclass(a, ASTNode) or a.is_entity_type,
                "Expected ASTNode subclass or entity, got {}".format(a)
            )
            check_source_language(a.matches(expr.type), (
                'When testing the dynamic subtype of an AST node, the type to'
                ' check must be a subclass of the value static type.'
            ))
        return IsA.Expr(expr, astnodes, abstract_expr=self)

    def __repr__(self):
        return '<IsA {}>'.format(', '.join(
            resolve_type(n).name().camel
            for n in self.astnodes
        ))


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
                    or match_type.is_entity_type,
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
        is_entity = input_type.is_entity_type

        if is_entity:
            input_type = input_type.el_type

        for i, (typ, _, _) in enumerate(self.matchers, 1):
            t_name = 'default one' if typ is None else typ.name().camel

            if typ and typ.is_entity_type:
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
        outer_scope = PropertyDef.get_scope()

        # Create a local variable so that in the generated code, we don't have
        # to re-compute the prefix for each type check. This is also required
        # for proper ref-counting.
        matched_expr = SavedExpr('Match_Prefix', construct(self.matched_expr))
        matched_var = matched_expr.result_var.ref_expr
        is_entity = matched_expr.type.is_entity_type

        check_source_language(issubclass(matched_expr.type, ASTNode)
                              or matched_expr.type.is_entity_type,
                              'Match expressions can only work on AST nodes '
                              'or entities')

        constructed_matchers = []

        # Check (i.e. raise an error if no true) the set of matchers is valid:

        # * all matchers must target allowed types, i.e. input type subclasses;
        for typ, var, expr in self.matchers:
            if is_entity and typ and not typ.is_entity_type:
                typ = typ.entity()
                var._type = typ
                var.local_var.type = typ

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

            # Create a scope so that match_var is contained in this branch as
            # is not exposed outside in the debug info.
            with outer_scope.new_child() as inner_scope:
                inner_scope.add(var.local_var)
                constructed_matchers.append((construct(var), construct(expr),
                                             inner_scope))

        # * all possible input types must have at least one matcher. Also warn
        #   if some matchers are unreachable.
        self._check_match_coverage(matched_expr.type)

        # Compute the return type as the unification of all branches
        _, expr, _ = constructed_matchers[-1]
        rtype = expr.type
        for _, expr, _ in constructed_matchers:
            rtype = expr.type.unify(
                rtype,
                'Mismatching types in Match expression: got {cls} but expected'
                ' {other} or sub/supertype'
            )

        # This is the expression execution will reach if we have a bug in our
        # code (i.e. if matchers did not cover all cases).
        result = UnreachableExpr(rtype)

        # Wrap this "failing" expression with all the cases to match in the
        # appropriate order, so that in the end the first matchers are tested
        # first.
        for match_var, expr, inner_scope in reversed(constructed_matchers):
            casted = SavedExpr('Match', Cast.Expr(matched_var, match_var.type))
            guard = Not.make_expr(Eq.make_expr(casted, NullExpr(casted.type)))
            if expr.type != rtype:
                # We already checked that type matches, so only way this is
                # true is if expr.type is an ASTNode type derived from
                # rtype. In that case, we need an explicity upcast.
                expr = Cast.Expr(expr, rtype)

            expr_with_scope = BindingScope(
                Let.Expr([match_var],
                         [casted.result_var.ref_expr],
                         expr),
                [],
                scope=inner_scope
            )
            result = If.Expr(guard, expr_with_scope, result, rtype)

        return SequenceExpr(
            NullCheckExpr(matched_expr,
                          implicit_deref=matched_expr.type.is_entity_type),
            result,
            abstract_expr=self
        )

    def __repr__(self):
        return '<Match>'
