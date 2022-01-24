from __future__ import annotations

import inspect
from typing import (Any as _Any, ContextManager, Dict, List, Optional,
                    Sequence, Tuple)

import funcy

from langkit import names
from langkit.compiled_types import (
    ASTNodeType, AbstractNodeData, Field, resolve_type
)
from langkit.diagnostics import (Severity, check_source_language,
                                 diagnostic_context, error)
from langkit.expressions import (
    AbstractExpression, AbstractVariable, BasicExpr, BindingScope,
    ComputingExpr, DynamicVariable, Entity, Let, NullCheckExpr, NullExpr,
    PropertyDef, ResolvedExpression, SavedExpr, Self, T, VariableExpr,
    attr_call, attr_expr, construct, construct_compile_time_known,
    dsl_document, gdb_end, gdb_property_call_start, render
)
from langkit.expressions.boolean import Eq
from langkit.expressions.utils import assign_var
from langkit.utils import TypeSet, collapse_concrete_nodes, memoized


@attr_call("cast", do_raise=False)
@attr_call("cast_or_raise", do_raise=True,
           doc='Like :dsl:`cast`, but raise a property error in case of'
               ' mismatch.')
class Cast(AbstractExpression):
    """
    Downcast the AST `node` to the more specific `dest_type` AST node type.

    This returns a null node if the actual type of `node` is not a subtype of
    `dest_type`.
    """

    class Expr(ComputingExpr):
        pretty_class_name = 'Cast'

        def __init__(self, expr, dest_type, do_raise=False, unsafe=False,
                     abstract_expr=None):
            """
            :type expr: ResolvedExpression
            :type dest_type: ASTNodeType
            :type do_raise: bool

            :param bool unsafe: If true, elide the type check before doing the
                cast. This is used to avoid noisy and useless type checks in
                generated code: these checks would fail only because of a bug
                in the code generator.
            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.do_raise = do_raise
            self.unsafe = unsafe
            self.expr = SavedExpr('Cast_Expr', expr)
            self.static_type = dest_type
            super().__init__('Cast_Result', abstract_expr=abstract_expr)

        def _render_pre(self):
            return render('properties/cast_ada', expr=self)

        @property
        def subexprs(self):
            return {'expr': self.expr, 'type': self.static_type.name}

        def __repr__(self):
            return '<Cast.Expr {}>'.format(self.static_type.dsl_name)

        @property
        def dest_node(self):
            """
            Return the node type (not entity) that is the result of the cast
            expression.
            """
            return (self.type.element_type
                    if self.type.is_entity_type else self.type)

        @property
        def input_node(self):
            """
            Return the node type (not entity) that is the input of the cast
            expression.
            """
            return (self.expr.type.element_type
                    if self.expr.type.is_entity_type else self.expr.type)

        @property
        def check_needed(self):
            """
            Return whether we must generate a dynamic check on the kind of the
            input expression before doing the cast.

            :rtype: bool
            """
            # If asked to do an unsafe conversion, then by definition we must
            # not generate a check.
            if self.unsafe:
                return False

            # Don't generate a dynamic check if we know statically that there
            # is no failure possible (this is an upcast, or a downcast when
            # there is only one subclass, etc.).
            return self.input_node not in TypeSet({self.dest_node})

    def __init__(self, node, dest_type, do_raise=False):
        """
        :param AbstractExpression node: Expression on which the cast is
            performed.
        :param ASTNodeType dest_type: AST node type to use for the cast.
        :param bool do_raise: Whether the exception should raise an
            exception or return null when the cast is invalid.
        """
        super().__init__()
        self.expr = node
        self.dest_type = dest_type
        self.do_raise = do_raise

    def do_prepare(self):
        self.dest_type = resolve_type(self.dest_type)

        check_source_language(
            self.dest_type.is_ast_node or self.dest_type.is_entity_type,
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

        dest_type = (self.dest_type.entity
                     if t.is_entity_type and not self.dest_type.is_entity_type
                     else self.dest_type)

        check_source_language(
            dest_type.matches(t) or t.matches(dest_type),
            'Cannot cast {} to {}: only (up/down)casting is '
            'allowed'.format(t.dsl_name, dest_type.dsl_name)
        )

        check_source_language(expr.type != dest_type,
                              'Casting to the same type',
                              severity=Severity.warning)

        return Cast.Expr(expr, dest_type, do_raise=self.do_raise,
                         abstract_expr=self)

    def __repr__(self):
        return '<Cast to {}>'.format(resolve_type(self.dest_type).dsl_name)


@attr_expr("is_null")
class IsNull(AbstractExpression):
    """
    If `expr` is an entity, return whether the corresponding AST node is null
    (even if the entity info is non null). For all other types, return whether
    it is the null value.
    """

    def __init__(self, expr):
        """
        :param AbstractExpression expr: Expression on which the test is
            performed.
        """
        super().__init__()
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
            '{} = null'.format('{}.Node'
                               if cexpr.type.is_entity_type else '{}'),
            T.Bool, [cexpr]
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


@dsl_document
class New(AbstractExpression):
    """
    Create a structure value or a new AST node.

    `struct_type` must be the type of the value to create and `field_values`
    must contain key/value associations for all fields this structure or node
    contains.

    For instance, assuming the following ``Struct`` subclass::

        class MyStruct(Struct):
            a = UserField(BoolType)
            b = UserField(SomeNode)

        New(MyStruct, a=True, b=No(SomeNode))

    When evaluating a property on node X, synthetizing a node Y does the
    following assignments:

    * Y's parents is X;
    * Y's environment (``.node_env`` and ``.children_env``) is
      ``X.children_env``.

    Note that because of this last assignment, synthetizing a node triggers PLE
    on the current analysis unit. In addition, synthetizing nodes is allowed
    only in memoized properties or lazy fields.
    """

    class StructExpr(ComputingExpr):
        """
        Resolved expression to create StructType values.
        """

        def __init__(self, struct_type, assocs, result_var_name=None,
                     abstract_expr=None):
            """
            :type struct_type: CompiledType
            :type assocs: {names.Name|langkit.compiled_types.AbstractNodeData:
                           ResolvedExpression}
            :type result_var_name: str|None
            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.static_type = struct_type

            # Convert names in `assocs` to the corresponding field in
            # struct_type. This lets callers use either names or fields,
            # depending on what's the most convenient for them.

            def field_or_lookup(field):
                if isinstance(field, names.Name):
                    fields = struct_type.get_abstract_node_data_dict()
                    return fields[field.lower]
                assert isinstance(field, AbstractNodeData)
                return field

            self.assocs = {field_or_lookup(field): expr
                           for field, expr in assocs.items()}

            super().__init__(
                result_var_name or 'New_Struct',
                abstract_expr=abstract_expr
            )

        def _iter_ordered(self):
            return sorted(
                [(field, expr) for field, expr in self.assocs.items()],
                key=lambda assoc: assoc[0].name
            )

        def _render_fields(self):
            """
            Helper to return the elaboration of structure fields plus the
            ref-counting adjustment to create new ownership shares.

            :rtype: str
            """
            assocs = list(self._iter_ordered())

            return '\n'.join(
                # Evaluate expressions for all operands
                [expr.render_pre() for _, expr in assocs]

                # Only then, create ownership shares for the returned record
                + ['Inc_Ref ({});'.format(expr.render_expr())
                   for _, expr in assocs
                   if expr.type.is_refcounted]
            )

        def _render_pre(self):
            record_expr = '({})'.format(', '.join(
                '{} => {}'.format(field.name.camel_with_underscores,
                                  expr.render_expr())
                for field, expr in self._iter_ordered()
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
            result = {field.name.lower: expr
                      for field, expr in self.assocs.items()}
            result['_type'] = self.static_type.dsl_name
            return result

        def __repr__(self):
            return '<New.{} {}>'.format(type(self).__name__,
                                        self.static_type.name.camel)

    class NodeExpr(StructExpr):
        """
        Resolved expression to create AST node values.
        """

        def __init__(self, astnode, assocs, abstract_expr=None):
            super().__init__(astnode, assocs, 'New_Node',
                             abstract_expr=abstract_expr)

            # The synthetized node inherits Self.Self_Env, so PLE must happen
            # before this property is run.
            PropertyDef.get().set_uses_envs()

        def _render_pre(self):
            return (super()._render_fields()
                    + render('properties/new_astnode_ada', expr=self))

    def __init__(self, struct_type, **field_values):
        """
        :param langkit.compiled_types.BaseStructType struct_type:
            BaseStructType subclass for the struct type this expression must
            create.
        :param dict[str, AbstractExpression] fields: Values to assign to the
            fields for the created struct value.
        """
        super().__init__()
        self.struct_type = struct_type
        self.field_values = field_values

    def do_prepare(self):
        self.struct_type = resolve_type(self.struct_type)

        check_source_language(
            self.struct_type.is_base_struct_type,
            'Invalid type, expected struct type or AST node, got {}'.format(
                self.struct_type.dsl_name
            )
        )
        check_source_language(
            not self.struct_type.is_ast_node
            or self.struct_type.synthetic,
            'Synthetized AST nodes must be annotated as synthetic'
        )

    def construct(self):
        """
        :rtype: StructExpr
        """
        if self.struct_type.is_ast_node:
            check_source_language(
                not self.struct_type.is_list_type,
                'List node synthetization is not supported for now'
            )
            prop = PropertyDef.get()
            check_source_language(
                prop.memoized or prop.lazy_field,
                'Node synthetization can only happen inside memoized'
                ' properties or lazy fields'
            )

        # Make sure the provided set of fields matches the one the struct
        # needs.
        def error_if_not_empty(name_set, message):
            check_source_language(not name_set, ('{}: {}'.format(
                message, ', '.join(name for name in name_set)
            )))

        # Create a dict of field names to fields in the struct type

        required_fields = self.struct_type.required_fields_in_exprs
        default_valued_fields = {n: f
                                 for n, f in required_fields.items()
                                 if f.default_value}

        error_if_not_empty(
            set(required_fields)
            - set(self.field_values.keys())
            - set(default_valued_fields),
            'Values are missing for {} fields'.format(
                self.struct_type.dsl_name
            )
        )
        error_if_not_empty(
            set(self.field_values.keys()) - set(required_fields),
            'Extraneous fields for {}'.format(self.struct_type.dsl_name)
        )

        # At this stage, we know that the user has only provided fields that
        # are valid for the struct type. First, construct expression for field
        # values.
        field_values = {required_fields[name]: construct(expr)
                        for name, expr in self.field_values.items()}

        # Add default values for missing fields. Note that we construct their
        # abstract expressions on purpose: even though a resolved expression is
        # already present in field.default_value, reusing ResolvedExpression
        # nodes in multiple expressions is forbidden. Constructing a new each
        # time avoids this problem.
        for name, field in default_valued_fields.items():
            if field not in field_values:
                field_values[field] = construct_compile_time_known(
                    field.abstract_default_value
                )

        # Then check that the type of these expressions match field types
        for field, expr in field_values.items():
            check_source_language(
                expr.type.matches(field.type),
                f"Wrong type for field {field.qualname}:"
                f" expected {field.type.dsl_name}, got {expr.type.dsl_name}"
            )

            # Annotate parsing/synthetic fields with precise type information
            if isinstance(field, Field):
                field.types_from_synthesis.include(expr.type)

            # Make sure we downcast input values so that they fit in the
            # fields.
            if field.type != expr.type:
                field_values[field] = Cast.Expr(expr, field.type)

        expr_cls = (New.NodeExpr
                    if self.struct_type.is_ast_node else
                    New.StructExpr)
        return expr_cls(self.struct_type, field_values, abstract_expr=self)

    def __repr__(self):
        return '<New {}>'.format(resolve_type(self.struct_type).name.camel)


class FieldAccess(AbstractExpression):
    """
    Abstract expression that is the result of a field access expression
    evaluation.
    """

    class Arguments:
        """
        Holder for arguments to pass to a property.
        """

        _traverse_in_prepare = True

        def __init__(self,
                     args: Sequence[AbstractExpression],
                     kwargs: Dict[str, AbstractExpression]):
            self.args = args
            self.kwargs = kwargs

        def associate(self, node_data: AbstractNodeData) -> List[
            Tuple[str, Optional[AbstractExpression]]
        ]:
            """
            Try to associate passed arguments with each natural argument in the
            `node_data` property. If invalid count or invalid argument names
            are detected, raise the appropriate user diagnostic.

            On success, return a list with all actuals and arg keyword/position
            to pass in the same order as natural arguments in the spec. None
            values are left for arguments that must be passed default values.
            """
            args = list(enumerate(self.args, 1))
            kwargs = dict(self.kwargs)
            result: List[Tuple[str, Optional[AbstractExpression]]] = []
            for arg_spec in node_data.natural_arguments:
                actual: Optional[Optional[AbstractExpression]]

                # Look for a keyword argument corresponding to `arg_spec`
                arg_name = arg_spec.name.lower
                key = arg_spec.name.lower
                try:
                    actual = kwargs.pop(arg_name)
                except KeyError:
                    # There is no keyword argument passed for this argument, so
                    # pick the first remaining one from positional arguments
                    # or, if there is no positional argument left, fallback to
                    # the default argument.
                    if args:
                        key, actual = args.pop(0)
                    else:
                        check_source_language(
                            arg_spec.default_value is not None,
                            'Missing actual for argument {}'.format(arg_name)
                        )
                        # Don't pass the argument explicitly: let Ada pass the
                        # default one instead.
                        actual = None

                result.append((key, actual))

            # At this point, we managed to find an actual for all arguments, so
            # all remaining passed arguments are unexpected.
            check_source_language(
                not args,
                'The last {} unexpected'.format(
                    '{} arguments are'.format(len(args))
                    if len(args) > 1 else 'argument is'
                )
            )
            check_source_language(
                not kwargs,
                'Invalid keyword arguments: {}'.format(', '.join(
                    sorted(kwargs)
                ))
            )
            return result

        def construct(
            self,
            node_data: AbstractNodeData,
        ) -> List[Optional[ResolvedExpression]]:
            """
            Associate passed arguments with each natural argument in the
            ``node_data`` property, then construct each argument and return
            their list.
            """
            exprs = self.associate(node_data)
            assert len(exprs) == len(node_data.natural_arguments)

            return [
                None if actual is None else construct(
                    actual, formal.type,
                    custom_msg='Invalid "{}" actual{} for {}:'.format(
                        formal.name.lower,
                        ' (#{})'.format(key) if isinstance(key, int) else '',
                        node_data.qualname,
                    ) + ' expected {expected} but got {expr_type}'
                )
                for (key, actual), formal in zip(exprs,
                                                 node_data.natural_arguments)
            ]

    class Expr(ResolvedExpression):
        """
        Resolved expression that represents a field access in generated code.

        Note that this automatically generates a check for null nodes, unless
        this is a simple field access.
        """
        pretty_class_name = 'FieldAccess'

        def __init__(self,
                     receiver_expr: ResolvedExpression,
                     node_data: AbstractNodeData,
                     arguments: Sequence[Optional[ResolvedExpression]],
                     actual_node_data: Optional[AbstractNodeData] = None,
                     implicit_deref: bool = False,
                     unsafe: bool = False,
                     abstract_expr: Optional[AbstractExpression] = None):
            """
            :param receiver_expr: The receiver of the field access.

            :param node_data: The accessed property or field.

            :param arguments: If non-empty, this field access will actually be
                a primitive call. Each item is a ResolvedExpression for an
                actual to pass, or None for arguments to let them have their
                default value. List list must have the same size as
                `node_data.natural_arguments`.

            :param actual_node_data: If not None, node data to access for code
                generation. In that case, ``node_data`` is just there to keep
                track of what was accessed at the DSL level.

            :param implicit_deref: Whether the receiver is an entity, and we
                want to access a field or property of the stored node.  In the
                case of an entity prefix for an AST node field, return an
                entity with the same entity info.

            :param unsafe: If true, don't generate the null check before doing
                the field access. This is used to avoid noisy and useless null
                checks in generated code: these checks would fail only because
                of a bug in the code generator.

            :param abstract_expr: See ResolvedExpression's constructor.
            """
            # When calling environment properties, the call itself happens are
            # outside a property. We cannot create a variable in this context,
            # and the field access is not supposed to require a "render_pre"
            # step.
            p = PropertyDef.get()
            self.simple_field_access = not p
            if implicit_deref:
                assert receiver_expr.type.is_entity_type
                assert not self.simple_field_access

            self.implicit_deref = implicit_deref
            self.unsafe = unsafe

            self.original_receiver_expr = receiver_expr
            self.receiver_expr = (
                receiver_expr
                if self.simple_field_access or self.unsafe else
                NullCheckExpr(receiver_expr, implicit_deref)
            )

            # Keep the original node data for debugging purposes
            self.original_node_data = node_data

            self.node_data = actual_node_data or node_data

            self.arguments = arguments
            if self.arguments is not None:
                assert (len(self.arguments) ==
                        len(self.node_data.natural_arguments))

            if isinstance(self.node_data, PropertyDef):
                prop = self.node_data

                def actual(dynvar):
                    """Return the value to pass for the given dynamic var."""
                    if dynvar.is_bound:
                        # If the variable is bound, just pass the binding value
                        return construct(dynvar)

                    else:
                        # Otherwise, pass the default value. Thanks to previous
                        # checks (DynamicVariable.check_call_bindings), we know
                        # it is never null.
                        value = prop.dynamic_var_default_value(dynvar)
                        assert value is not None
                        return construct(value)

                self.dynamic_vars = [
                    actual(dynvar) for dynvar in self.node_data.dynamic_vars
                ]

            self.static_type = self.node_data.type
            if self.wrap_result_in_entity:
                assert isinstance(self.static_type, ASTNodeType)
                self.static_type = self.static_type.entity

            # Create a variable for all field accesses in properties. This is
            # needed because the property will return an owning reference, so
            # we need it to be attached to the scope. In other cases, this can
            # make debugging easier.
            super().__init__(
                None if self.simple_field_access else 'Fld',
                abstract_expr=abstract_expr,
            )

        def __repr__(self) -> str:
            return "<FieldAccessExpr {} {} {}>".format(
                self.receiver_expr, self.node_data, self.type
            )

        @property
        def wrap_result_in_entity(self) -> bool:
            """
            Whether the result is an AST node that must be wrapped as an
            entity.
            """
            return (
                self.implicit_deref
                and isinstance(self.node_data, Field)
                and self.node_data.type.is_ast_node
            )

        @property  # type: ignore
        @memoized
        def prefix(self) -> str:
            """
            Compute the prefix expression, render it and return it.
            """
            if self.simple_field_access:
                prefix = self.receiver_expr.render()
            else:
                prefix = self.receiver_expr.render_expr()

            return prefix

        @property  # type: ignore
        @memoized
        def entity_info_expr(self) -> Optional[str]:
            """
            Return the value of the entity info parameter along, compute its
            value. Return None otherwise.
            """
            # If the property that this field accesses requires entity info,
            # then the prefix is supposed to be an entity. There are only two
            # exceptions to this: either the entity info is actually optional,
            # either we are out of any property. In these cases, leave the
            # default entity info.
            #
            # See CompileCtx.compute_uses_entity_info_attr for how we check
            # that the assertion always holds.
            if not self.implicit_deref:
                assert (self.node_data.optional_entity_info
                        or PropertyDef.get() is None)
                return None

            # When it is required, entity info comes from the entity, if we're
            # calling the property on an entity.
            return '{}.Info'.format(self.prefix)

        @property
        def field_access_expr(self) -> str:
            """
            Return the code for the expression that evaluates the actual field
            access.
            """
            prefix = self.prefix
            if self.implicit_deref:
                prefix = '{}.Node'.format(prefix)

            if isinstance(self.node_data, PropertyDef):
                # If we're calling a property, then pass the arguments

                # TODO: For the moment, the first argument is named Node for
                # properties on node & entity types, and Self for other
                # properties. For the moment, properties that are not on nodes
                # are necessarily built-in properties, so the inconsistency is
                # not too bothering, but long-term we want to rename *every*
                # self argument to Self.
                rec_type = self.receiver_expr.type
                first_arg_name = (
                    'Node' if rec_type.is_ast_node or rec_type.is_entity_type
                    else 'Self'
                )

                # Create a collection of name => expression for parameters.
                # First argument is the node itself.
                args = [(first_arg_name, prefix)] + [
                    (formal.name, actual.render_expr())
                    for actual, formal in zip(
                        self.arguments, self.node_data.natural_arguments
                    ) if actual is not None
                ]

                # If the property has dynamically bound variables, then pass
                # them along.
                for formal, actual in zip(self.node_data.dynamic_vars,
                                          self.dynamic_vars):
                    args.append((formal.argument_name,
                                 actual.render_expr()))

                # If the called property uses entity information, pass it
                # along.
                if self.node_data.uses_entity_info:
                    einfo_expr = self.entity_info_expr
                    if einfo_expr:
                        args.append((str(PropertyDef.entity_info_name),
                                     einfo_expr))

                # Build the call
                ret = '{} ({})'.format(
                    self.node_data.qual_impl_name,
                    ', '.join('{} => {}'.format(name, value)
                              for name, value in args)
                )

            elif self.node_data.abstract:
                # Call the accessor for abstract fields
                ret = 'Implementation.{} ({})'.format(
                    self.node_data.internal_name,
                    prefix
                )

            else:
                # If we reach this point, we know that we are accessing a
                # struct field: make sure we return the public API type,
                # which may be different from the type thas is stored in the
                # struct.
                ret = self.node_data.type.extract_from_storage_expr(
                    prefix,
                    '{}.{}'.format(prefix, self.node_data.internal_name)
                )

            if self.wrap_result_in_entity:
                ret = '{} (Node => {}, Info => {})'.format(
                    self.type.constructor_name, ret, self.entity_info_expr
                )

            return ret

        def _render_pre(self) -> str:
            # As long as this method is called, this should not be a simple
            # field access and thus we should have a result variable.
            assert not self.simple_field_access and self.result_var

            # Emit debug helper directives to describe the call if the target
            # is a property we generated code for.
            call_debug_info = (isinstance(self.node_data, PropertyDef)
                               and not self.node_data.external)

            sub_exprs = [self.receiver_expr] + funcy.lfilter(
                lambda e: e is not None,
                self.arguments
            )
            result = [e.render_pre() for e in sub_exprs]

            if call_debug_info:
                result.append(gdb_property_call_start(self.node_data))

            result.append('{} := {};'.format(self.result_var.name,
                                             self.field_access_expr))

            if call_debug_info:
                result.append(gdb_end())

            # We need to make sure we create a new ownership share for the
            # result of the field access.  Property calls already do that, but
            # we must inc-ref ourselves for other cases.
            if self.type.is_refcounted and self.node_data.access_needs_incref:
                result.append('Inc_Ref ({});'.format(self.result_var.name))

            return '\n'.join(result)

        def _render_expr(self) -> str:
            return (self.result_var.name
                    if self.result_var else self.field_access_expr)

        @property
        def subexprs(self) -> dict:
            result = {'0-prefix': self.receiver_expr,
                      '1-field': self.original_node_data}
            if self.arguments:
                result['2-args'] = self.arguments
            return result

    def __init__(self,
                 receiver: AbstractExpression,
                 field: str,
                 arguments: Optional[FieldAccess.Arguments] = None):
        """
        :param receiver: Expression on which the field access was done.
        :param field: The name of the field that is accessed.
        :param arguments: Assuming field is a property that takes arguments,
            these are passed to it.
        """
        super().__init__()
        self.receiver = receiver
        self.field = field
        self.arguments = arguments
        self.is_deref = False

        self.node_data: AbstractNodeData

    @property
    def diagnostic_context(self) -> ContextManager[None]:
        return diagnostic_context(self.location)

    def resolve_field(self) -> AbstractNodeData:
        """
        Resolve the field that should be accessed, by:

        - Constructing the receiver;
        - Getting its corresponding field.
        """
        self.receiver_expr = construct(self.receiver)
        pfx_type = self.receiver_expr.type

        self.node_data = pfx_type.get_abstract_node_data_dict().get(self.field,
                                                                    None)

        # If still not found, maybe the receiver is an entity, in which case we
        # want to do implicit dereference.
        if not self.node_data and pfx_type.is_entity_type:
            self.node_data = (
                pfx_type.element_type.get_abstract_node_data_dict()
                .get(self.field, None)
            )
            self.is_deref = bool(self.node_data)

        return self.node_data

    @staticmethod
    def common_construct(
        prefix: ResolvedExpression,
        node_data: AbstractNodeData,
        actual_node_data: AbstractNodeData,
        arguments: FieldAccess.Arguments,
        implicit_deref: bool = False,
        abstract_expr: Optional[AbstractExpression] = None,
    ) -> ResolvedExpression:
        """
        Create a resolved expression to access the given field, passing to it
        the given arguments.
        """
        # Check that this property actually accepts these arguments and that
        # they are correctly typed.
        arg_exprs = arguments.construct(node_data)

        # If this field overrides expression construction, delegate it to the
        # corresponding callback.
        if node_data.access_constructor:
            return node_data.access_constructor(prefix, actual_node_data,
                                                arg_exprs, abstract_expr)
        else:
            # Even though it is redundant with DynamicVariable.construct, check
            # that the callee's dynamic variables are bound here so we can emit
            # a helpful error message if that's not the case.
            if isinstance(node_data, PropertyDef):
                DynamicVariable.check_call_bindings(node_data,
                                                    'In call to {prop}')

            return FieldAccess.Expr(
                receiver_expr=prefix,
                node_data=node_data,
                arguments=arg_exprs,
                actual_node_data=actual_node_data,
                implicit_deref=implicit_deref,
                abstract_expr=abstract_expr,
            )

    def construct(self) -> ResolvedExpression:
        """
        Constructs the resolved expression corresponding to this field access.
        It can be either a field access or a property call.
        """

        actual_node_data = node_data = self.resolve_field()

        # If still not found, we have a problem
        check_source_language(
            node_data is not None,
            f"Type {self.receiver_expr.type.dsl_name} has no '{self.field}'"
            f" field or property"
        )

        check_source_language(
            not node_data.is_internal,
            '{} is for internal use only'.format(node_data.qualname)
        )

        # If this is a property call, actually call the root property, as it
        # will be turned into a dispatcher.
        if isinstance(actual_node_data, PropertyDef):
            actual_node_data = actual_node_data.root_property

        args = self.arguments or FieldAccess.Arguments([], {})
        result = self.common_construct(
            self.receiver_expr, node_data, actual_node_data, args,
            implicit_deref=self.is_deref,
            abstract_expr=self,
        )

        # RA22-015: keep a reference to the constructed expr and original
        # accessed field (node data), so that we can introspect which field is
        # accessed in dsl_unparse.
        self.constructed_expr = result
        self.constructed_node_data = node_data
        return result

    def __call__(self, *args: _Any, **kwargs: _Any) -> FieldAccess:
        """
        Build a new FieldAccess instance passing the given arguments.

        :param args: List of arguments for the call.
        :param kwargs: Mapping of arguments for the call.
        """
        assert not self.arguments, 'Cannot call the result of a property'
        return FieldAccess(self.receiver, self.field,
                           self.Arguments(args, kwargs))

    def __repr__(self) -> str:
        return "<FieldAccess .{}{}>".format(self.field,
                                            '(...)' if self.arguments else '')


@attr_call("super")
class Super(AbstractExpression):
    """
    Call the overriden property.

    Note that this construct is valid only in an overriding property.
    """

    def __init__(self, prefix, *args, **kwargs):
        super().__init__()
        self.prefix = prefix
        self.arguments = FieldAccess.Arguments(args, kwargs)

    def __repr__(self) -> str:
        return "<Super {self.prefix}({self.arguments})>"

    def construct(self):
        # This expression calls the property that the current one overrides:
        # get it, making sure it exists and it is concrete.
        prop = PropertyDef.get().base_property
        if prop is None:
            error("There is no overridden property to call")
        check_source_language(
            not prop.abstract,
            "Cannot call abstract overridden property"
        )
        prop.called_by_super = True

        # Make sure the prefix is Self or Entity
        prefix_expr = construct(self.prefix)
        check_source_language(
            isinstance(prefix_expr, VariableExpr)
            and (prefix_expr.abstract_var is Self
                 or prefix_expr.abstract_var is Entity),
            ".super() is allowed on Self or Entity only"
        )

        # If it is Entity, we are calling a property on the corresponding node,
        # i.e. we are performing an implicit dereference.
        implicit_deref = prefix_expr.abstract_var is Entity

        return FieldAccess.common_construct(
            prefix=prefix_expr,
            node_data=prop,
            actual_node_data=prop,
            arguments=self.arguments,
            implicit_deref=implicit_deref,
            abstract_expr=self,
        )


@attr_call('is_a')
class IsA(AbstractExpression):
    """
    Return whether the kind of `node_or_entity` is one of `astnodes` (a list of
    ``ASTNode`` subclasses). Note that if `node_or_entity` is an entity, entity
    types are accepted in `astnodes`.
    """

    class Expr(ComputingExpr):
        pretty_class_name = 'IsA'

        def __init__(self, expr, astnodes, abstract_expr=None):
            """
            :param ResolvedExpr expr: Expression on which the test is
                performed.
            :param [ASTNodeType] astnodes: ASTNodeType subclasses to use for
                the test.
            :param AbstractExpression|None abstract_expr: See
                ResolvedExpression's constructor.
            """
            self.static_type = T.Bool
            self.expr = expr
            self.astnodes = [a.element_type if a.is_entity_type else a
                             for a in astnodes]

            super().__init__('Is_A', abstract_expr=abstract_expr)

        def _render_pre(self):
            target = (('{}.Node' if self.expr.type.is_entity_type else '{}')
                      .format(self.expr.render_expr()))
            result_expr = (
                '{target} /= null \nand then {target}.Kind in {nodes}'.format(
                    target=target,
                    nodes=' | '.join(a.ada_kind_range_name
                                     for a in self.astnodes)
                )
            )
            return '{}\n{}'.format(
                self.expr.render_pre(),
                assign_var(self.result_var.ref_expr, result_expr)
            )

        @property
        def subexprs(self):
            return {'expr': self.expr,
                    'types': [astnode.name for astnode in self.astnodes]}

        def __repr__(self):
            return '<IsA.Expr {}>'.format(', '.join(
                astnode.name.camel for astnode in self.astnodes
            ))

    def __init__(self, node_or_entity, *astnodes):
        """
        :param AbstractExpression node_or_entity: Expression on which the test
            is performed.
        :param ASTNode astnode: ASTNode subclass to use for the test.
        """
        super().__init__()
        self.expr = node_or_entity
        self.astnodes = astnodes

    def construct(self):
        """
        Construct a resolved expression that is the result of testing the kind
        of a node.

        :rtype: IsAExpr
        """
        expr = construct(self.expr)
        as_entity = expr.type.is_entity_type

        def resolve(astnode):
            t = resolve_type(astnode)
            return t.entity if as_entity and t.is_ast_node else t
        astnodes = [resolve(a) for a in self.astnodes]

        for a in astnodes:
            check_source_language(
                a.is_ast_node or a.is_entity_type,
                "Expected ASTNode subclass or entity, got {}".format(a)
            )
            check_source_language(a.matches(expr.type), (
                'When testing the dynamic subtype of an AST node, the type to'
                ' check must be a subclass of the value static type. Here, {}'
                ' is not a subclass of {}.'.format(
                    a.dsl_name, expr.type.dsl_name
                )
            ))
        return IsA.Expr(expr, astnodes, abstract_expr=self)

    def __repr__(self):
        return '<IsA {}>'.format(', '.join(
            resolve_type(n).dsl_name
            for n in self.astnodes
        ))


@attr_call('match')
class Match(AbstractExpression):
    """
    Evaluate various expressions depending on `node_or_entity`'s kind.

    `matchers` must be a sequence of functions that return the expressions to
    evaluate depending on the kind. There are two cases.

    1.  Either they all must accept one optional argument whose default values
        are the types to match. In this case, the set of types to match must
        cover all possible kinds. For instance, given the following type tree::

            Statement
            Expr:
                BinaryOp:
                    PlusOp
                    MinusOp
                Call

        Then given an ``expr`` parameter that yields an ``Expr`` value, the
        following matchers are valid::

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

    2.  Otherwise, all but one must accept such an optional argument.  The only
        other one must accept a mandatory argument and will match the remaining
        cases. For instance::

            Match(expr,
                  lambda e=BinaryOp: X,
                  lambda e: Y)
    """

    class Matcher:
        def __init__(self, match_var, match_expr, inner_scope):
            """
            :param VariableExpr match_var: Variable to hold the matched
                node/entity.
            :param ResolvedExpression match_expr: Expression to evaluate when
                this matcher is active.
            :param LocalVars.Scope inner_scope: Scope that will wrap
                `match_expr` so that the definition of `match_var` is confined
                to it.
            """
            self.match_var = match_var
            self.match_expr = match_expr
            self.inner_scope = inner_scope

            self.matched_concrete_nodes = None
            """
            Set of all concrete AST nodes that this matcher can accept.
            Computed in Match.Expr's constructor.

            :type: set[ASTNodeType]
            """

        @property
        def match_type(self):
            """
            Return the type that this matcher matches (AST node or entity).

            :rtype: ASTNodeType|StructType
            """
            return self.match_var.type

        @property
        def match_astnode_type(self):
            """
            Return the AST node type that this matcher matches, or the
            corresponding AST node if it matches an entity type.

            :rtype: ASTNodeType
            """
            return (self.match_type.element_type
                    if self.match_type.is_entity_type else
                    self.match_type)

    class Expr(ComputingExpr):

        def __init__(self, prefix_expr, matchers, abstract_expr=None):
            """
            :param ResolvedExpression prefix_expr: The expression on which the
                dispatch occurs. It must be either an AST node or an entity.
            :param list[Match.Matcher] matchers: List of matchers for this
                node.
            """
            assert (prefix_expr.type.is_ast_node or
                    prefix_expr.type.is_entity_type)
            self.prefix_expr = NullCheckExpr(
                prefix_expr,
                implicit_deref=prefix_expr.type.is_entity_type
            )

            # Variable to hold the value of which we do dispatching
            # (prefix_expr).
            self.prefix_var = PropertyDef.get().vars.create(
                'Match_Prefix', self.prefix_expr.type)

            # Compute the return type as the unification of all branches
            rtype = matchers[-1].match_expr.type
            for m in matchers:
                rtype = m.match_expr.type.unify(
                    rtype,
                    'Mismatching types in Match expression: got {self} but'
                    ' expected {other} or sub/supertype'
                )
            self.static_type = rtype

            # Wrap each matcher expression so that all capture variables are
            # bound and initialized.
            self.matchers = []
            for m in matchers:
                # Initialize match_var. Note that assuming the code generation
                # is bug-free, this cast cannot fail, so don't generate type
                # check boilerplate.
                let_expr = Let.Expr(
                    [m.match_var],
                    [Cast.Expr(self.prefix_var.ref_expr, m.match_var.type,
                               unsafe=True)],

                    # ... and cast this matcher's result to the Match result's
                    # type, as required by OOP with access types in Ada.
                    (m.match_expr
                     if m.match_expr.type == rtype else
                     Cast.Expr(m.match_expr, rtype))
                )

                # Now do the binding for static analysis and debugging
                self.matchers.append(Match.Matcher(
                    m.match_var,
                    BindingScope(let_expr, [], scope=m.inner_scope),
                    m.inner_scope
                ))

            # Determine for each matcher the set of concrete AST nodes it can
            # actually match.
            prefix_type = self.prefix_expr.type
            if prefix_type.is_entity_type:
                prefix_type = prefix_type.element_type
            matched_types, remainder = collapse_concrete_nodes(
                (prefix_type.element_type
                 if prefix_type.is_entity_type else prefix_type),
                [m.match_astnode_type for m in self.matchers]
            )
            assert not remainder
            for matcher, matched in zip(self.matchers, matched_types):
                matcher.matched_concrete_nodes = matched

            super().__init__('Match_Result', abstract_expr=abstract_expr)

        def _render_pre(self):
            return render('properties/match_ada', expr=self)

        @property
        def subexprs(self):
            return {'prefix': self.prefix_expr,
                    'matchers': [m.match_expr for m in self.matchers]}

        def __repr__(self):
            return '<Match.Expr>'

    def __init__(self, node_or_entity, *matchers):
        """
        :param AbstractExpression node_or_entity: The expression to match.

        :param matchers: See the class docstring.
        :type matchers: list[() -> AbstractExpression]
        """
        super().__init__()
        self.matched_expr = node_or_entity
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
                    match_type.is_ast_node
                    or match_type.is_entity_type,
                    'Invalid matching type: {}'.format(
                        match_type.dsl_name
                    )
                )
            else:
                match_type = None

            # We allow only valid identifiers or the wildcard ("_") for match
            # bindings.
            var_name = argspec.args[0]
            if var_name != "_":
                try:
                    _ = names.Name.from_lower(var_name)
                except ValueError as exc:
                    check_source_language(False, str(exc))

            match_var = AbstractVariable(
                names.Name('Match_{}'.format(i)),
                type=match_type,
                create_local=True,
                source_name=var_name,
            )
            self.matchers.append((match_type, match_var, match_fn(match_var)))

    def _check_match_coverage(self, input_type):
        """
        Given some input type for this match expression, make sure the set of
        matchers cover all cases. check_source_language will raise an error if
        it's not the case. Also emit warnings for unreachable matchers.

        :param ASTNodeType input_type: Type to check.
        :rtype: None
        """

        type_set = TypeSet()
        is_entity = input_type.is_entity_type

        if is_entity:
            input_type = input_type.element_type

        for i, (typ, _, _) in enumerate(self.matchers, 1):
            t_name = 'default one' if typ is None else typ.dsl_name

            if typ and typ.is_entity_type:
                typ = typ.element_type

            check_source_language(not type_set.include(typ or input_type),
                                  'The #{} matcher ({}) is unreachable'
                                  ' as all previous matchers cover all the'
                                  ' nodes it can match'.format(i, t_name),
                                  Severity.warning)

        mm = sorted(type_set.unmatched_types(input_type),
                    key=lambda cls: cls.hierarchical_name)

        check_source_language(
            not mm,
            'The following AST nodes have no handler: {} (all {} subclasses'
            ' require one)'.format(
                ', '.join(typ.dsl_name for typ in mm),
                input_type.dsl_name
            )
        )

    def construct(self):
        """
        Construct a resolved expression for this.

        :rtype: ResolvedExpression
        """
        outer_scope = PropertyDef.get_scope()

        matched_expr = construct(
            self.matched_expr,
            lambda t: t.is_ast_node or t.is_entity_type,
            'Match expressions can only work on AST nodes or entities: got'
            ' {expr_type} instead'
        )
        is_entity = matched_expr.type.is_entity_type
        matchers = []

        # Check (i.e. raise an error if no true) the set of matchers is valid:

        # * all matchers must target allowed types, i.e. input type subclasses;
        for typ, var, expr in self.matchers:
            if is_entity and typ and not typ.is_entity_type:
                typ = typ.entity
                var._type = typ
                var.local_var.type = typ

            if typ is not None:
                check_source_language(
                    typ.matches(matched_expr.type),
                    'Cannot match {} (input type is {})'.format(
                        typ.dsl_name,
                        matched_expr.type.dsl_name
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
                matchers.append(Match.Matcher(construct(var),
                                construct(expr),
                                inner_scope))

        # * all possible input types must have at least one matcher. Also warn
        #   if some matchers are unreachable.
        self._check_match_coverage(matched_expr.type)

        return Match.Expr(matched_expr, matchers, abstract_expr=self)

    def __repr__(self):
        return '<Match>'


@attr_call('update',
           doc='Create a new struct value, replacing fields with the given'
               ' values.')
class StructUpdate(AbstractExpression):

    class Expr(ComputingExpr):
        pretty_class_name = 'StructUpdate'

        def __init__(self, expr, assocs, abstract_expr=None):
            """
            :type expr: ResolvedExpression
            :type assocs: dict[UserField, ResolvedExpression
            :type None|AbstractExpression: ResolvedExpression
            """
            self.static_type = expr.type
            self.expr = expr
            self.assocs = assocs
            super().__init__('Update_Result', abstract_expr=abstract_expr)

        def _render_pre(self):
            return render('properties/update_ada', expr=self)

        @property
        def subexprs(self):
            return {'expr': self.expr,
                    'assocs': {f.original_name: f_expr
                               for f, f_expr in self.assocs.items()}}

        def __repr__(self):
            return '<StructUpdate.Expr>'

    def __init__(self, expr, **kwargs):
        """
        :param AbstractExpression expr: Original structure copy.
        :param dict[str, AbstractExpression] kwargs: Field/value associations
            to replace in the copy.
        """
        super().__init__()
        self.expr = expr
        self.assocs = kwargs

    def construct(self):
        # Construct the expression for the original struct
        expr = construct(
            self.expr,
            lambda expr_type: expr_type.is_struct_type,
            'Struct expected, got {expr_type}'
        )

        # Check that all fields are valid structure fields. Also compile them,
        # checking their types.
        fields = expr.type.required_fields_in_exprs
        assocs = {}
        for name, field_expr in sorted(self.assocs.items()):
            check_source_language(
                name in fields,
                'Invalid {} field: {}'.format(expr.type.dsl_name, name)
            )
            field = fields[name]
            assocs[field] = construct(
                field_expr,
                fields[name].type,
                f"Wrong type for field {fields[name].qualname}:"
                f" expected {{expected}}, got {{expr_type}}"
            )

        return StructUpdate.Expr(expr, assocs, abstract_expr=self)

    def __repr__(self):
        return '<StructUpdate>'
