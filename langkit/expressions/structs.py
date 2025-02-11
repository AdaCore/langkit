from __future__ import annotations

from contextlib import AbstractContextManager
from typing import Any as _Any, Sequence

import funcy

from langkit import names
from langkit.compiled_types import (
    ASTNodeType,
    AbstractNodeData,
    BaseField,
    BaseStructType,
    CompiledType,
    EntityType,
    Field,
    NodeBuilderType,
    StructType,
)
from langkit.diagnostics import (
    Location,
    Severity,
    check_source_language,
    diagnostic_context,
    error,
)
from langkit.expressions import (
    AbstractExpression,
    AbstractVariable,
    BasicExpr,
    BindingScope,
    ComputingExpr,
    DynamicVariable,
    Let,
    LocalVars,
    NullCheckExpr,
    NullExpr,
    PropertyDef,
    ResolvedExpression,
    SavedExpr,
    T,
    VariableExpr,
    abstract_expression_from_construct,
    construct,
    construct_compile_time_known,
    dsl_document,
    gdb_end,
    gdb_property_call_start,
    render,
)
from langkit.expressions.boolean import Eq
from langkit.expressions.utils import assign_var
from langkit.utils import TypeSet, collapse_concrete_nodes, memoized


class Cast(AbstractExpression):
    """
    Downcast the AST `node` to the more specific `dest_type` AST node type.

    This returns a null node if the actual type of `node` is not a subtype of
    `dest_type`.
    """

    class Expr(ComputingExpr):
        pretty_class_name = 'Cast'

        def __init__(
            self,
            expr: ResolvedExpression,
            dest_type: ASTNodeType | EntityType,
            do_raise: bool = False,
            unsafe: bool = False,
            abstract_expr: AbstractExpression | None = None,
        ):
            """
            :param unsafe: If true, elide the type check before doing the cast.
                This is used to avoid noisy and useless type checks in
                generated code: these checks would fail only because of a bug
                in the code generator.
            :param abstract_expr: See ResolvedExpression's constructor.
            """
            self.do_raise = do_raise
            self.unsafe = unsafe
            self.expr = SavedExpr('Cast_Expr', expr)
            self.static_type = dest_type
            super().__init__('Cast_Result', abstract_expr=abstract_expr)

        def _render_pre(self) -> str:
            return render('properties/cast_ada', expr=self)

        @property
        def subexprs(self) -> dict:
            assert self.static_type is not None
            return {'expr': self.expr, 'type': self.static_type.name}

        def __repr__(self) -> str:
            assert self.static_type is not None
            return '<Cast.Expr {}>'.format(self.static_type.dsl_name)

        @property
        def dest_node(self) -> ASTNodeType:
            """
            Return the node type (not entity) that is the result of the cast
            expression.
            """
            return (self.type.element_type
                    if self.type.is_entity_type else self.type)

        @property
        def input_node(self) -> ASTNodeType:
            """
            Return the node type (not entity) that is the input of the cast
            expression.
            """
            return (self.expr.type.element_type
                    if self.expr.type.is_entity_type else self.expr.type)

        @property
        def check_needed(self) -> bool:
            """
            Return whether we must generate a dynamic check on the kind of the
            input expression before doing the cast.
            """
            # If asked to do an unsafe conversion, then by definition we must
            # not generate a check.
            if self.unsafe:
                return False

            # Don't generate a dynamic check if we know statically that there
            # is no failure possible (this is an upcast, or a downcast when
            # there is only one subclass, etc.).
            return self.input_node not in TypeSet({self.dest_node})

    def __init__(
        self,
        location: Location,
        node: AbstractExpression,
        dest_type: CompiledType,
        do_raise: bool = False,
    ):
        """
        :param node: Expression on which the cast is performed.
        :param dest_type: Type to use for the cast.
        :param do_raise: Whether the exception should raise an exception or
            return null when the cast is invalid.
        """
        super().__init__(location)
        self.expr = node
        self.dest_type = dest_type
        self.do_raise = do_raise

    def construct(self) -> ResolvedExpression:
        """
        Construct a resolved expression that is the result of casting a AST
        node.
        """
        expr = construct(self.expr)
        t = expr.type

        # Determine the bare node type for the cast
        node_dest_type: ASTNodeType
        match self.dest_type:
            case ASTNodeType():
                node_dest_type = self.dest_type
            case EntityType():
                node_dest_type = self.dest_type.element_type
            case _:
                error(
                    "One can only cast to an ASTNode subtype or to an entity"
                )

        # If the cast prefix is an entity, promote the dest type to the entity
        # type.
        dest_type = (
            node_dest_type.entity
            if t.is_entity_type else
            node_dest_type
        )

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

    def __repr__(self) -> str:
        return f"<Cast to {self.dest_type.dsl_name} at {self.location_repr}>"


def make_is_null(
    expr: ResolvedExpression,
    abstract_expr: AbstractExpression | None = None,
) -> ResolvedExpression:
    if isinstance(expr.type, ASTNodeType):
        return BasicExpr(
            "Is_Null", "{} = null", T.Bool, [expr], abstract_expr=abstract_expr
        )
    elif isinstance(expr.type, EntityType):
        return BasicExpr(
            "Is_Null",
            "{}.Node = null",
            T.Bool,
            [expr],
            abstract_expr=abstract_expr,
        )
    else:
        return Eq.make_expr(
            expr, NullExpr(expr.type), abstract_expr=abstract_expr
        )


@abstract_expression_from_construct
def is_null(
    self: AbstractExpression,
    expr: AbstractExpression,
) -> ResolvedExpression:
    """
    If ``expr`` is an entity, return whether the corresponding node is null
    (even if the entity info is non null). For all other types, return whether
    it is the null value.
    """
    return make_is_null(construct(expr), abstract_expr=self)


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

        def __init__(
            self,
            struct_type: BaseStructType,
            assocs: dict[names.Name | BaseField, ResolvedExpression],
            result_var_name: str | names.Name | None = None,
            abstract_expr: AbstractExpression | None = None,
        ):
            self.static_type = struct_type

            # Convert names in `assocs` to the corresponding field in
            # struct_type. This lets callers use either names or fields,
            # depending on what's the most convenient for them.

            def field_or_lookup(field: names.Name | BaseField) -> BaseField:
                if isinstance(field, names.Name):
                    fields = struct_type.get_abstract_node_data_dict()
                    return fields[field.lower]
                assert isinstance(field, BaseField)
                return field

            self.assocs = {field_or_lookup(field): expr
                           for field, expr in assocs.items()}

            super().__init__(
                result_var_name or 'New_Struct',
                abstract_expr=abstract_expr
            )

        def _iter_ordered(self) -> list[tuple[BaseField, ResolvedExpression]]:
            return sorted(
                [(field, expr) for field, expr in self.assocs.items()],
                key=lambda assoc: assoc[0].names.index
            )

        def _render_fields(self) -> str:
            """
            Helper to return the elaboration of structure fields plus the
            ref-counting adjustment to create new ownership shares.
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

        def _render_pre(self) -> str:
            record_expr = '({})'.format(', '.join(
                '{} => {}'.format(field.names.codegen, expr.render_expr())
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
        def subexprs(self) -> dict:
            result = {field.names.index: expr
                      for field, expr in self.assocs.items()}
            assert self.static_type is not None
            result['_type'] = self.static_type.dsl_name
            return result

        def __repr__(self) -> str:
            assert self.static_type is not None
            return '<New.{} {}>'.format(type(self).__name__,
                                        self.static_type.name.camel)

    class NodeExpr(StructExpr):
        """
        Resolved expression to create AST node values.
        """

        def __init__(
            self,
            astnode: ASTNodeType,
            assocs: dict[names.Name | BaseField, ResolvedExpression],
            abstract_expr: AbstractExpression | None = None,
        ):
            super().__init__(
                astnode, assocs, 'New_Node', abstract_expr=abstract_expr
            )

            # The synthetized node inherits Self.Self_Env, so PLE must happen
            # before this property is run.
            PropertyDef.get().set_uses_envs()

        def _render_pre(self) -> str:
            return (super()._render_fields()
                    + render('properties/new_astnode_ada', expr=self))

    def __init__(
        self,
        location: Location,
        struct_type: CompiledType,
        **field_values: AbstractExpression,
    ):
        """
        :param struct_type: CompiledType instance for the struct type this
            expression must create.
        :param field_values: Values to assign to the fields for the created
            struct value.
        """
        super().__init__(location)
        self.struct_type = struct_type
        self.field_values = field_values

    @staticmethod
    def construct_fields(
        struct_type: BaseStructType,
        field_values: dict[str, AbstractExpression],
        for_node_builder: bool = False,
    ) -> dict[BaseField, ResolvedExpression]:
        """
        Helper to lower fields in a struct/node constructor.

        :param struct_type: Struct/node type for which we construct fields.
        :param field_values: Mapping from field DSL name to associated field
            initialization expression.
        :param for_node_builder: Whether we are in the context of node
            builders: parse field values are expected to be node builders.
        :return: A mapping from fields to initialize to the corresponding
            constructed initialization expression.
        """
        # Create a dict of field names to fields in the struct type, and
        # another dict for default values.
        required_fields = struct_type.required_fields_in_exprs
        default_valued_fields = {
            n: (f, f.abstract_default_value)
            for n, f in required_fields.items()
            if f.abstract_default_value is not None
        }

        # Make sure the provided set of fields matches the one the struct needs

        def error_if_not_empty(name_set: set[str], message: str) -> None:
            if name_set:
                names_str = ", ".join(sorted(name_set))
                error(f"{message}: {names_str}")

        error_if_not_empty(
            set(required_fields)
            - set(field_values)
            - set(default_valued_fields),
            f"Values are missing for {struct_type.dsl_name} fields",
        )
        error_if_not_empty(
            set(field_values.keys()) - set(required_fields),
            f"Extraneous fields for {struct_type.dsl_name}",
        )

        # At this stage, we know that the user provided all required fields,
        # and no spurious fields. Construct expressions for field values.
        result = {
            required_fields[name]: construct(expr)
            for name, expr in field_values.items()
        }

        # Add default values for missing fields. Note that we construct their
        # abstract expressions on purpose: even though a resolved expression is
        # already present in field.default_value, reusing ResolvedExpression
        # nodes in multiple expressions is forbidden. Constructing a new each
        # time avoids this problem.
        for name, (field, default_value) in default_valued_fields.items():
            if field not in result:
                result[field] = construct_compile_time_known(default_value)

        # Then check that the type of these expressions match field types
        for field, expr in result.items():
            expected_type = field.type
            actual_type = expr.type
            actual_node_type: ASTNodeType

            # If we are creating a node builder, expect node builders to
            # initialize parse fields.
            if for_node_builder and isinstance(field, Field):
                assert isinstance(expected_type, ASTNodeType)
                expected_type = expected_type.builder_type

            check_source_language(
                actual_type.matches(expected_type),
                f"Wrong type for field {field.qualname}: expected"
                f" {expected_type.dsl_name}, got {actual_type.dsl_name}"
            )

            if for_node_builder and isinstance(field, Field):
                assert isinstance(actual_type, NodeBuilderType)
                actual_node_type = actual_type.node_type
            elif isinstance(field, Field):
                assert isinstance(actual_type, ASTNodeType)
                actual_node_type = actual_type

            # Annotate parsing/synthetic fields with precise type information
            if isinstance(field, Field):
                field.types_from_synthesis.include(actual_node_type)

            # Make sure we downcast input values so that they fit in the fields
            if (
                (
                    isinstance(expected_type, ASTNodeType)
                    or isinstance(expected_type, EntityType)
                )
                and actual_type != expected_type
            ):
                result[field] = Cast.Expr(expr, expected_type)

            # Also mark parse fields as synthetized
            if isinstance(field, Field):
                field.set_synthetized()

        return result

    def construct(self) -> ResolvedExpression:
        if not isinstance(self.struct_type, BaseStructType):
            error(
                "Invalid type, expected struct type or AST node, got"
                f" {self.struct_type.dsl_name}"
            )

        if isinstance(self.struct_type, ASTNodeType):
            if not self.struct_type.synthetic:
                error(
                    "Cannot synthetize a node that is not annotated as"
                    f" synthetic ({self.struct_type.dsl_name})"
                )

            if self.struct_type.is_list_type:
                error('List node synthetization is not supported for now')

            prop = PropertyDef.get()
            if not prop.memoized and not prop.lazy_field:
                error(
                    "Node synthetization can only happen inside memoized"
                    " properties or lazy fields"
                )

        field_values = self.construct_fields(
            self.struct_type, self.field_values
        )

        expr_cls = (New.NodeExpr
                    if self.struct_type.is_ast_node else
                    New.StructExpr)
        return expr_cls(self.struct_type, field_values, abstract_expr=self)

    def __repr__(self) -> str:
        return f"<New {self.struct_type.dsl_name} at {self.location_repr}>"


class FieldAccess(AbstractExpression):
    """
    Abstract expression that is the result of a field access expression
    evaluation.
    """

    class Arguments:
        """
        Holder for arguments to pass to a property.
        """

        def __init__(self,
                     args: Sequence[AbstractExpression],
                     kwargs: dict[str, AbstractExpression]):
            self.args = args
            self.kwargs = kwargs

        def associate(self, node_data: AbstractNodeData) -> list[
            tuple[int | str, AbstractExpression | None]
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
            result: list[tuple[int | str, AbstractExpression | None]] = []
            for arg_spec in node_data.natural_arguments:
                actual: AbstractExpression | None

                # Look for a keyword argument corresponding to `arg_spec`
                arg_name = arg_spec.name.lower
                key: int | str = arg_spec.name.lower
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
        ) -> list[ResolvedExpression | None]:
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
                     arguments: Sequence[ResolvedExpression | None],
                     actual_node_data: AbstractNodeData | None = None,
                     implicit_deref: bool = False,
                     unsafe: bool = False,
                     abstract_expr: AbstractExpression | None = None):
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
            p = PropertyDef.get_or_none()
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

                def actual(dynvar: DynamicVariable) -> ResolvedExpression:
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
        def entity_info_expr(self) -> str | None:
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
                        or PropertyDef.get_or_none() is None)
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
                    args.append((formal.argument_name.camel_with_underscores,
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
                assert isinstance(self.type, EntityType)
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
                assert isinstance(self.node_data, PropertyDef)
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
            return (str(self.result_var.name)
                    if self.result_var else
                    self.field_access_expr)

        @property
        def subexprs(self) -> dict:
            result = {'0-prefix': self.receiver_expr,
                      '1-field': self.original_node_data}
            if self.arguments:
                result['2-args'] = self.arguments
            return result

    def __init__(
        self,
        location: Location,
        receiver: AbstractExpression,
        field: str,
        arguments: FieldAccess.Arguments | None = None,
        check_call_syntax: bool = False,
    ):
        """
        :param receiver: Expression on which the field access was done.
        :param field: The name of the field that is accessed.
        :param arguments: Assuming field is a property that takes arguments,
            these are passed to it.
        :param check_call_syntax: Whether the presence/absence of a call syntax
            for the accessed node data must be checked (True for Lkt
            expressions, False for DSL expressions).
        """
        super().__init__(location)
        self.receiver = receiver
        self.field = field
        self.arguments = arguments
        self.is_deref = False
        self.check_call_syntax = check_call_syntax

        self.node_data: AbstractNodeData

    @property
    def diagnostic_context(self) -> AbstractContextManager[None]:
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
        abstract_expr: AbstractExpression | None = None,
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
            actual_node_data = actual_node_data.root

            if self.check_call_syntax:
                # Reject the call syntax for 1) lazy fields and 2) properties
                # with the "property" annotation, and mandate it for all the
                # other properties.
                if actual_node_data.lazy_field:
                    check_source_language(
                        self.arguments is None, "cannot call a lazy field"
                    )

                elif actual_node_data.has_property_syntax:
                    check_source_language(
                        self.arguments is None,
                        "argument list forbidden with @property",
                    )

                else:
                    check_source_language(
                        self.arguments is not None,
                        "call syntax is mandatory for properties",
                    )
        elif self.check_call_syntax:
            # Reject the call syntax for anything that is not a property
            check_source_language(
                self.arguments is None, "cannot call a field"
            )

        args = self.arguments or FieldAccess.Arguments([], {})
        return self.common_construct(
            self.receiver_expr, node_data, actual_node_data, args,
            implicit_deref=self.is_deref,
            abstract_expr=self,
        )

    def __call__(self, *args: _Any, **kwargs: _Any) -> FieldAccess:
        """
        Build a new FieldAccess instance passing the given arguments.

        :param args: List of arguments for the call.
        :param kwargs: Mapping of arguments for the call.
        """
        assert not self.arguments, 'Cannot call the result of a property'
        return FieldAccess(
            self.location,
            self.receiver,
            self.field,
            self.Arguments(args, kwargs),
        )

    def __repr__(self) -> str:
        return f"<FieldAccess for {self.field} at {self.location_repr}>"


class Super(AbstractExpression):
    """
    Call the overriden property.

    Note that this construct is valid only in an overriding property.
    """

    def __init__(
        self,
        location: Location,
        prefix: AbstractExpression,
        *args: AbstractExpression,
        **kwargs: AbstractExpression
    ):
        super().__init__(location)
        self.prefix = prefix
        self.arguments = FieldAccess.Arguments(args, kwargs)

    @staticmethod
    def implicit_deref_required(
        prefix: ResolvedExpression,
        current_prop: PropertyDef,
    ) -> bool:
        """
        Return whether calling the overriden property on ``prefix`` requires an
        implicit entity dereference (properties are called on bare nodes in the
        generated code). This also validates that ``prefix`` is either a
        reference to the ``node`` or the ``self`` special variables.

        :param prefix: ``Super`` prefix expression.
        :param current_prop: Property that contains the ``Super`` expression.
        """
        if isinstance(prefix, VariableExpr):
            if prefix.abstract_var is current_prop.node_var:
                return False
            elif (
                current_prop.has_self_var
                and prefix.abstract_var is current_prop.self_var
            ):
                return True

        error(".super() is allowed on Self or Entity only")

    def construct(self) -> ResolvedExpression:
        p = PropertyDef.get()

        # This expression calls the property that the current one overrides:
        # get it, making sure it exists and it is concrete.
        base = p.base
        if base is None:
            error("There is no overridden property to call")
        check_source_language(
            not base.abstract,
            "Cannot call abstract overridden property"
        )
        base.called_by_super = True

        prefix_expr = construct(self.prefix)
        return FieldAccess.common_construct(
            prefix=prefix_expr,
            node_data=base,
            actual_node_data=base,
            arguments=self.arguments,
            implicit_deref=self.implicit_deref_required(prefix_expr, p),
            abstract_expr=self,
        )


class IsAExpr(ComputingExpr):
    pretty_class_name = 'IsA'

    def __init__(
        self,
        expr: ResolvedExpression,
        types: list[ASTNodeType],
        abstract_expr: AbstractExpression | None = None,
    ):
        """
        :param expr: Expression on which the test is performed.
        :param types: ASTNodeType instances to use for the test.
        :param abstract_expr: See ResolvedExpression's constructor.
        """
        self.static_type = T.Bool
        self.expr = expr
        self.types = types

        super().__init__('Is_A', abstract_expr=abstract_expr)

    def _render_pre(self) -> str:
        target = (('{}.Node' if self.expr.type.is_entity_type else '{}')
                  .format(self.expr.render_expr()))
        result_expr = (
            '{target} /= null \nand then {target}.Kind in {nodes}'.format(
                target=target,
                nodes=' | '.join(a.ada_kind_range_name for a in self.types)
            )
        )
        return '{}\n{}'.format(
            self.expr.render_pre(),
            assign_var(self.result_var.ref_expr, result_expr)
        )

    @property
    def subexprs(self) -> dict:
        return {'expr': self.expr,
                'types': [astnode.name for astnode in self.types]}

    def __repr__(self) -> str:
        return '<IsA.Expr {}>'.format(', '.join(
            astnode.name.camel for astnode in self.types
        ))


@abstract_expression_from_construct
def is_a(
    self: AbstractExpression,
    expr: AbstractExpression,
    types: list[CompiledType],
) -> ResolvedExpression:
    """
    Return whether the kind of ``expr`` is one of ``types``. Note that if
    ``expr`` is an entity, entity types are accepted in ``types``.
    """
    e = construct(expr)
    if isinstance(e.type, EntityType):
        expr_node = e.type.element_type
        is_entity = True
    elif isinstance(e.type, ASTNodeType):
        expr_node = e.type
        is_entity = False
    else:
        error(f"node or entity expected, got {e.type.dsl_name}")

    node_types: list[ASTNodeType] = []
    for t in types:
        nt: ASTNodeType | None = None
        if isinstance(t, ASTNodeType):
            nt = t
        elif isinstance(t, EntityType) and is_entity:
            nt = t.element_type

        if nt is None:
            error(f"node or entity type expected, got {t.dsl_name}")
        check_source_language(
            nt.matches(expr_node),
            "When testing the dynamic subtype of an AST node, the type to"
            " check must be a subclass of the value static type. Here,"
            f" {t.dsl_name} is not a subclass of {expr_node.dsl_name}.",
        )
        node_types.append(nt)
    return IsAExpr(e, node_types, abstract_expr=self)


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
        def __init__(
            self,
            match_var: VariableExpr,
            match_expr: ResolvedExpression,
            inner_scope: LocalVars.Scope,
        ):
            """
            :param match_var: Variable to hold the matched node/entity.
            :param match_expr: Expression to evaluate when this matcher is
                active.
            :param inner_scope: Scope that will wrap `match_expr` so that the
                definition of `match_var` is confined to it.
            """
            self.match_var = match_var
            self.match_expr = match_expr
            self.inner_scope = inner_scope

            self.matched_concrete_nodes: set[ASTNodeType]
            """
            Set of all concrete nodes that this matcher can accept. Set in
            Match.Expr's constructor.
            """

        @property
        def match_type(self) -> ASTNodeType | EntityType:
            """
            Return the type that this matcher matches (node or entity).
            """
            result = self.match_var.type
            assert isinstance(result, (ASTNodeType, EntityType))
            return result

        @property
        def match_astnode_type(self) -> ASTNodeType:
            """
            Return the node type that this matcher matches, or the
            corresponding node if it matches an entity type.
            """
            return (
                self.match_type.element_type
                if isinstance(self.match_type, EntityType) else
                self.match_type
            )

    class Expr(ComputingExpr):

        def __init__(
            self,
            prefix_expr: ResolvedExpression,
            matchers: list[Match.Matcher],
            abstract_expr: AbstractExpression | None = None,
        ):
            """
            :param prefix_expr: The expression on which the dispatch occurs. It
                must be either an AST node or an entity.
            :param matchers: List of matchers for this node.
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
                m_var_type = m.match_var.type
                assert isinstance(m_var_type, (ASTNodeType, EntityType))
                # Initialize match_var. Note that assuming the code generation
                # is bug-free, this cast cannot fail, so don't generate type
                # check boilerplate.
                let_expr = Let.Expr(
                    [
                        (
                            m.match_var,
                            Cast.Expr(
                                self.prefix_var.ref_expr,
                                m_var_type,
                                unsafe=True,
                            ),
                        )
                    ],

                    # ... and cast this matcher's result to the Match result's
                    # type, as required by OOP with access types in Ada.
                    m.match_expr.unified_expr(rtype)
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

        def _render_pre(self) -> str:
            return render('properties/match_ada', expr=self)

        @property
        def subexprs(self) -> dict:
            return {'prefix': self.prefix_expr,
                    'matchers': [m.match_expr for m in self.matchers]}

        def __repr__(self) -> str:
            return '<Match.Expr>'

    def __init__(
        self,
        location: Location,
        expr: AbstractExpression,
        matchers: Sequence[
            tuple[CompiledType | None, AbstractVariable, AbstractExpression]
        ],
    ):
        """
        :param expr: The expression to match.
        :param matchers: Sequence of descriptions for each matcher: the type
            that must be matched, the variable assigned for this matcher and
            the expression for the result.
        """
        super().__init__(location)
        self.expr = expr
        self.matchers = matchers

    def _check_match_coverage(
        self,
        input_type: ASTNodeType,
        matched_types: list[ASTNodeType | None],
    ) -> None:
        """
        Given some input type for this match expression, make sure the set of
        matchers cover all cases. check_source_language will raise an error if
        it's not the case. Also emit warnings for unreachable matchers.

        :param input_type: Type to check.
        :param matched_types: Node types that are matched, in the same order as
            matchers.
        """
        type_set: TypeSet[ASTNodeType] = TypeSet()

        for i, typ in enumerate(matched_types, 1):
            t_name = 'default one' if typ is None else typ.dsl_name
            check_source_language(
                not type_set.include(typ or input_type),
                f"The #{i} matcher ({t_name}) is unreachable as all previous"
                " matchers cover all the nodes it can match",
                Severity.warning,
            )

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

    def construct(self) -> ResolvedExpression:
        outer_scope = PropertyDef.get_scope()

        expr = construct(self.expr)
        is_entity = expr.type.is_entity_type
        if isinstance(expr.type, ASTNodeType):
            input_node = expr.type
        elif isinstance(expr.type, EntityType):
            input_node = expr.type.element_type
        else:
            error(
                "Match expressions can only work on AST nodes or entities: got"
                f" {expr.type.dsl_name} instead"
            )

        matched_types: list[ASTNodeType | None] = []
        matchers: list[Match.Matcher] = []

        # Check (i.e. raise an error if no true) the set of matchers is valid:

        # * all matchers must target allowed types, i.e. input type subclasses;
        for typ, var, sub_expr in self.matchers:
            assert var.local_var is not None
            if typ is not None:
                if isinstance(typ, EntityType):
                    if not is_entity:
                        error("bare node expected")
                    node_type = typ.element_type
                elif isinstance(typ, ASTNodeType):
                    node_type = typ
                else:
                    error(
                        f"Cannot match {typ.dsl_name} (input type is"
                        f" {expr.type.dsl_name})"
                    )

                matched_type = node_type.entity if is_entity else node_type
                var._type = matched_type
                var.local_var.type = matched_type

                check_source_language(
                    matched_type.matches(expr.type),
                    'Cannot match {} (input type is {})'.format(
                        matched_type.dsl_name,
                        expr.type.dsl_name
                    )
                )
                matched_types.append(node_type)
            else:
                # The default matcher (if any) matches the most general type,
                # which is the input type.
                var.set_type(expr.type)
                matched_types.append(None)

            # Create a scope so that match_var is contained in this branch as
            # is not exposed outside in the debug info.
            with outer_scope.new_child() as inner_scope:
                inner_scope.add(var.local_var)
                v = construct(var)
                assert isinstance(v, VariableExpr)
                matchers.append(
                    Match.Matcher(v, construct(sub_expr), inner_scope)
                )

        # * all possible input types must have at least one matcher. Also warn
        #   if some matchers are unreachable.
        self._check_match_coverage(input_node, matched_types)

        return Match.Expr(expr, matchers, abstract_expr=self)


class StructUpdate(AbstractExpression):
    """
    Create a new struct value, replacing fields with the given values.
    """

    class Expr(ComputingExpr):
        pretty_class_name = 'StructUpdate'

        def __init__(
            self,
            expr: ResolvedExpression,
            assocs: dict[BaseField, ResolvedExpression],
            abstract_expr: AbstractExpression | None = None,
        ):
            """
            """
            self.static_type = expr.type
            self.expr = expr
            self.assocs = assocs
            super().__init__('Update_Result', abstract_expr=abstract_expr)

        def _render_pre(self) -> str:
            return render('properties/update_ada', expr=self)

        @property
        def subexprs(self) -> dict:
            return {'expr': self.expr,
                    'assocs': {f.original_name: f_expr
                               for f, f_expr in self.assocs.items()}}

        def __repr__(self) -> str:
            return '<StructUpdate.Expr>'

    def __init__(
        self,
        location: Location,
        expr: AbstractExpression,
        **kwargs: AbstractExpression,
    ):
        """
        :param expr: Original structure copy.
        :param kwargs: Field/value associations to replace in the copy.
        """
        super().__init__(location)
        self.expr = expr
        self.assocs = kwargs

    def construct(self) -> ResolvedExpression:
        # Construct the expression for the original struct
        expr = construct(self.expr)
        if not isinstance(expr.type, StructType):
            error(
                f"Struct expected, got {expr.type.dsl_name}",
                location=self.expr.location,
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
