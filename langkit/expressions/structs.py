from __future__ import annotations

from typing import Sequence, TYPE_CHECKING

import funcy

from langkit import names
from langkit.compiled_types import (
    ASTNodeType,
    AbstractNodeData,
    BaseField,
    BaseStructType,
    EntityType,
    Field,
    NodeBuilderType,
)
from langkit.diagnostics import Location, check_source_language, error
from langkit.expressions import (
    BasicExpr,
    BindingScope,
    ComputingExpr,
    Expr,
    ExprDebugInfo,
    LetExpr,
    LocalVars,
    NullCheckExpr,
    NullExpr,
    PropertyDef,
    SavedExpr,
    T,
    gdb_end,
    gdb_property_call_start,
    render,
)
from langkit.expressions.boolean import make_eq_expr
from langkit.expressions.utils import assign_var
from langkit.utils import TypeSet, collapse_concrete_nodes, memoized


if TYPE_CHECKING:
    import liblktlang as L


class CastExpr(ComputingExpr):
    """
    Downcast the AST `node` to the more specific `dest_type` AST node type.

    This returns a null node if the actual type of `node` is not a subtype of
    `dest_type`.
    """

    pretty_class_name = "Cast"

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        expr: Expr,
        dest_type: ASTNodeType | EntityType,
        do_raise: bool = False,
        unsafe: bool = False,
    ):
        """
        :param unsafe: If true, elide the type check before doing the cast.
            This is used to avoid noisy and useless type checks in generated
            code: these checks would fail only because of a bug in the code
            generator.
        """
        self.do_raise = do_raise
        self.unsafe = unsafe
        self.expr = SavedExpr(None, "Cast_Expr", expr)

        # If the input expression computes a bare node, our result is a bare
        # node. If it is an entity, our result is the entity corresponding to
        # ``dest_type``.
        self.static_type = (
            dest_type.entity
            if (
                isinstance(expr.type, EntityType)
                and isinstance(dest_type, ASTNodeType)
            )
            else dest_type
        )

        super().__init__(debug_info, "Cast_Result")

    def _render_pre(self) -> str:
        return render("properties/cast_ada", expr=self)

    @property
    def subexprs(self) -> dict:
        assert self.static_type is not None
        return {"expr": self.expr, "type": self.static_type.name}

    def __repr__(self) -> str:
        assert self.static_type is not None
        return "<CastExpr {}>".format(self.static_type.lkt_name)

    @property
    def dest_node(self) -> ASTNodeType:
        """
        Return the node type (not entity) that is the result of the cast
        expression.
        """
        if isinstance(self.type, EntityType):
            return self.type.element_type
        else:
            assert isinstance(self.type, ASTNodeType)
            return self.type

    @property
    def input_node(self) -> ASTNodeType:
        """
        Return the node type (not entity) that is the input of the cast
        expression.
        """
        if isinstance(self.expr.type, EntityType):
            return self.expr.type.element_type
        else:
            assert isinstance(self.expr.type, ASTNodeType)
            return self.expr.type

    @property
    def check_needed(self) -> bool:
        """
        Return whether we must generate a dynamic check on the kind of the
        input expression before doing the cast.
        """
        # If asked to do an unsafe conversion, then by definition we must not
        # generate a check.
        if self.unsafe:
            return False

        # Don't generate a dynamic check if we know statically that there is no
        # failure possible (this is an upcast, or a downcast when there is only
        # one subclass, etc.).
        return self.input_node not in TypeSet({self.dest_node})


def make_is_null(debug_info: ExprDebugInfo | None, expr: Expr) -> Expr:
    if isinstance(expr.type, ASTNodeType):
        return BasicExpr(debug_info, "Is_Null", "{} = null", T.Bool, [expr])
    elif isinstance(expr.type, EntityType):
        return BasicExpr(
            debug_info, "Is_Null", "{}.Node = null", T.Bool, [expr]
        )
    else:
        return make_eq_expr(debug_info, expr, NullExpr(None, expr.type))


class New:
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
        Expression to create StructType values.
        """

        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            struct_type: BaseStructType,
            assocs: (
                dict[str, Expr]
                | dict[names.Name, Expr]
                | dict[BaseField, Expr]
            ),
            result_var_name: str | names.Name | None = None,
        ):
            self.static_type = struct_type

            # Convert names in `assocs` to the corresponding field in
            # struct_type. This lets callers use either names or fields,
            # depending on what's the most convenient for them.

            def field_or_lookup(
                field: str | names.Name | BaseField,
            ) -> BaseField:
                if isinstance(field, BaseField):
                    return field

                elif isinstance(field, names.Name):
                    field_name = field.lower

                else:
                    assert isinstance(field, str)
                    field_name = field

                fields = struct_type.get_abstract_node_data_dict()
                result = fields[field_name]
                assert isinstance(result, BaseField)
                return result

            self.assocs = {
                field_or_lookup(field): expr for field, expr in assocs.items()
            }

            super().__init__(debug_info, result_var_name or "New_Struct")

        def _iter_ordered(self) -> list[tuple[BaseField, Expr]]:
            return sorted(
                [(field, expr) for field, expr in self.assocs.items()],
                key=lambda assoc: assoc[0].names.index,
            )

        def _render_fields(self) -> str:
            """
            Helper to return the elaboration of structure fields plus the
            ref-counting adjustment to create new ownership shares.
            """
            assocs = list(self._iter_ordered())

            return "\n".join(
                # Evaluate expressions for all operands
                [expr.render_pre() for _, expr in assocs]
                # Only then, create ownership shares for the returned record
                + [
                    "Inc_Ref ({});".format(expr.render_expr())
                    for _, expr in assocs
                    if expr.type.is_refcounted
                ]
            )

        def _render_pre(self) -> str:
            assocs = self._iter_ordered()
            if assocs:
                record_expr = "({})".format(
                    ", ".join(
                        f"{field.names.codegen} => {expr.render_expr()}"
                        for field, expr in assocs
                    )
                )
            else:
                # In order to facilitate language bindings, we generate a dummy
                # field for empty structs.
                record_expr = "(Dummy => ' ')"

            return "{}\n{}".format(
                self._render_fields(),
                # We must not inc-ref the resulting record as we already
                # inc-ref'd manually all the ref-counted members.
                assign_var(
                    self.result_var.ref_expr,
                    record_expr,
                    requires_incref=False,
                ),
            )

        @property
        def subexprs(self) -> dict:
            result: dict = {
                field.names.index: expr for field, expr in self.assocs.items()
            }
            assert self.static_type is not None
            result["_type"] = self.static_type.lkt_name
            return result

        def __repr__(self) -> str:
            assert self.static_type is not None
            return "<New.{} {}>".format(
                type(self).__name__, self.static_type.name.camel
            )

    class NodeExpr(StructExpr):
        """
        Expression to create AST node values.
        """

        def __init__(
            self,
            debug_info: ExprDebugInfo | None,
            astnode: ASTNodeType,
            assocs: dict[names.Name, Expr] | dict[BaseField, Expr],
        ):
            super().__init__(debug_info, astnode, assocs, "New_Node")

            # The synthetized node inherits Self.Self_Env, so PLE must happen
            # before this property is run.
            PropertyDef.get().set_uses_envs()

        def _render_pre(self) -> str:
            return super()._render_fields() + render(
                "properties/new_astnode_ada", expr=self
            )

    @staticmethod
    def construct_fields(
        error_location: Location | L.LktNode,
        struct_type: BaseStructType,
        field_values: dict[str, Expr],
        for_node_builder: bool = False,
    ) -> dict[BaseField, Expr]:
        """
        Helper to lower fields in a struct/node constructor.

        :param error_location: Location for error diagnostics, if emitted.
        :param struct_type: Struct/node type for which we construct fields.
        :param field_values: Mapping from field DSL name to associated field
            initialization expression.
        :param for_node_builder: Whether we are in the context of node
            builders: parse field values are expected to be node builders.
        :return: A mapping from fields to initialize to the corresponding
            initialization expression.
        """
        # Create a dict of field names to fields in the struct type, and
        # another dict for default values.
        required_fields = struct_type.required_fields_in_exprs
        default_valued_fields = {
            n: (f, f.default_value)
            for n, f in required_fields.items()
            if f.default_value is not None
        }

        # Make sure the provided set of fields matches the one the struct needs

        def error_if_not_empty(name_set: set[str], message: str) -> None:
            if name_set:
                names_str = ", ".join(sorted(name_set))
                error(f"{message}: {names_str}", location=error_location)

        error_if_not_empty(
            set(required_fields)
            - set(field_values)
            - set(default_valued_fields),
            f"Values are missing for {struct_type.lkt_name} fields",
        )
        error_if_not_empty(
            set(field_values.keys()) - set(required_fields),
            f"Extraneous fields for {struct_type.lkt_name}",
        )

        # At this stage, we know that the user provided all required fields,
        # and no spurious fields. Build a map whose keys are ``BaseField``
        # instances.
        result = {
            required_fields[name]: expr for name, expr in field_values.items()
        }

        # Add default values for missing fields
        for name, (field, default_value) in default_valued_fields.items():
            if field not in result:
                result[field] = default_value

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
                f" {expected_type.lkt_name}, got {actual_type.lkt_name}",
                location=error_location,
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
                isinstance(expected_type, ASTNodeType)
                or isinstance(expected_type, EntityType)
            ) and actual_type != expected_type:
                result[field] = CastExpr(None, expr, expected_type)

            # Also mark parse fields as synthetized
            if isinstance(field, Field):
                field.set_synthetized()

        return result


class EvalMemberExpr(Expr):
    """
    Expression that represents the evaluation of a type member (field or
    property).

    Note that this automatically generates a check for null nodes, unless
    this is a simple field access.
    """

    pretty_class_name = "EvalMemberExpr"

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        receiver_expr: Expr,
        node_data: AbstractNodeData,
        arguments: Sequence[Expr | None],
        dynvar_args: Sequence[Expr] | None = None,
        actual_node_data: AbstractNodeData | None = None,
        implicit_deref: bool = False,
        is_super: bool = False,
        unsafe: bool = False,
    ):
        """
        :param receiver_expr: The receiver of the field access.

        :param node_data: The accessed property or field.

        :param arguments: If non-empty, this field access will actually be a
            primitive call. Each item is a Expr for an actual to pass, or None
            for arguments to let them have their default value. List list must
            have the same size as `node_data.natural_arguments`.

        :param dynvar_args: List of values to pass for ``node_data``'s
            dynamic variables.

        :param actual_node_data: If not None, node data to access for code
            generation. In that case, ``node_data`` is just there to keep track
            of what was accessed at the DSL level.

        :param implicit_deref: Whether the receiver is an entity, and we want
            to access a field or property of the stored node.  In the case of
            an entity prefix for an AST node field, return an entity with the
            same entity info.

        :param is_super: Whether this field access materializes a "super"
            expression.

        :param unsafe: If true, don't generate the null check before doing the
            field access. This is used to avoid noisy and useless null checks
            in generated code: these checks would fail only because of a bug in
            the code generator.
        """
        # When calling environment properties, the call itself happens are
        # outside a property. We cannot create a variable in this context, and
        # the field access is not supposed to require a "render_pre" step.
        p = PropertyDef.get_or_none()
        self.simple_field_access = not p
        if implicit_deref:
            assert receiver_expr.type.is_entity_type
            assert not self.simple_field_access

        self.implicit_deref = implicit_deref
        self.is_super = is_super
        self.unsafe = unsafe

        self.original_receiver_expr = receiver_expr
        self.receiver_expr = (
            receiver_expr
            if self.simple_field_access or self.unsafe
            else NullCheckExpr(receiver_expr, implicit_deref)
        )

        # Keep the original node data for debugging purposes
        self.original_node_data = node_data

        self.node_data = actual_node_data or node_data

        self.arguments = arguments
        if self.arguments is not None:
            assert len(self.arguments) == len(self.node_data.natural_arguments)

        self.dynamic_vars = list(dynvar_args) if dynvar_args else []
        expected_dynvars = (
            len(self.dynamic_vars)
            if isinstance(self.node_data, PropertyDef)
            else 0
        )
        assert len(self.dynamic_vars) == expected_dynvars

        self.static_type = self.node_data.type
        if self.wrap_result_in_entity:
            assert isinstance(self.static_type, ASTNodeType)
            self.static_type = self.static_type.entity

        # Create a variable for all field accesses in properties. This is
        # needed because the property will return an owning reference, so we
        # need it to be attached to the scope. In other cases, this can make
        # debugging easier.
        super().__init__(
            debug_info, None if self.simple_field_access else "Fld"
        )

    def __repr__(self) -> str:
        return "<FieldAccessExpr {} {} {}>".format(
            self.receiver_expr, self.node_data, self.type
        )

    @property
    def wrap_result_in_entity(self) -> bool:
        """
        Whether the result is an AST node that must be wrapped as an entity.
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
        Return the value of the entity info parameter along, compute its value.
        Return None otherwise.
        """
        # If the property that this field accesses requires entity info, then
        # the prefix is supposed to be an entity. There are only two exceptions
        # to this: either the entity info is actually optional, either we are
        # out of any property. In these cases, leave the default entity info.
        #
        # See CompileCtx.compute_uses_entity_info_attr for how we check that
        # the assertion always holds.
        if not self.implicit_deref:
            assert (
                self.node_data.optional_entity_info
                or PropertyDef.get_or_none() is None
            )
            return None

        # When it is required, entity info comes from the entity, if we're
        # calling the property on an entity.
        return "{}.Info".format(self.prefix)

    @property
    def field_access_expr(self) -> str:
        """
        Return the code for the expression that evaluates the actual field
        access.
        """
        prefix = self.prefix
        if self.implicit_deref:
            prefix = "{}.Node".format(prefix)

        if isinstance(self.node_data, PropertyDef):
            # If we're calling a property, then pass the arguments

            # TODO: For the moment, the first argument is named Node for
            # properties on node & entity types, and Self for other properties.
            # For the moment, properties that are not on nodes are necessarily
            # built-in properties, so the inconsistency is not too bothering,
            # but long-term we want to rename *every* self argument to Self.
            rec_type = self.receiver_expr.type
            first_arg_name = (
                "Node"
                if rec_type.is_ast_node or rec_type.is_entity_type
                else "Self"
            )

            # Create a collection of name => expression for parameters. First
            # argument is the node itself.
            args = [(first_arg_name, prefix)] + [
                (formal.name, actual.render_expr())
                for actual, formal in zip(
                    self.arguments, self.node_data.natural_arguments
                )
                if actual is not None
            ]

            # If the property has dynamically bound variables, then pass them
            # along.
            for dv_arg, dv_value in zip(
                self.node_data.dynamic_var_args, self.dynamic_vars
            ):
                args.append(
                    (
                        dv_arg.dynvar.name.camel_with_underscores,
                        dv_value.render_expr(),
                    )
                )

            # If the called property uses entity information, pass it along
            if self.node_data.uses_entity_info:
                einfo_expr = self.entity_info_expr
                if einfo_expr:
                    args.append(
                        (str(PropertyDef.entity_info_name), einfo_expr)
                    )

            # Build the call
            ret = "{} ({})".format(
                self.node_data.qual_impl_name,
                ", ".join(
                    "{} => {}".format(name, value) for name, value in args
                ),
            )

        elif self.node_data.abstract:
            # Call the accessor for abstract fields
            ret = "Implementation.{} ({})".format(
                self.node_data.internal_name, prefix
            )

        else:
            # If we reach this point, we know that we are accessing a struct
            # field: make sure we return the public API type, which may be
            # different from the type thas is stored in the struct.
            ret = self.node_data.type.extract_from_storage_expr(
                prefix, "{}.{}".format(prefix, self.node_data.internal_name)
            )

        if self.wrap_result_in_entity:
            assert isinstance(self.type, EntityType)
            ret = "{} (Node => {}, Info => {})".format(
                self.type.constructor_name, ret, self.entity_info_expr
            )

        return ret

    def _render_pre(self) -> str:
        # As long as this method is called, this should not be a simple field
        # access and thus we should have a result variable.
        assert not self.simple_field_access and self.result_var

        # Emit debug helper directives to describe the call if the target is a
        # property we generated code for.
        call_debug_info = (
            isinstance(self.node_data, PropertyDef)
            and not self.node_data.external
        )

        sub_exprs = [self.receiver_expr] + funcy.lfilter(
            lambda e: e is not None, self.arguments
        )
        result = [e.render_pre() for e in sub_exprs]

        if call_debug_info:
            assert isinstance(self.node_data, PropertyDef)
            result.append(gdb_property_call_start(self.node_data))

        result.append(
            f"{self.result_var.codegen_name} := {self.field_access_expr};"
        )

        if call_debug_info:
            result.append(gdb_end())

        # We need to make sure we create a new ownership share for the result
        # of the field access.  Property calls already do that, but we must
        # inc-ref ourselves for other cases.
        if self.type.is_refcounted and self.node_data.access_needs_incref:
            result.append(f"Inc_Ref ({self.result_var.codegen_name});")

        return "\n".join(result)

    def _render_expr(self) -> str:
        return (
            str(self.result_var.codegen_name)
            if self.result_var
            else self.field_access_expr
        )

    @property
    def subexprs(self) -> dict:
        result = {
            "0-prefix": self.receiver_expr,
            "1-field": self.original_node_data,
        }
        if self.arguments:
            result["2-args"] = self.arguments
        if self.dynamic_vars:
            result["3-dynvars"] = self.dynamic_vars
        return result


class IsAExpr(ComputingExpr):
    pretty_class_name = "IsA"

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        expr: Expr,
        types: list[ASTNodeType],
    ):
        """
        :param expr: Expression on which the test is performed.
        :param types: ASTNodeType instances to use for the test.
        """
        self.static_type = T.Bool
        self.expr = expr
        self.types = types

        super().__init__(debug_info, "Is_A")

    def _render_pre(self) -> str:
        target = ("{}.Node" if self.expr.type.is_entity_type else "{}").format(
            self.expr.render_expr()
        )
        result_expr = (
            "{target} /= null \nand then {target}.Kind in {nodes}".format(
                target=target,
                nodes=" | ".join(a.ada_kind_range_name for a in self.types),
            )
        )
        return "{}\n{}".format(
            self.expr.render_pre(),
            assign_var(self.result_var.ref_expr, result_expr),
        )

    @property
    def subexprs(self) -> dict:
        return {
            "expr": self.expr,
            "types": [astnode.name for astnode in self.types],
        }

    def __repr__(self) -> str:
        return "<IsA.Expr {}>".format(
            ", ".join(astnode.name.camel for astnode in self.types)
        )


class MatchExpr(ComputingExpr):
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
            match_var: LocalVars.LocalVar,
            match_expr: Expr,
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
            MatchExpr's constructor.
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
            if isinstance(self.match_type, EntityType):
                result = self.match_type.element_type
                assert isinstance(result, ASTNodeType)
                return result
            else:
                return self.match_type

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        error_location: Location | L.LktNode,
        prefix_expr: Expr,
        matchers: list[MatchExpr.Matcher],
    ):
        """
        :param debug_info: Debug info for this expression.
        :param prefix_expr: The expression on which the dispatch occurs. It
            must be either an AST node or an entity.
        :param matchers: List of matchers for this node.
        """
        assert prefix_expr.type.is_ast_node or prefix_expr.type.is_entity_type
        self.prefix_expr = NullCheckExpr(
            prefix_expr, implicit_deref=prefix_expr.type.is_entity_type
        )

        # Variable to hold the value of which we do dispatching
        # (prefix_expr).
        self.prefix_var = PropertyDef.get().vars.create(
            Location.builtin, "Match_Prefix", self.prefix_expr.type
        )

        # Compute the return type as the unification of all branches
        rtype = matchers[-1].match_expr.type
        for m in matchers:
            rtype = m.match_expr.type.unify(
                rtype,
                error_location,
                "Mismatching types in Match expression: got {self} but"
                " expected {other} or sub/supertype",
            )
        self.static_type = rtype

        # Wrap each matcher expression so that all capture variables are bound
        # and initialized.
        self.matchers = []
        for m in matchers:
            m_var_type = m.match_var.type
            assert isinstance(m_var_type, (ASTNodeType, EntityType))
            # Initialize match_var. Note that assuming the code generation is
            # bug-free, this cast cannot fail, so don't generate type check
            # boilerplate. Also create the Let expression inside the branch
            # scope, so that its result variable is finalized when we are
            # leaving the matcher.
            with m.inner_scope.use():
                let_expr = LetExpr(
                    None,
                    [
                        (
                            m.match_var.ref_expr,
                            CastExpr(
                                None,
                                self.prefix_var.ref_expr,
                                m_var_type,
                                unsafe=True,
                            ),
                        )
                    ],
                    # ... and cast this matcher's result to the Match result's
                    # type, as required by OOP with access types in Ada.
                    m.match_expr.unified_expr(rtype),
                )

            # Now do the binding for static analysis and debugging
            self.matchers.append(
                MatchExpr.Matcher(
                    m.match_var,
                    BindingScope(None, let_expr, [], scope=m.inner_scope),
                    m.inner_scope,
                )
            )

        # Determine for each matcher the set of concrete AST nodes it can
        # actually match.
        prefix_type = self.prefix_expr.type
        if isinstance(prefix_type, EntityType):
            node_prefix_type = prefix_type.element_type
        else:
            assert isinstance(prefix_type, ASTNodeType)
            node_prefix_type = prefix_type
        matched_types, remainder = collapse_concrete_nodes(
            node_prefix_type, [m.match_astnode_type for m in self.matchers]
        )
        assert not remainder
        for matcher, matched in zip(self.matchers, matched_types):
            matcher.matched_concrete_nodes = matched

        super().__init__(debug_info, "Match_Result")

    def _render_pre(self) -> str:
        return render("properties/match_ada", expr=self)

    @property
    def subexprs(self) -> dict:
        return {
            "prefix": self.prefix_expr,
            "matchers": [m.match_expr for m in self.matchers],
        }

    def __repr__(self) -> str:
        return "<MatchExpr>"


class StructUpdateExpr(ComputingExpr):
    """
    Create a new struct value, replacing fields with the given values.
    """

    pretty_class_name = "StructUpdate"

    def __init__(
        self,
        debug_info: ExprDebugInfo | None,
        expr: Expr,
        assocs: dict[BaseField, Expr],
    ):
        self.static_type = expr.type
        self.expr = expr
        self.assocs = assocs
        super().__init__(debug_info, "Update_Result")

    def _render_pre(self) -> str:
        return render("properties/update_ada", expr=self)

    @property
    def subexprs(self) -> dict:
        return {
            "expr": self.expr,
            "assocs": {
                f.original_name: f_expr for f, f_expr in self.assocs.items()
            },
        }

    def __repr__(self) -> str:
        return "<StructUpdate.Expr>"
