from __future__ import annotations

from typing import Callable, TYPE_CHECKING, Union

from langkit.c_api import CAPISettings
import langkit.compiled_types as ct
from langkit.compiled_types import T
from langkit.expressions import PropertyDef
from langkit.language_api import AbstractAPISettings


if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx

# names that are not really keywords but that we still don't want to generate
inconvenient_names = ["ref"]

# list found here: https://ocaml.org/manual/lex.html
ocaml_keywords = set(
    """and as assert asr begin class constraint do done downto
    else end exception external false for fun function functor if in include
    inherit initializer land lazy let lor lsl lsr lxor match method mod module
    mutable new nonrec object of open or private rec sig struct then to true
    try type val virtual when while with
""".split()
    + inconvenient_names
)


class DummyAnalysisContextType:
    """
    Placeholder to represent the analysis context type in our depedency
    tracking system (see the type_graph and add_dep methods below).
    """

    pass


TypeOrPlaceholder = Union[ct.CompiledType, DummyAnalysisContextType]


class OCamlAPISettings(AbstractAPISettings):
    """Container for OCaml API generation settings."""

    name = "ocaml"

    context: CompileCtx
    c_api_settings: CAPISettings
    type_graph: dict[TypeOrPlaceholder, list[TypeOrPlaceholder]]

    def actual_c_type(
        self,
        typ: TypeOrPlaceholder,
    ) -> TypeOrPlaceholder:
        """
        Return the C type used to encode ``typ`` values.

        In the C/OCaml binding layer, some types are encoded as other types.
        For instance: all entities are exposed as the root entity type.

        :param typ: Type to encode in the C/OCaml binding layer.
        """
        result: ct.CompiledType

        # DummyAnalysisContextType is a placeholder, not a real CompiledType
        if isinstance(typ, DummyAnalysisContextType):
            return typ

        if typ.is_entity_type:
            # Expose all entity types as the root entity type
            result = T.entity
        elif typ.is_array_type:
            element_type = self.actual_c_type(typ.element_type)
            assert isinstance(element_type, ct.CompiledType)
            result = element_type.array
        else:
            result = typ

        return result

    def add_dep(self, typ: TypeOrPlaceholder, dep: TypeOrPlaceholder) -> None:
        """
        Adds the dependency dep to the type typ in the type_graph.
        """
        # Add a dependency both to the actual C type (for C structs order) and
        # to the given type (for OCaml modules order).
        for dep in [dep, self.actual_c_type(dep)]:
            # Make sure that both "dep" and "typ" are both valid vertexes
            self.type_graph.setdefault(dep, [])
            deps = self.type_graph.setdefault(typ, [])

            # Append the typ -> dep vertex if not already present
            if dep not in deps:
                deps.append(dep)

    def __init__(self, ctx: CompileCtx, c_api_settings: CAPISettings):
        self.context = ctx
        self.c_api_settings = c_api_settings
        self.AnalysisContext = DummyAnalysisContextType()

        # Dependency graph for types
        self.type_graph = {}

    def init_type_graph(self) -> None:
        """
        Initialize the type graph.
        """
        self.add_dep(T.AnalysisUnit, T.root_node.entity)
        self.add_dep(T.Symbol, self.AnalysisContext)
        self.add_dep(self.AnalysisContext, T.AnalysisUnit)

    def is_empty_type(self, t: TypeOrPlaceholder) -> bool:
        """
        Test if the given type is an empty type. An empty type is a structure
        that does not contain any field, or the fields are also
        empty structs. An empty type can also be an ASTNodeType that does not
        contain any concrete subclasses. We Want to check this because we
        cannot write empty sum types or empty structures in OCaml.

        :param t: The type we want to check if it is empty.
        """
        match t:
            case ct.ASTNodeType():
                return len(t.concrete_subclasses) == 0
            case ct.EntityType():
                return self.is_empty_type(t.astnode)
            case ct.ArrayType():
                return self.is_empty_type(t.element_type)
            case ct.IteratorType():
                return self.is_empty_type(t.element_type)
            case ct.StructType():
                return all(
                    self.is_empty_type(field.type) for field in t.get_fields()
                )
            case _:
                assert isinstance(t, ct.CompiledType)
                return False

    def polymorphic_variant_name(self, type: ct.ASTNodeType) -> str:
        """
        Return a string representing the polymorphic variant for the given
        concrete type.

        :param type: The type for which we want the polymorphic variant name.
        """
        return "`{}".format(type.entity.api_name.camel)

    def fields_name(self, type: ct.ASTNodeType) -> str:
        """
        Return a string representing the name of the OCaml type for the
        fields of a node.

        :param type: Ast node type from which the field comes.
        """
        return "{}_fields".format(type.kwless_raw_name.lower)

    def get_field_type(self, field: ct.Field) -> list[ct.CompiledType]:
        """
        Return a precice type for the given Field instance. The list contains
        the possible concrete types of the given field. If there is only
        one element, the only element can be abstract, this means that the
        type is precise enough.

        :param field: The field for which we want a precice type.
        """

        precise_types = field.precise_types
        concrete_precise_types = list(
            frozenset(
                typ
                for precise_type in precise_types.matched_types
                for typ in precise_type.concrete_subclasses
            )
        )

        # If the list of concrete precise types has the same length as the list
        # of concrete subclasses of the minimal common ancestor type, we can
        # replace the list of concrete precise types by a singleton list
        # containing the common ancestor type.
        super_type = precise_types.minimal_common_type
        if len(super_type.concrete_subclasses) == len(concrete_precise_types):
            concrete_precise_types = [super_type]

        # For determinism, sort the list of concrete precise types
        return sorted(concrete_precise_types, key=lambda t: t.lkt_name)

    def get_field_minimal_type(self, field: ct.Field) -> list[ct.CompiledType]:
        """
        Return a precice type for the given Field instance. While
        get_field_type returns a concrete list of types, this function returns
        the minimal set of concrete or abstract types.

        :param field: The field for which we want a precice type.
        """

        minimal_precise_types = list(field.precise_types.minimal_matched_types)

        # minimal_matched_types returns a set.
        # For determinism, sort the list of types.
        return sorted(minimal_precise_types, key=lambda t: t.lkt_name)

    def get_parse_fields(self, node: ct.ASTNodeType) -> list[ct.Field]:
        """
        Return all the parse fields to be exposed as a field in record of node.

        :param node: The node type of which we want to get the parse fields.
        """
        return node.get_parse_fields(
            lambda x: not x.is_overriding and not x.abstract and not x.null
        )

    def get_properties(self, node: ct.ASTNodeType) -> list[PropertyDef]:
        """
        Return the list of all properties that should be exposed to the user.

        :param ct.ASTNodeType node: The node for which we want to get the
            properties.
        """
        return [
            field
            for field in node.fields_with_accessors()
            if isinstance(field, PropertyDef)
        ]

    def array_wrapper(self, array_type: ct.ArrayType) -> str:
        return (
            ct.T.entity.array
            if array_type.element_type.is_entity_type
            else array_type
        ).api_name.camel

    def iterator_wrapper(self, iterator_type: ct.IteratorType) -> str:
        return (
            ct.T.entity.iterator
            if iterator_type.element_type.is_entity_type
            else iterator_type
        ).api_name.camel

    def struct_name(self, t: ct.CompiledType) -> str:
        """
        Returns the OCaml module containing the low-level structure for the
        given type.

        :param t: Type we want to get the module name.
        """
        match t:
            case T.BigInt | T.Token | T.SourceLocation | T.Symbol:
                return self.module_name(t)
            case ct.EnumType():
                return self.module_name(t)
            case ct.ASTNodeType():
                return self.module_name(t.entity)
            case ct.EntityType():
                return "EntityStruct"
            case T.AnalysisUnit:
                return "{}Struct".format(t.api_name.camel)
            case ct.ArrayType():
                return "{}Struct".format(self.array_wrapper(t))
            case ct.IteratorType():
                return "{}Struct".format(self.iterator_wrapper(t))
            case ct.StructType():
                return "{}Struct".format(self.module_name(t))
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding (struct_name):"
                    f" {t!r}"
                )

    def node_name(self, t: ct.EntityType | ct.ASTNodeType) -> str:
        """
        Returns the OCaml name for an ASTNodeType or an EntityType.

        :param t: Type we want to get the name.
        """
        match t:
            case ct.ASTNodeType():
                return self.node_name(t.entity)
            case ct.EntityType():
                return t.astnode.kwless_raw_name.camel
            case _:
                raise AssertionError("unhandled type: {t!r}")

    def module_name(self, t: ct.CompiledType) -> str:
        """
        Returns the OCaml module containing the definition for the given type.

        :param t: Type we want to get the module name.
        """

        match t:
            case ct.EnumType():
                return t.api_name.camel
            case T.Token:
                return "Token"
            case T.SourceLocation:
                return "Sloc"
            case T.Symbol:
                return "Symbol"
            case T.Char:
                return "Character"
            case T.String:
                return "StringType"
            case ct.ArrayType():
                return t.api_name.camel
            case ct.IteratorType():
                return t.api_name.camel
            case ct.StructType():
                return t.api_name.camel
            case T.BigInt:
                return "BigInteger"
            case _:
                raise AssertionError("unhandled type: {t!r}")

    def wrap_requires_context(self, t: ct.CompiledType) -> bool:
        """
        Returns true if the given type need the context to be wrapped in an
        OCaml value.

        :param t: Type for which we want to know if the context is needed.
        """

        match t:
            case (
                ct.EnumType()
                | ct.ASTNodeType()
                | T.Token
                | T.SourceLocation
                | T.Symbol
                | T.Bool
                | T.Int
                | T.Char
                | T.String
                | T.BigInt
                | T.EnvRebindings
            ):
                return False
            case T.AnalysisUnit | ct.EntityType():
                return True
            case ct.ArrayType():
                return self.wrap_requires_context(t.element_type)
            case ct.IteratorType():
                return self.wrap_requires_context(t.element_type)
            case ct.StructType():
                return any(
                    self.wrap_requires_context(field.type)
                    for field in t.get_fields()
                )
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding (wrapping):"
                    f" {t!r}"
                )

    def has_ctype_view(self, t: ct.CompiledType) -> bool:
        """
        Given a type, return true if it has a ctype view. Having a ctype view
        means that wrap and unwrap operations are not needed explicitely.

        :param t: The type to check.
        """
        match t:
            case T.AnalysisUnit:
                return False
            case ct.EnumType():
                return True
            case ct.ASTNodeType():
                return True
            case ct.EntityType():
                return False
            case T.Token:
                return False
            case T.SourceLocation:
                return True
            case T.Symbol:
                return False
            case T.Bool:
                return True
            case T.Int:
                return True
            case T.Char:
                return True
            case T.String:
                return False
            case ct.ArrayType():
                return False
            case ct.IteratorType():
                return False
            case ct.StructType():
                return False
            case T.BigInt:
                return False
            case T.EnvRebindings:
                return True
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding"
                    f" (has_ctype_view): {t!r}"
                )

    def convert_function_name(
        self,
        t: ct.CompiledType,
        convert: str,
        convert_ast_node: Callable[[ct.ASTNodeType], str],
        from_module: ct.ASTNodeType | None = None,
    ) -> str:
        """
        Return the wrap/unwrap function name used to wrap/unwrap the given
        type. The wrap or unwrap is selected with the given convert string.
        Call the given convert function on an ASTNodeType.

        :param t: The type for which we want the conversion function.
        :param convert: Conversion function name, either wrap, or unwrap.
        :param convert_ast_node: Function called to get the name of the
            conversion function to use for an ASTNodeType.
        :param from_module: The module from which we want to get the name of
            the conversion function. This is useful because if the module is
            the same as the type, then we don't want to prefix it with the
            module name.
        """

        def from_module_name(t: ct.CompiledType) -> str:
            if from_module == t:
                return convert
            else:
                return "{}.{}".format(self.module_name(t), convert)

        def plain_name(t: ct.CompiledType) -> str:
            return "{}_{}".format(convert, t.api_name.lower)

        match t:
            case ct.ASTNodeType():
                return convert_ast_node(t)
            case ct.EntityType():
                return self.convert_function_name(
                    t.astnode, convert, convert_ast_node, from_module
                )
            case T.AnalysisUnit | T.EntityInfo | T.env_md:
                return plain_name(t)
            case T.Token | ct.CompiledType():
                return from_module_name(t)
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding"
                    f" (convert_function_name): {t!r}"
                )

    def wrap_function_name(
        self, type: ct.CompiledType, from_module: ct.ASTNodeType | None = None
    ) -> str:
        def convert_ast_node(type: ct.ASTNodeType) -> str:
            return "wrap_{}".format(type.kwless_raw_name.lower)

        return self.convert_function_name(
            type, "wrap", convert_ast_node, from_module
        )

    def unwrap_function_name(
        self, type: ct.CompiledType, from_module: ct.ASTNodeType | None = None
    ) -> str:
        def convert_ast_node(type: ct.ASTNodeType) -> str:
            # We need only one unwrap function for the entire hierarchy
            return "unwrap_{}".format(T.root_node.kwless_raw_name.lower)

        return self.convert_function_name(
            type, "unwrap", convert_ast_node, from_module
        )

    def wrap_value(
        self,
        value: str,
        type: ct.CompiledType,
        context: str | None,
        check_for_null: bool = False,
        dec_ref: str = "true",
    ) -> str:
        """
        Given an expression for a low-level value and the associated type,
        return an other expression that yields the corresponding high-level
        value.

        :param value: Expression yielding a low-level value.
        :param type: Type corresponding to the "value" expression.
        :param check_for_null: If true, return an OCaml expression that first
            checks if the underlying node value is null. If it is the case, the
            expression is evaluated to None. We check null only for entity
            types.
        :param dec_ref: This value is passed to the wrap function to signal
            that it can safely call the dec_ref function of the wrapped type
            after being done.
        """

        def from_function(typ: ct.CompiledType, value: str) -> str:
            dec_ref_arg = (
                "~dec_ref:{} ".format(dec_ref) if typ.is_refcounted else ""
            )

            context_arg = (
                "{} ".format(context)
                if self.wrap_requires_context(typ)
                else ""
            )

            return "{} {}{}({})".format(
                self.wrap_function_name(typ), dec_ref_arg, context_arg, value
            )

        wrapped_result = (
            value if self.has_ctype_view(type) else from_function(type, value)
        )

        if check_for_null and type.is_entity_type:
            return (
                "if is_null (getf {} EntityStruct.node) then None "
                + "else Some ({})"
            ).format(value, wrapped_result)
        else:
            return wrapped_result

    def unwrap_value(
        self,
        value: str,
        type: ct.CompiledType,
        context: str | None,
        check_for_none: bool = False,
    ) -> str:
        """
        Given an expression for a low-level value and the associated type,
        return an other expression that yields the corresponding high-level
        value.

        :param value: Expression yielding a low-level value.
        :param type: Type corresponding to the "value" expression.
        :param check_for_none: If true, check if the value is None before
            returning a C API value. If it is the case, the expression is
            evaluated to an empty struct. We check for None only for entity
            types.
        """

        def from_function(typ: ct.CompiledType, value: str) -> str:
            context_arg = (
                "{} ".format(context)
                if type.conversion_requires_context
                else ""
            )

            return "{} {}({})".format(
                self.unwrap_function_name(typ), context_arg, value
            )

        if type.is_entity_type and check_for_none:
            return (
                "match {} with"
                " Some n -> {}"
                " | None -> make EntityStruct.c_type".format(
                    value, from_function(type, "n")
                )
            )
        elif self.has_ctype_view(type):
            return value
        else:
            return from_function(type, value)

    def is_struct(
        self, t: ct.CompiledType, from_module: ct.ASTNodeType | None = None
    ) -> bool:
        """
        Return true if the given type is defined as a ctypes structure without
        a view.

        :param t: Type we want to test.
        :param from_module: The field is assumed to be accessed within this
            module.
        """
        if not t.is_entity_type and from_module == t:
            return True

        match t:
            case (
                T.Bool
                | T.Int
                | T.Char
                | T.Symbol
                | ct.EnumType()
                | ct.ASTNodeType()
                | T.BigInt
                | T.EnvRebindings
            ):
                return False
            case (
                T.String
                | T.Token
                | T.SourceLocation
                | ct.EntityType()
                | T.AnalysisUnit
                | ct.ArrayType()
                | ct.IteratorType()
                | ct.StructType()
            ):
                return True
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding (is_struct):"
                    f" {t!r}"
                )

    def finalize_function(self, t: ct.CompiledType) -> str | None:
        """
        Return the name of the finalization function if a value of the given
        type must be finalized. A value needs a finalization if it is
        completly converted to an OCaml value. This function is used
        when calling a C API function, where, we allocated a C value when
        unwrapping an OCaml value. Thus, we need to finalize it after the call
        to the C API function.

        :param t: The type for which we want to get the finalization function
            name.
        """

        def dec_ref(t: ct.CompiledType) -> str | None:
            if t.is_refcounted:
                return "{}.dec_ref".format(self.struct_name(t))
            else:
                return None

        match t:
            case T.BigInt:
                return "{}.decref".format(self.struct_name(t))
            case ct.ASTNodeType | ct.EntityType() | T.AnalysisUnit:
                return None
            case T.String:
                return "StringType.string_dec_ref"
            case ct.CompiledType():
                return dec_ref(t)
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding"
                    f" (finalize_function): {t!r}"
                )

    def c_type(
        self, t: ct.CompiledType, from_module: ct.CompiledType | None = None
    ) -> str:
        """
        Return the name of the OCaml ctypes value defining the type to use in
        the C API for ``t``. For ctypes, types passed to the foreign function
        that import a c function are values. This returns the associated value
        for the given type.

        :param t: The type for which we want to get the C type name.
        :param from_module: Module from which we want to access the name.
        """
        if not t.is_entity_type and from_module == t:
            return "c_type"

        match t:
            case T.Bool:
                return "bool"
            case T.Int:
                return "int"
            case ct.ASTNodeType() | T.EnvRebindings:
                return "(ptr void)"
            case (
                T.Char
                | T.String
                | T.Token
                | T.SourceLocation
                | T.Symbol
                | ct.EnumType()
                | T.BigInt
            ):
                return "{}.c_type".format(self.module_name(t))
            case (
                ct.EntityType()
                | T.AnalysisUnit
                | ct.ArrayType()
                | ct.IteratorType()
                | ct.StructType()
            ):
                return "{}.c_type".format(self.struct_name(t))
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding (c_type):"
                    f" {t!r}"
                )

    def c_value_type(
        self, t: ct.CompiledType, from_module: ct.CompiledType | None = None
    ) -> str:
        """
        Return the name of the type to use in the C API for ``t``. This is the
        type of a c value.

        :param t: The type for which we want to get the C type name.
        :param from_module: Module from which we want to access the name.
        """
        if not t.is_entity_type and from_module == t:
            return "c_type"

        match t:
            case T.Bool:
                return "bool"
            case T.Int:
                return "int"
            case T.Char | T.String:
                return "string"
            case T.BigInt:
                return "unit ptr"
            case ct.EnumType() | T.EnvRebindings:
                return self.type_public_name(t)
            case ct.ASTNodeType():
                return "BareNode.t"
            case T.Token | T.SourceLocation | T.Symbol:
                return "{}.t structure".format(self.module_name(t))
            case T.AnalysisUnit:
                return "{}.t".format(self.struct_name(t))
            case ct.StructType():
                return "{}.t structure".format(self.struct_name(t))
            case ct.ArrayType() | ct.IteratorType():
                return "{}.t structure ptr".format(self.struct_name(t))
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding (c_value_type):"
                    f" {t!r}"
                )

    def type_public_name(
        self, t: ct.CompiledType, from_module: ct.CompiledType | None = None
    ) -> str:
        """
        Return the public API name for a given CompiledType instance.

        :param t: The type for which we want to get the name.
        :param from_module: Module from which we want to access the name.
        """

        def from_module_name(t: ct.CompiledType) -> str:
            if from_module == t:
                return "t"
            else:
                return "{}.t".format(self.module_name(t))

        def entity_type_name(t: ct.EntityType) -> str:
            name = t.astnode.kwless_raw_name.lower

            # Add _node to the name if it is an existing ocaml keyword
            return name + "_node" if name in ocaml_keywords else name

        match t:
            case T.Bool:
                return "bool"
            case T.Int:
                return "int"
            case T.Symbol | T.Char | T.String:
                return "string"
            case ct.IteratorType():
                return "unit"
            case T.EnvRebindings:
                return "Rebindings.t"
            case T.AnalysisUnit | T.EntityInfo | T.env_md:
                return t.api_name.lower
            case ct.ASTNodeType():
                return self.type_public_name(t.entity)
            case ct.EntityType():
                return entity_type_name(t)
            case (
                T.Token
                | T.BigInt
                | T.SourceLocation
                | ct.EnumType()
                | ct.StructType()
            ):
                return from_module_name(t)
            case ct.ArrayType():
                return "{} list".format(self.type_public_name(t.element_type))
            case _:
                raise AssertionError(
                    "Unhandled field type in the OCaml binding"
                    f" (type_public_name): {t!r}"
                )

    def register_struct_type(self, typ: ct.CompiledType) -> None:
        """
        Register a struct type together with all the types it depends on, to
        the graph of type dependencies "type_graph".

        :param typ: The type we want to register in the graph.
        """
        for f in typ.get_fields():
            self.add_dep(typ, f.type)

    def register_array_type(self, typ: ct.CompiledType) -> None:
        """
        Register an array type in the graph of dependency, adding a dependency
        between the array type and the element type.

        :param CompiledType typ: The type we want to register in the graph.
        """
        self.add_dep(typ, typ.element_type)

    def ordered_types(self) -> list[TypeOrPlaceholder]:
        """
        Return all the types sorted so that if type T1 depends on type T2, T2
        appears before T1. Returns the topological order of the types.
        """
        marks = {typ: "white" for typ in self.type_graph}
        topo: list[TypeOrPlaceholder] = []

        def dfs(vertex: TypeOrPlaceholder) -> None:
            if marks[vertex] == "black":
                # Already visited
                return
            else:
                # TODO: cycle detected, this is not yet implemented
                assert marks[vertex] == "white", "cycle detected"

                marks[vertex] = "gray"

                for succ in self.type_graph[vertex]:
                    dfs(succ)

                topo.append(vertex)

                marks[vertex] = "black"

        for vertex in self.type_graph:
            dfs(vertex)

        return topo

    def field_name(self, field: ct.Field) -> str:
        """
        Given a Field instance, return the name of the field to be used for an
        OCaml identifier.

        :param field: The field for which we want it's name.
        """
        name = field.api_name.lower

        # Add _ to the name if it is an ocaml keyword
        return "{}_".format(name) if name in ocaml_keywords else name
