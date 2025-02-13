from __future__ import annotations

from typing import Callable, TYPE_CHECKING, Union

from langkit.c_api import CAPISettings
import langkit.compiled_types as ct
from langkit.compiled_types import T, resolve_type
from langkit.expressions import PropertyDef
from langkit.language_api import AbstractAPISettings
from langkit.utils import dispatch_on_type


if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx

# names that are not really keywords but that we still don't want to generate
inconvenient_names = ["ref"]

# list found here: https://ocaml.org/manual/lex.html
ocaml_keywords = set("""and as assert asr begin class constraint do done downto
    else end exception external false for fun function functor if in include
    inherit initializer land lazy let lor lsl lsr lxor match method mod module
    mutable new nonrec object of open or private rec sig struct then to true
    try type val virtual when while with
""".split() + inconvenient_names)


class DummyAnalysisContextType:
    """
    Placeholder to represent the analysis context type in our depedency
    tracking system (see the type_graph and add_dep methods below).
    """
    pass


TypeOrPlaceholder = Union[ct.CompiledType, DummyAnalysisContextType]


class OCamlAPISettings(AbstractAPISettings):
    """Container for OCaml API generation settings."""

    name = 'ocaml'

    context: CompileCtx
    c_api_settings: CAPISettings
    type_graph: dict[TypeOrPlaceholder, list[TypeOrPlaceholder]]

    def actual_c_type(
        self,
        typ: TypeOrPlaceholder | ct.TypeRepo.Defer,
    ) -> TypeOrPlaceholder:
        """
        Return the C type used to encode ``typ`` values.

        In the C/OCaml binding layer, some types are encoded as other types.
        For instance: all entities are exposed as the root entity type.

        :param typ: Type to encode in the C/OCaml binding layer.
        """
        result: ct.CompiledTypeOrDefer

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

        # Make sure we get a CompiledType instance
        return resolve_type(result)

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

    def is_empty_type(self, type: TypeOrPlaceholder) -> bool:
        """
        Test if the given type is an empty type. An empty type is a structure
        that does not contain any field, or the fields are also
        empty structs. An empty type can also be an ASTNodeType that does not
        contain any concrete subclasses. We Want to check this because we
        cannot write empty sum types or empty structures in OCaml.

        :param type: The type we want to check if it is empty.
        """
        return dispatch_on_type(type, [
            (ct.ASTNodeType, lambda t:
                len(t.concrete_subclasses) == 0),
            (ct.EntityType, lambda t: self.is_empty_type(t.astnode)),
            (ct.ArrayType, lambda t: self.is_empty_type(t.element_type)),
            (ct.IteratorType, lambda t: self.is_empty_type(t.element_type)),
            (ct.StructType, lambda t:
                all(self.is_empty_type(field.type)
                    for field in t.get_fields())),
            (ct.CompiledType, lambda _: False)
        ])

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
        return '{}_fields'.format(type.kwless_raw_name.lower)

    def get_field_type(self, field: ct.Field) -> list[ct.CompiledType]:
        """
        Return a precice type for the given Field instance. The list contains
        the possible concrete types of the given field. If there is only
        one element, the only element can be abstract, this means that the
        type is precise enough.

        :param field: The field for which we want a precice type.
        """

        precise_types = field.precise_types
        concrete_precise_types = list(frozenset(
            typ
            for precise_type in precise_types.matched_types
            for typ in precise_type.concrete_subclasses
        ))

        # If the list of concrete precise types has the same length as the list
        # of concrete subclasses of the minimal common ancestor type, we can
        # replace the list of concrete precise types by a singleton list
        # containing the common ancestor type.
        super_type = precise_types.minimal_common_type
        if len(super_type.concrete_subclasses) == len(concrete_precise_types):
            concrete_precise_types = [super_type]

        # For determinism, sort the list of concrete precise types
        return sorted(concrete_precise_types, key=lambda t: t.dsl_name)

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
        return sorted(minimal_precise_types, key=lambda t: t.dsl_name)

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
        return [field
                for field in node.fields_with_accessors()
                if field.is_property]

    def array_wrapper(self, array_type: ct.ArrayType) -> str:
        return (ct.T.entity.array
                if array_type.element_type.is_entity_type
                else array_type).api_name.camel

    def iterator_wrapper(self, iterator_type: ct.IteratorType) -> str:
        return (ct.T.entity.iterator
                if iterator_type.element_type.is_entity_type
                else iterator_type).api_name.camel

    def struct_name(self, type: ct.CompiledType) -> str:
        """
        Returns the OCaml module containing the low-level structure for the
        given type.

        :param type: Type we want to get the module name.
        """

        return dispatch_on_type(type, [
            (ct.EnumType, lambda t: self.module_name(t)),
            (T.Token, lambda t: self.module_name(t)),
            (T.SourceLocation, lambda t: self.module_name(t)),
            (T.Symbol, lambda t: self.module_name(t)),
            (ct.ASTNodeType, lambda t: self.module_name(t.entity)),
            (ct.EntityType, lambda _: 'EntityStruct'),
            (T.AnalysisUnit, lambda t: '{}Struct'.format(t.api_name.camel)),
            (ct.ArrayType, lambda t: '{}Struct'.format(self.array_wrapper(t))),
            (ct.IteratorType, lambda t: '{}Struct'.format(
                self.iterator_wrapper(t)
            )),
            (ct.StructType, lambda t: '{}Struct'.format(self.module_name(t))),
            (T.BigInt, lambda t: self.module_name(t)),
        ])

    def node_name(self, type: ct.EntityType | ct.ASTNodeType) -> str:
        """
        Returns the OCaml name for an ASTNodeType or an EntityType.

        :param type: Type we want to get the name.
        """
        return dispatch_on_type(type, [
            (ct.ASTNodeType, lambda t: self.node_name(t.entity)),
            (ct.EntityType, lambda t: t.astnode.kwless_raw_name.camel),
        ])

    def module_name(self, type: ct.CompiledType) -> str:
        """
        Returns the OCaml module containing the definition for the given type.

        :param type: Type we want to get the module name.
        """

        return dispatch_on_type(type, [
            (ct.EnumType, lambda enum: enum.api_name.camel),
            (T.Token, lambda _: 'Token'),
            (T.SourceLocation, lambda _: 'Sloc'),
            (T.Symbol, lambda _: 'Symbol'),
            (T.Character, lambda _: 'Character'),
            (T.String, lambda _: 'StringType'),
            (ct.ArrayType, lambda t: t.api_name.camel),
            (ct.IteratorType, lambda t: t.api_name.camel),
            (ct.StructType, lambda t: t.api_name.camel),
            (T.BigInt, lambda t: 'BigInteger'),
        ])

    def wrap_requires_context(self, type: ct.CompiledType) -> bool:
        """
        Returns true if the given type need the context to be wrapped in an
        OCaml value.

        :param type: Type for which we want to know if the context is needed.
        """

        return dispatch_on_type(type, [
            (T.AnalysisUnit, lambda _: True),
            (ct.EnumType, lambda _: False),
            (ct.ASTNodeType, lambda _: False),
            (ct.EntityType, lambda _: True),
            (T.Token, lambda _: False),
            (T.SourceLocation, lambda _: False),
            (T.Symbol, lambda t: False),
            (T.Bool, lambda _: False),
            (T.Int, lambda _: False),
            (T.Character, lambda _: False),
            (T.String, lambda _: False),
            (ct.ArrayType, lambda t:
                self.wrap_requires_context(t.element_type)),
            (ct.IteratorType, lambda t:
                self.wrap_requires_context(t.element_type)),
            (ct.StructType, lambda t:
                any(self.wrap_requires_context(field.type)
                    for field in t.get_fields())),
            (T.BigInt, lambda _: False),
            (T.EnvRebindings, lambda _: False),
        ], exception=TypeError(
            'Unhandled field type in the OCaml binding'
            ' (wrapping): {}'.format(type)
        ))

    def has_ctype_view(self, type: ct.CompiledType) -> bool:
        """
        Given a type, return true if it has a ctype view. Having a ctype view
        means that wrap and unwrap operations are not needed explicitely.

        :param type: The type to check.
        """

        return dispatch_on_type(type, [
            (T.AnalysisUnit, lambda _: False),
            (ct.EnumType, lambda _: True),
            (ct.ASTNodeType, lambda _: True),
            (ct.EntityType, lambda _: False),
            (T.Token, lambda _: False),
            (T.SourceLocation, lambda _: True),
            (T.Symbol, lambda _: False),
            (T.Bool, lambda _: True),
            (T.Int, lambda _: True),
            (T.Character, lambda _: True),
            (T.String, lambda _: False),
            (ct.ArrayType, lambda _: False),
            (ct.IteratorType, lambda _: False),
            (ct.StructType, lambda _: False),
            (T.BigInt, lambda _: False),
            (T.EnvRebindings, lambda _: True),
        ], exception=TypeError(
            'Unhandled field type in the OCaml binding'
            ' (has_ctype_view): {}'.format(type)
        ))

    def convert_function_name(
        self,
        type: ct.CompiledType,
        convert: str,
        convert_ast_node: Callable[[ct.ASTNodeType], str],
        from_module: ct.ASTNodeType | None = None
    ) -> str:
        """
        Return the wrap/unwrap function name used to wrap/unwrap the given
        type. The wrap or unwrap is selected with the given convert string.
        Call the given convert function on an ASTNodeType.

        :param type: The type for which we want the conversion function.
        :param convert: Conversion function name, either wrap, or unwrap.
        :param convert_ast_node: Function called to get the name of the
            conversion function to use for an ASTNodeType.
        :param from_module: The module from which we want to get the name of
            the conversion function. This is useful because if the module is
            the same as the type, then we don't want to prefix it with the
            module name.
        """

        def from_module_name(type: ct.CompiledType) -> str:
            if from_module == type:
                return convert
            else:
                return "{}.{}".format(self.module_name(type), convert)

        def plain_name(type: ct.CompiledType) -> str:
            return "{}_{}".format(convert, type.api_name.lower)

        return dispatch_on_type(type, [
            (ct.ASTNodeType, lambda t: convert_ast_node(t)),
            (ct.EntityType, lambda t: self.convert_function_name(
                t.astnode, convert, convert_ast_node, from_module
            )),
            (T.AnalysisUnit, lambda t: plain_name(t)),
            (T.EntityInfo, lambda t: plain_name(t)),
            (T.env_md, lambda t: plain_name(t)),
            (T.Token, lambda t: from_module_name(t)),
            (ct.CompiledType, lambda t: from_module_name(t))
        ])

    def wrap_function_name(
        self,
        type: ct.CompiledType,
        from_module: ct.ASTNodeType | None = None
    ) -> str:
        def convert_ast_node(type: ct.ASTNodeType) -> str:
            return "wrap_{}".format(type.kwless_raw_name.lower)

        return self.convert_function_name(
            type, "wrap", convert_ast_node, from_module)

    def unwrap_function_name(
        self,
        type: ct.CompiledType,
        from_module: ct.ASTNodeType | None = None
    ) -> str:
        def convert_ast_node(type: ct.ASTNodeType) -> str:
            # We need only one unwrap function for the entire hierarchy
            return "unwrap_{}".format(T.root_node.kwless_raw_name.lower)

        return self.convert_function_name(
            type, "unwrap", convert_ast_node, from_module)

    def wrap_value(self,
                   value: str,
                   type: ct.CompiledType,
                   context: str | None,
                   check_for_null: bool = False,
                   dec_ref: str = "true") -> str:
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
                '~dec_ref:{} '.format(dec_ref)
                if typ.is_refcounted else ''
            )

            context_arg = (
                '{} '.format(context)
                if self.wrap_requires_context(typ) else ''
            )

            return "{} {}{}({})".format(
                self.wrap_function_name(typ),
                dec_ref_arg,
                context_arg,
                value
            )

        wrapped_result = (
            value
            if self.has_ctype_view(type)
            else from_function(type, value)
        )

        if check_for_null and type.is_entity_type:
            return ("if is_null (getf {} EntityStruct.node) then None "
                    + "else Some ({})").format(value, wrapped_result)
        else:
            return wrapped_result

    def unwrap_value(self,
                     value: str,
                     type: ct.CompiledType,
                     context: str | None,
                     check_for_none: bool = False) -> str:
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
                '{} '.format(context)
                if type.conversion_requires_context
                else ''
            )

            return "{} {}({})".format(
                self.unwrap_function_name(typ),
                context_arg,
                value
            )

        if type.is_entity_type and check_for_none:
            return ('match {} with'
                    ' Some n -> {}'
                    ' | None -> make EntityStruct.c_type'
                    .format(value, from_function(type, 'n')))
        elif self.has_ctype_view(type):
            return value
        else:
            return from_function(type, value)

    def is_struct(self,
                  type: ct.CompiledType,
                  from_module: ct.ASTNodeType | None = None) -> bool:
        """
        Return true if the given type is defined as a ctypes structure without
        a view.

        :param type: Type we want to test.
        :param from_module: The field is assumed to be accessed within this
            module.
        """
        if not type.is_entity_type and from_module == type:
            return True

        return dispatch_on_type(type, [
            (T.Bool, lambda _: False),
            (T.Int, lambda _: False),
            (T.Character, lambda _: False),
            (T.String, lambda _: True),
            (T.Token, lambda _: True),
            (T.SourceLocation, lambda _: True),
            (T.Symbol, lambda _: False),
            (ct.EnumType, lambda _: False),
            (ct.ASTNodeType, lambda _: False),
            (ct.EntityType, lambda _: True),
            (T.AnalysisUnit, lambda _: True),
            (ct.ArrayType, lambda _: True),
            (ct.IteratorType, lambda _: True),
            (ct.StructType, lambda _: True),
            (T.BigInt, lambda _: False),
            (T.EnvRebindings, lambda _: False),
        ])

    def finalize_function(self, type: ct.CompiledType) -> str | None:
        """
        Return the name of the finalization function if a value of the given
        type must be finalized. A value needs a finalization if it is
        completly converted to an OCaml value. This function is used
        when calling a C API function, where, we allocated a C value when
        unwrapping an OCaml value. Thus, we need to finalize it after the call
        to the C API function.

        :param type: The type for which we want to get the finalization
            function name.
        """

        def dec_ref(type: ct.CompiledType) -> str | None:
            if type.is_refcounted:
                return '{}.dec_ref'.format(self.struct_name(type))
            else:
                return None

        return dispatch_on_type(type, [
            (T.BigInt, lambda t: '{}.decref'.format(self.struct_name(t))),
            (ct.ASTNodeType, lambda _: None),
            (ct.EntityType, lambda _: None),
            (T.AnalysisUnit, lambda _: None),
            (T.String, lambda _: 'StringType.string_dec_ref'),
            (ct.CompiledType, lambda t: dec_ref(t))
        ])

    def c_type(self,
               type: ct.CompiledType,
               from_module: ct.CompiledType | None = None) -> str:
        """
        Return the name of the OCaml ctypes value defining the type to use in
        the C API for ``type``. For ctypes, types passed to the foreign
        function that import a c function are values. This returns the
        associated value for the given type.

        :param type: The type for which we want to get the C type name.
        :param from_module: Module from which we want to access the name.
        """
        if not type.is_entity_type and from_module == type:
            return "c_type"

        return dispatch_on_type(type, [
            (T.Bool, lambda _: 'bool'),
            (T.Int, lambda _: 'int'),
            (T.Character, lambda t: '{}.c_type'.format(self.module_name(t))),
            (T.String, lambda t: '{}.c_type'.format(self.module_name(t))),
            (T.Token, lambda t: '{}.c_type'.format(self.module_name(t))),
            (T.SourceLocation, lambda t:
                '{}.c_type'.format(self.module_name(t))),
            (T.Symbol, lambda t: '{}.c_type'.format(self.module_name(t))),
            (ct.EnumType, lambda t: '{}.c_type'.format(self.module_name(t))),
            (ct.ASTNodeType, lambda _: '(ptr void)'),
            (ct.EntityType, lambda t: '{}.c_type'.format(self.struct_name(t))),
            (T.AnalysisUnit, lambda t:
                '{}.c_type'.format(self.struct_name(t))),
            (ct.ArrayType, lambda t: '{}.c_type'.format(self.struct_name(t))),
            (ct.IteratorType, lambda t:
                '{}.c_type'.format(self.struct_name(t))),
            (ct.StructType, lambda t: "{}.c_type".format(self.struct_name(t))),
            (T.BigInt, lambda t: '{}.c_type'.format(self.module_name(t))),
            (T.EnvRebindings, lambda _: '(ptr void)'),
        ])

    def c_value_type(self,
                     type: ct.CompiledType,
                     from_module: ct.CompiledType | None = None) -> str:
        """
        Return the name of the type to use in the C API for ``type``. This
        is the type of a c value.

        :param type: The type for which we want to get the C type name.
        :param from_module: Module from which we want to access the name.
        """
        if not type.is_entity_type and from_module == type:
            return "c_type"

        return dispatch_on_type(type, [
            (T.Bool, lambda _: 'bool'),
            (T.Int, lambda _: 'int'),
            (T.Character, lambda _: 'string'),
            (T.String, lambda _: 'string'),
            (T.Token, lambda t:
                '{}.t structure'.format(self.module_name(t))),
            (T.SourceLocation, lambda t:
                '{}.t structure'.format(self.module_name(t))),
            (T.Symbol, lambda t:
                '{}.t structure'.format(self.module_name(t))),
            (ct.EnumType, lambda t: self.type_public_name(t)),
            (ct.ASTNodeType, lambda _: 'BareNode.t'),
            (ct.EntityType, lambda t:
                '{}.t structure'.format(self.struct_name(t))),
            (T.AnalysisUnit, lambda t:
                '{}.t'.format(self.struct_name(t))),
            (ct.ArrayType, lambda t:
                '{}.t structure ptr'.format(self.struct_name(t))),
            (ct.IteratorType, lambda t:
                '{}.t structure ptr'.format(self.struct_name(t))),
            (ct.StructType, lambda t:
                '{}.t structure'.format(self.struct_name(t))),
            (T.BigInt, lambda _: 'unit ptr'),
            (T.EnvRebindings, lambda t: self.type_public_name(t)),
        ])

    def type_public_name(self,
                         type: ct.CompiledType,
                         from_module: ct.CompiledType | None = None) -> str:
        """
        Return the public API name for a given CompiledType instance.

        :param type: The type for which we want to get the name.
        :param from_module: Module from which we want to access the name.
        """
        def from_module_name(type: ct.CompiledType) -> str:
            if from_module == type:
                return "t"
            else:
                return "{}.t".format(self.module_name(type))

        def entity_type_name(type: ct.EntityType) -> str:
            name = type.astnode.kwless_raw_name.lower

            # Add _node to the name if it is an existing ocaml keyword
            return name + "_node" if name in ocaml_keywords else name

        return dispatch_on_type(type, [
            (T.Bool, lambda _: 'bool'),
            (T.Int, lambda _: 'int'),
            (T.Token, lambda t: from_module_name(t)),
            (T.SourceLocation, lambda t: from_module_name(t)),
            (T.Symbol, lambda _: 'string'),
            (ct.EnumType, lambda t: from_module_name(t)),
            (ct.ASTNodeType, lambda t: self.type_public_name(t.entity)),
            (ct.EntityType, lambda t: entity_type_name(t)),
            (T.AnalysisUnit, lambda t: t.api_name.lower),
            (T.EntityInfo, lambda t: t.api_name.lower),
            (T.env_md, lambda t: t.api_name.lower),
            (T.Character, lambda _: 'string'),
            (T.String, lambda _: 'string'),
            (ct.ArrayType, lambda t:
                '{} list'.format(self.type_public_name(type.element_type))),
            (ct.IteratorType, lambda t: 'unit'),
            (ct.StructType, lambda t: from_module_name(t)),
            (T.BigInt, lambda t: from_module_name(t)),
            (T.EnvRebindings, lambda _: 'Rebindings.t'),
        ])

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
        marks = {typ: 'white' for typ in self.type_graph}
        topo: list[TypeOrPlaceholder] = []

        def dfs(vertex: TypeOrPlaceholder) -> None:
            if marks[vertex] == 'black':
                # Already visited
                return
            else:
                # TODO: cycle detected, this is not yet implemented
                assert marks[vertex] == 'white', 'cycle detected'

                marks[vertex] = 'gray'

                for succ in self.type_graph[vertex]:
                    dfs(succ)

                topo.append(vertex)

                marks[vertex] = 'black'

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
