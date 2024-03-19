from __future__ import annotations
from dataclasses import dataclass

from typing import TYPE_CHECKING

from langkit.c_api import CAPISettings
import langkit.compiled_types as ct
from langkit.compiled_types import ArrayType, BaseField, CompiledType, T
from langkit.language_api import AbstractAPISettings
from langkit.names import Name
from langkit.utils import dispatch_on_type


if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx


# ===== Util functions =====

def format_name(name: str) -> str:
    """
    Format the name to the Java method camel case.
    For example: F_My_Field become fMyField.

    :param name: The name you want to format.
    """
    res = Name.from_lower(name).camel
    return ''.join((res[:1].lower(), res[1:]))


# ===== Util classes =====

@dataclass
class ToRelease:
    """
    A variable to release after a method.
    """

    name: str
    """
    The name of the variable to release.
    """

    public_type: CompiledType
    """
    The type of the variable to release.
    """


@dataclass
class StructField:
    """
    Information necessary to emit Java code for a structure field.
    """

    lower_name: str
    """
    The name of the field formatted in lower case.
    """

    public_type: CompiledType
    """
    The Langkit compiled type of the field.
    """

    fields: list[StructField] | None = None
    """
    List of sub-fields if this field is a structure, None otherwise.
    """

    @property
    def name(self) -> str:
        """
        Get the field formatted name.
        """
        return format_name(self.lower_name)

    @property
    def native_name(self) -> str:
        """
        Get the field name as it appears in the native header.
        """
        return self.lower_name


@dataclass
class FlatStructField:
    """
    Structure field in its flat form.

    See JavaAPISettings.flatten_struct_field for details about that field
    format.
    """

    lower_name: str
    """
    The name of the field formatted in lower case.
    """

    public_type: CompiledType
    """
    The Langkit compiled type of the field.
    """

    base: list[str]
    """
    The list of parent structs in which the current field is nested in.
    """

    @property
    def native_access(self) -> str:
        """
        Get the access name of the field in the native image field naming
        convention.
        """
        full_path = self.base + [self.lower_name]
        return '_'.join(full_path)

    def custom_access(self, sep: str) -> str:
        """
        Get the field access with a custom separator.

        :param sep: The custom seperator for the access.
        """
        full_path = self.base + [self.lower_name]
        return sep.join(full_path)


@dataclass
class JavaMethod:
    """
    Java version of a Langkit field accessor or property.
    """

    lower_name: str
    """
    The name of the method formatted in lower case.
    """

    public_type: CompiledType
    """
    The return Langkit compiled type of the method.
    """

    params: list[JavaParam]
    """
    A list of the method parameters.
    """

    @property
    def name(self) -> str:
        """
        Get the Java formatted method name.
        """
        return format_name(self.lower_name)

    @property
    def native_name(self) -> str:
        """
        Get the function name in its native format.
        """
        return self.lower_name


@dataclass
class JavaParam:
    """
    Parameter in a Java method.
    """

    lower_name: str
    """
    The name of the parameter formatted in lower case.
    """

    public_type: CompiledType
    """
    The Langkit compiled type of the parameter.
    """

    default_value_expr: str | None = None
    """
    The Java expression for the parameter default value, None if the there
    is none.
    """

    @property
    def name(self) -> str:
        """
        Get the Java formatted argument name.
        """
        return format_name(self.lower_name)

    @property
    def native_name(self) -> str:
        """
        Get the argument name in its native format.
        """
        return self.lower_name


# ===== The main settings class =====

class JavaAPISettings(AbstractAPISettings):
    """
    Container for the Java API generation settings.
    """

    name = 'java'
    ni_pointer_types = [
        "PointerBase",
        "Pointer",
        "VoidPointer",
        "WordPointer",
        "CCharPointer",
        "CCharPointerPointer",
        "CDoublePointer",
        "CFloatPointer",
        "CIntPointer",
        "CIntPointerPointer",
        "CLongPointer",
        "CShortPointer",
    ]
    excluded_fields = [
        "unit"
    ]

    def __init__(self,
                 ctx: CompileCtx,
                 c_api_settings: CAPISettings):
        """
        Create a new Java api settings class.
        """
        self.context = ctx
        self.c_api_settings = c_api_settings

    # ----- Util methods -----

    def should_emit_struct(self, struct_type: ct.StructType) -> bool:
        """
        Whether this struct type should be emitted in the Java bindings.
        """
        if struct_type.is_entity_type:
            return struct_type == T.root_node.entity
        else:
            return (struct_type is T.entity_info
                    or struct_type is T.env_md
                    or struct_type.exposed)

    def get_struct_fields(self, struct: ct.StructType) -> list[StructField]:
        """
        Get the structure fields as a list.

        :param struct: The structure type to get the fields from.
        """
        # Compute the structure fields
        res = []
        for field in struct.get_fields():
            if field.type.is_struct_type:
                inner_fields = self.get_struct_fields(field.type)
                res.append(StructField(
                    field.name.lower,
                    field.type,
                    inner_fields
                ))
            else:
                res.append(StructField(field.name.lower, field.type))
        return res

    def flatten_struct_fields(
        self,
        to_flatten: list[StructField],
        base: list[str] | None = None
    ) -> list[FlatStructField]:
        """
        Flatten the given struct field list to a list of flat fields.

        Example with those structures:

        .. code-block::

            MyStruct = {
                field_1: BigInteger;
                field_2: OtherStruct;
            }
            OtherStruct = {
                other_field_1: String;
                other_field_2: Bool;
            }

        We get the following field list:

        .. code-block::

            [
                field_1: StructField[BigInteger];
                field_2: StructField[OtherStruct] = {
                    other_field_1: StructField[String];
                    other_field_2: StructField[Bool];
                };
            ]

        Then we can flatten the fields to get a list this:

        .. code-block::

            [
                field_1: FlatStructField[BigInteger];
                field_2.other_field_1: FlatStructField[String];
                field_2.other_field_2: FlatStructField[Bool];
            ]

        :param to_flatten: The list of dict to flatten.
        :param base: The base of the current list to flatten.
        """
        # Get the base default value
        base = base or []

        # Compute the flat the given field
        res = []
        for field in to_flatten:
            if field.fields is None:
                res.append(FlatStructField(
                    field.lower_name,
                    field.public_type,
                    base
                ))
            elif field.fields:
                res.extend(self.flatten_struct_fields(
                    field.fields,
                    base + [field.lower_name]
                ))
            else:
                res.append(FlatStructField(
                    "dummy",
                    T.Bool,
                    base + [field.lower_name]
                ))
        return res

    def get_node_formatted_fields(self, cls: ct.ASTNodeType) -> list[str]:
        """
        Get the list of Java formatted field names for the given node type.

        :param cls: The node type to get the fields and properties from.
        """
        res = (
            []
            if cls.base is None else
            self.get_node_formatted_fields(cls.base)
        )
        res.extend(
            format_name(field.api_name.lower)
            for field in cls.get_parse_fields(
                predicate=lambda f: not f.abstract and not f.null,
                include_inherited=False
            )
        )
        return res

    def get_java_method(self, field: ct.Field) -> JavaMethod:
        """
        Get the Java method from the given field.

        :param field: The field to extract the Java method from.
        """
        # Prepare the argument list
        params = []

        # Add all arguments
        for arg in field.arguments:
            default_value_expr: str | None = None
            if arg.default_value is not None:
                default_value_expr = arg.default_value.render_java_constant()
            params.append(JavaParam(
                arg.name.lower,
                arg.public_type,
                default_value_expr
            ))

        return JavaMethod(field.api_name.lower, field.public_type, params)

    def field_needs_unit(self, field: BaseField) -> bool:
        """
        Return if the field needs the analysis unit of the node to wrap/
        unwrap its argument and return value.

        :param field: The field to test.
        """
        # The return type is a token
        if field.public_type.is_token_type:
            return True

        # Default result
        return False

    def field_needs_context(self, field: BaseField) -> bool:
        """
        Return if the field needs the analysis context to wrap/unwrap its
        arguments and return value.

        :param field: The field to test.
        """
        for arg in field.arguments:
            # An argument is a symbol
            if arg.public_type.is_symbol_type:
                return True

        # Default result
        return False

    def extend_release_list(self,
                            release_list: list[ToRelease],
                            the_type: CompiledType,
                            var: str) -> None:
        """
        Extends the given release list according to the variable type.

        :param release_list: The release list to extend.
        :param the_type: The type of the value.
        :param var: The variable to eventually release.
        """
        dispatch_on_type(the_type, [
            (T.BigInt, lambda t: release_list.append(ToRelease(var, t))),
            (T.String, lambda t: release_list.append(ToRelease(var, t))),
            (ct.ArrayType, lambda t: release_list.append(ToRelease(var, t))),
            (
                ct.StructType, lambda t:
                    release_list.append(ToRelease(var, the_type))
                    if t.is_refcounted else
                    None
            ),
            (object, lambda _: None),
        ])

    def mangle_enum(self, name: str) -> str:
        """
        Mangle the given enum name to avoid reserved names.

        :param name: The name to mangle.
        """
        return (
            name
            if name != "NONE" else
            "NONE_ENUM"
        )

    # ----- Typing methods -----

    def wrapping_type(self,
                      the_type: CompiledType,
                      ast_wrapping: bool = True) -> str:
        """
        Get the type name for the Java nomination.

        :param the_type: The type you want the name from.
        :param ast_wrapping: If the function should wrap the AST types.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "boolean"),
            (T.Int, lambda _: "int"),
            (T.Character, lambda _: "Char"),
            (T.BigInt, lambda _: "BigInteger"),
            (ct.EnumType, lambda t: t.api_name.camel),
            (T.Symbol, lambda _: "Symbol"),
            (T.String, lambda _: "String"),
            (T.Text, lambda _: "Text"),
            (T.SourceLocation, lambda _: "SourceLocation"),
            (T.SourceLocationRange, lambda _: "SourceLocationRange"),
            (T.Diagnostic, lambda _: "Diagnostic"),
            (T.EnvRebindings, lambda _: "PointerWrapper"),
            (T.FileReader, lambda _: "FileReader"),
            (T.UnitProvider, lambda _: "UnitProvider"),
            (T.EventHandler, lambda _: "EventHandler"),
            (T.Token, lambda _: "Token"),
            (T.AnalysisUnit, lambda _: "AnalysisUnit"),
            (T.AnalysisContext, lambda _: "AnalysisContext"),
            (
                ct.ASTNodeType, lambda t:
                    self.wrapping_type(t.entity)
                    if ast_wrapping else
                    "PointerWrapper"
            ),
            (
                ct.EntityType, lambda t:
                    t.astnode.kwless_raw_name.camel
                    if ast_wrapping else
                    "Entity"
            ),
            (ct.StructType, lambda t: t.api_name.camel),
            (ct.ArrayType, lambda t: self.array_wrapping_type(t)),
            (ct.IteratorType, lambda t: t.api_name.camel),
        ])

    def array_wrapping_type(self, array_type: ArrayType) -> str:
        """
        Get the array type in Java for the wanted compiled type.
        """
        return (
            ct.T.entity.array
            if array_type.element_type.is_entity_type else
            array_type
        ).api_name.camel

    def wrapper_class(self,
                      the_type: CompiledType,
                      ast_wrapping: bool = True) -> str:
        """
        Return the name of the class which contains the wrapping operation for
        the given type.

        :param the_type: Type to get the class for.
        :param ast_wrapping: If the AST node should be wrapped in their Java
            class.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "BooleanWrapper"),
            (T.Int, lambda _: "IntegerWrapper"),
            (T.BigInt, lambda _: "BigIntegerWrapper"),
            (T.String, lambda _: "StringWrapper"),
            (object, lambda t: self.wrapping_type(t, ast_wrapping))
        ])

    def none_value(self,
                   the_type: CompiledType,
                   ast_wrapping: bool = True) -> str:
        """
        Return the none (default) value Java expression for the given type.

        :param the_type: The type of the expression to get the none for.
        :param ast_wrapping: If the AST node should be wrapped.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "false"),
            (T.Int, lambda _: "0"),
            (T.EnvRebindings, lambda _: "PointerWrapper.nullPointer()"),
            (T.Token, lambda _: "Token.NONE(currentUnit)"),
            (
                ct.ASTNodeType, lambda t:
                    self.none_value(t.entity, ast_wrapping)
                    if ast_wrapping else
                    "PointerWrapper.nullPointer()"
            ),
            (
                ct.EntityType, lambda t:
                    f"{t.astnode.kwless_raw_name.camel}.NONE"
                    if ast_wrapping else
                    "Entity.NONE"
            ),
            (object, lambda t: f"{self.wrapper_class(t, ast_wrapping)}.NONE"),
        ])

    def is_java_nullable(self, the_type: CompiledType) -> bool:
        """
        Get if the given type is nullable in the Java semantic.

        :param the_type: The type to verify the nullability.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: False),
            (T.Int, lambda _: False),
            (object, lambda _: True),
        ])

    # ----- Native-Image methods -----

    def ni_type(self, the_type: CompiledType) -> str:
        """
        Get the Java type that represents the given type for the Graal C API.

        :param the_type: The type you want the name from.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "byte"),
            (T.Int, lambda _: "int"),
            (T.Character, lambda _: "int"),
            (T.BigInt, lambda _: "BigIntegerNative"),
            (ct.EnumType, lambda _: "int"),
            (T.Symbol, lambda _: "SymbolNative"),
            (T.String, lambda _: "StringNative"),
            (T.Text, lambda _: "TextNative"),
            (T.SourceLocation, lambda _: "SourceLocationNative"),
            (T.SourceLocationRange, lambda _: "SourceLocationRangeNative"),
            (T.Diagnostic, lambda _: "DiagnosticNative"),
            (T.EnvRebindings, lambda _: "Pointer"),
            (T.FileReader, lambda _: "FileReaderNative"),
            (T.UnitProvider, lambda _: "UnitProviderNative"),
            (T.EventHandler, lambda _: "EventHandlerNative"),
            (T.Token, lambda _: "TokenNative"),
            (T.AnalysisUnit, lambda _: "AnalysisUnitNative"),
            (T.AnalysisContext, lambda _: "AnalysisContextNative"),
            (T.entity_info, lambda _: "EntityInfoNative"),
            (T.env_md, lambda _: "MetadataNative"),
            (ct.EntityType, lambda _: "EntityNative"),
            (ct.ASTNodeType, lambda _: "Pointer"),
            (ct.StructType, lambda t: f"{t.api_name.camel}Native"),
            (ct.ArrayType, lambda t: f"{self.array_wrapping_type(t)}Native"),
            (ct.IteratorType, lambda t: f"{t.api_name.camel}Native"),
        ])

    def ni_reference_type(self, the_type: CompiledType) -> str:
        """
        Get the Java type that represents a reference to the given type for
        the Graal C API.

        :param the_type: The type to get the reference type for.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "CCharPointer"),
            (T.Int, lambda _: "CIntPointer"),
            (T.Character, lambda _: "CIntPointer"),
            (T.BigInt, lambda _: "WordPointer"),
            (T.String, lambda _: "WordPointer"),
            (T.AnalysisUnit, lambda _: "WordPointer"),
            (T.AnalysisContext, lambda _: "WordPointer"),
            (ct.ArrayType, lambda _: "WordPointer"),
            (ct.IteratorType, lambda _: "WordPointer"),
            (ct.EnumType, lambda _: "CIntPointer"),
            (object, lambda t: self.ni_type(t))
        ])

    def ni_stack_value(self, the_type: CompiledType) -> str:
        """
        Return the Java expression of a new stack value for the given type.

        :param the_type: The type to get a new stack value for.
        """
        ref_type = self.ni_reference_type(the_type)
        if ref_type in self.ni_pointer_types:
            return "StackValue.get(SizeOf.get(WordPointer.class))"
        else:
            return f"StackValue.get({ref_type}.class)"

    def ni_wrap(self,
                the_type: CompiledType,
                source: str,
                release_list: list[ToRelease],
                ast_wrapping: bool = True) -> str:
        """
        Returns the Java expression (in a string) to wrap the `source` Native
        Image C API value.

        :param the_type: The type of the expression to wrap.
        :param source: The Java expression to wrap.
        :param release_list: The list that will be filled with elements to
            release after the `source` wrapping.
        :param ast_wrapping: Whether to wrap node types in their Java classes.
        """
        # Extends the release list
        self.extend_release_list(release_list, the_type, source)

        # Return the wrapping expression
        return dispatch_on_type(the_type, [
            (
                ct.EnumType, lambda t:
                    f"{self.wrapper_class(t)}.fromC({source})"
            ),
            (T.Token, lambda _: f"Token.wrap({source}, currentUnit)"),
            (
                ct.ASTNodeType, lambda t:
                    self.ni_wrap(t.entity, source, release_list)
                    if ast_wrapping else
                    f"PointerWrapper.wrap({source})"
            ),
            (
                ct.EntityType, lambda t:
                    (
                        f"{self.wrapper_class(t)}.fromEntity"
                        f"(Entity.wrap({source}))"
                    )
                    if ast_wrapping else
                    f"Entity.wrap({source})"
            ),
            (object, lambda t: f"{self.wrapper_class(t)}.wrap({source})"),
        ])

    def ni_unwrap(self,
                  the_type: CompiledType,
                  source: str,
                  export: str,
                  release_list: list[ToRelease]) -> str:
        """
        Get the Java unwrap operation to get a NI value from the expression.

        :param the_type: The type of the expression to unwrap.
        :param source: The expression to unwrap.
        :param export: The name of the variable to place the result in.
        :param release_list: The list of the element to release because the
            unwrapping caused allocation.
        """
        # Extends the release list
        self.extend_release_list(release_list, the_type, export)

        # Return the unwrapping statement
        ni_type = self.ni_type(the_type)

        res = f"{ni_type} {export} = "

        res += dispatch_on_type(the_type, [
            (
                T.Bool, lambda _:
                    f"({source} ? (byte) 1 : (byte) 0);"
            ),
            (T.Int, lambda _: f"{source};"),
            (T.Character, lambda _: f"{source}.value;"),
            (
                T.BigInt, lambda t: (
                    f"StackValue.get(SizeOf.get(WordPointer.class));"
                    f"BigIntegerWrapper.unwrap({source},"
                    f"(WordPointer) {export});"
                )
            ),
            (ct.EnumType, lambda _: f"{source}.toC();"),
            (
                T.Symbol, lambda _: (
                    f"StackValue.get(SymbolNative.class);"
                    f"{source}.unwrap({export}, currentContext);"
                )
            ),
            (
                T.String, lambda t:
                    f"StringWrapper.unwrap({source});"
            ),
            (
                ct.ASTNodeType, lambda t:
                    self.ni_unwrap(t.entity, source, export, release_list)
            ),
            (
                ct.EntityType, lambda _: (
                    "StackValue.get(EntityNative.class);"
                    f"{source}.entity.unwrap({export});"
                )
            ),
            (
                ct.ArrayType, lambda t:
                    f"{source}.unwrap(currentContext);"
                    if t.element_type.is_symbol_type else
                    f"{source}.unwrap();"
            ),
            (
                object, lambda _:
                    self.object_unwrap(the_type, source, export)
            ),
        ])

        return res

    def object_unwrap(self,
                      the_type: CompiledType,
                      source: str,
                      export: str) -> str:
        """
        Unwrap an object for the native image C API.

        :param the_type: The type of the expression to unwrap.
        :param source: The expression to unwrap.
        :param export: The name to place the unwrapped result in.
        """
        ref_type = self.ni_reference_type(the_type)

        if ref_type not in self.ni_pointer_types:
            return (
                f"StackValue.get({ref_type}.class);"
                f"{source}.unwrap({export});"
            )

        else:
            if the_type.is_ada_record:
                return (
                    f"StackValue.get({ref_type}.class);"
                    f"{source}.unwrap({export});"
                )
            else:
                return f"{source}.unwrap();"

    def ni_write(self,
                 the_type: CompiledType,
                 source: str,
                 pointer: str) -> str:
        """
        Return the Java statement to write the source Java expression in the
        target Graal C API pointer.

        :param the_type: The type of the expression to write.
        :param source: The source Java expression to write.
        :param pointer: The pointer to write in.
        """

        return dispatch_on_type(the_type, [
            (
                T.Bool, lambda _:
                    f"{pointer}.write({source} ? (byte) 1 : (byte) 0);"
            ),
            (T.Int, lambda _: f"{pointer}.write({source});"),
            (T.Character, lambda _: f"{pointer}.write({source}.value);"),
            (
                T.Symbol, lambda _:
                    f"{source}.unwrap({pointer}, currentContext);"
            ),
            (
                T.BigInt, lambda _:
                    f"BigIntegerWrapper.unwrap({source}, {pointer});"
            ),
            (ct.EnumType, lambda _: f"{pointer}.write({source}.toC());"),
            (
                T.String, lambda _:
                    f"StringWrapper.unwrap({source}, {pointer});"
            ),
            (
                T.EnvRebindings, lambda _:
                    f"{pointer}.writeWord(0, {source}.ni());"
            ),
            (
                T.AnalysisContext, lambda _:
                f"{pointer}.write({source}.unwrap());"
            ),
            (
                T.AnalysisUnit, lambda _:
                    f"{pointer}.write({source}.unwrap());"
            ),
            (
                ct.IteratorType, lambda _:
                    f"{pointer}.write({source}.unwrap());"
            ),
            (
                ct.ArrayType, lambda t:
                    f"{source}.unwrap({pointer}, currentContext);"
                    if t.element_type.is_symbol_type else
                    f"{source}.unwrap({pointer});"
            ),
            (
                ct.ASTNodeType, lambda t:
                    f"{pointer}.writeWord(0, {source}.ni());"
            ),
            (ct.EntityType, lambda _: f"{source}.unwrap({pointer});"),
            (object, lambda _: f"{source}.unwrap({pointer});")
        ])

    def ni_field_wrap(self,
                      field: StructField,
                      base: list[str] | None = None) -> str:
        """
        Get the wrapping statement for the given structure field.

        :param field: The field to wrap.
        :param base: The base of the current fields.
        """
        base = base or []
        field_type = self.wrapping_type(field.public_type, False)
        field_name = "_".join(base + [field.native_name])

        # If the field to wrap is a leaf (not a struct) then generate the
        # wrapping operation on it by getting the native value from the
        # native struct.
        if field.fields is None:
            return self.ni_wrap(
                field.public_type,
                f"structNative.get_{field_name}()",
                [],
                ast_wrapping=False
            )

        # Else, it the field has sub-fields, this is a composite value
        # (struct), we call the wrapping operation for the field type with
        # the native address of the field.
        elif field.fields:
            return self.ni_wrap(
                field.public_type,
                f"structNative.address_{field_name}()",
                [],
                ast_wrapping=False
            )

        # Else, the field is a struct without any fields, we just return its
        # `NONE` singleton.
        else:
            return f"{field_type}.NONE"

    def ni_field_unwrap(self, field: StructField) -> str:
        """
        Unwrap the given structure field and write the result in the assumed
        initialized 'structNative' variable.

        :param field: The field to unwrap.
        """
        to_write = f"{field.name}Native"
        res = (
            f"{self.ni_reference_type(field.public_type)} {to_write} = "
            f"structNative.address_{field.lower_name}();"
        )

        return res + self.ni_write(
            field.public_type,
            f"this.{field.name}",
            to_write
        )

    # ----- JNI methods -----

    def jni_func_name(self, name: str, do_nat: bool = True) -> str:
        """
        Get the JNI function name from the original function name.

        See (https://docs.oracle.com/javase/8/docs/technotes/guides
        /jni/spec/design.html) for the name mangling documentation.

        :param name: The original function name.
        :param do_nat: If the native name conversion should be done.
        """
        # Get the function base name according to the do_nat parameter
        func_base_name = (
            self.c_api_settings.get_name(name)
            if do_nat else
            name
        ).replace('_', '_1')

        # Return the mangled function name
        return '_'.join([
            # Everything belongs to the Java com.adacore package
            'Java_com_adacore',

            # The library specific package ("libfoolang")
            self.context.lib_name.lower,

            # The top-level Java class ("Libfoolang")
            self.context.lib_name.camel,

            # The function belongs to the JNI_LIB class
            '00024JNI_1LIB',

            # The function name
            func_base_name
        ])

    def jni_func_sig(self,
                     name: str,
                     return_type: str,
                     do_nat: bool = True) -> str:
        """
        Create a JNI function signature with the given function name
        and return type.

        :param name: The name of the function.
        :param return_type: The return type of the function.
        :param do_nat: If the native name conversion should be done.
        """
        return f"JNIEXPORT {return_type} {self.jni_func_name(name, do_nat)}"

    def java_jni_wrap(self, the_type: CompiledType, expr: str) -> str:
        """
        Get the Java expression to wrap the given expression of the given type.

        :param the_type: The type of the expression to export.
        :param expr: The Java expression to wrap.
        """
        return dispatch_on_type(the_type, [
            (
                ct.EntityType, lambda t:
                    f"{self.wrapping_type(t)}.fromEntity({expr})"
            ),
            (ct.ASTNodeType, lambda t: self.java_jni_wrap(t.entity, expr)),
            (object, lambda _: expr),
        ])

    def java_jni_unwrap(self, the_type: CompiledType, expr: str) -> str:
        """
        Get the Java expression that unwrap the given expression.

        :param the_type: The type of the expression to unwrap.
        :param expr: The expression to unwrap.
        """
        return dispatch_on_type(the_type, [
            (
                ct.EntityType, lambda _:
                    f"({expr} != null ? {expr}.entity : null)"
            ),
            (ct.ASTNodeType, lambda t: self.java_jni_unwrap(t.entity, expr)),
            (object, lambda _: expr),
        ])

    def jni_c_type(self, the_type: CompiledType) -> str:
        """
        Give the JNI c type for the given compiled type.

        :param the_type: The type.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "jboolean"),
            (T.Int, lambda _: "jint"),
            (object, lambda _: "jobject"),
        ])

    def jni_c_none(self, the_type: CompiledType) -> str:
        """
        Give the C none expression for the given compiled type.

        :param the_type: The type to get the none expression for.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "0"),
            (T.Int, lambda _: "0"),
            (object, lambda _: "NULL"),
        ])

    def jni_sig_type(self, the_type: CompiledType, base_class: str) -> str:
        """
        Get the type representation for the JNI signature definition.

        :param the_type: The original compiled type.
        :param base_class: The parent class of all Java classes.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "Z"),
            (T.Int, lambda _: "I"),
            (T.BigInt, lambda _: "Ljava/math/BigInteger;"),
            (T.String, lambda _: "Ljava/lang/String;"),
            (
                ct.ASTNodeType, lambda t:
                    f"L{base_class}$PointerWrapper;"
                    if t == T.root_node else
                    f"L{base_class}${self.wrapping_type(t, False)};"
            ),
            (T.EnvRebindings, lambda _: f"L{base_class}$PointerWrapper;"),
            (
                object, lambda t:
                    f"L{base_class}${self.wrapping_type(t, False)};"
            ),
        ])

    def jni_field_access(self, the_type: CompiledType) -> str:
        """
        Return the JNI field accessor function for the given type.

        :param the_type: The type.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "GetBooleanField"),
            (T.Int, lambda _: "GetIntField"),
            (object, lambda _: "GetObjectField"),
        ])

    def jni_new_value(self, the_type: CompiledType) -> str:
        """
        Get the C expression for a new value for the type.

        :param the_type: The type.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "0"),
            (T.Int, lambda _: "0"),
            (
                ct.ASTNodeType, lambda _:
                    "PointerWrapper_new_value()"
                    if the_type == T.root_node else
                    "Entity_new_value()"
            ),
            (T.EnvRebindings, lambda _: "PointerWrapper_new_value()"),
            (object, lambda t: f"{self.wrapping_type(t, False)}_new_value()"),
        ])

    def jni_wrap(self,
                 the_type: CompiledType,
                 expr: str,
                 release_list: list[ToRelease]) -> str:
        """
        Get the C expression to wrap the given expression in a Java value.

        :param the_type: The type of the expression.
        :param expr: The name of the value to wrap.
        :param release_list: The list of the variable to release after
            wrapping.
        """
        # Extends the release list
        self.extend_release_list(release_list, the_type, expr)

        # Return the wrapping expression
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: f"(jboolean) {expr}"),
            (T.Int, lambda _: f"(jint) {expr}"),
            (
                T.Token, lambda _: (
                    f"Token_wrap(env, {expr}, "
                    "AnalysisUnit_wrap(env, unit_native))"
                )
            ),
            (
                ct.ASTNodeType, lambda t:
                    f"PointerWrapper_wrap(env, (void*) {expr})"
                    if t == T.root_node else
                    f"Entity_wrap(env, {expr})"
            ),
            (
                T.EnvRebindings, lambda _:
                    f"PointerWrapper_wrap(env, (void*) {expr})"
            ),
            (
                object, lambda t:
                    f"{self.wrapping_type(t, False)}_wrap(env, {expr})"
            ),
        ])

    def jni_unwrap(self,
                   the_type: CompiledType,
                   expr: str,
                   export: str,
                   release_list: list[ToRelease]) -> str:
        """
        Get the C statement to unwrap the given expression in the given export
        name.

        :param the_type: The type.
        :param expr: The name of the value to unwrap.
        :param export: The variable to place the unwrapping result in.
        :param release_list: The list of the variable to release after the
            unwrapping.
        """
        # Extends the release list
        self.extend_release_list(release_list, the_type, export)

        # Return the unwrapping expression
        export_type = the_type.c_type(self.c_api_settings).name
        res = f"{export_type} {export} = "

        res += dispatch_on_type(the_type, [
            (
                T.Bool, lambda t:
                    f"({export_type}) {expr};"
            ),
            (T.Int, lambda _: f"(int) {expr};"),
            (
                T.Symbol, lambda _:
                    f"Symbol_unwrap(env, {expr}, context_native);"
            ),
            (
                ct.ASTNodeType, lambda t:
                    f"PointerWrapper_unwrap(env, {expr});"
                    if t == T.root_node else
                    f"Entity_unwrap(env, {expr});"
            ),
            (
                T.EnvRebindings, lambda _:
                    f"PointerWrapper_unwrap(env, {expr});"
            ),
            (
                ct.ArrayType, lambda t:
                    (
                        f"{self.wrapping_type(t, False)}_unwrap"
                        f"(env, {expr}, context_native);"
                    ) if t.element_type.is_symbol_type else
                    f"{self.wrapping_type(t, False)}_unwrap(env, {expr});"
            ),
            (
                object, lambda t:
                    f"{self.wrapping_type(t, False)}_unwrap(env, {expr});"
            ),
        ])

        return res
