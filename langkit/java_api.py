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

    @property
    def java_access(self) -> str:
        """
        Get the Java pointed access to the nested field.
        """
        full_path = self.base + [self.lower_name]
        return '.'.join([format_name(part) for part in full_path])

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

    def get_struct_fields(self, struct: ct.StructType) -> list[StructField]:
        """
        Get the structure fields as a list.

        :param struct: The structure type to get the fields from.
        """
        # Compute the structure fields
        res = []
        for field in struct.get_fields():
            if field.type.is_struct_type:
                if field.type.get_fields():
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
            if field.fields:
                res.extend(self.flatten_struct_fields(
                    field.fields,
                    base + [field.lower_name]
                ))
            else:
                res.append(FlatStructField(
                    field.lower_name,
                    field.public_type,
                    base
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

    def field_need_context(self, field: BaseField) -> bool:
        """
        Return either the field need the analysis context to wrap/unwrap its
        arguments.

        :param field: The field to test.
        """
        for arg in field.arguments:
            if arg.public_type.is_symbol_type:
                return True

        # Default result
        return False

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
            (T.EnvRebindings, lambda _: "PointerWrapper"),
            (ct.EnumType, lambda t: t.api_name.camel),
            (T.BigInt, lambda _: "BigInteger"),
            (T.Symbol, lambda _: "Symbol"),
            (T.String, lambda _: "StringWrapper"),
            (T.Text, lambda _: "Text"),
            (T.SourceLocation, lambda _: "SourceLocation"),
            (T.SourceLocationRange, lambda _: "SourceLocationRange"),
            (T.Diagnostic, lambda _: "Diagnostic"),
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

    # ----- Native-Image methods -----

    def java_ni_type(self, the_type: CompiledType) -> str:
        """
        Get the Native-Image type for the given Langkit type.

        :param the_type: The type you want the name from.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "byte"),
            (T.Int, lambda _: "int"),
            (T.Character, lambda _: "int"),
            (T.EnvRebindings, lambda _: "Pointer"),
            (ct.EnumType, lambda _: "int"),
            (T.BigInt, lambda _: "BigIntegerNative"),
            (T.Symbol, lambda _: "SymbolNative"),
            (T.String, lambda _: "StringNative"),
            (T.Text, lambda _: "TextNative"),
            (T.SourceLocation, lambda _: "SourceLocationNative"),
            (T.SourceLocationRange, lambda _: "SourceLocationRangeNative"),
            (T.Diagnostic, lambda _: "DiagnosticNative"),
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
        Get the reference type for the compiled type.

        :param the_type: The type to get the reference type for.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "CCharPointer"),
            (T.Int, lambda _: "CIntPointer"),
            (T.Character, lambda _: "CIntPointer"),
            (T.String, lambda _: "Pointer"),
            (T.BigInt, lambda _: "Pointer"),
            (T.AnalysisUnit, lambda _: "Pointer"),
            (T.AnalysisContext, lambda _: "Pointer"),
            (ct.ArrayType, lambda _: "Pointer"),
            (ct.IteratorType, lambda _: "Pointer"),
            (ct.EnumType, lambda _: "CIntPointer"),
            (object, lambda t: self.java_ni_type(t))
        ])

    def ni_new_value(self, the_type: CompiledType) -> str:
        """
        Allocate a new stack value for the given type.

        :param the_type: The type to get a new stack value for.
        """
        ref_type = self.ni_reference_type(the_type)
        if ref_type in self.ni_pointer_types:
            return "StackValue.get(SizeOf.get(VoidPointer.class))"
        else:
            return f"StackValue.get({ref_type}.class)"

    def ni_null_value(self, the_type: CompiledType) -> str:
        """
        Get the null (default) value of the given type.

        :param the_type: The type to get the empty value for.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "(byte) 0"),
            (T.Int, lambda _: "0"),
            (T.Character, lambda _: "0"),
            (T.EnvRebindings, lambda _: "WordFactory.nullPointer()"),
            (ct.EnumType, lambda _: "0"),
            (T.BigInt, lambda _: "WordFactory.nullPointer()"),
            (T.Symbol, lambda _: "Symbol.createEmpty()"),
            (T.String, lambda _: "WordFactory.nullPointer()"),
            (T.FileReader, lambda _: "WordFactory.nullPointer()"),
            (T.UnitProvider, lambda _: "WordFactory.nullPointer()"),
            (T.EventHandler, lambda _: "WordFactory.nullPointer()"),
            (T.AnalysisUnit, lambda _: "WordFactory.nullPointer()"),
            (T.AnalysisContext, lambda _: "WordFactory.nullPointer()"),
            (ct.ASTNodeType, lambda _: "WordFactory.nullPointer()"),
            (ct.ArrayType, lambda _: "WordFactory.nullPointer()"),
            (ct.IteratorType, lambda _: "WordFactory.nullPointer()"),
        ])

    def ni_wrap(self, the_type: CompiledType, expr: str) -> str:
        """
        Get the Java operation to wrap the given expression of the given
        type.

        :param the_type: The type of the expression to wrap.
        :param expr: The expression to wrap.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: f"({expr}.read() != 0)"),
            (T.Int, lambda _: f"{expr}.read()"),
            (
                ct.EnumType, lambda t:
                    f"{self.wrapping_type(t)}.fromC({expr}.read())"
            ),
            (T.Token, lambda _: f"Token.wrap({expr}, this.getUnit())"),
            (ct.ASTNodeType, lambda t: self.ni_wrap(t.entity, expr)),
            (
                ct.EntityType, lambda t: (
                    f"{self.wrapping_type(t)}.fromEntity"
                    f"(Entity.wrap({expr}))"
                )
            ),
            (object, lambda t: f"{self.wrapping_type(t)}.wrap({expr})"),
        ])

    def ni_unwrap(self,
                  the_type: CompiledType,
                  expr: str,
                  export: str) -> str:
        """
        Get the Java unwrap operation to get a NI value from the expression.

        :param the_type: The type of the expression to unwrap.
        :param expr: The expression to unwrap.
        :param export: The name of the variable to place the result in.
        """
        return dispatch_on_type(the_type, [
            (
                T.Bool, lambda _:
                    f"byte {export} = ({expr} ? (byte) 1 : (byte) 0)"
            ),
            (T.Character, lambda _: f"int {export} = {expr}.value"),
            (T.Int, lambda _: f"int {export} = {expr}"),
            (ct.EnumType, lambda _: f"int {export} = {expr}.toC()"),
            (
                ct.ASTNodeType, lambda t:
                    self.ni_unwrap(t.entity, expr, export)
            ),
            (
                ct.EntityType, lambda _:
                    self.node_unwrap(expr, export)
            ),
            (
                object, lambda _:
                    self.object_unwrap(the_type, expr, export)
            )
        ])

    def node_unwrap(self, expr: str, export: str) -> str:
        """
        Unwrap the given node expression in a native image value.

        :param expr: The node Java expression.
        :param export: The variable to put the result in.
        """
        return (
            f"EntityNative {export}=StackValue.get(EntityNative.class);"
            f"if({expr} != null) {expr}.entity.unwrap({export});"
            f"else Entity.defaultValue({export})"
        )

    def object_unwrap(self,
                      the_type: CompiledType,
                      expr: str,
                      export: str) -> str:
        """
        Unwrap an object for the native image C API.

        :param the_type: The type of the expression to unwrap.
        :param expr: The expression to unwrap.
        :param export: The name to place the unwrapped result in.
        """
        ref_type = self.ni_reference_type(the_type)
        wrap_type = self.wrapping_type(the_type)
        ni_type = self.java_ni_type(the_type)

        res = f"{ni_type} {export} = "
        if ref_type not in self.ni_pointer_types:
            res += f"{self.ni_new_value(the_type)}; if({expr} != null)"
            if the_type.is_symbol_type:
                res += f"{expr}.unwrap({export}, currentContext);"
            else:
                res += f"{expr}.unwrap({export});"

            if the_type.is_ada_record:
                res += f"else {wrap_type}.defaultValue({export})"
            else:
                res += (
                    f"else {export} = ({ni_type}) WordFactory.nullPointer()"
                )

        else:
            res += (
                f"({expr} != null ? {expr}.reference.ni() "
                f": WordFactory.nullPointer())"
            )

        return res

    def java_ni_write(self,
                      the_type: CompiledType,
                      source: str,
                      pointer: str) -> str:
        """
        Write the source Java object in the given pointer.

        :param the_type: The type of the expression to write.
        :param source: The source Java expression to write.
        :param pointer: The pointer to write in.
        """
        return dispatch_on_type(the_type, [
            (
                T.Bool, lambda _:
                    f"{pointer}.write({source} ? (byte) 1 : (byte) 0)"
            ),
            (T.Character, lambda _: f"{pointer}.write({source}.value)"),
            (T.Int, lambda _: f"{pointer}.write({source})"),
            (
                T.Symbol, lambda _:
                    f"{source}.unwrap({pointer}, currentContext)"
            ),
            (
                T.String, lambda _:
                    f"{pointer}.writeWord(0, {source}.reference.ni())"
            ),
            (
                T.BigInt, lambda _:
                    f"{pointer}.writeWord(0, {source}.reference.ni())"
            ),
            (
                T.AnalysisUnit, lambda _:
                    f"{pointer}.writeWord(0, {source}.reference.ni())"
            ),
            (
                T.AnalysisContext, lambda _:
                    f"{pointer}.writeWord(0, {source}.reference.ni())"
            ),
            (
                ct.ArrayType, lambda _:
                    f"{pointer}.writeWord(0, {source}.reference.ni())"
            ),
            (
                ct.IteratorType, lambda _:
                    f"{pointer}.writeWord(0, {source}.reference.ni())"
            ),
            (ct.EnumType, lambda _: f"{pointer}.write({source}.toC())"),
            (
                ct.ASTNodeType, lambda t:
                    self.java_ni_write(t.entity, source, pointer)
            ),
            (ct.EntityType, lambda _: f"{source}.entity.unwrap({pointer})"),
            (object, lambda _: f"{source}.unwrap({pointer})")
        ])

    def ni_field_wrap(self,
                      field: StructField,
                      base: list[str] | None = None) -> str:
        """
        Get the wrapping statement for the given structure field.

        :param field: The field to wrap.
        :param base: The base of the current fields.
        """
        # Get the base or an empty list
        base = base or []

        field_type = self.wrapping_type(field.public_type, ast_wrapping=False)
        if field.fields:
            inside = [
                self.ni_field_wrap(
                    inner_field,
                    base + [field.native_name]
                )
                for inner_field in field.fields
            ]
            return f"new {field_type}({', '.join(inside)})"
        else:
            field_name = "_".join(base + [field.native_name])
            getter = f"structNative.get_{field_name}()"
            return dispatch_on_type(field.public_type, [
                (T.Bool, lambda _: f"{getter} != 0"),
                (T.Int, lambda _: getter),
                (ct.EnumType, lambda _: f"{field_type}.fromC({getter})"),
                (
                    object, lambda t:
                        f"{field_type}.wrap({getter})"
                        if (
                            self.ni_reference_type(t) in self.ni_pointer_types
                        ) else
                        f"new {field_type}({getter})"
                ),
            ])

    def ni_field_unwrap(self, flat: FlatStructField) -> str:
        """
        Unwrap the given flat field in a native structure.

        :param flat: The flat field to unwrap.
        """
        return dispatch_on_type(flat.public_type, [
            (
                T.Bool, lambda _:
                    f"{flat.java_access} ? (byte) 1 : (byte) 0"
            ),
            (T.Int, lambda _: f"{flat.java_access}"),
            (T.Character, lambda _: f"{flat.java_access}.toChar()"),
            (T.EnvRebindings, lambda _: f"{flat.java_access}.ni()"),
            (ct.EnumType, lambda _: f"{flat.java_access}.toC()"),
            (ct.ASTNodeType, lambda _: f"{flat.java_access}.ni()"),
            (object, lambda t: f"{flat.java_access}.reference.ni()"),
        ])

    # ----- JNI methods -----

    def java_jni_type(self, the_type: CompiledType) -> str:
        """
        Get the JNI corresponding type from the Langkit type.

        :param the_type: The type you want the JNI equivalent from.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "boolean"),
            (T.Int, lambda _: "int"),
            (T.Character, lambda _: "Char"),
            (T.EnvRebindings, lambda _: "PointerWrapper"),
            (ct.EnumType, lambda t: self.wrapping_type(t)),
            (T.BigInt, lambda _: "BigInteger"),
            (T.Symbol, lambda _: "Symbol"),
            (T.String, lambda _: "StringWrapper"),
            (T.Text, lambda _: "Text"),
            (T.SourceLocation, lambda _: "SourceLocation"),
            (T.SourceLocationRange, lambda _: "SourceLocationRange"),
            (T.Diagnostic, lambda _: "Diagnostic"),
            (T.FileReader, lambda _: "FileReader"),
            (T.UnitProvider, lambda _: "UnitProvider"),
            (T.EventHandler, lambda _: "EventHandler"),
            (T.Token, lambda _: "Token"),
            (T.AnalysisUnit, lambda _: "AnalysisUnit"),
            (T.AnalysisContext, lambda _: "AnalysisContext"),
            (T.entity_info, lambda _: "EntityInfo"),
            (T.env_md, lambda _: "Metadata"),
            (ct.EntityType, lambda _: "Entity"),
            (ct.ASTNodeType, lambda _: "PointerWrapper"),
            (ct.StructType, lambda t: self.wrapping_type(t)),
            (ct.ArrayType, lambda t: self.array_wrapping_type(t)),
            (ct.IteratorType, lambda t: t.api_name.camel),
        ])

    def java_jni_wrap(self, the_type: CompiledType, expr: str) -> str:
        """
        Get the Java wrapping operation for the type.

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
        Get the Java unwrap operation to get a JNI value from the expression.

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

    def jni_sig_type(self, the_type: CompiledType, base_class: str) -> str:
        """
        Get the type representation for the JNI signature definition.

        :param the_type: The original compiled type.
        :param base_class: The parent class of all Java classes.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: "Z"),
            (T.Int, lambda _: "I"),
            (
                ct.ASTNodeType, lambda t:
                    f"L{base_class}$PointerWrapper;"
                    if t == T.root_node else
                    f"L{base_class}${self.java_jni_type(t)};"
            ),
            (T.EnvRebindings, lambda _: f"L{base_class}$PointerWrapper;"),
            (object, lambda t: f"L{base_class}${self.java_jni_type(t)};"),
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
        Get the expression for a new value for the type.

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
            (object, lambda t: f"{self.java_jni_type(t)}_new_value()"),
        ])

    def jni_wrap(self, the_type: CompiledType, expr: str) -> str:
        """
        Get the wrapping operation for the JNI using of the type.

        :param the_type: The type.
        :param expr: The name of the value to wrap.
        """
        return dispatch_on_type(the_type, [
            (T.Bool, lambda _: f"(jboolean) {expr}"),
            (T.Int, lambda _: f"(jint) {expr}"),
            (
                T.Token, lambda _:
                    f"Token_wrap(env, {expr}, get_node_unit(env, entity))"
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
                    f"{self.java_jni_type(t)}_wrap(env, {expr})"
            ),
        ])

    def jni_unwrap(self, the_type: CompiledType, expr: str) -> str:
        """
        Get the unwrapping operation in JNI for the given type.

        :param the_type: The type.
        :param expr: The name of the value to unwrap.
        """
        return dispatch_on_type(the_type, [
            (
                T.Bool, lambda t:
                    f"({t.c_type(self.c_api_settings).name}) {expr}"
            ),
            (T.Int, lambda _: f"(int) {expr}"),
            (
                T.Symbol, lambda _:
                    f"Symbol_unwrap(env, {expr}, context_native)"
            ),
            (
                ct.ASTNodeType, lambda t:
                    f"PointerWrapper_unwrap(env, {expr})"
                    if t == T.root_node else
                    f"Entity_unwrap(env, {expr})"
            ),
            (
                T.EnvRebindings, lambda _:
                    f"PointerWrapper_unwrap(env, {expr})"
            ),
            (
                object, lambda t:
                    f"{self.java_jni_type(t)}_unwrap(env, {expr})"
            ),
        ])