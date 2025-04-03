from __future__ import annotations
from dataclasses import dataclass

from typing import TYPE_CHECKING

from langkit.c_api import CAPISettings
import langkit.compiled_types as ct
from langkit.compiled_types import BaseField, CompiledType, T
from langkit.generic_interface import (
    ArrayInterface,
    BaseGenericInterface,
    GenericArgument,
    GenericInterface,
    InterfaceMethodProfile,
)
from langkit.language_api import AbstractAPISettings
from langkit.names import Name


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
    return "".join((res[:1].lower(), res[1:]))


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

    implements: InterfaceMethodProfile | None = None
    """
    Generic field implemented.
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
        return "_".join(full_path)

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

    name = "java"
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
    excluded_fields = ["unit"]

    def __init__(self, ctx: CompileCtx, c_api_settings: CAPISettings):
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
            return (
                struct_type is T.EntityInfo
                or struct_type is T.env_md
                or struct_type.exposed
            )

    def get_struct_fields(self, struct: ct.StructType) -> list[StructField]:
        """
        Get the structure fields as a list.

        :param struct: The structure type to get the fields from.
        """
        # Compute the structure fields
        res = []
        for field in struct.get_fields():
            assert field.names.api is not None
            res.append(
                StructField(
                    field.names.api.lower,
                    field.type,
                    fields=(
                        self.get_struct_fields(field.type)
                        if isinstance(field.type, ct.StructType)
                        else None
                    ),
                    implements=field.implements,
                )
            )
        return res

    def flatten_struct_fields(
        self, to_flatten: list[StructField], base: list[str] | None = None
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
                res.append(
                    FlatStructField(field.lower_name, field.public_type, base)
                )
            elif field.fields:
                res.extend(
                    self.flatten_struct_fields(
                        field.fields, base + [field.lower_name]
                    )
                )
            else:
                res.append(
                    FlatStructField("dummy", T.Bool, base + [field.lower_name])
                )
        return res

    def get_node_formatted_fields(self, cls: ct.ASTNodeType) -> list[str]:
        """
        Get the list of Java formatted field names for the given node type.

        :param cls: The node type to get the fields and properties from.
        """
        res = (
            []
            if cls.base is None
            else self.get_node_formatted_fields(cls.base)
        )
        res.extend(
            field.api_name.lower
            for field in cls.get_parse_fields(
                predicate=lambda f: not f.abstract and not f.null,
                include_inherited=False,
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
            params.append(
                JavaParam(arg.name.lower, arg.public_type, default_value_expr)
            )

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

    def extend_release_list(
        self, release_list: list[ToRelease], the_type: CompiledType, var: str
    ) -> None:
        """
        Extends the given release list according to the variable type.

        :param release_list: The release list to extend.
        :param the_type: The type of the value.
        :param var: The variable to eventually release.
        """
        match the_type:
            case T.BigInt | T.String | ct.ArrayType():
                release_list.append(ToRelease(var, the_type))
            case ct.StructType():
                if the_type.is_refcounted:
                    release_list.append(ToRelease(var, the_type))

    def mangle_enum(self, name: str) -> str:
        """
        Mangle the given enum name to avoid reserved names.

        :param name: The name to mangle.
        """
        return name if name != "NONE" else "NONE_ENUM"

    # ----- Typing methods -----

    @classmethod
    def wrapping_type(
        cls,
        the_type: CompiledType,
        ast_wrapping: bool = True,
        java_wrapping: bool = False,
    ) -> str:
        """
        Get the type name for the Java nomination.

        :param the_type: The type you want the name from.
        :param ast_wrapping: If the function should wrap the AST types.
        :param java_wrapping: Whether to use Java wrapping classes for
            primitive types.
        """
        match the_type:
            case T.Bool:
                return "Boolean" if java_wrapping else "boolean"
            case T.Int:
                return "Integer" if java_wrapping else "int"
            case T.Char:
                return "Char"
            case T.BigInt:
                return "BigInteger"
            case ct.EnumType():
                return the_type.api_name.camel
            case T.Symbol:
                return "Symbol"
            case T.String:
                return "String"
            case T.SourceLocation:
                return "SourceLocation"
            case T.SourceLocationRange:
                return "SourceLocationRange"
            case T.EnvRebindings:
                return "PointerWrapper"
            case T.Token:
                return "Token"
            case T.AnalysisUnit:
                return "AnalysisUnit"
            case ct.ASTNodeType():
                return (
                    cls.wrapping_type(the_type.entity)
                    if ast_wrapping
                    else "PointerWrapper"
                )
            case ct.EntityType():
                return (
                    the_type.astnode.kwless_raw_name.camel
                    if ast_wrapping
                    else "Entity"
                )
            case ct.ArrayType():
                return f"{cls.wrapping_type(the_type.element_type)}[]"
            case ct.IteratorType():
                return cls.iterator_wrapping_type(the_type)
            case _:
                return the_type.api_name.camel

    @classmethod
    def iterator_wrapping_type(cls, iterator_type: ct.IteratorType) -> str:
        """
        Get the Java type corresponding to the given compiled iterator type.
        """
        return (
            ct.T.entity.iterator
            if iterator_type.element_type.is_entity_type
            else iterator_type
        ).api_name.camel

    def wrapper_class(
        self, the_type: CompiledType, ast_wrapping: bool = True
    ) -> str:
        """
        Return the name of the class which contains the wrapping operation for
        the given type.

        :param the_type: Type to get the class for.
        :param ast_wrapping: If the AST node should be wrapped in their Java
            class.
        """
        match the_type:
            case T.Bool:
                return "BooleanWrapper"
            case T.Int:
                return "IntegerWrapper"
            case T.BigInt:
                return "BigIntegerWrapper"
            case T.String:
                return "StringWrapper"
            case ct.ArrayType():
                return f"{the_type.api_name.camel}Wrapper"
            case _:
                return self.wrapping_type(the_type, ast_wrapping)

    def none_value(
        self, the_type: CompiledType, ast_wrapping: bool = True
    ) -> str:
        """
        Return the none (default) value Java expression for the given type.

        :param the_type: The type of the expression to get the none for.
        :param ast_wrapping: If the AST node should be wrapped.
        """
        match the_type:
            case T.Bool:
                return "false"
            case T.Int:
                return "0"
            case T.EnvRebindings:
                return "PointerWrapper.nullPointer()"
            case T.Token:
                return "Token.NONE(currentUnit)"
            case ct.ASTNodeType():
                return (
                    self.none_value(the_type.entity, ast_wrapping)
                    if ast_wrapping
                    else "PointerWrapper.nullPointer()"
                )
            case ct.EntityType():
                return (
                    f"{the_type.astnode.kwless_raw_name.camel}.NONE"
                    if ast_wrapping
                    else "Entity.NONE"
                )
            case _:
                return f"{self.wrapper_class(the_type, ast_wrapping)}.NONE"

    def is_java_primitive(self, the_type: CompiledType) -> bool:
        """
        Get whether the given type maps to a primitive Java type.

        :param the_type: The type.
        """
        return the_type in (T.Bool, T.Int)

    # ----- Native-Image methods -----

    def ni_type(
        self, the_type: CompiledType, ast_wrapping: bool = True
    ) -> str:
        """
        Get the Java type that represents the given type for the Graal C API.

        :param the_type: The type you want the name from.
        :param ast_wrapping: If the AST node should be wrapped.
        """
        match the_type:
            case T.Bool:
                return "byte"
            case T.Int | T.Char | ct.EnumType():
                return "int"
            case T.BigInt:
                return "BigIntegerNative"
            case T.Symbol:
                return "SymbolNative"
            case T.String:
                return "StringNative"
            case T.SourceLocation:
                return "SourceLocationNative"
            case T.SourceLocationRange:
                return "SourceLocationRangeNative"
            case T.EnvRebindings | ct.ASTNodeType():
                return "Pointer"
            case T.Token:
                return "TokenNative"
            case T.AnalysisUnit:
                return "AnalysisUnitNative"
            case T.EntityInfo:
                return "EntityInfoNative"
            case T.env_md:
                return "MetadataNative"
            case ct.EntityType():
                return "EntityNative"
            case ct.ArrayType():
                return (
                    f"{the_type.api_name.camel}Native"
                    if (
                        ast_wrapping
                        or not the_type.element_type.is_entity_type
                    )
                    else f"{ct.T.entity.array.api_name.camel}Native"
                )
            case ct.IteratorType():
                return f"{self.iterator_wrapping_type(the_type)}Native"
            case _:
                return f"{the_type.api_name.camel}Native"

    def ni_reference_type(self, the_type: CompiledType) -> str:
        """
        Get the Java type that represents a reference to the given type for
        the Graal C API.

        :param the_type: The type to get the reference type for.
        """
        match the_type:
            case T.Bool:
                return "CCharPointer"
            case T.Int | T.Char | ct.EnumType():
                return "CIntPointer"
            case (
                T.BigInt
                | T.String
                | T.AnalysisUnit
                | ct.ArrayType()
                | ct.IteratorType()
            ):
                return "WordPointer"
            case _:
                return self.ni_type(the_type)

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

    def ni_wrap(
        self,
        the_type: CompiledType,
        source: str,
        release_list: list[ToRelease],
        ast_wrapping: bool = True,
    ) -> str:
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
        match the_type:
            case ct.EnumType():
                return f"{self.wrapper_class(the_type)}.fromC({source})"
            case T.Token:
                return f"Token.wrap({source}, currentUnit)"
            case ct.ASTNodeType():
                return (
                    (
                        f"{self.wrapper_class(the_type)}.fromEntity"
                        f"(Entity.wrapBareNode({source}))"
                    )
                    if ast_wrapping
                    else f"PointerWrapper.wrap({source})"
                )
            case ct.EntityType():
                return (
                    (
                        f"{self.wrapper_class(the_type)}.fromEntity"
                        f"(Entity.wrap({source}))"
                    )
                    if ast_wrapping
                    else f"Entity.wrap({source})"
                )
            case _:
                return f"{self.wrapper_class(the_type)}.wrap({source})"

    def ni_unwrap(
        self,
        the_type: CompiledType,
        source: str,
        export: str,
        release_list: list[ToRelease],
    ) -> str:
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
        ni_type = self.ni_type(the_type, ast_wrapping=False)

        res = f"{ni_type} {export} = "

        match the_type:
            case T.Bool:
                res += f"({source} ? (byte) 1 : (byte) 0);"
            case T.Int:
                res += f"{source};"
            case T.Char:
                res += f"{source}.value;"
            case T.BigInt:
                res += (
                    f"StackValue.get(SizeOf.get(WordPointer.class));"
                    f"BigIntegerWrapper.unwrap({source},"
                    f"(WordPointer) {export});"
                )
            case ct.EnumType():
                res += f"{source}.toC();"
            case T.Symbol:
                res += (
                    f"StackValue.get(SymbolNative.class);"
                    f"{source}.unwrap({export}, currentContext);"
                )
            case T.String:
                res += f"StringWrapper.unwrap({source});"
            case ct.ASTNodeType():
                res += self.ni_unwrap(
                    the_type.entity, source, export, release_list
                )
            case ct.EntityType():
                res += (
                    "StackValue.get(EntityNative.class);"
                    f"{source}.entity.unwrap({export});"
                )
            case ct.ArrayType():
                res += (
                    f"{self.wrapper_class(the_type)}"
                    f".unwrap({source}, currentContext);"
                    if the_type.element_type.is_symbol_type
                    else f"{self.wrapper_class(the_type)}.unwrap({source});"
                )
            case _:
                res += self.object_unwrap(the_type, source, export)

        return res

    def object_unwrap(
        self, the_type: CompiledType, source: str, export: str
    ) -> str:
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

    def ni_write(
        self, the_type: CompiledType, source: str, pointer: str
    ) -> str:
        """
        Return the Java statement to write the source Java expression in the
        target Graal C API pointer.

        :param the_type: The type of the expression to write.
        :param source: The source Java expression to write.
        :param pointer: The pointer to write in.
        """

        match the_type:
            case T.Bool:
                return f"{pointer}.write({source} ? (byte) 1 : (byte) 0);"
            case T.Int:
                return f"{pointer}.write({source});"
            case T.Char:
                return f"{pointer}.write({source}.value);"
            case T.Symbol:
                return f"{source}.unwrap({pointer}, currentContext);"
            case T.BigInt:
                return f"BigIntegerWrapper.unwrap({source}, {pointer});"
            case ct.EnumType():
                return f"{pointer}.write({source}.toC());"
            case T.String:
                return f"StringWrapper.unwrap({source}, {pointer});"
            case T.EnvRebindings:
                return f"{pointer}.writeWord(0, {source}.ni());"
            case T.AnalysisUnit | ct.IteratorType():
                return f"{pointer}.write({source}.unwrap());"
            case ct.ArrayType():
                return (
                    f"{self.wrapper_class(the_type)}"
                    f".unwrap({source}, {pointer}, currentContext);"
                    if the_type.element_type.is_symbol_type
                    else f"{self.wrapper_class(the_type)}"
                    f".unwrap({source}, {pointer});"
                )
            case ct.ASTNodeType():
                return f"{pointer}.writeWord(0, {source}.ni());"
            case _:
                return f"{source}.unwrap({pointer});"

    def ni_field_wrap(
        self,
        field: StructField,
        base: list[str] | None = None,
        ast_wrapping: bool = True,
    ) -> str:
        """
        Get the wrapping statement for the given structure field.

        :param field: The field to wrap.
        :param base: The base of the current fields.
        :param ast_wrapping: Whether to wrap node types in their Java classes.
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
                ast_wrapping=ast_wrapping,
            )

        # Else, it the field has sub-fields, this is a composite value
        # (struct), we call the wrapping operation for the field type with
        # the native address of the field.
        elif field.fields:
            return self.ni_wrap(
                field.public_type,
                f"structNative.address_{field_name}()",
                [],
                ast_wrapping=ast_wrapping,
            )

        # Else, the field is a struct without any fields, we just return its
        # `NONE` singleton.
        else:
            return f"{field_type}.NONE"

    def ni_field_unwrap(
        self, field: StructField, ast_wrapping: bool = True
    ) -> str:
        """
        Unwrap the given structure field and write the result in the assumed
        initialized 'structNative' variable.

        :param field: The field to unwrap.
        :param ast_wrapping: Whether node types have been wrapped in their
            Java classes.
        """
        to_write = f"{field.name}Native"
        getter = (
            (
                f"this.{field.name}.entity.node"
                if ast_wrapping
                else f"this.{field.name}"
            )
            if isinstance(field.public_type, ct.ASTNodeType)
            else f"this.{field.name}"
        )
        res = (
            f"{self.ni_reference_type(field.public_type)} {to_write} = "
            f"structNative.address_{field.lower_name}();"
        )

        return res + self.ni_write(field.public_type, getter, to_write)

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
            self.c_api_settings.get_name(name) if do_nat else name
        ).replace("_", "_1")

        # Return the mangled function name
        return "_".join(
            [
                # Everything belongs to the Java com.adacore package
                "Java_com_adacore",
                # The library specific package ("libfoolang")
                self.context.lib_name.lower,
                # The top-level Java class ("Libfoolang")
                self.context.lib_name.camel,
                # The function belongs to the JNI_LIB class
                "00024JNI_1LIB",
                # The function name
                func_base_name,
            ]
        )

    def jni_func_sig(
        self, name: str, return_type: str, do_nat: bool = True
    ) -> str:
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
        match the_type:
            case ct.EntityType():
                return f"{self.wrapping_type(the_type)}.fromEntity({expr})"
            case ct.ASTNodeType():
                return self.java_jni_wrap(the_type.entity, expr)
            case _:
                return expr

    def java_jni_unwrap(self, the_type: CompiledType, expr: str) -> str:
        """
        Get the Java expression that unwrap the given expression.

        :param the_type: The type of the expression to unwrap.
        :param expr: The expression to unwrap.
        """
        match the_type:
            case ct.EntityType():
                return f"({expr} != null ? {expr}.entity : null)"
            case ct.ASTNodeType() as t:
                return self.java_jni_unwrap(t.entity, expr)
            case _:
                return expr

    def jni_c_type(self, the_type: CompiledType) -> str:
        """
        Give the JNI c type for the given compiled type.

        :param the_type: The type.
        """
        match the_type:
            case T.Bool:
                return "jboolean"
            case T.Int:
                return "jint"
            case _:
                return "jobject"

    def jni_c_none(self, the_type: CompiledType) -> str:
        """
        Give the C none expression for the given compiled type.

        :param the_type: The type to get the none expression for.
        """
        match the_type:
            case T.Bool | T.Int:
                return "0"
            case _:
                return "NULL"

    def jni_sig_type(
        self,
        the_type: CompiledType,
        base_class: str,
        ast_wrapping: bool = True,
    ) -> str:
        """
        Get the type representation for the JNI signature definition.

        :param the_type: The original compiled type.
        :param base_class: The parent class of all Java classes.
        :param ast_wrapping: Whether to wrap node types in their Java classes.
        """
        match the_type:
            case T.Bool:
                return "Z"
            case T.Int:
                return "I"
            case T.BigInt:
                return "Ljava/math/BigInteger;"
            case T.String:
                return "Ljava/lang/String;"
            case ct.ArrayType() as t:
                return f"[L{base_class}${self.wrapping_type(t.element_type)};"
            case t:
                return (
                    f"L{base_class}$"
                    f"{self.wrapping_type(t, ast_wrapping=ast_wrapping)};"
                )

    def jni_field_access(self, the_type: CompiledType) -> str:
        """
        Return the JNI field accessor function for the given type.

        :param the_type: The type.
        """
        match the_type:
            case T.Bool:
                return "GetBooleanField"
            case T.Int:
                return "GetIntField"
            case _:
                return "GetObjectField"

    def jni_new_array(self, the_type: CompiledType) -> str:
        """
        Get the JNI function to create a new JNI array of the given type.

        :param the_type: The type to get the array creation function for.
        """
        match the_type.element_type:
            case T.Bool:
                return "NewBooleanArray"
            case T.Int:
                return "NewIntArray"
            case _:
                return "NewObjectArray"

    def jni_array_access(self, the_type: CompiledType) -> str:
        """
        Get the JNI function to access array elements in an array of the given
        type.

        :param the_type: The type to get the access function for.
        """
        match the_type.element_type:
            case T.Bool:
                return "GetBooleanArrayRegion"
            case T.Int:
                return "GetIntArrayRegion"
            case _:
                return "GetObjectArrayElement"

    def jni_array_writing(self, the_type: CompiledType) -> str:
        """
        Get the JNI function to write array elements in an array of the given
        type.

        :param the_type: The type to get the writing function for.
        """
        match the_type.element_type:
            case T.Bool:
                return "SetBooleanArrayRegion"
            case T.Int:
                return "SetIntArrayRegion"
            case _:
                return "SetObjectArrayElement"

    def jni_new_value(self, the_type: CompiledType) -> str:
        """
        Get the C expression for a new value for the type.

        :param the_type: The type.
        """
        return (
            "0"
            if the_type in (T.Bool, T.Int)
            else f"{self.wrapper_class(the_type, False)}_new_value()"
        )

    def jni_wrap(
        self,
        the_type: CompiledType,
        expr: str,
        release_list: list[ToRelease],
        ast_wrapping: bool = True,
    ) -> str:
        """
        Get the C expression to wrap the given expression in a Java value.

        :param the_type: The type of the expression.
        :param expr: The name of the value to wrap.
        :param release_list: The list of the variable to release after
            wrapping.
        :param ast_wrapping: Whether to wrap node types in their Java classes.
        """
        # Extends the release list
        self.extend_release_list(release_list, the_type, expr)

        # Return the wrapping expression
        match the_type:
            case T.Bool:
                return f"(jboolean) {expr}"
            case T.Int:
                return f"(jint) {expr}"
            case T.Token:
                return (
                    f"Token_wrap(env, {expr}, "
                    "AnalysisUnit_wrap(env, unit_native))"
                )
            case ct.ASTNodeType():
                return (
                    (
                        f"node_from_entity(env, Entity_wrap_bare_node"
                        f"(env, {expr}))"
                    )
                    if ast_wrapping
                    else f"PointerWrapper_wrap(env, (void*) {expr})"
                )
            case ct.EntityType():
                return (
                    f"node_from_entity(env, Entity_wrap(env, {expr}))"
                    if ast_wrapping
                    else f"Entity_wrap(env, {expr})"
                )
            case t:
                return (
                    f"{self.wrapper_class(t, ast_wrapping=ast_wrapping)}"
                    f"_wrap(env, {expr})"
                )

    def jni_unwrap(
        self,
        the_type: CompiledType,
        expr: str,
        export: str,
        release_list: list[ToRelease],
        ast_wrapping: bool = True,
    ) -> str:
        """
        Get the C statement to unwrap the given expression in the given export
        name.

        :param the_type: The type.
        :param expr: The name of the value to unwrap.
        :param export: The variable to place the unwrapping result in.
        :param release_list: The list of the variable to release after the
            unwrapping.
        :param ast_wrapping: Whether node types have been wrapped in their
            Java classes.
        """
        # Extends the release list
        self.extend_release_list(release_list, the_type, export)

        # Return the unwrapping expression
        export_type = the_type.c_type(self.c_api_settings).name
        res = f"{export_type} {export} = "

        match the_type:
            case T.Bool:
                res += f"({export_type}) {expr};"
            case T.Int:
                res += f"({export_type}) {expr};"
            case T.Symbol:
                res += f"Symbol_unwrap(env, {expr}, context_native);"
            case ct.ArrayType():
                res += (
                    f"{self.wrapper_class(the_type)}"
                    f"_unwrap(env, {expr}, context_native);"
                    if the_type.element_type.is_symbol_type
                    else f"{self.wrapper_class(the_type)}_unwrap(env, {expr});"
                )
            case ct.ASTNodeType():
                res += (
                    f"Entity_unwrap(env, get_node_entity(env, {expr})).node;"
                    if ast_wrapping
                    else f"({export_type}) PointerWrapper_unwrap(env, {expr});"
                )
            case ct.EntityType():
                res += (
                    f"Entity_unwrap(env, get_node_entity(env, {expr}));"
                    if ast_wrapping
                    else f"Entity_unwrap(env, {expr});"
                )
            case t:
                res += (
                    f"{self.wrapper_class(t, ast_wrapping=ast_wrapping)}"
                    f"_unwrap(env, {expr});"
                )

        return res

    @classmethod
    def support_type_name(
        cls,
        t: BaseGenericInterface | CompiledType,
        prefix: bool = False,
    ) -> str:
        """
        Get the name of the Java type corresponding to ``t`` when generating
        the method prototypes from the generic interface. If prefix is True,
        add "LangkitSupport." as a prefix.
        """
        match t:
            case CompiledType():
                prefix_str = (
                    "LangkitSupport."
                    if prefix and t not in (T.Bool, T.Int, T.String)
                    else ""
                )
                return prefix_str + cls.wrapping_type(t)
            case GenericInterface():
                return (
                    f"LangkitSupport.{t.name.camel}"
                    if prefix
                    else t.name.camel
                )
            case ArrayInterface():
                return f"{cls.support_type_name(t.element_type, prefix)}[]"
            case _:
                raise AssertionError(
                    "Unhandled field type in the Java binding"
                    f" (support_type_name): {t!r}"
                )

    @classmethod
    def create_support_prototype_args(
        cls, args: list[GenericArgument], prefix: bool = False
    ) -> str:
        """
        Create a string for the Generic Langkit interfaces containing the
        arguments of a method.
        """
        return ",".join(
            [
                f"final {cls.support_type_name(a.type, prefix)} "
                f"{format_name(a.name.lower)}"
                for a in args
            ]
        )

    @classmethod
    def cast_arguments_from_interface(cls, params: list[JavaParam]) -> str:
        """
        Create a string to cast list of arguments from an interface method to
        their corresponding concrete types.
        """
        return ", ".join(
            [
                (
                    f"Arrays.copyOf({p.name}, {p.name}.length, "
                    f"{cls.wrapping_type(p.public_type)}.class)"
                    if p.public_type.is_array
                    else f"({cls.wrapping_type(p.public_type)}) {p.name}"
                )
                for p in params
            ]
        )

    @classmethod
    def support_interfaces(
        cls, interfaces: list[GenericInterface]
    ) -> list[str]:
        """
        Get a list of strings containing all interfaces from the generic API
        that `cls` should implement.
        """
        return [f"LangkitSupport.{i.name.camel}" for i in interfaces]
