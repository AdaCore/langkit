from __future__ import annotations

import re
from typing import TYPE_CHECKING

from langkit.c_api import CAPISettings
import langkit.compiled_types as ct
from langkit.compiled_types import ArrayType, CompiledType, IteratorType, T
from langkit.diagnostics import Location, diagnostic_context, error
from langkit.language_api import AbstractAPISettings


if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx


pep440_public_version_identifier_re = re.compile(
    # Epoch segment
    r"(\d+!)?"
    # Release segment
    r"\d+(\.\d+)*"
    # Pre-release segment
    r"((a|b|rc)\d+)?"
    # Post-release segment
    r"(.post\d+)?"
    # Development release segment
    r"(.dev\d+)?"
)
"""
Regular expression that matches PEP440-compliant version numbers.
"""

pep440_local_version_identifier_re = re.compile("^[a-zA-Z0-9.]*$")
"""
Regular expression that matches PEP440-compliant local version identifiers.
"""


class PythonAPISettings(AbstractAPISettings):
    """Container for Python API generation settings."""

    name = "python"

    def __init__(self, ctx: CompileCtx, c_api_settings: CAPISettings) -> None:
        self.context = ctx
        self.c_api_settings = c_api_settings

        # The version numbers for Python packages are now required to follow
        # PEP 440. First check that "ctx.version" has a PEP 440-compliant
        # prefix (use a default version number if none is given).
        version = ctx.config.library.version
        if version == "undefined":
            version = "0.1"
        m = pep440_public_version_identifier_re.match(version)
        if m is None:
            with diagnostic_context(Location.nowhere):
                error(
                    f"Version number does not comply with PEP 440: {version}"
                )

        # Decompose "version" into the PEP 440-compliant part, and put the rest
        # of it plus the build date to the "local version identifier".
        pep440_version = m.group(0)
        local_version_identifier = version[m.end(0) :]
        build_date = ctx.config.library.build_date
        if build_date != "undefined":
            local_version_identifier = (
                f"{local_version_identifier}.{build_date}"
                if local_version_identifier
                else build_date
            )

        m = pep440_local_version_identifier_re.match(local_version_identifier)
        if m is None:
            with diagnostic_context(Location.nowhere):
                error(
                    "Local version identifier does not comply with PEP 440:"
                    f" {local_version_identifier}"
                )
        if local_version_identifier:
            pep440_version += f"+{local_version_identifier}"
        self.pep440_version = pep440_version

    @property
    def root_astnode_name(self) -> str:
        """
        Shortcut for ``self.type_public_name(T.root_node)``. Useful in
        templates.
        """
        return self.type_public_name(T.root_node)

    @property
    def module_name(self) -> str:
        return self.context.lib_name.lower

    def wrap_value(
        self, value: str, t: CompiledType, from_field_access: bool = False
    ) -> str:
        """
        Given an expression for a low-level value and the associated type,
        return an other expression that yields the corresponding high-level
        value.

        :param value: Expression yielding a low-level value.
        :param t: Type corresponding to the "value" expression.
        :param from_field_access: True if "value" is a record field or array
            item access (False by default). This is a special case because of
            the way ctypes works.
        """
        value_suffix = "" if from_field_access else ".value"
        match t:
            case T.AnalysisUnit:
                return f"AnalysisUnit._wrap({value})"
            case ct.EnumType():
                return f"{t.py_helper}._wrap({value})"
            case ct.ASTNodeType():
                return (
                    f"{self.type_public_name(ct.T.root_node)}"
                    f"._wrap_bare_node({value})"
                )
            case ct.EntityType():
                return (
                    f"{self.type_public_name(ct.T.root_node)}._wrap({value})"
                )
            case T.Token:
                return f"Token._wrap({value})"
            case T.SourceLocation:
                return f"{value}._wrap()"
            case T.Symbol:
                return f"_symbol_type.wrap({value})"
            case T.Bool:
                return f"bool({value}{value_suffix})"
            case T.Int:
                return f"{value}{value_suffix}"
            case T.Character:
                return f"chr({value}{value_suffix})"
            case T.String:
                return f"_String.wrap({value})"
            case ct.ArrayType():
                return (
                    f"{self.array_wrapper(t)}"
                    f".wrap({value}, {from_field_access})"
                )
            case ct.IteratorType():
                return f"{self.iterator_wrapper(t)}._wrap({value})"
            case ct.StructType():
                return f"{self.type_public_name(t)}._wrap({value})"
            case T.BigInt:
                return f"_big_integer.wrap({value})"
            case _:
                raise AssertionError(
                    "Unhandled field type in the Python binding (wrap_value):"
                    f" {t!r}"
                )

    def unwrap_value(self, value: str, t: CompiledType, context: str) -> str:
        """
        Given an expression for a high-level value and the associated type,
        return an other expression that yields the corresponding low-level
        value.

        Note that because of the way we handle resource (de)allocation, for
        some types, this does *not* yield a value that can be passed to C
        functions: for instance, arrays will yield an instance of a _BaseArray
        subclass. In order to get the C value, use the ``extract_c_value``
        method::

           py_value_expr = ...
           c_holder_expr = pyapi.unwrap_value(py_value_expr, my_type, context)
           c_value_expr = pyapi.extract_c_value(
               c_holder_expr, my_type, for_arg=True,
           )

        :param value: Expression yielding a high-level value.
        :param t: Type corresponding to the "value" expression.
        :param context: Expression to return a C value for the context.  This
            is required to unwrap some types of value.
        """
        context_arg = (
            ", {}".format(context) if t.conversion_requires_context else ""
        )
        match t:
            case T.AnalysisUnit:
                return f"AnalysisUnit._unwrap({value})"
            case ct.EnumType():
                return f"{t.py_helper}._unwrap({value})"
            case ct.ASTNodeType():
                return f"{value}._node_c_value"
            case ct.EntityType():
                return f"{self.type_public_name(t)}._unwrap({value})"
            case T.Bool:
                return f"bool({value})"
            case T.Int:
                return f"int({value})"
            case T.Character:
                return f"ord({value})"
            case T.String:
                return f"_String.unwrap({value})"
            case ct.ArrayType():
                return f"{self.array_wrapper(t)}.unwrap({value}{context_arg})"
            case ct.IteratorType():
                return f"{self.iterator_wrapper(t)}.unwrap({value})"
            case ct.StructType():
                return (
                    f"{self.type_public_name(t)}._unwrap({value}{context_arg})"
                )
            case T.Token:
                return f"Token._unwrap({value})"
            case T.SourceLocation:
                return f"Sloc._c_type._unwrap({value})"
            case T.Symbol:
                return f"_symbol_type.unwrap({value}{context_arg})"
            case T.BigInt:
                return f"_big_integer.unwrap({value})"
            case _:
                raise AssertionError(
                    "Unhandled field type in the Python binding"
                    f" (unwrap_value): {t!r}"
                )

    def extract_c_value(
        self,
        value: str,
        type: CompiledType,
        for_arg: bool,
    ) -> str:
        """
        See ``unwrap_value``.

        :param value: Expression that yields the wrapped value to extract.
        :param type: Type for this expression.
        :param for_arg: Whether the extracted value is to be passed as an
            argument. Depending on the type, arguments may need to be passed as
            references.
        """
        if type.is_token_type:
            # Token._unwrap() returns the C struct, and these are passed by
            # reference in the C API, so we just have to pass properties a
            # reference to it.
            return f"ctypes.byref({value})" if for_arg else value

        elif type.is_struct_type and not type.is_entity_type:
            # _BaseStruct._unwrap() returns a Holder instance that wraps the C
            # struct.
            result = f"{value}.c_value"

            # Such struct must be passed to properties by reference
            return f"ctypes.pointer({result})" if for_arg else result

        elif type.is_iterator_type or not type.is_refcounted:
            # There is no particular processing to do for iterators and all
            # remaining non-reference counted types.
            return value

        else:
            # All remaining ref-counted types are translated to fully native
            # Python objects. For them, we need to create a wrapper that owns
            # the C value during the call to a property. Such wrappers always
            # have a "c_value" attribute to get the actual value to pass to C
            # APIs.
            return f"{value}.c_value"

    def c_type(self, t: CompiledType) -> str:
        """
        Return the name of the type to use in the C API for ``t``.

        :param t: The type for which we want to get the C type name.
        """

        def ctype_type(name: str) -> str:
            return "ctypes.{}".format(name)

        match t:
            case T.Bool:
                return ctype_type("c_uint8")
            case T.Int:
                return ctype_type("c_int")
            case T.EnvRebindings:
                return "_EnvRebindings_c_type"
            case T.Token:
                return "Token._c_struct"
            case T.SourceLocation:
                return "Sloc._c_type"
            case T.Symbol:
                return "_symbol_type"
            case T.AnalysisUnit:
                return "AnalysisUnit._c_type"
            case ct.EnumType():
                return ctype_type("c_int")
            case ct.ASTNodeType():
                return f"{self.type_public_name(ct.T.root_node)}._node_c_type"
            case T.Character:
                return ctype_type("c_uint32")
            case T.String:
                return "_String.c_type"
            case ct.ArrayType():
                return f"{self.array_wrapper(t)}.c_type"
            case ct.IteratorType():
                return f"{self.iterator_wrapper(t)}._c_type"
            case T.EntityInfo:
                return "_EntityInfo_c_type"
            case T.env_md:
                return "_Metadata_c_type"
            case ct.EntityType():
                return "_Entity_c_type"
            case ct.StructType():
                return f"{self.type_public_name(t)}._c_type"
            case T.BigInt:
                return "_big_integer.c_type"
            case _:
                raise AssertionError(
                    "Unhandled field type in the Python binding (c_type):"
                    f" {t!r}"
                )

    def array_wrapper(self, array_type: ArrayType) -> str:
        return (
            ct.T.entity.array
            if array_type.element_type.is_entity_type
            else array_type
        ).py_converter

    def iterator_wrapper(self, iterator_type: IteratorType) -> str:
        return (
            ct.T.entity.iterator
            if iterator_type.element_type.is_entity_type
            else iterator_type
        ).api_name.camel

    def type_public_name(self, t: CompiledType) -> str:
        """
        Python specific helper. Return the public API name for a given
        CompiledType instance.

        :param t: The type for which we want to get the name.
        """
        match t:
            case T.Bool:
                return "bool"
            case T.Int | T.BigInt:
                return "int"
            case T.SourceLocation:
                return "Sloc"
            case T.Token:
                return "Token"
            case T.Character | T.String | T.Symbol | ct.EnumType():
                return "str"
            case ct.ASTNodeType():
                return self.type_public_name(t.entity)
            case ct.EntityType():
                return t.astnode.kwless_raw_name.camel
            case T.AnalysisUnit:
                return t.api_name
            case ct.ArrayType():
                elt_type = self.type_public_name(t.element_type)
                return f"List[{elt_type}]"
            case ct.IteratorType() | ct.StructType():
                return t.api_name.camel
            case _:
                raise AssertionError(
                    "Unhandled field type in the Python binding"
                    f" (type_public_name): {t!r}"
                )
