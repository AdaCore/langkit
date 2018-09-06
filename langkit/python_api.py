from __future__ import absolute_import, division, print_function

import langkit.compiled_types as ct
from langkit.compiled_types import T
from langkit.language_api import AbstractAPISettings
from langkit.utils import dispatch_on_type


class PythonAPISettings(AbstractAPISettings):
    """Container for Python API generation settings."""

    name = 'python'

    def __init__(self, ctx, c_api_settings):
        self.context = ctx
        self.c_api_settings = c_api_settings

    @property
    def module_name(self):
        return self.context.lib_name.lower

    def wrap_value(self, value, type, from_field_access=False):
        """
        Given an expression for a low-level value and the associated type,
        return an other expression that yields the corresponding high-level
        value.

        :param str value: Expression yielding a low-level value.
        :param ct.CompiledType type: Type corresponding to the "value"
            expression.
        :param bool from_field_access: True if "value" is a record field or
            array item access (False by default). This is a special case
            because of the way ctypes works.
        :rtype: str
        """
        value_suffix = '' if from_field_access else '.value'
        return dispatch_on_type(type, [
            (T.AnalysisUnit, lambda _: 'AnalysisUnit._wrap({})'),
            (ct.EnumType, lambda _: '{}._wrap({{}})'.format(type.py_helper)),
            (ct.ASTNodeType, lambda _: '{}._wrap_bare_node({{}})'.format(
                self.type_public_name(ct.T.root_node))),
            (ct.EntityType, lambda _: '{}._wrap({{}})'.format(
                self.type_public_name(ct.T.root_node))),
            (T.Token, lambda _: '{}'),
            (T.Symbol, lambda _: '_symbol_type.wrap({})'),
            (T.Bool, lambda _: 'bool({{}}{})'.format(value_suffix)),
            (T.Int, lambda _: '{{}}{}'.format(value_suffix)),
            (T.Character, lambda _: 'unichr({{}}{})'.format(value_suffix)),
            (ct.ArrayType, lambda _: '{}.wrap({{}})'.format(
                self.array_wrapper(type)
            )),
            (ct.StructType, lambda _: '{}._wrap({{}})'.format(
                self.type_public_name(type))),
            (T.BigInt, lambda _: '_big_integer.wrap({})'),
        ], exception=TypeError(
            'Unhandled field type in the python binding'
            ' (wrapping): {}'.format(type)
        )).format(value)

    def unwrap_value(self, value, type, context):
        """
        Given an expression for a high-level value and the associated type,
        return an other expression that yields the corresponding low-level
        value.

        Note that because of the way we handle resource (de)allocation, for
        some types, this does *not* yield a value that can be passed to C
        functions: for instance, arrays will yield an instance of a _BaseArray
        subclass. In order to get the C value, use the ``extract_c_value``
        method:

        >>> py_value_expr = ...
        >>> c_holder_expr = pyapi.unwrap_value(py_value_expr, my_type, context)
        >>> c_value_expr = pyapi.extract_c_value(c_holder_expr, my_type)

        :param str value: Expression yielding a high-level value.
        :param ct.CompiledType type: Type corresponding to the "value"
            expression.
        :param str context: Expression to return a C value for the context.
            This is required to unwrap some types of value.
        :rtype: str
        """
        context_arg = (', {}'.format(context)
                       if type.conversion_requires_context else '')
        return dispatch_on_type(type, [
            (T.AnalysisUnit, lambda _: 'AnalysisUnit._unwrap({value})'),
            (ct.EnumType, lambda _:
                '{}._unwrap({{value}})'.format(type.py_helper)),
            (ct.ASTNodeType, lambda _: '{value}._node_c_value'),
            (ct.EntityType, lambda _: '{}._unwrap({{value}})'.format(
                self.type_public_name(ct.T.root_node))),
            (T.Bool, lambda _: 'bool({value})'),
            (T.Int, lambda _: 'int({value})'),
            (T.Character, lambda _: 'ord({value})'),
            (ct.ArrayType, lambda cls:
                '{}.unwrap({{value}}{{context}})'
                .format(self.array_wrapper(cls))),
            (ct.StructType, lambda _:
                '{}._unwrap({{value}}{{context}})'
                .format(self.type_public_name(type))),
            (T.Symbol, lambda _: '_symbol_type.unwrap({value}{context})'),
            (T.BigInt, lambda _: '_big_integer.unwrap({value})'),
        ], exception=TypeError(
            'Unhandled field type in the python binding'
            ' (unwrapping): {}'.format(type)
        )).format(value=value, context=context_arg)

    def extract_c_value(self, value, type):
        """
        See ``unwrap_value``.
        """
        return '{}.c_value'.format(value) if type.is_refcounted else value

    def c_type(self, type):
        """
        Return the name of the type to use in the C API for ``type``.

        :param CompiledType type: The type for which we want to get the C type
            name.
        :rtype: str
        """
        def ctype_type(name):
            return 'ctypes.{}'.format(name)

        return dispatch_on_type(type, [
            (T.Bool, lambda _: ctype_type('c_uint8')),
            (T.Int, lambda _: ctype_type('c_int')),
            (T.Character, lambda _: ctype_type('c_uint32')),
            (T.EnvRebindings, lambda _: '_EnvRebindings_c_type'),
            (T.Token, lambda _: 'Token'),
            (T.Symbol, lambda _: '_symbol_type'),
            (T.AnalysisUnit, lambda _: 'AnalysisUnit._c_type'),
            (ct.EnumType, lambda _: ctype_type('c_int')),
            (ct.ASTNodeType, lambda _: '{}._node_c_type'.format(
                self.type_public_name(ct.T.root_node))),
            (ct.ArrayType, lambda cls:
                '{}.c_type'.format(self.array_wrapper(cls))),
            (T.entity_info, lambda _: '_EntityInfo_c_type'),
            (T.env_md, lambda _: '_Metadata_c_type'),
            (ct.EntityType, lambda _: '_Entity_c_type'),
            (ct.StructType, lambda _:
                '{}._c_type'.format(self.type_public_name(type))),
            (T.BigInt, lambda _: '_big_integer.c_type'),
        ])

    def array_wrapper(self, array_type):
        return (ct.T.entity.array
                if array_type.element_type.is_entity_type else
                array_type).py_converter

    def type_public_name(self, type):
        """
        Python specific helper. Return the public API name for a given
        CompiledType instance.

        :param CompiledType type: The type for which we want to get the name.
        :rtype: str
        """
        return dispatch_on_type(type, [
            (T.Bool, lambda _: 'bool'),
            (T.Int, lambda _: 'int'),
            (T.Character, lambda _: 'unicode'),
            (T.Token, lambda _: 'Token'),
            (T.Symbol, lambda _: 'unicode'),
            (ct.EnumType, lambda _: 'str'),
            (ct.ASTNodeType, lambda t: self.type_public_name(t.entity)),
            (ct.EntityType, lambda t: t.astnode.kwless_raw_name.camel),
            (T.AnalysisUnit, lambda t: t.api_name),
            (ct.ArrayType, lambda _: 'list[{}]'.format(
                type.element_type.name.camel
            )),
            (ct.StructType, lambda _: type.api_name.camel),
            (T.BigInt, lambda _: 'int'),
        ])
