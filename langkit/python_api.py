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

    def get_enum_alternative(self, type_name, alt_name, suffix):
        return alt_name.upper

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
            (T.AnalysisUnitType, lambda _: 'AnalysisUnit._wrap({})'),
            (T.AnalysisUnitKind, lambda _: '_unit_kind_to_str[{}]'),
            (ct.ASTNodeType, lambda _: '{}._wrap_bare_node({{}})'.format(
                self.type_public_name(ct.T.root_node))),
            (ct.EntityType, lambda _: '{}._wrap({{}})'.format(
                self.type_public_name(ct.T.root_node))),
            (T.TokenType, lambda _: '{}'),
            (T.SymbolType, lambda _: '_symbol_type.wrap({})'),
            (T.BoolType, lambda _: 'bool({{}}{})'.format(value_suffix)),
            (T.LongType, lambda _: '{{}}{}'.format(value_suffix)),
            (T.CharacterType, lambda _: 'unichr({{}}{})'.format(value_suffix)),
            (ct.ArrayType, lambda _: '{}._wrap({{}})'.format(
                self.array_wrapper(type)
            )),
            (ct.StructType, lambda _: '{}._wrap({{}})'.format(
                type.name.camel)),
            (T.EnvRebindingsType, lambda _: '{}'),
            (T.BigIntegerType, lambda _: '_big_integer._wrap({})'),
        ], exception=TypeError(
            'Unhandled field type in the python binding'
            ' (wrapping): {}'.format(type)
        )).format(value)

    def unwrap_value(self, value, type, context):
        """
        Given an expression for a high-level value and the associated type,
        return an other expression that yields the corresponding low-level
        value.

        :param str value: Expression yielding a high-level value.
        :param ct.CompiledType type: Type corresponding to the "value"
            expression.
        :param str context: Expression to return a C value for the context.
            This is required to unwrap some types of value.
        :rtype: str
        """
        context_arg = (', {}'.format(context)
                       if self.unwrap_requires_context(type) else '')
        return dispatch_on_type(type, [
            (T.AnalysisUnitType, lambda _: 'AnalysisUnit._unwrap({value})'),
            (T.AnalysisUnitKind, lambda _: '_unwrap_unit_kind({value})'),
            (ct.ASTNodeType, lambda _: '{value}'),
            (ct.ASTNodeType, lambda _: '{value}._node_c_value'),
            (ct.EntityType, lambda _: '{}._unwrap({{value}})'.format(
                self.type_public_name(ct.T.root_node))),
            (T.BoolType, lambda _: 'bool({value})'),
            (T.LongType, lambda _: 'int({value})'),
            (T.CharacterType, lambda _: 'ord({value})'),
            (ct.ArrayType, lambda cls:
                '{}._unwrap({{value}}{{context}})'
                .format(self.array_wrapper(cls))),
            (ct.StructType, lambda _:
                '{}._unwrap({{value}}{{context}})'.format(type.name.camel)),
            (T.SymbolType, lambda _: '_symbol_type.unwrap({value}{context})'),
            (T.EnvRebindingsType, lambda _: '{value}'),
            (T.BigIntegerType, lambda _: '_big_integer._unwrap({value})'),
        ], exception=TypeError(
            'Unhandled field type in the python binding'
            ' (unwrapping): {}'.format(type)
        )).format(value=value, context=context_arg)

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
            (T.BoolType, lambda _: ctype_type('c_uint8')),
            (T.LongType, lambda _: ctype_type('c_int')),
            (T.CharacterType, lambda _: ctype_type('c_uint32')),
            (T.EnvRebindingsType, lambda _: '_EnvRebindings_c_type'),
            (T.TokenType, lambda _: 'Token'),
            (T.SymbolType, lambda _: '_symbol_type'),
            (T.AnalysisUnitType, lambda _: 'AnalysisUnit._c_type'),
            (T.AnalysisUnitKind, lambda _: ctype_type('c_uint')),
            (ct.ASTNodeType, lambda _: '{}._node_c_type'.format(
                self.type_public_name(ct.T.root_node))),
            (ct.ArrayType, lambda cls:
                '{}._c_type'.format(self.array_wrapper(cls))),
            (T.entity_info, lambda _: '_EntityInfo_c_type'),
            (ct.EntityType, lambda _: '_Entity_c_type'),
            (ct.StructType, lambda _:
                '{}._c_type'.format(type.name.camel)),
            (T.BigIntegerType, lambda _: '_big_integer'),
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
            (T.BoolType, lambda _: 'bool'),
            (T.LongType, lambda _: 'int'),
            (T.CharacterType, lambda _: 'unicode'),
            (T.TokenType, lambda _: 'Token'),
            (T.SymbolType, lambda _: 'unicode'),
            (ct.ASTNodeType, lambda t: self.type_public_name(t.entity)),
            (ct.EntityType, lambda t: t.astnode.kwless_raw_name.camel),
            (T.AnalysisUnitType, lambda t: t.api_name),
            (ct.ArrayType, lambda _: 'list[{}]'.format(
                type.element_type.name.camel
            )),
            (ct.StructType, lambda _: type.name.camel),
            (T.AnalysisUnitKind, lambda _: 'str'),
            (T.BigIntegerType, lambda _: 'int'),
        ])

    def unwrap_requires_context(self, type):
        """
        Return whether unwrapping values for the given ``type`` requires having
        an analysis context. For types that do, the ``.(_)unwrap`` method
        should take the context as a C value in addition to the Python value to
        unwrap.

        :param CompiledType type: Type to analyze.
        :rtype: bool
        """
        return dispatch_on_type(type, [
            (T.SymbolType, lambda _: True),
            (T.ArrayType, lambda _:
                self.unwrap_requires_context(type.element_type)),
            (T.EntityType, lambda _: False),
            (T.StructType, lambda _: any(
                self.unwrap_requires_context(f.type)
                for f in type.get_fields())),

            # By default, assume the context is not required
            (ct.CompiledType, lambda _: False),
        ])
