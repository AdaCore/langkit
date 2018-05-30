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

    def wrap_value(self, value, type, from_field_access=False, inc_ref=False):
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
        :param bool inc_ref: If True, this conversion also creates a new
            ownership share for "value".
        :rtype: str
        """

        # TODO: handle all types
        assert (not inc_ref
                or not type.is_refcounted
                or isinstance(type, (ct.ArrayType, ct.StructType))), (
            'Incrementing ref-count of {} in the Python API is not handled'
            ' yet'.format(type.name)
        )

        value_suffix = '' if from_field_access else '.value'
        return dispatch_on_type(type, [
            (T.AnalysisUnitType, lambda _: 'AnalysisUnit._wrap({})'),
            (T.AnalysisUnitKind, lambda _: '_unit_kind_to_str[{}]'),
            (ct.ASTNodeType, lambda _: '{}'),
            (ct.EntityType, lambda _: '{}._wrap({{}})'.format(
                ct.T.root_node.kwless_raw_name.camel
            )),
            (T.TokenType, lambda _: '{}'),
            (T.SymbolType, lambda _: '{}._wrap()'),
            (T.BoolType, lambda _: 'bool({{}}{})'.format(value_suffix)),
            (T.LongType, lambda _: '{{}}{}'.format(value_suffix)),
            (ct.ArrayType, lambda cls: '{}({{}}, inc_ref={})'.format(
                self.array_wrapper(type),
                inc_ref
            )),
            (ct.StructType, lambda _: '{}._wrap({{}}, inc_ref={})'.format(
                type.name.camel,
                inc_ref
            )),
            (T.EnvRebindingsType, lambda _: 'EnvRebindings._wrap({})'),
            (T.BigIntegerType, lambda _: '_big_integer._wrap({})'),
        ], exception=TypeError(
            'Unhandled field type in the python binding'
            ' (wrapping): {}'.format(type)
        )).format(value)

    def unwrap_value(self, value, type):
        """
        Given an expression for a high-level value and the associated type,
        return an other expression that yields the corresponding low-level
        value.

        :param str value: Expression yielding a high-level value.
        :param ct.CompiledType type: Type corresponding to the "value"
            expression.
        :rtype: str
        """
        return dispatch_on_type(type, [
            (T.AnalysisUnitType, lambda _: 'AnalysisUnit._unwrap({})'),
            (T.AnalysisUnitKind, lambda _: '_unwrap_unit_kind({})'),
            (ct.ASTNodeType, lambda _: '{}'),
            (ct.EntityType, lambda _: '{}._unwrap({{}})'.format(
                ct.T.root_node.kwless_raw_name.camel
            )),
            (T.BoolType, lambda _: 'bool({})'),
            (T.LongType, lambda _: 'int({})'),
            (ct.ArrayType, lambda cls: '{}._unwrap({{}})'.format(
                self.array_wrapper(cls)
            )),
            (ct.StructType, lambda _: '{}._unwrap({{}})'.format(
                type.name.camel
            )),
            (T.SymbolType, lambda _: '_text._unwrap({})'),
            (T.EnvRebindingsType, lambda _: 'EnvRebindings._unwrap({})'),
            (T.BigIntegerType, lambda _: '_big_integer._unwrap({})'),
        ], exception=TypeError(
            'Unhandled field type in the python binding'
            ' (unwrapping): {}'.format(type)
        )).format(value)

    def type_internal_name(self, type):
        """
        Python specific helper, to get the internal name of a type that is
        wrapped.

        :param CompiledType type: The type for which we want to get the
            internal name.
        :rtype: str
        """
        def ctype_type(name):
            return "ctypes.{}".format(name)

        def wrapped_type(name):
            return "_{}".format(name)

        return dispatch_on_type(type, [
            (T.BoolType, lambda _: ctype_type('c_uint8')),
            (T.LongType, lambda _: ctype_type('c_int')),
            (T.EnvRebindingsType, lambda _: 'EnvRebindings._c_type'),
            (T.TokenType, lambda _: 'Token'),
            (T.SymbolType, lambda _: wrapped_type('text')),
            (T.AnalysisUnitType, lambda _: 'AnalysisUnit._c_type'),
            (T.AnalysisUnitKind, lambda _: ctype_type('c_uint')),
            (ct.ASTNodeType, lambda _: '{}._node_c_type'.format(
                ct.T.root_node.kwless_raw_name.camel)),
            (ct.ArrayType, lambda cls:
                '{}._c_type'.format(self.array_wrapper(cls))),
            (ct.EntityType, lambda _: '{}._c_type'.format(
                ct.T.entity.name.camel
            )),
            (ct.StructType, lambda _:
                '{}._c_type'.format(type.name.camel)),
            (T.BigIntegerType, lambda _: '_big_integer'),
        ])

    def array_wrapper(self, array_type):
        return (ct.T.entity.array
                if array_type.element_type.is_entity_type else
                array_type).array_type_name.camel
