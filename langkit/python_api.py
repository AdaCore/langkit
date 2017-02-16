from __future__ import absolute_import

import langkit.compiled_types as ct
from langkit.language_api import AbstractAPISettings
from langkit.utils import dispatch_on_type


class PythonAPISettings(AbstractAPISettings):
    """Container for Python API generation settings."""

    name = 'python'

    def __init__(self, module_name, c_api_settings):
        self.c_api_settings = c_api_settings
        self.module_name = module_name

    def get_enum_alternative(self, type_name, alt_name, suffix):
        return alt_name.upper

    def wrap_value(self, value, type, from_field_access=False, inc_ref=False):
        """
        Given an expression for a low-level value and the associated type,
        return an other expression that yields the corresponding high-level
        value.

        :param str value: Expression yielding a low-level value.
        :param ct.CompiledType type: Type parameter. Type corresponding to
            the "value" expression.
        :param bool from_field_access: True if "value" is a record field
            access (False by default). This is a special case because of the
            way ctypes works.
        :param bool inc_ref: If True, this conversion also creates a new
            ownership share for "value".
        :rtype: str
        """

        # TODO: handle all types
        assert (not inc_ref
                or not type.is_refcounted()
                or issubclass(type, ct.ArrayType)), (
            'Incrementing ref-count of {} in the Python API is not handled'
            ' yet'.format(type.name())
        )

        value_suffix = '' if from_field_access else '.value'
        return dispatch_on_type(type, [
            (ct.AnalysisUnitType, lambda _: 'AnalysisUnit.wrap({})'),
            (ct.AnalysisUnitKind, lambda _: 'unit_kind_to_str[{}]'),
            (ct.ASTNode, lambda _: '_wrap_astnode({})'),
            (ct.SourceLocationRangeType, lambda _: '_wrap_sloc_range({})'),
            (ct.Token, lambda _: '{}'),
            (ct.Symbol, lambda _: '{}.wrap()'),
            (ct.BoolType, lambda _: 'bool({{}}{})'.format(value_suffix)),
            (ct.LongType, lambda _: '{{}}{}'.format(value_suffix)),
            (ct.EnumType, lambda _: '{}_to_str[{{}}{}]'.format(
                type.c_type(self.c_api_settings).name,
                value_suffix,
            )),
            (ct.ArrayType, lambda cls: '{}({{}}, inc_ref={})'.format(
                type.name().camel,
                inc_ref
            )),
            (ct.Struct, lambda _: '{}'),
            (ct.LexicalEnvType, lambda _: 'LexicalEnv.wrap({})'),
            (ct.EnvRebindingsType, lambda _: 'EnvRebindings.wrap({})'),
        ], exception=TypeError(
            'Unhandled field type in the python binding'
            '(wrapping): {}'.format(type)
        )).format(value)

    def unwrap_value(self, value, type):
        """
        Given an expression for a high-level value and the associated type,
        return an other expression that yields the corresponding low-level
        value.

        :param str value: Expression yielding a high-level value.
        :param ct.CompiledType type: Type parameter. Type corresponding to the
            "value" expression.
        :rtype: str
        """
        return dispatch_on_type(type, [
            (ct.AnalysisUnitType, lambda _: '{}._c_value'),
            (ct.AnalysisUnitKind, lambda _: '_unwrap_unit_kind({})'),
            (ct.ASTNode, lambda _: '_unwrap_astnode({})'),
            (ct.BoolType, lambda _: 'bool({})'),
            (ct.LongType, lambda _: 'int({})'),
            (ct.EnumType, lambda _: '_unwrap_enum({{}}, str_to_{}, {})'.format(
                type.c_type(self.c_api_settings).name,
                type.name().camel
            )),
            (ct.Symbol, lambda _: '_text.unwrap({})'),
            (ct.LexicalEnvType, lambda _: '{}.unwrap()'),
        ], exception=TypeError(
            'Unhandled field type in the python binding '
            '(unwrapping): {}'.format(type)
        )).format(value)

    def type_internal_name(self, type):
        """
        Python specific helper, to get the internal name of a type that is
        wrapped.

        :param CompiledType type: Type parameter. The type for which we want to
            get the internal name.
        :rtype: str
        """
        def ctype_type(name):
            return "ctypes.{}".format(name)

        def wrapped_type(name):
            return "_{}".format(name)

        return dispatch_on_type(type, [
            (ct.BoolType, lambda _: ctype_type('c_uint8')),
            (ct.LongType, lambda _: ctype_type('c_long')),
            (ct.LexicalEnvType, lambda _: wrapped_type('lexical_env')),
            (ct.EnvRebindingsType, lambda _: wrapped_type('env_rebindings')),
            (ct.SourceLocationRangeType, lambda _: wrapped_type('SlocRange')),
            (ct.Token, lambda _: 'Token'),
            (ct.Symbol, lambda _: wrapped_type('text')),
            (ct.AnalysisUnitType, lambda _: wrapped_type('analysis_unit')),
            (ct.AnalysisUnitKind, lambda _: ctype_type('c_uint')),
            (ct.ASTNode, lambda _: wrapped_type('node')),
            (ct.EnumType, lambda _: ctype_type('c_uint')),
            (ct.ArrayType, lambda cls: wrapped_type(cls.name().camel)),
            (ct.Struct, lambda _: type.name().camel),
        ])
