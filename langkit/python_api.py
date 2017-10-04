from __future__ import absolute_import, division, print_function

import langkit.compiled_types as ct
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
                or type in (ct.lexical_env_type, ct.equation_type)
                or isinstance(type, (ct.ArrayType, ct.StructType))), (
            'Incrementing ref-count of {} in the Python API is not handled'
            ' yet'.format(type.name)
        )

        value_suffix = '' if from_field_access else '.value'
        return dispatch_on_type(type, [
            (ct.analysis_unit_type, lambda _: 'AnalysisUnit._wrap({})'),
            (ct.analysis_unit_kind, lambda _: '_unit_kind_to_str[{}]'),
            (ct.ASTNodeType, lambda _: '{}'),
            (ct.EntityType, lambda _: '{}._wrap({{}})'.format(
                ct.T.root_node.name.camel
            )),
            (ct.token_type, lambda _: '{}'),
            (ct.symbol_type, lambda _: '{}._wrap()'),
            (ct.bool_type, lambda _: 'bool({{}}{})'.format(value_suffix)),
            (ct.long_type, lambda _: '{{}}{}'.format(value_suffix)),
            (ct.EnumType, lambda _: '{}_to_str[{{}}{}]'.format(
                type.c_type(self.c_api_settings).name,
                value_suffix,
            )),
            (ct.ArrayType, lambda cls: '{}({{}}, inc_ref={})'.format(
                self.array_wrapper(type),
                inc_ref
            )),
            (ct.StructType, lambda _: '{}._wrap({{}}, inc_ref={})'.format(
                self.struct_wrapper(type),
                inc_ref
            )),
            (ct.lexical_env_type, lambda _:
                'LexicalEnv._wrap({{}}, inc_ref={})'.format(inc_ref)),
            (ct.logic_var_type, lambda _: 'LogicVar._wrap({})'),
            (ct.equation_type, lambda _:
                'Equation._wrap({{}}, inc_ref={})'.format(inc_ref)),
            (ct.env_rebindings_type, lambda _: 'EnvRebindings._wrap({})'),
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
            (ct.analysis_unit_type, lambda _: 'AnalysisUnit._unwrap({})'),
            (ct.analysis_unit_kind, lambda _: '_unwrap_unit_kind({})'),
            (ct.ASTNodeType, lambda _: '{}'),
            (ct.EntityType, lambda _: '{}._unwrap({{}})'.format(
                ct.T.root_node.name.camel
            )),
            (ct.bool_type, lambda _: 'bool({})'),
            (ct.long_type, lambda _: 'int({})'),
            (ct.EnumType, lambda _:
                '_unwrap_enum({{}}, str_to_{}, {})'.format(
                    type.c_type(self.c_api_settings).name,
                    type.name.camel
                )),
            (ct.ArrayType, lambda cls: '{}._unwrap({{}})'.format(
                self.array_wrapper(cls)
            )),
            (ct.StructType, lambda _: '{}._unwrap({{}})'.format(
                self.struct_wrapper(type)
            )),
            (ct.symbol_type, lambda _: '_text._unwrap({})'),
            (ct.lexical_env_type, lambda _: 'LexicalEnv._unwrap({})'),
            (ct.logic_var_type, lambda _: 'LogicVar._unwrap({})'),
            (ct.equation_type, lambda _: 'Equation._unwrap({})'),
            (ct.env_rebindings_type, lambda _: 'EnvRebindings._unwrap({})'),
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
            (ct.bool_type, lambda _: ctype_type('c_uint8')),
            (ct.long_type, lambda _: ctype_type('c_int')),
            (ct.lexical_env_type, lambda _: 'LexicalEnv._c_type'),
            (ct.logic_var_type, lambda _: 'LogicVar._c_type'),
            (ct.equation_type, lambda _: 'Equation._c_type'),
            (ct.env_rebindings_type, lambda _: 'EnvRebindings._c_type'),
            (ct.token_type, lambda _: 'Token'),
            (ct.symbol_type, lambda _: wrapped_type('text')),
            (ct.analysis_unit_type, lambda _: 'AnalysisUnit._c_type'),
            (ct.analysis_unit_kind, lambda _: ctype_type('c_uint')),
            (ct.ASTNodeType, lambda _: '_ASTNodeExtension.c_type'),
            (ct.EnumType, lambda _: ctype_type('c_uint')),
            (ct.ArrayType, lambda cls:
                '{}._c_type'.format(self.array_wrapper(cls))),
            (ct.EntityType, lambda _: '{}._c_type'.format(
                ct.T.entity.name.camel
            )),
            (ct.StructType, lambda _:
                '{}._c_type'.format(self.struct_wrapper(type))),
        ])

    def array_wrapper(self, array_type):
        return (ct.T.entity.array
                if array_type.element_type.is_entity_type else
                array_type).array_type_name.camel

    def struct_wrapper(self, struct_type):
        return (ct.T.entity
                if struct_type.is_entity_type else
                struct_type).name.camel
