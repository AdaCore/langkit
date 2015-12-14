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

    def wrap_value(self, value, type):
        return dispatch_on_type(type, [
            (ct.ASTNode, lambda _: '_wrap_astnode({})'),
            (ct.SourceLocationRangeType, lambda _: '_wrap_sloc_range({})'),
            (ct.Token, lambda _: 'Token({})'),
            (ct.BoolType, lambda _: 'bool({}.value)'),
            (ct.LongType, lambda _: '{}.value'),
            (ct.EnumType, lambda _: '{}_to_str[{{}}.value]'.format(
                type.c_type(self.c_api_settings).name)),
            (ct.ArrayType, lambda cls: '{}({{}})'.format(type.name().camel)),
            (ct.Struct, lambda _: '{}'),
        ], exception_msg='Unhandled field type'
                         ' in the python binding: {}'.format(type)
        ).format(value)

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
            (ct.BoolType, lambda _: ctype_type('c_int')),
            (ct.LongType, lambda _: ctype_type('c_long')),
            (ct.SourceLocationRangeType, lambda _: wrapped_type('SlocRange')),
            (ct.Token, lambda _: wrapped_type('token')),
            (ct.ASTNode, lambda _: wrapped_type('node')),
            (ct.EnumType, lambda _: ctype_type('c_uint')),
            (ct.ArrayType, lambda cls: wrapped_type(cls.name().camel)),
            (ct.Struct, lambda _: type.name().camel),
        ])
