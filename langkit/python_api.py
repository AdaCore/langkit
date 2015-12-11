from langkit.language_api import AbstractAPISettings


class PythonAPIType(object):
    """
    Python API generation helper: encapsulate the logic of Python types names.
    """

    def __init__(self, python_api_settings,
                 name_low, external_low):
        """Create a stub for a Python type.

        :param PythonAPISettings python_api_settings: A
            python_api.PythonAPISettings instance.
        :param str name_low: Name for the type used in the low-level binding.
        :param bool external_low: Whether this type is declared in the "ctypes"
            standard package. If not, we consider that this type is declared in
            the generated Python module.
        """
        self.python_api_settings = python_api_settings

        self._name_low = name_low
        self._external_low = external_low

    @property
    def name_low(self):
        """
        Return the name to be used for the low-level Python API for this type.

        :rtype: str
        """
        return ('ctypes.{}'.format(self._name_low)
                if self._external_low else '_{}'.format(self._name_low))


class PythonAPISettings(AbstractAPISettings):
    """Container for Python API generation settings."""

    name = 'python'

    def __init__(self, module_name, c_api_settings):
        self.c_api_settings = c_api_settings
        self.module_name = module_name

    def get_enum_alternative(self, type_name, alt_name, suffix):
        return alt_name.upper

    def wrap_value(self, value, type):
        import compiled_types

        # Depending on the type of the field, we need to convert the value to
        # the most appropriate Python type.
        if issubclass(type, compiled_types.ASTNode):
            result = '_wrap_astnode({})'
        elif type == compiled_types.SourceLocationRangeType:
            result = '_wrap_sloc_range({})'
        elif type == compiled_types.Token:
            result = 'Token({})'
        elif type == compiled_types.BoolType:
            result = 'bool({}.value)'
        elif type == compiled_types.LongType:
            result = '{}.value'
        elif issubclass(type, compiled_types.EnumType):
            result = '{}_to_str[{{}}.value]'.format(
                type.c_type(self.c_api_settings).name
            )
        else:
            raise Exception('Unhandled field type in the python binding:'
                            ' {}'.format(type))

        return result.format(value)
