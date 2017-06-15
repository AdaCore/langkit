from __future__ import absolute_import, division, print_function


class AbstractAPISettings(object):
    """
    Abstract class that defines an interface for the settings for a language we
    want to generate bindings for.
    """

    def get_enum_alternative(self, type_name, alt_name, suffix):
        """
        Return a name that is suitable for code generation for the `alt_name`
        alternative in the `type_name` enumeration type.

        :param names.Name type_name: Name of the type.
        :param names.Name alt_name: Name of the alternative.
        :param names.Name suffix: Used to post-process names that are invalid
            enumerators.
        """
        raise NotImplementedError()

    def wrap_value(self, value, type):
        """
        Return generated code to wrap some value coming from the C API. This is
        relevant only for a subset of language bindings, typically the ones
        that wrap the C API and provide high-level wrappers for values, such as
        objects in Python.

        :param str value: The value to wrap.
        :param langkit.compiled_types.CompiledType type: The type for "value".
        :rtype: str
        """
        raise NotImplementedError()
