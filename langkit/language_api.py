from __future__ import absolute_import, division, print_function


class AbstractAPISettings(object):
    """
    Abstract class that defines an interface for the settings for a language we
    want to generate bindings for.
    """

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
