import re

from langkit import names
from langkit.diagnostics import check_source_language
from langkit.language_api import AbstractAPISettings


class CAPIType(object):
    """
    C API generation helper: encapsulate the logic of C types formatting.
    """

    def __init__(self, c_api_settings, name, external=False):
        """Create a stub for a C API type.

        :param CAPISettings c_api_settings: API settings to use for this type.

        :param name: The name for the type. Can be either a Name or a
            lower-case formatted name.
        :type name: str|names.Name

        :param bool external: Whether this type is already declared outside the
            C API. For instance: "int" is external, but "node" is not.
        """
        self.c_api_settings = c_api_settings
        self.external = external

        # Make private the following in order to avoid accidental use of these
        # instead of the properties.
        self._name = (name if isinstance(name, names.Name) else
                      names.Name(name))

    @property
    def name(self):
        """Return the C name for this type, properly wrapped if needed."""
        # All names we define as part of the C API must be wrapped so that they
        # don't conflict with "external" names. Keep "external" ones untouched
        # since we don't control them.
        return (self._name if self.external else
                self.c_api_settings.get_name(self._name))


class CAPISettings(AbstractAPISettings):
    """Convenient container for C API generation settings.

    The convention is to make instances for this class available to templates
    as `capi`."""

    LIB_NAME_RE = re.compile('[a-zA-Z][a-zA-Z0-9_-]+')

    def __init__(self, ctx, symbol_prefix=''):
        """
        Create C API generation settings.

        :param CompileCtx ctx: Compile context for this C API.

        :param str symbol_prefix: Valid C identifier used as a prefix for all
        top-level declarations in the generated C API. Empty string (default)
        if no prefix is needed.
        """
        self.context = ctx
        self.symbol_prefix = symbol_prefix
        self._lib_name = None

    @property
    def lib_name(self):
        """
        Name of the generated library, generated from the context's lib_name.

        This will be used to build the name of header files, library (static
        and shared object) files, etc. It must be a valid C identifier with the
        exception that dashes ("-") are allowed.  Case matters (but you still
        choose it).
        """
        assert self._lib_name
        return self._lib_name

    @lib_name.setter
    def lib_name(self, lib_name):
        check_source_language(self.LIB_NAME_RE.match(lib_name),
                              'Invalid library name: {}'.format(lib_name))
        self._lib_name = lib_name

    #
    # Helpers for templates
    #

    @property
    def shared_object_basename(self):
        """
        Return the basename to use for the shared object.

        In order to get the fullname, format the following string::

            lib{shared_object_basename}.{extension}

        `extension` must be "so" on Linux, "dll" on Windows, etc.
        """
        basename = self.lib_name.lower()
        return basename[3:] if basename.startswith('lib') else basename

    @property
    def header_guard_id(self):
        return self.lib_name.upper().replace('-', '_')

    def get_name(self, name):
        """
        Wrap `name` as a top-level scope symbol.

        :type name: Name|str
        :rtype: str
        """
        if isinstance(name, str):
            name = names.Name(name)
        return names.Name('{}_{}'.format(self.symbol_prefix, name.base_name)
                          if self.symbol_prefix else name).lower
