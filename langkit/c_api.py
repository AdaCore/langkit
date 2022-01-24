from __future__ import annotations

import re
from typing import Optional, TYPE_CHECKING, Union

from langkit import names
from langkit.diagnostics import check_source_language
from langkit.language_api import AbstractAPISettings


if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx


class CAPIType:
    """
    C API generation helper: encapsulate the logic of C types formatting.
    """

    c_api_settings: CAPISettings
    external: bool
    _name: names.Name

    def __init__(self,
                 c_api_settings: CAPISettings,
                 name: Union[str, names.Name],
                 external: bool = False) -> None:
        """Create a stub for a C API type.

        :param c_api_settings: API settings to use for this type.

        :param name: The name for the type. Can be either a Name or a
            lower-case formatted name.

        :param external: Whether this type is already declared outside the C
            API. For instance: "int" is external, but "node" is not.
        """
        self.c_api_settings = c_api_settings
        self.external = external

        # Make private the following in order to avoid accidental use of these
        # instead of the properties.
        self._name = (name if isinstance(name, names.Name) else
                      names.Name.from_lower(name))

    @property
    def name(self) -> str:
        """Return the C name for this type, properly wrapped if needed."""
        # All names we define as part of the C API must be wrapped so that they
        # don't conflict with "external" names. Keep "external" ones untouched
        # since we don't control them.
        return (self._name.lower if self.external else
                self.c_api_settings.get_name(self._name))


class CAPISettings(AbstractAPISettings):
    """Convenient container for C API generation settings.

    The convention is to make instances for this class available to templates
    as `capi`."""

    LIB_NAME_RE = re.compile('[a-zA-Z][a-zA-Z0-9_-]+')

    context: CompileCtx
    symbol_prefix: str
    _lib_name: Optional[str]

    def __init__(self, ctx: CompileCtx, symbol_prefix: str = '') -> None:
        """
        Create C API generation settings.

        :param ctx: Compile context for this C API.

        :param symbol_prefix: Valid C identifier used as a prefix for all
            top-level declarations in the generated C API. Empty string
            (default) if no prefix is needed.
        """
        self.context = ctx
        self.symbol_prefix = symbol_prefix
        self._lib_name = None

    @property
    def lib_name(self) -> str:
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
    def lib_name(self, lib_name: str) -> None:
        check_source_language(bool(self.LIB_NAME_RE.match(lib_name)),
                              'Invalid library name: {}'.format(lib_name))
        self._lib_name = lib_name

    #
    # Helpers for templates
    #

    @property
    def shared_object_basename(self) -> str:
        """
        Return the basename to use for the shared object.

        In order to get the fullname, format the following string::

            lib{shared_object_basename}.{extension}

        `extension` must be "so" on Linux, "dll" on Windows, etc.
        """
        basename = self.lib_name.lower()
        return basename[3:] if basename.startswith('lib') else basename

    @property
    def header_guard_id(self) -> str:
        return self.lib_name.upper().replace('-', '_')

    def get_name(self, name: Union[str, names.Name]) -> str:
        """
        Wrap `name` as a top-level scope symbol.
        """
        if isinstance(name, str):
            name = names.Name.from_lower(name)
        return (f"{self.symbol_prefix}_{name.lower}"
                if self.symbol_prefix
                else name.lower)
