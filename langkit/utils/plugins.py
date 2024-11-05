"""
Helpers to load Langkit plugins.

This package provides the PluginLoader class to enable Langkit to load
arbitrary Python code so that language specifications and build command
arguments can inject callables at various points in the compilation process.
"""

from __future__ import annotations

import importlib
import re
import sys
from typing import Callable, NoReturn, TypeVar


Plugin = TypeVar("Plugin")


class PluginLoadingError(Exception):
    pass


class PluginLoader:

    plugin_ref_re = re.compile(
        r"(?P<module>[a-zA-Z_][a-zA-Z0-9_]*(\.[a-zA-Z_][a-zA-Z0-9_]*)*)"
        r"\."
        "(?P<func>[a-zA-Z_][a-zA-Z0-9_]*)"
    )

    def __init__(self, root_dir: str | None = None):
        """
        :param root_dir: Directory meant to contain plugin code. If provided,
            insert it first in Python's module lookup directory.
        """
        if root_dir is not None:
            sys.path.insert(0, root_dir)

    # Users of these methods may have to give up type checking here because
    # mypy does not accept abstract types for type[Plugin] arguments, even
    # though it is sound here (see https://github.com/python/mypy/issues/4717).

    def load(self, ref: str | Plugin, typ: type[Plugin]) -> Plugin:
        """
        Load a plugin.

        If ``ref`` is a plugin instance, return it directly.

        Otherwise, it must be a string that matches
        ``PluginLoader.plugin_ref_re`` that references a callable. That
        callable is imported/loaded, then called without argument, and the
        result is checked to be an instance of ``typ``, then returned.

        If anything goes wrong, raise a ``PluginLoadingError``.
        """
        if isinstance(ref, typ):
            return ref
        assert isinstance(ref, str)

        m = self.plugin_ref_re.match(ref)
        if m is None:
            raise PluginLoadingError(
                "invalid syntax for a plugin pass (MODULE.CALLABLE expecetd):"
                f" {ref}"
            )
        module_name = m.group("module")
        func_name = m.group("func")

        def abort(exc: Exception | str) -> NoReturn:
            raise PluginLoadingError(f"cannot load plugin {ref!r}: {exc}")

        try:
            module = importlib.import_module(module_name)
        except ImportError as exc:
            abort(exc)

        try:
            func = getattr(module, func_name)
        except AttributeError as exc:
            abort(exc)

        try:
            result = func()
        except Exception as exc:
            abort(exc)

        if not isinstance(result, typ):
            abort("callback did not return a pass")
        return result

    def loader_for(
        self,
        typ: type[Plugin]
    ) -> Callable[[str | Plugin], Plugin]:
        """
        Return a wrapper around ``self.load`` for a given plugin type.
        """
        def load(ref: str | Plugin) -> Plugin:
            return self.load(ref, typ)

        return load
