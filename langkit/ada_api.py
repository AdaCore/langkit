from __future__ import annotations

from typing import TYPE_CHECKING

from langkit.common import is_keyword
from langkit.language_api import AbstractAPISettings
import langkit.names as names


if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx


class AdaAPISettings(AbstractAPISettings):
    """Convenient container for Ada API generation settings.

    The convention is to make instances for this class available to templates
    as `adaapi`.
    """

    context: CompileCtx

    def __init__(self, ctx: CompileCtx) -> None:
        self.context = ctx

    @property
    def lib_name(self) -> str:
        return self.context.lib_name.camel_with_underscores

    @classmethod
    def escape(cls, name: names.Name, suffix: names.Name) -> names.Name:
        return name + suffix if is_keyword(name.base_name) else name
