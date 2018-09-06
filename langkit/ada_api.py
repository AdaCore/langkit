from __future__ import absolute_import, division, print_function

from langkit.common import is_keyword
from langkit.language_api import AbstractAPISettings


class AdaAPISettings(AbstractAPISettings):
    """Convenient container for Ada API generation settings.

    The convention is to make instances for this class available to templates
    as `adaapi`.
    """

    def __init__(self, ctx):
        self.context = ctx

    @property
    def lib_name(self):
        return self.context.lib_name.camel_with_underscores

    @classmethod
    def escape(cls, name, suffix):
        return (name + suffix if is_keyword(name.base_name) else name)
