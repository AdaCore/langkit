from __future__ import absolute_import, division, print_function

from langkit import common
from langkit.language_api import AbstractAPISettings


class AdaAPISettings(AbstractAPISettings):
    """Convenient container for Ada API generation settings.

    The convention is to make instances for this class available to templates
    as `adaapi`.
    """

    KEYWORDS = common.keywords['ada']

    def __init__(self, ctx):
        self.context = ctx

    @property
    def lib_name(self):
        return self.context.lib_name.camel_with_underscores

    @classmethod
    def escape(cls, name, suffix):
        return (name + suffix
                if name.base_name.lower() in cls.KEYWORDS else
                name)

    def get_enum_alternative(self, type_name, alt_name, suffix):
        return self.escape(alt_name, suffix)
