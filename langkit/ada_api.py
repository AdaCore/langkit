import common
from language_api import AbstractAPISettings
import names


class AdaAPISettings(AbstractAPISettings):
    """Convenient container for Ada API generation settings.

    The convention is to make instances for this class available to templates
    as `adaapi`.
    """

    KEYWORDS = common.keywords['ada']

    def __init__(self, lib_name):
        self.lib_name = lib_name

    @classmethod
    def escape(cls, name, suffix):
        if isinstance(suffix, basestring):
            suffix = names.Name.from_lower(suffix)
        return (name + suffix
                if name.base_name.lower() in cls.KEYWORDS else
                name)

    def get_enum_alternative(self, type_name, alt_name, suffix):
        return self.escape(alt_name, suffix)
