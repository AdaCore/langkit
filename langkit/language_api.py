class AbstractAPISettings(object):
    """
    Abstract class that defines an interface for the settings for a language we
    want to generate bindings for.
    """

    def get_enum_alternative(self, type_name, alt_name, suffix):
        """
        Return a name that is suitable for code generation for the `alt_name`
        alternative in the `type_name` enumeration type.

        :param names.Name type_name: Name of the type
        :param names.Name alt_name: Name of the alternative
        :param names.Name suffix: Used to post-process names that are invalid
            enumerators.
        """
        raise NotImplementedError()
