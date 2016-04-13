from __future__ import absolute_import


class Name(object):
    """
    Code generation helpers to format names with various casing conventions.

    This class is intended to be used as a more evoluted version of "str": you
    can use it as a key in dict or as a key for sorting. In other words:
    hashing and order checking are supported and behave as one could expect.
    """

    default_formatting = None
    formatting_stack = []

    def __init__(self, mixed_with_underscores):
        """
        Create a name from a string with mixed case and underscores.

        For instance: C_OOP_Extension.

        :param str mixed_with_underscores: Name in the mixed case and
            underscore format.
        """
        self.base_name = mixed_with_underscores

    def __len__(self):
        return len(self.base_name)

    def __hash__(self):
        return hash(self.base_name)

    def __eq__(self, other):
        return self.base_name == other.base_name

    def __lt__(self, other):
        return self.base_name < other.base_name

    def __gt__(self, other):
        return self.base_name > other.base_name

    @property
    def camel_with_underscores(self):
        """
        Format to mixed case with undercore (e.g. C_OOP_Extension).

        :rtype: str
        """
        return self.base_name

    @property
    def camel(self):
        """
        Format to camel case (e.g. COOPExtension).

        :rtype: str
        """
        return self.base_name.replace('_', '')

    @property
    def lower(self):
        """
        Format to lower case (e.g. c_oop_extension).

        :rtype: str
        """
        return self.base_name.lower()

    @property
    def upper(self):
        """
        Format to upper case (e.g. C_OOP_EXTENSION).

        :rtype: str
        """
        return self.base_name.upper()

    def __str__(self):
        """Format to default casing convention."""
        assert Name.default_formatting is not None
        return getattr(self, Name.default_formatting)

    def __repr__(self):
        return "<Name {}>".format(self.camel_with_underscores)

    def __add__(self, other):
        """
        Returns a name which is a concatenation of two names, so that
        A_Name + Another_Name = A_Name_Another_Name.

        :type other: Name
        :rtype: Name
        """
        return Name('{}_{}'.format(self.base_name, other.base_name))

    @classmethod
    def from_camel_with_underscores(cls, name):
        """
        Creates a name from a string, which is formatted according to the
        camel case with underscores convention, such as
        "Camel_Case_With_Underscores".

        :param str name: The string to create the name from.
        :rtype: Name
        """
        return cls(name)

    @classmethod
    def from_camel(cls, name):
        """
        Creates a name from a string, which is formatted according to the
        camel case convention, such as "CamelCaseName".

        :param str name: The string to create the name from.
        :rtype: Name
        """
        result = list(name)
        inserted_underscores = []
        for i, c in enumerate(result):
            if i <= 1:
                continue
            c_1 = result[i - 1]
            if c_1 == c_1.upper() and c == c.lower():
                inserted_underscores.append(i - 1)
        for index in reversed(inserted_underscores):
            result.insert(index, '_')
        return cls(''.join(result))

    @classmethod
    def from_lower(cls, name):
        """
        Creates a name from a string, which is formatted according to the
        lower case convention, such as "lower_case_name".

        :param str name: The string to create the name from.
        :rtype: Name
        """
        return cls('_'.join(word.lower().capitalize()
                            for word in name.split('_')))

    @classmethod
    def from_upper(cls, name):
        """
        Creates a name from a string, which is formatted according to the
        lower case convention, such as "UPPER_CASE_NAME".

        :param str name: The string to create the name from.
        :rtype: Name
        """
        return cls('_'.join(word.lower().capitalize()
                            for word in name.split('_')))

    @classmethod
    def get(cls, name_or_str):
        """
        Convenience function that will take a name or a string, and return a
        name.

        :param str|Name name_or_str:
        :rtype: Name
        """
        if isinstance(name_or_str, Name):
            return name_or_str
        return Name(name_or_str)


class Convention(object):
    """Guard to set a default convention."""

    def __init__(self, name):
        self.name = name

    def __enter__(self):
        """Set the current convention to self's convention."""
        Name.formatting_stack.append(Name.default_formatting)
        Name.default_formatting = self.name

    def __exit__(self, exc, exc_type, traceback):
        """Sets the convention back to the old convention."""
        del exc, exc_type, traceback
        Name.default_formatting = Name.formatting_stack.pop()


camel_with_underscores = Convention('camel_with_underscores')
camel = Convention('camel')
lower = Convention('lower')
upper = Convention('upper')
