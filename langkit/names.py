from __future__ import annotations

from typing import Any, List, Optional, Type, Union


class Name:
    """
    Code generation helpers to format names with various casing conventions.

    This class is intended to be used as a more evoluted version of "str": you
    can use it as a key in dict or as a key for sorting. In other words:
    hashing and order checking are supported and behave as one could expect.
    """

    default_formatting: Optional[str] = None
    formatting_stack: List[str] = []

    def __init__(self, mixed_with_underscores: str):
        """
        Create a name from a string with mixed case and underscores.

        For instance: C_OOP_Extension.

        :param mixed_with_underscores: Name in the mixed case and underscore
            format.
        """
        self.base_name = mixed_with_underscores

    def __len__(self) -> int:
        return len(self.base_name)

    def __hash__(self) -> int:
        return hash(self.base_name)

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Name) and self.base_name == other.base_name

    def __ne__(self, other: Any) -> bool:
        return not (self == other)

    def __lt__(self, other: Name) -> bool:
        return self.base_name < other.base_name

    @property
    def camel_with_underscores(self) -> str:
        """
        Format to mixed case with undercore (e.g. C_OOP_Extension).
        """
        return self.base_name

    @property
    def camel(self) -> str:
        """
        Format to camel case (e.g. COOPExtension).
        """
        return self.base_name.replace('_', '')

    @property
    def lower(self) -> str:
        """
        Format to lower case (e.g. c_oop_extension).
        """
        return self.base_name.lower()

    @property
    def upper(self) -> str:
        """
        Format to upper case (e.g. C_OOP_EXTENSION).
        """
        return self.base_name.upper()

    def __str__(self) -> str:
        """Format to default casing convention."""
        assert Name.default_formatting is not None
        return getattr(self, Name.default_formatting)

    def __repr__(self) -> str:
        return "<Name {}>".format(self.camel_with_underscores)

    def __add__(self, other: Name) -> Name:
        """
        Returns a name which is a concatenation of two names, so that
        A_Name + Another_Name = A_Name_Another_Name.
        """
        if self.base_name == '':
            return other
        elif other.base_name == '':
            return self
        else:
            return Name('{}_{}'.format(self.base_name, other.base_name))

    @classmethod
    def from_camel_with_underscores(cls, name: str) -> Name:
        """
        Creates a name from a string, which is formatted according to the
        camel case with underscores convention, such as
        "Camel_Case_With_Underscores".

        :param name: The string to create the name from.
        """
        return cls(name)

    @classmethod
    def from_camel(cls, name: str) -> Name:
        """
        Creates a name from a string, which is formatted according to the
        camel case convention, such as "CamelCaseName".

        :param name: The string to create the name from.
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
    def from_lower(cls, name: str) -> Name:
        """
        Creates a name from a string, which is formatted according to the
        lower case convention, such as "lower_case_name".

        :param name: The string to create the name from.
        """
        return cls('_'.join(word.lower().capitalize()
                            for word in name.split('_')))

    @classmethod
    def from_upper(cls, name: str) -> Name:
        """
        Creates a name from a string, which is formatted according to the
        lower case convention, such as "UPPER_CASE_NAME".

        :param name: The string to create the name from.
        """
        return cls('_'.join(word.lower().capitalize()
                            for word in name.split('_')))

    @classmethod
    def get(cls, name_or_str: Union[str, Name]) -> Name:
        """
        Convenience function that will take a name or a string, and return a
        name.
        """
        if isinstance(name_or_str, Name):
            return name_or_str
        return Name(name_or_str)


class Convention:
    """Guard to set a default convention."""

    def __init__(self, convention: str):
        self.convention = convention

    def __enter__(self) -> None:
        """Set the current convention to self's convention."""
        if Name.default_formatting is not None:
            Name.formatting_stack.append(Name.default_formatting)
        Name.default_formatting = self.convention

    def __exit__(self,
                 exc: Exception,
                 exc_type: Type[Exception],
                 traceback: Any) -> None:
        """Sets the convention back to the old convention."""
        del exc, exc_type, traceback
        Name.default_formatting = (
            Name.formatting_stack.pop()
            if Name.formatting_stack
            else None
        )


camel_with_underscores = Convention('camel_with_underscores')
camel = Convention('camel')
lower = Convention('lower')
upper = Convention('upper')
