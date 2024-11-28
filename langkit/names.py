from __future__ import annotations

from typing import Any, Type

from langkit.diagnostics import error


def check_common(name: str) -> None:
    """
    Check than ``name``:

    * does not end nor starts with an underscore;
    * does not contain consecutive underscores;
    * contains only ASCII letters and digits.
    """
    if not name:
        raise ValueError("names cannot be empty")

    first_char = name[0].lower()
    if first_char < "a" or first_char > "z":
        raise ValueError(f"names must start with a letter: {repr(name)}")

    if name.startswith("_") or name.endswith("_"):
        raise ValueError(
            f"name cannot end or start with an underscore: {repr(name)}"
        )

    if "__" in name:
        raise ValueError(
            f"name cannot contain consecutive underscores: {repr(name)}"
        )

    if any((c < "0" or c > "9") and (c < "a" or c > "z") and c != "_"
           for c in name.lower()):
        raise ValueError(f"illegal character in name: {repr(name)}")


def check_camel_with_underscores(name: str) -> None:
    """
    Check that ``name`` respects the "camel with underscores" convention.
    """
    check_common(name)
    if any(w.capitalize() != w for w in name.split("_")):
        raise ValueError(
            "words must start with an upper case character followed by lower"
            f" case characters: {repr(name)}")


def check_camel(name: str) -> None:
    """
    Check that ``name`` respects the camel case convention.
    """
    check_common(name)
    if "_" in name:
        raise ValueError(
            f"camel case names cannot contain underscores: {repr(name)}"
        )
    if name[0].upper() != name[0]:
        raise ValueError(
            "first letter of camel case names must be an upper case letter:"
            f" {repr(name)}"
        )


def check_lower(name: str) -> None:
    """
    Check that ``name`` respects the lower case convention.
    """
    check_common(name)
    if name.lower() != name:
        raise ValueError(f"no upper case allowed: {repr(name)}")


def check_upper(name: str) -> None:
    """
    Check that ``name`` respects the upper case convention.
    """
    check_common(name)
    if name.upper() != name:
        raise ValueError(f"no lower case allowed: {repr(name)}")


class Name:
    """
    Code generation helpers to format names with various casing conventions.

    This class is intended to be used as a more evoluted version of "str": you
    can use it as a key in dict or as a key for sorting. In other words:
    hashing and order checking are supported and behave as one could expect.
    """

    default_formatting: str | None = None
    formatting_stack: list[str] = []

    def __init__(self, mixed_with_underscores: str):
        """
        Create a name from a string with mixed case and underscores.

        For instance: C_OOP_Extension.

        :param mixed_with_underscores: Name in the mixed case and underscore
            format.
        """
        check_camel_with_underscores(mixed_with_underscores)
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
        check_camel(name)

        # Split ``name`` into words: each upper case character starts a new
        # word. Thanks to ``check_camel`` we know that the first character in
        # ``name`` is an upper case letter.
        result = []
        for c in name:
            if c.isupper():
                result.append(c)
            else:
                result[-1] += c

        return cls("_".join(result))

    @classmethod
    def from_lower(cls, name: str) -> Name:
        """
        Creates a name from a string, which is formatted according to the
        lower case convention, such as "lower_case_name".

        :param name: The string to create the name from.
        """
        check_lower(name)
        return cls('_'.join(word.lower().capitalize()
                            for word in name.split('_')))

    @classmethod
    def from_upper(cls, name: str) -> Name:
        """
        Creates a name from a string, which is formatted according to the
        lower case convention, such as "UPPER_CASE_NAME".

        :param name: The string to create the name from.
        """
        check_upper(name)
        return cls('_'.join(word.lower().capitalize()
                            for word in name.split('_')))

    @classmethod
    def check_from_camel_with_underscores(cls, name: str) -> Name:
        """
        Like ``from_camel_with_underscores``, but create a diagnostic error if
        casing is wrong.
        """
        try:
            return cls.from_camel_with_underscores(name)
        except ValueError as exc:
            error(str(exc))

    @classmethod
    def check_from_camel(cls, name: str) -> Name:
        """
        Like ``from_camel``, but create a diagnostic error if casing is wrong.
        """
        try:
            return cls.from_camel(name)
        except ValueError as exc:
            error(str(exc))

    @classmethod
    def check_from_lower(cls, name: str) -> Name:
        """
        Like ``from_lower``, but create a diagnostic error if casing is wrong.
        """
        try:
            return cls.from_lower(name)
        except ValueError as exc:
            error(str(exc))

    @classmethod
    def check_from_upper(cls, name: str) -> Name:
        """
        Like ``from_upper``, but create a diagnostic error if casing is wrong.
        """
        try:
            return cls.from_upper(name)
        except ValueError as exc:
            error(str(exc))

    @classmethod
    def get(cls, name_or_str: str | Name) -> Name:
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
