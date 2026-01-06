"""
Unit tests for Langkit's JSON-like deserialization helper.
"""

import dataclasses
import itertools

from enum import Enum

from langkit.utils.deserialization import (
    DataclassFieldMetadata,
    DeserializationError,
    Deserializer,
)


def test(rtype, obj):
    print(repr(obj))

    d = Deserializer()
    try:
        value = d.deserialize("<input>", rtype, obj)
    except (DeserializationError, TypeError) as exc:
        print(f"    {type(exc).__name__}: {exc}")
    else:
        print("   ", value)


counter = itertools.count(1)


@dataclasses.dataclass(frozen=True)
class DCSimple:
    a: str
    b: int


@dataclasses.dataclass(frozen=True)
class DCInvalidType1:
    a: str | int


@dataclasses.dataclass(frozen=True)
class DCInvalidType2:
    a: None | str


@dataclasses.dataclass(frozen=True)
class DCWithAutoValue:
    a: int = dataclasses.field(
        metadata=DataclassFieldMetadata.make(auto_value=42)
    )
    b: int


@dataclasses.dataclass(frozen=True)
class DCWithKey:
    a: int = dataclasses.field(metadata=DataclassFieldMetadata.make(key="b"))


@dataclasses.dataclass(frozen=True)
class DCWithOpt:
    a: int
    b: int | None


@dataclasses.dataclass(frozen=True)
class DCWithDefault:
    a: int
    b: int = 420


@dataclasses.dataclass(frozen=True)
class DCWithDefaultFactory:
    a: int
    b: int = dataclasses.field(default_factory=lambda: next(counter))


@dataclasses.dataclass(frozen=True)
class DCNesting:
    a: int
    b: "DCSimple"


class EColor(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3


print("# bool")
print()
test(bool, True)
test(bool, "foo")
print()

print("# int")
print()
test(int, True)
test(int, 0)
test(int, "foo")
print()

print("# str")
print()
test(str, "foo")
test(str, 0)
print()

print("# list[str]")
print()
test(list[str], 1)
test(list[str], [])
test(list[str], ["a", "b"])
test(list[str], "foo")
test(list[str], ["a", 1])
print()

print("# dict[str, int]")
print()
test(dict[str, int], 1)
test(dict[str, int], {})
test(dict[str, int], {"a": 1})
test(dict[str, int], {"a": 1, "b": "foo"})

print("# dict[int, int]")
print()
test(dict[int, int], {})
print()

print("# Dataclasses")
print()
test(DCSimple, {"a": "foo", "b": 1})
test(DCSimple, {"a": "foo"})
test(DCSimple, {"a": "foo", "b": None})
test(DCSimple, {"a": "foo", "b": 1, "c": 2})
test(DCSimple, [])
test(DCInvalidType1, {})
test(DCInvalidType2, {})
test(DCWithAutoValue, {"b": 1})
test(DCWithAutoValue, {"a": 0, "b": 1})
test(DCWithKey, {"a": 0})
test(DCWithKey, {"b": 0})
test(DCWithOpt, {"a": 0})
test(DCWithOpt, {"a": 0, "b": 1})
test(DCWithDefault, {"a": 10})
test(DCWithDefault, {"a": 20, "b": 1})
test(DCWithDefaultFactory, {"a": 0})
test(DCWithDefaultFactory, {"a": 10, "b": 1})
test(DCWithDefaultFactory, {"a": 20})
test(DCWithDefaultFactory, {"a": 30})
test(DCNesting, {"a": 1, "b": {"a": "bar", "b": 42}})
print()

print("# Enum")
print()
test(EColor, "RED")
test(EColor, "GRAY")
print()

print("# Invalid types")
print()
test(int | None, None)
test(dict[int, int], {1: 10})
test(list[object], [1])
test(EColor, [1])
print()

print("Done")
