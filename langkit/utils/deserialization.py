"""
Simple deserialization module based on dataclasses to decode JSON-like objects.

The entry point to this module is the ``Deserializer`` class, that is
responsible for embedding the knowledge of how to deserialize each type. Its
``deserialize`` method is the one to deserialize individual JSON-like objects.
Basic types (booleans, integers, strings) as well as lists and dicts are
handled automatically (dict keys must be strings)::

    d = Deserializer()

    # Returns True
    d.deserialize("<in-memory string>", bool, True)

    # <DeserializationError: <in-memory string>: boolean expected, got a str>
    d.deserialize("<in-memory string>", bool, "foo")

    # <DeserializationError: <in-memory string>[1]: string expected, got a int>
    d.deserialize("<in-memory string>", list[str], ["a", 1])

Dataclasses are where this module has added value::

    @dataclass
    class MyDC:
        a: str
        b: int = 42

    d.add_type(MyDC)

    # MyDC("foo", 42)
    d.deserialize("<in-memory string>", MyDC, {"a": "foo"})

    # MyDC("foo", 0)
    d.deserialize("<in-memory string>", MyDC, {"a": "foo", "b": 0})

    # <DeserializationError: <in-memory string>: missing 'a' entry>
    d.deserialize("<in-memory string>", MyDC, {"b": 0})

This supports a narrow subset of valid type annotations for dataclass fields::

* "atomic" deserializable types (basic types, other dataclasses);
* optional types (precisely ``T | None`` where ``T`` is a deserialized type
  itself);
* lists and dicts (``list[T]``, ``dict[str, T]``).

Note that the deserialization process will raise a ``DeserializationError``
exception in case of invalid JSON-like object to deserialize, but will raise
``TypeError`` exceptions if it' the given types that are invalid.

Non-dataclasses, or dataclasses that needs custom deserialization logic must be
registered with the deserialization callback with the ``add_type`` method::

    class QualifiedName:
        def __init__(self, *names: str):
            self.names = names

        @staticmethod
        def _deserialize(
            deserializer: Deserializer,
            context: str,
            obj: object,
        ) -> QualifiedName:
            if not isinstance(obj, str):
                deserializer.error(
                    context, f"string expected, got {type(obj)}"
                )
            return QualifiedName(*obj.split("."))

    d.add_type(QualifiedName, QualifiedName._deserializer)

    # QualifiedName(["a", "b"])
    d.deserialize("<in-memory string>", QualifiedName, "a.b")
"""

from __future__ import annotations

import dataclasses
import functools
import inspect
import types
from typing import (
    Any,
    NoReturn,
    Protocol,
    TYPE_CHECKING,
    Type,
    TypeVar,
    get_type_hints,
)


if TYPE_CHECKING:
    from _typeshed import DataclassInstance


_T = TypeVar("_T")


class TypeDeserializer(Protocol):
    """
    Callback that knows how to deserialize data for a specific type.
    """

    def __call__(
        self,
        deserializer: Deserializer,
        context: str,
        obj: object,
    ) -> Any:
        """
        :param deserializer: Deserializer instance that is taking care of the
            deserialization process.
        :param context: Human-readable string used to contextualize error
            messages.
        :param obj: JSON-like object to deserialize.
        """
        ...


class MissingType:
    """
    Type for the ``MISSING`` singleton, used to denote the absence of an
    automatic value in ``DataclassFieldMetadata.auto_value``.
    """

    pass


MISSING = MissingType()


@dataclasses.dataclass(frozen=True)
class DataclassFieldMetadata:
    """
    Metatadata for a dataclass field. Used to tune how a dataclass field is
    processed during deserialization.
    """

    auto_value: Any = MISSING
    """
    If set to any other value than the ``MISSING`` singleton, the field is
    expected not to be present in the JSON-like object, and the given value is
    used instead.
    """

    key: str | None = None
    """
    If not None, the field is expected to be found at the given key in the
    deserialized JSON-like dict. Otherwise, the original field name is used
    instead.
    """

    @classmethod
    def make(
        cls,
        auto_value: Any = MISSING,
        key: str | None = None,
    ) -> dict[str, Any]:
        """
        Shortcut to create the metadata mapping for
        ``dataclasses.field(metadata=...)`` with a single entry for
        ``DataclassFieldMetadata``.
        """
        return {"langkit.utils.deserialization": cls(auto_value, key)}


default_metadata = DataclassFieldMetadata()


class T:
    """
    Namespace class helper to match on ``dict`` and ``list`` in match clauses.
    """

    dict = dict
    list = list


class DeserializationError(Exception):
    """
    Exception raised when the JSON-like object to deserialize contains an
    error.
    """

    pass


class Deserializer:
    """
    Object that takes care of the logistics around deserialization of JSON-like
    objects.
    """

    @dataclasses.dataclass(frozen=True)
    class _TypeDesc:
        """
        Description of a deserializable type.

        Note that this is reserved for actual types. Generic aliases like
        ``list[int]`` are not registered.
        """

        name: str
        """
        Name for the deserializable type.
        """

        type: type
        """
        Reference to the deserializable type itself.
        """

        deserializer: TypeDeserializer
        """
        Callback to run deserialization for this type.
        """

    def __init__(self) -> None:
        self.known_types: dict[type, Deserializer._TypeDesc] = {}
        self.known_types_by_name: dict[str, Deserializer._TypeDesc] = {}

        # Automatically register basic atomic types
        self.add_type(bool, self._deserialize_bool)
        self.add_type(int, self._deserialize_int)
        self.add_type(str, self._deserialize_str)

    def add_type(
        self,
        t: type,
        deserializer: TypeDeserializer | None = None,
    ) -> None:
        """
        Register a deserialization type.

        :param t: Type to register as deserializable.
        :param deserializer: Callback to run the deserialization for this type.
            Can be left to ``None`` iff this is for a dataclass type: the
            default behavior is used in that case.

        Note that dataclasses with default deserialization do not need to be
        registered manually.
        """
        self._add_type(t, deserializer)

    def _add_type(
        self,
        t: type,
        deserializer: TypeDeserializer | None = None,
    ) -> Deserializer._TypeDesc:
        """
        Like ``add_type``, but return the ``_TypeDesc`` instance created for
        this type.
        """
        if deserializer is None:
            if not dataclasses.is_dataclass(t):
                raise TypeError(
                    "Deserializer callback needed for the non-dataclass type"
                    f" {t}"
                )
            deserializer = functools.partial(self._deserialize_dataclass, t)
        assert deserializer is not None

        name = t.__name__
        assert t not in self.known_types
        assert name not in self.known_types_by_name
        entry = Deserializer._TypeDesc(name, t, deserializer)
        self.known_types[t] = entry
        self.known_types_by_name[name] = entry
        return entry

    def _get_type(self, t: type) -> Deserializer._TypeDesc:
        """
        Return the ``_TypeDesc`` instance corresponding to ``t``, or try to
        infer one for it.
        """
        result = self.known_types.get(t)
        return result or self._add_type(t)

    # Deserialization callbacks for basic atomic types

    def _deserialize_bool(
        self,
        deserializer: Deserializer,
        context: str,
        obj: object,
    ) -> bool:
        if isinstance(obj, bool):
            return obj
        else:
            self.type_error(context, obj, "boolean")

    def _deserialize_int(
        self,
        deserializer: Deserializer,
        context: str,
        obj: object,
    ) -> int:
        if isinstance(obj, int):
            return obj
        else:
            self.type_error(context, obj, "integer")

    def _deserialize_str(
        self,
        deserializer: Deserializer,
        context: str,
        obj: object,
    ) -> str:
        if isinstance(obj, str):
            return obj
        else:
            self.type_error(context, obj, "string")

    @staticmethod
    def _bad_type(t: type | types.UnionType, context: str) -> NoReturn:
        """
        Internal helper to complain about invalid deserialization types.
        """
        raise TypeError(f"invalid deserialization type for {context}: {t}")

    @staticmethod
    def _deserialize_dataclass(
        rtype: Type[DataclassInstance],
        deserializer: Deserializer,
        context: str,
        obj: object,
    ) -> DataclassInstance:
        """
        Deserialize a JSON-like object into the given dataclass.

        See ``TypeDeserializer`` for the semantics of the ``deserializer``,
        ``context`` and ``obj`` parameters.
        """
        if not isinstance(obj, dict):
            deserializer.type_error(context, obj, "dict")

        # The "pop" operations below mutate the dict, so create a copy to leave
        # the original dict unchanged.
        input_values = dict(obj)

        # Iterate on each dataclass field to find a value in the input dict
        field_values = {}
        annotations = get_type_hints(obj=rtype)
        for f in dataclasses.fields(rtype):
            metadata = f.metadata.get(
                "langkit.utils.deserialization", default_metadata
            )
            assert isinstance(metadata, DataclassFieldMetadata)

            # Expect a dict entry with the key given by the metadata, or by
            # default the dataclass field name.
            field_name = metadata.key or f.name

            # If instructed by the field metadata, use the automatic value
            if metadata.auto_value is not MISSING:
                field_values[field_name] = metadata.auto_value
                continue

            # Determine the type for this field, as well as whether it is
            # optional.
            field_type = annotations.get(f.name)
            optional = False
            if isinstance(field_type, types.UnionType):
                union_args = field_type.__args__
                if len(union_args) != 2 or union_args[1] not in (
                    None,
                    type(None),
                ):
                    deserializer._bad_type(
                        field_type, f"{rtype.__name__}.{f.name}"
                    )
                field_type = union_args[0]
                optional = True

            # Finally decode the field value if available, then add it to
            # field_values.
            try:
                field_value = input_values.pop(field_name)
            except KeyError:
                # Accept that a field is missing iff a default value is
                # available for it or it is optional (`| None` in its type
                # annotation).
                if (
                    f.default != dataclasses.MISSING
                    or f.default_factory != dataclasses.MISSING
                ):
                    continue
                if optional:
                    field_values[f.name] = None
                    continue
                deserializer.error(context, f"missing {field_name!r} entry")
            else:
                field_values[f.name] = deserializer.deserialize(
                    deserializer.sub_context(context, field_name),
                    field_type,
                    field_value,
                )

        # Extra named values in dicts are forbidden
        if input_values:
            deserializer.error(
                context,
                "invalid entries: {}".format(", ".join(sorted(input_values))),
            )

        return rtype(**field_values)

    def deserialize(
        self,
        context: str,
        rtype: Type[_T],
        obj: object,
    ) -> _T:
        """
        Decode a JSON-like object and return the corresponding Python instance.

        :param context: Terse human-readable string used to communicate the
            location of the sub-object that we are decoding. Used to format
            error mesages.
        :param rtype: Expected type for the result. How the value is decoded is
            determined from it.
        :param obj: JSON-like object to deserialize.
        """
        match (rtype, obj):
            case (types.GenericAlias(__origin__=T.dict), dict(obj)):
                key_type, value_type = rtype.__args__
                if key_type != str:
                    self._bad_type(key_type, "dict key")
                result = {}
                for name, value in obj.items():
                    result[name] = self.deserialize(
                        self.sub_context(context, name),
                        value_type,
                        value,
                    )
                return result

            case (types.GenericAlias(__origin__=T.dict), obj):
                self.type_error(context, obj, "dict")

            case (types.GenericAlias(__origin__=T.list), list(obj)):
                (element_type,) = rtype.__args__
                result = []
                for i, item in enumerate(obj):
                    result.append(
                        self.deserialize(f"{context}[{i}]", element_type, item)
                    )
                return result

            case (types.GenericAlias(__origin__=T.list), obj):
                self.type_error(context, obj, "array")

            case (t, _) if not inspect.isclass(t):
                raise TypeError(f"type expected, got {t!r}")

        return self._get_type(rtype).deserializer(self, context, obj)

    @staticmethod
    def sub_context(context: str, name: str) -> str:
        """
        Return a sub-context.
        """
        if ":" in name:
            name = repr(name)
        return f"{context}:{name}"

    @staticmethod
    def error(context: str, message: str) -> NoReturn:
        """
        Format and propagate a diagnostic error for the given configuration
        context and the given message.
        """
        raise DeserializationError(f"{context}: {message}")

    @classmethod
    def type_error(
        cls,
        context: str,
        value: object,
        exp_type: str,
    ) -> NoReturn:
        """
        Specialization of ``json_error`` to reject an invalid value type found
        in the configuration.

        :param context: Context where the invalid value was found.
        :param value: Invalid value that was found.
        :param exp_type: Human-readable description of the type that was
            expected.
        """
        cls.error(
            context, f"{exp_type} expected, got a {type(value).__name__}"
        )
