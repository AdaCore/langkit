"""
This package contains general utility functions. It is basically the support
library for langkit, aggregating general python utilities that we could not
find in the standard library.
"""

from __future__ import annotations

import argparse
from contextlib import ExitStack, contextmanager
from copy import copy
from enum import Enum
import os
import shlex
import shutil
from typing import (
    Any, Callable, ContextManager, Dict, Iterable, Iterator, List, Optional,
    Protocol, Sequence, TYPE_CHECKING, Type, TypeVar
)


if TYPE_CHECKING:
    from langkit.utils.types import _P, _T


T = TypeVar("T")
T_contra = TypeVar("T_contra", contravariant=True)


class Comparable(Protocol[T_contra]):
    def __eq__(self, other: object) -> bool: ...
    def __lt__(self, other: T_contra) -> bool: ...


ComparableT = TypeVar("ComparableT", bound=Comparable)


class LibraryType(Enum):
    static = "static"
    static_pic = "static-pic"
    relocatable = "relocatable"

    @classmethod
    def add_argument(cls, parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            '--library-types',
            type=parse_list_of_choices(cls),
            default=[cls.relocatable],
            help="Comma-separated list of library types to build (relocatable,"
            " static-pic and static). By default, build only shared"
            " libraries."

        )


class BuildMode(Enum):
    dev = "dev"
    prod = "prod"
    prof = "prof"


def copy_with(obj: T, **kwargs: object) -> T:
    """
    Copy an object, and add every key value association passed in kwargs to it.

    :param kwargs: Key value associations to attach to the object.
    :param obj: The object to copy.

    Example use::

        class A:
            def __init__(self, a, b):
                self.a = a
                self.b = b

        inst = A(12, 15)
        inst_2 = copy_with(inst, a=16)
        print inst_2.a
        # Prints "16"
        print inst_2.b
        # Prints "12"
    """
    c = copy(obj)

    for k, v in kwargs.items():
        setattr(c, k, v)

    return c


def is_same(coll: Iterable[object]) -> bool:
    """
    Helper function, returns True if every element in collection hashes to
    the same value.
    """
    return len(set(coll)) == 1


class TopologicalSortError(Exception):
    """
    Exception to be raised when a topological sort fails due to a dependency
    loop. This exception stores the chain of items that form the loop as a
    list.
    """

    def __init__(self, loop: Iterable[object]):
        super().__init__(
            'Dependency loop detected: {}'
            .format(', '.join(str(item) for item in loop))
        )
        self.loop = loop


def topological_sort(
    items: list[tuple[Comparable, list[Comparable]]]
) -> list[Comparable]:
    """
    Yield elements from the ``items`` collection in topological order.

    ``items`` must be an iterable of ``(item, dependencies)`` pairs, where
    ``dependencies`` is an iterable of the same type as ``items``. The result
    is deterministic.

    This raises a ``TopologicalSortError`` if ``items`` contains a dependency
    loop.
    """
    # Result is the sequence we return (its order matters) whereas satisfied is
    # here only to check for membership to the result.
    result = []
    satisfied = set()

    deps_map = {item: sorted(dependencies) for item, dependencies in items}

    def process(item: Comparable, current_chain: list[Comparable]) -> None:
        """
        Add ``item`` to the result, processing its dependencies first if
        needed. Raise a TopologicalSortError if we detect a loop.
        ``current_chain`` contains the sequence of items we're recursively
        trying to add to the result.
        """
        # Check for dependency loops
        for i, it in enumerate(current_chain):
            if item == it:
                raise TopologicalSortError(current_chain[i:])
        current_chain = current_chain + [item]

        # If this item is already satisfied, there is nothing to do here
        if item in satisfied:
            return

        # Make sure we satisfy dependencies before sending this item
        for dep in deps_map[item]:
            process(dep, current_chain)

        result.append(item)
        satisfied.add(item)

    # Process the queue until all items are consumed. Reverse-sort the input
    # queue so that 1) the output is deterministic and 2) independent types are
    # sorted by name.
    queue = list(reversed(sorted(deps_map)))
    while queue:
        process(queue.pop(), [])

    return result


class classproperty(property):
    """
    Decorator to have a class property, equivalent to::

        @classmethod
        @property

    If the above was valid.
    """
    def __get__(self, cls, owner):  # type: ignore
        return classmethod(self.fget).__get__(None, owner)()


def inherited_property(
    parent_getter: Callable[_P, _T],
    default_val: bool = False,
) -> Callable[[Callable[_P, _T]], property]:
    """
    Decorate a method, from an object with a parent, so that if the returned
    value is None, it will query it on the parent.
    """
    def impl(fn: Callable[_P, _T]) -> property:
        def internal(*args: _P.args, **kwargs: _P.kwargs) -> _T:
            val = fn(*args, **kwargs)
            if val is not None:
                return val
            else:
                parent = parent_getter(args[0])
                return (internal(parent, *args[1:], **kwargs)
                        if parent else default_val)

        return property(internal)

    return impl


def ensure_clean_dir(dirname: str) -> None:
    """
    Remove any existing file/directory at ``dirname`` and create an empty
    directory instead.

    :param dirname: Name of the directory to create.
    """
    if os.path.exists(dirname):
        shutil.rmtree(dirname)
    os.makedirs(dirname)


def copy_to_dir(filename: str, dirname: str) -> None:
    """
    Copy the ``filename`` regular file to the ``dirname`` directory.

    :param filename: File to copy.
    :param dirname: Destination directory.
    """
    shutil.copy(filename, os.path.join(dirname, os.path.basename(filename)))


@contextmanager
def nested(
    *contexts: ContextManager[Any]
) -> Iterator[Sequence[ContextManager[Any]]]:
    """
    Reimplementation of nested in python 3.
    """
    with ExitStack() as stack:
        for ctx in contexts:
            stack.enter_context(ctx)
        yield contexts


def get_cpu_count() -> int:
    # The "multiprocessing" module is not available on GNATpython's
    # distribution for PPC AIX and the "cpu_count" is not available on Windows:
    # give up on default parallelism on these platforms.
    try:
        import multiprocessing
        return multiprocessing.cpu_count()
    except (ImportError, NotImplementedError):
        return 1


def add_to_path(env: Dict[str, str], name: str, item: str) -> None:
    """
    Add ``item`` to the ``name`` path environment variable in ``env``.
    """
    # GDB helpers import this unit, but they do not necessarily have access to
    # funcy, so use a local import.
    from funcy import keep

    env[name] = os.path.pathsep.join(keep([item, env.get(name, '')]))


def format_setenv(name: str, path: str) -> str:
    """
    Return a Bourne shell command to prepend ``path`` to the ``name``
    environment variable.
    """
    return (
        f'{name}={shlex.quote(path)}"{os.pathsep}${name}";'
        f" export {name}"
    )


def parse_choice(choice_enum: Type[Enum]) -> Callable[[str], Enum]:
    """
    Helper for argparse. When used as an argument for the ``type`` parameter of
    argparse options, this allows to parse a string representing an enum member
    into an enum member.
    """

    def convert(s: str) -> Enum:
        try:
            return choice_enum(s)
        except KeyError:
            raise ValueError(f"Wrong value for {choice_enum.__name__}: {s}")

    return convert


def parse_list_of_choices(
    choice_enum: Type[Enum]
) -> Callable[[str], List[Enum]]:
    """
    Helper for argparse. When used as an argument for the ``type`` parameter of
    argparse options, this allows to parse a list of choices of the form
    A,B,C,...,Z, and to automatically transform individual elements of the list
    into ``choice_enum`` members.
    """

    convert = parse_choice(choice_enum)

    def parse_list(arg: str) -> List[Enum]:
        ret = [convert(c.strip()) for c in arg.split(",")]
        if len(ret) != len(set(ret)):
            raise ValueError(
                f"Can't have the same value twice for {choice_enum.__name__}"
            )
        return ret

    return parse_list


def parse_cmdline_args(args: Optional[List[str]]) -> List[str]:
    """
    Considering a list of shell-formatted argument lists, return the
    corresponding flattened list of single arguments.

    For instance::

        >>> parse_args(["foo bar", "'hello world'"])
        ["foo", "bar", "hello world"]

    If passed ``None``, just return an empty list.
    """
    return (sum((shlex.split(a) for a in args), [])
            if args
            else [])


# pyflakes off
from langkit.utils.colors import *
from langkit.utils.logging import *
from langkit.utils.memoization import *
from langkit.utils.types import *
# pyflakes on
