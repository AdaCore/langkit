"""
This package contains general utility functions. It is basically the support
library for langkit, aggregating general python utilities that we could not
find in the standard library.
"""

from __future__ import absolute_import, division, print_function

from copy import copy
import os
import shutil


def copy_with(obj, **kwargs):
    """
    Copy an object, and add every key value association passed in kwargs to it.

    :param dict[str, Any] kwargs: key value associations to attach to the
        object.
    :param T obj: The object to copy.
    :rtype: T

    Example use::

        class A(object):
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


def is_same(coll):
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

    def __init__(self, loop):
        super(TopologicalSortError, self).__init__(
            'Dependency loop detected: {}'
            .format(', '.join(str(item) for item in loop)))
        self.loop = loop


def topological_sort(items):
    """
    Yield elements from the ``items`` collection in topological order.

    ``items`` must be an iterable of ``(item, dependencies)`` pairs, where
    ``dependencies`` is an iterable of the same type as ``items``. The result
    is deterministic.

    This raises a ``TopologicalSortError`` if ``items`` contains a dependency
    loop.

    :type items: list[(T, list[T])]
    :rtype: generator[T]
    """
    # Result is the sequence we return (its order matters) whereas satisfied is
    # here only to check for membership to the result.
    result = []
    satisfied = set()

    deps_map = {item: sorted(dependencies) for item, dependencies in items}

    def process(item, current_chain):
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
    def __get__(self, cls, owner):
        return classmethod(self.fget).__get__(None, owner)()


def inherited_property(parent_getter, default_val=False):
    """
    Decorate a method, from an object with a parent, so that if the returned
    value is None, it will query it on the parent.
    """
    def impl(fn):
        def internal(self, *args, **kwargs):
            val = fn(self, *args, **kwargs)
            if val is not None:
                return val
            else:
                parent = parent_getter(self)
                return (internal(parent, *args, **kwargs)
                        if parent else default_val)

        return property(internal)

    return impl


def ensure_clean_dir(dirname):
    """
    Remove any existing file/directory at ``dirname`` and create an empty
    directory instead.

    :param str dirname: Name of the directory to create.
    """
    if os.path.exists(dirname):
        shutil.rmtree(dirname)
    os.makedirs(dirname)


try:
    from contextlib import nested  # Python 2
except ImportError:
    from contextlib import ExitStack, contextmanager

    @contextmanager
    def nested(*contexts):
        """
        Reimplementation of nested in python 3.
        """
        with ExitStack() as stack:
            for ctx in contexts:
                stack.enter_context(ctx)
            yield contexts

# pyflakes off
from langkit.utils.colors import *
from langkit.utils.logging import *
from langkit.utils.memoization import *
from langkit.utils.types import *
# pyflakes on
