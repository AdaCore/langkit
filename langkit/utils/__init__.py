"""
This package contains general utility functions. It is basically the support
library for langkit, aggregating general python utilities that we could not
find in the standard library.
"""

from __future__ import absolute_import, division, print_function

from copy import copy


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


def topological_sort(items):
    """
    'items' is an iterable of (item, dependencies) pairs, where 'dependencies'
    is an iterable of the same type as 'items'.

    If 'items' is a generator rather than a data structure, it should not be
    empty. Passing an empty generator for 'items' (zero yields before return)
    will cause topological_sort() to raise TopologicalSortFailure.

    An empty iterable (e.g. list, tuple, set, ...) produces no items but
    raises no exception.
    """
    provided = set()
    while items:
        remaining_items = []
        emitted = False
        for item, dependencies in items:
            dependencies = set(dependencies)
            if provided.issuperset(dependencies):
                yield item
                provided.add(item)
                emitted = True
            else:
                remaining_items.append((item, dependencies))
        assert emitted
        items = remaining_items


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


# pyflakes off
from langkit.utils.colors import *
from langkit.utils.logging import *
from langkit.utils.memoization import *
from langkit.utils.types import *
# pyflakes on
