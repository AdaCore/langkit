"""
This file contains general utility functions. It is basically the support
library for langkit, aggregating general python utilities that we could not
find in the standard library.
"""

from __future__ import absolute_import

from copy import copy
from itertools import takewhile


class StructEq(object):
    """
    Mixin for structural equality.
    """
    def __eq__(self, other):
        if type(other) is type(self):
            if hasattr(self, "_eq_keys"):
                eq_keys = self._eq_keys
            elif hasattr(self, "_excl_eq_keys"):
                eq_keys = set(self.__dict__.keys()) ^ set(self._excl_eq_keys)
            else:
                return self.__dict__ == other.__dict__

            return all(v == other.__dict__[k] for k, v in self.__dict__
                       if k in eq_keys)

        return False


def unescape(char):
    """
    Unescape char if it is escaped.

    :param str char: An eventually escaped character.
    :rtype: str
    """
    if char[0] == "\\":
        return char[1:]
    return char


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


class Colors:
    """
    Utility escape sequences to color output in terminal.
    """
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'


def col(msg, color):
    return "{0}{1}{2}".format(color, msg, Colors.ENDC)


def printcol(msg, color):
    """
    Utility print function that will print `msg` in color `color`.
    :param basestring msg: The message to print.
    :param basestring color: The color escape sequence from the enum class
        Colors which represents the color to use.
    :return: The color-escaped string, resetting the color to blank at the end.
    :rtype: basestring
    """
    print col(msg, color)


def common_ancestor(*classes):
    """
    Return the bottom-most common parent class for all `classes`.

    :param classes: The classes for which we are searching a common ancestor.
    :type classes: list[types.ClassType]
    :rtype: types.ClassType
    """
    def rmro(klass):
        return reversed(klass.mro())

    result = list(takewhile(lambda a: len(set(a)) == 1,
                            zip(*map(rmro, classes))))[-1][0]
    return result


def memoized(func):
    """
    Decorator to memoize a function.
    This function must be passed only hashable arguments.

    :param func: The function to decorate.
    """
    cache = {}

    def wrapper(*args, **kwargs):
        key = (args, tuple(kwargs.items()))
        try:
            result = cache[key]
        except KeyError:
            result = func(*args, **kwargs)
            cache[key] = result
            result = result
        return result

    return wrapper


def type_check_exact(klass):
    """
    Return a predicate that will return true if its parameter is exactly egal
    to `klass`.

    :param type klass: Class to check against.
    :rtype: (T) -> bool
    """
    return lambda t: t and t == klass


def type_check(klass):
    """
    Return a predicate that will return true if its parameter is a subclass
    of `klass`.
    :param type klass: Class to check against.
    :rtype: (T) -> bool
    """
    return lambda t: t and issubclass(t, klass)


def type_check_instance(klass):
    """
    Return a predicate that will return true if its parameter is a subclass
    of `klass`.
    :param type klass: Class to check against.
    :rtype: (T) -> bool
    """
    return lambda t: isinstance(t, klass)


def dispatch_on_type(type, type_to_action_assocs, exception_msg=""):
    """
    Dispatch on the type parameter, execute the corresponding action
    depending on the type. Every type in type_to_action_assocs will be
    tested in turn. If type is a subtype of one of them, then the
    corresponding action will be executed.

    :param type: The type to dispatch upon.

    :param type_to_action_assocs: An association of types to actions that
        returns something.
    :type type_to_action_assocs: list[(type, (type) -> T)]

    :param str exception_msg: The exception message for the exception that
    will be raised.

    :rtype: T
    """
    for target_type, action in type_to_action_assocs:
        if issubclass(type, target_type):
            return action(type)
    raise Exception(exception_msg)


def assert_type(obj, typ):
    """
    Ensure that obj is a subtype or subclass of type, depending on whether
    obj is a type or an instance. Return obj. This allows a few things
    compared to regular asserts:

    1. The type of the return is correctly inferred by Pycharm.
    2. You can use a "fluent interface" style for style checks, using::

        b = assert_type(c, int) + 15

    rather than::

        assert isinstance(c, int)
        b = c + 15

    3. Provides a relatively informative error message by default.

    :param Any obj: The object to check.
    :param T typ: Type parameter. The expected type of obj.
    :rtype: T
    """
    if issubclass(type(obj), type):
        assert issubclass(obj, typ), (
            "Wrong base type for type {}, expected {}".format(obj, typ)
        )
    else:
        assert isinstance(obj, typ), (
            "Wrong type for object {}, expected {}, found {}".format(
                obj, typ, type(obj)
            )
        )
    return obj


class TypeSet(object):
    """
    This class is an helper for when you need to check wether an abstract
    operation was fulfilled for a whole hierarchy of classes, given that if
    the operation is fulfilled for all subclasses, then we consider that it is
    done for the parent class.

    This allows to automate the logic of, for example, checking if abstract
    properties are overridden properly, or if a match expression handles all
    cases.
    """

    def __init__(self):
        # Working set of ASTNode subclasses for the types that are covered by
        # matchers. Updated as we go through the list of matchers.
        self.matched_types = set()

    def include(self, t):
        """
        Include a class and all of its subclasses.

        If t is the last subclass for some base class, this adds the parent
        subclass. This makes sense as once all concrete subclasses of the
        abstract A type are handled, it is true that A is handled.

        Return whether t was already present in self.

        :param ASTNode|None t: Type parameter.
        :rtype: bool
        """
        if t in self.matched_types:
            return True

        # Include "t" and all its subclasses
        self.matched_types.add(t)
        for child in t.subclasses:
            self.include(child)

        # Include its parents if all their children are matched
        parents = list(t.get_inheritance_chain())[:-1]
        for parent in reversed(parents):
            if parent in self.matched_types:
                break
            subclasses = set(parent.subclasses)
            if not subclasses.issubset(self.matched_types):
                break
            # If we reach this point, all parent's subclasses are matched,
            # so we can state that parent itself is always matched.
            self.matched_types.add(parent)

        return False

    def unmatched_types(self, t):
        """
        Return the set of t subclasses that are not matched by any matcher.

        Omit subclasses when none of them are matched: only the parent is
        returned in this case, so that we don't flood users with whole
        hierarchies of classes.

        :param ASTNode t: Type parameter.
        :rtype: set[ASTNode]
        """
        if t in self.matched_types:
            return set()

        subclasses = set(t.subclasses)
        if subclasses & self.matched_types:
            result = set()
            for cls in subclasses:
                result.update(self.unmatched_types(cls))
        else:
            result = {t}
        return result
