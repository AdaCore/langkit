from __future__ import absolute_import, division, print_function

from collections import defaultdict
from contextlib import contextmanager
from functools import partial
import sys

from langkit.utils.colors import Colors, col


class Log():
    """
    This class is a relatively generic logging handler. It includes decorators
    and context managers to make logging easier.
    """
    enabled = defaultdict(bool)
    nesting_level = 0

    @staticmethod
    def log_return(trace):
        """
        Decorates a function, so that its return value is logged.
        """
        def decorator(fn):
            def internal(*args, **kwargs):
                ret = fn(*args, **kwargs)
                Log.log(trace, "{} returned {}".format(fn.__name__, ret))
                return ret
            return internal
        return decorator

    @staticmethod
    def log(trace, message, *args, **kwargs):
        """
        Log arguments on given trace.
        """
        if Log.enabled[trace]:
            sys.stdout.write(col("[{}] ".format(trace), Colors.YELLOW))
            if Log.nesting_level:
                sys.stdout.write("  " * Log.nesting_level)
            if args or kwargs:
                message = message.format(*args, **kwargs)
            print(message)

    def logger(self, trace):
        """
        Return a logger function for the given trace.
        """
        return partial(self.log, trace)

    @staticmethod
    def enable(trace):
        Log.enabled[trace] = True

    @staticmethod
    @contextmanager
    def nest():
        """
        Context manager that will increase the nesting level by 1.
        """
        Log.nesting_level += 1
        yield
        Log.nesting_level -= 1

    @staticmethod
    def recursive(fn):
        """
        Decorator for recursive functions that use logging. Every call to the
        function will increase the nesting level by 1.
        """
        def internal(*args, **kwargs):
            with Log.nest():
                return fn(*args, **kwargs)
        return internal
