from __future__ import annotations

from collections import defaultdict
from contextlib import contextmanager
from functools import partial
import sys
from typing import Callable, ClassVar, Iterator, Protocol, TYPE_CHECKING

from langkit.utils.colors import Colors, col


if TYPE_CHECKING:
    from langkit.utils.types import _P, _T


class _Logger(Protocol):
    def __call__(
        self, message: str, *args: object, **kwargs: object
    ) -> None: ...


class Log:
    """
    This class is a relatively generic logging handler. It includes decorators
    and context managers to make logging easier.
    """

    enabled: ClassVar[dict[str, bool]] = defaultdict(bool)
    nesting_level: ClassVar[int] = 0

    @staticmethod
    def log_return(
        trace: str,
    ) -> Callable[[Callable[_P, _T]], Callable[_P, _T]]:
        """
        Decorates a function, so that its return value is logged.
        """

        def decorator(fn: Callable[_P, _T]) -> Callable[_P, _T]:
            def internal(*args: _P.args, **kwargs: _P.kwargs) -> _T:
                ret = fn(*args, **kwargs)
                Log.log(trace, "{} returned {}".format(fn.__name__, ret))
                return ret

            return internal

        return decorator

    @staticmethod
    def log(trace: str, message: str, *args: object, **kwargs: object) -> None:
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

    def logger(self, trace: str) -> _Logger:
        """
        Return a logger function for the given trace.
        """
        return partial(self.log, trace)

    @staticmethod
    def enable(trace: str) -> None:
        Log.enabled[trace] = True

    @staticmethod
    @contextmanager
    def nest() -> Iterator[None]:
        """
        Context manager that will increase the nesting level by 1.
        """
        Log.nesting_level += 1
        yield
        Log.nesting_level -= 1

    @staticmethod
    def recursive(fn: Callable[_P, _T]) -> Callable[_P, _T]:
        """
        Decorator for recursive functions that use logging. Every call to the
        function will increase the nesting level by 1.
        """

        def internal(*args: _P.args, **kwargs: _P.kwargs) -> _T:
            with Log.nest():
                return fn(*args, **kwargs)

        return internal
