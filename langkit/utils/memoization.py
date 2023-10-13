from __future__ import annotations

from typing import Callable, Optional, TYPE_CHECKING, TypeVar


if TYPE_CHECKING:
    from langkit.utils.types import _P, _T


_Self = TypeVar("_Self")


def memoized(
    func: Callable[_P, _T],
    pre_cache_miss: Optional[Callable[_P, _T]] = None,
) -> Callable[_P, _T]:
    """
    Decorator to memoize a function.
    This function must be passed only hashable arguments.

    :param func: The function to decorate.
    :param pre_cache_miss: The function to call in case of a cache miss, before
        `func` is called. Can be used for example to prepare a value in the
        memoization cache to break infinite recursions.
    """
    cache: dict[object, _T] = {}

    try:
        all_caches: list[dict[object, _T]] = getattr(memoized, "caches")
    except AttributeError:
        all_caches = []
        setattr(memoized, "caches", all_caches)

    all_caches.append(cache)

    def wrapper(*args: _P.args, **kwargs: _P.kwargs) -> _T:
        key = (args, tuple(kwargs.items()))
        try:
            result = cache[key]
        except KeyError:
            if pre_cache_miss is not None:
                cache[key] = pre_cache_miss(*args, **kwargs)

            result = func(*args, **kwargs)
            cache[key] = result
        return result

    return wrapper


def memoized_with_default(
    default_value: _T
) -> Callable[[Callable[_P, _T]], Callable[_P, _T]]:
    """
    Decorator to memoize a function.

    Unlike for the 'memoized' decorator, it prevents infinite recursions by
    using the provided `default_value`, which will be used as the cached value
    when an infinite recursion is detected.

    :param default_value: The value to use to break infinite recursions.
    """
    def memoize(func: Callable[_P, _T]) -> Callable[_P, _T]:
        return memoized(func, lambda *args, **kwargs: default_value)

    return memoize


def self_memoized(func: Callable[_P, _T]) -> Callable[_P, _T]:
    """
    Like `memoized`, but specific to instance methods and offers a
    instance-specific reset switch.

    :param func: The instance method to decorate.
    """

    cache_name = '_cache_{}'.format(func.__name__)

    def wrapper(*args: _P.args, **kwargs: _P.kwargs) -> _T:
        self = args[0]
        args_without_self = args[1:]

        # Install the self-specific cache, if needed
        cache = getattr(self, cache_name, {})
        setattr(self, cache_name, cache)

        key = (args_without_self, tuple(kwargs.items()))
        try:
            result = cache[key]
        except KeyError:
            result = func(*args, **kwargs)
            cache[key] = result
        return result

    def reset(self: object) -> None:
        setattr(self, cache_name, {})

    setattr(wrapper, "reset", reset)

    return wrapper


def reset_memoized() -> None:
    for cache in getattr(memoized, "caches", []):
        cache.clear()
