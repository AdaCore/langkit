from __future__ import annotations

from typing import Any, Callable, List, Optional, TypeVar, cast


F = TypeVar('F', bound=Callable[..., Any])


def memoized(func: F, pre_cache_miss: Optional[F] = None) -> F:
    """
    Decorator to memoize a function.
    This function must be passed only hashable arguments.

    :param func: The function to decorate.
    :param pre_cache_miss: The function to call in case of a cache miss, before
        `func` is called. Can be used for example to prepare a value in the
        memoization cache to break infinite recursions.
    """
    cache: dict = {}

    all_caches: List[dict] = getattr(memoized, "caches", None)
    if all_caches is None:
        all_caches = []
        setattr(memoized, "caches", all_caches)

    all_caches.append(cache)

    def wrapper(*args, **kwargs):
        key = (args, tuple(kwargs.items()))
        try:
            result = cache[key]
        except KeyError:
            if pre_cache_miss is not None:
                cache[key] = pre_cache_miss(*args, **kwargs)

            result = func(*args, **kwargs)
            cache[key] = result
        return result

    return cast(F, wrapper)


def memoized_with_default(default_value):
    """
    Decorator to memoize a function.

    Unlike for the 'memoized' decorator, it prevents infinite recursions by
    using the provided `default_value`, which will be used as the cached value
    when an infinite recursion is detected.

    :param default_value: The value to use to break infinite recursions.
    """
    def memoize(func):
        return memoized(func, lambda *args, **kwargs: default_value)

    return memoize


def self_memoized(func):
    """
    Like `memoized`, but specific to instance methods and offers a
    instance-specific reset switch.

    :param func: The instance method to decorate.
    """

    cache_name = '_cache_{}'.format(func.__name__)

    def wrapper(self, *args, **kwargs):
        # Install the self-specific cache, if needed
        cache = getattr(self, cache_name, {})
        setattr(self, cache_name, cache)

        key = (args, tuple(kwargs.items()))
        try:
            result = cache[key]
        except KeyError:
            result = func(self, *args, **kwargs)
            cache[key] = result
        return result

    def reset(self):
        setattr(self, cache_name, {})

    wrapper.reset = reset

    return wrapper


def reset_memoized():
    for cache in getattr(memoized, "caches", []):
        cache.clear()
