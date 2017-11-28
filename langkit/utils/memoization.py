from __future__ import absolute_import, division, print_function


def memoized(func):
    """
    Decorator to memoize a function.
    This function must be passed only hashable arguments.

    :param func: The function to decorate.
    """
    cache = {}

    if not getattr(memoized, "caches", None):
        memoized.caches = []

    memoized.caches.append(cache)

    def wrapper(*args, **kwargs):
        key = (args, tuple(kwargs.items()))
        try:
            result = cache[key]
        except KeyError:
            result = func(*args, **kwargs)
            cache[key] = result
        return result

    return wrapper


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
