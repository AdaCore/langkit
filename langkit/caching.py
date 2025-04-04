import hashlib
import json


class Cache:
    """
    General purpose content cache.

    Generating and building libraries can be quite long. This cache class is an
    attempt to reduce the time to do this.
    """

    db: dict[str, str]

    def __init__(self, cache_file: str) -> None:
        """Load the cache from `cache_file`, or create a new cache if new.

        :param cache_file: Name of the file that contains cache data from
            another run.
        """
        self.cache_file = cache_file
        self._load()

    def _load(self) -> None:
        try:
            f = open(self.cache_file, "r")
        except IOError:
            self.db = {}
        else:
            with f:
                self.db = json.load(f)

    def is_stale(self, key: str, content: str) -> bool:
        """Return whether the `key` cache entry is staled.

        The first time this is called for some `key`, always return False. The
        next times, return whether `content` is the same as the previous time.

        :param str key: Key for the cache entry to test.
        :param str content: Content for the cache entry to test.
        :rtype: bool
        """
        m = hashlib.md5()
        m.update(content.encode("utf-8"))
        new_hash = m.hexdigest()

        try:
            old_hash = self.db[key]
        except KeyError:
            stale = True
        else:
            stale = old_hash != new_hash

        self.db[key] = new_hash
        return stale

    def save(self) -> None:
        """Save the content of the cache to a file."""
        with open(self.cache_file, "w") as f:
            json.dump(self.db, f)
