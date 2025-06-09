import hashlib
import json


class Cache:
    """
    General purpose content cache.

    Generating and building libraries can be quite long. This cache class is an
    attempt to reduce the time to do this.
    """

    db: dict[str, object]

    def __init__(self, cache_file: str) -> None:
        """Load the cache from `cache_file`, or create a new cache if new.

        :param cache_file: Name of the file that contains cache data from
            another run.
        """
        self.cache_file = cache_file
        self._load()

    def _load(self) -> None:
        # If for any reason we cannot read the cache, just start from an empty
        # one.
        try:
            f = open(self.cache_file, "r")
            with f:
                self.db = json.load(f)
        except (IOError, json.JSONDecodeError):
            self.db = {}

    def _content_hash(self, content: str) -> str:
        """
        Return the hash of the given string.
        """
        m = hashlib.md5()
        m.update(content.encode("utf-8"))
        return m.hexdigest()

    def _file_hash(self, filename: str) -> str:
        """
        Return the hash of the content of the given file (assumed to be
        readable).
        """
        with open(filename, "rb") as f:
            return hashlib.file_digest(f, "md5").hexdigest()

    def has_same_value(self, key: str, value: object) -> bool:
        """
        Return whether the cached value for ``key`` is equal to ``value``.
        Update the cache entry accordingly. ``value`` must be
        JSON-serializable.
        """
        try:
            old_value = self.db[key]
        except KeyError:
            result = False
        else:
            result = old_value == value

        self.db[key] = value
        return result

    def has_same_hash(self, key: str, content: str) -> bool:
        """
        Return whether ``content`` has the same hash as previously. Update the
        cache entry accordingly.
        """
        return self.has_same_value(key, self._content_hash(content))

    def has_file_changed(self, filename: str) -> bool:
        """
        Return whether the given filename changed since the last time we
        checked. Update the cache entry accordingly.
        """
        try:
            return not self.has_same_value(filename, self._file_hash(filename))
        except IOError:
            self.db.pop(filename)
            return True

    def set_entry(self, key: str, value: object) -> None:
        """
        Add a cache entry that associates the given key and the given value.
        """
        json.dumps(value)
        self.db[key] = value

    def set_entry_from_hash(self, key: str, content: str) -> None:
        """
        Add a cache entry that associates the given key and the hash of the
        given string.
        """
        self.set_entry(key, self._content_hash(content))

    def set_entry_from_file_hash(self, filename: str) -> None:
        """
        Add a cache entry that associates the given key and the hash of the
        content of the given file (assumed to be readable).
        """
        return self.set_entry(filename, self._file_hash(filename))

    def save(self) -> None:
        """Save the content of the cache to a file."""
        with open(self.cache_file, "w") as f:
            # For readability, sort dict keys (most are filenames) and format
            # indented entries (we'll have one key/hash entry per line). This
            # is meant to make debugging easier.
            json.dump(self.db, f, indent=2, sort_keys=True)
