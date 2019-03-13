from __future__ import absolute_import, division, print_function

import os.path
import unicodedata


# We don't want to restrict the range of Unicode characters depending on
# whether the Python interpreter was built to use UCS-2 or UCS-4, so roll our
# own constant, to simulate the effect of PEP 393.
MAXUNICODE = 0x10FFFF


def format_char(char):
    ochar = ord(char)
    return ('\\U{:x}'.format(ochar)
            if ochar < ord(' ') or ord('~') < ochar else
            str(char))


def format_char_ranges(ranges):
    def format_interval(interval):
        if interval is None:
            return '...'
        else:
            l, h = interval
            return (format_char(l)
                    if l == h else
                    '{}:{}'.format(format_char(l), format_char(h)))
    return '[{}]'.format(', '.join(format_interval(interval)
                                   for interval in ranges))


class CharSet(object):
    """
    Set of characters.
    """

    _repr_ellipsis = True
    """
    Whether __repr__ should put an ellipsis for characters beyond ASCII.

    :bool: True
    """

    def __init__(self, *items):
        self.ranges = []
        """
        Sorted, disjoint and as merged as possible list of ranges for character
        ordinals in the set. Both bounds are included in the ranges.

        :type: list[(int, int)]
        """

        for item in items:
            if isinstance(item, basestring):
                self.add(item)
            elif isinstance(item, tuple):
                low, high = item
                self.add_range(low, high)
            else:
                raise TypeError('Invalid CharSet item: {}'.format(repr(item)))

    @classmethod
    def from_int(cls, item):
        return cls.from_int_ranges((item, item))

    @classmethod
    def from_int_ranges(cls, *items):
        result = cls()
        for l, h in items:
            result.add_int_range(l, h)
        return result

    def __repr__(self):
        ranges = []
        for l, h in self.ranges:
            if not self._repr_ellipsis or (l <= 127 and h <= 127):
                ranges.append((unichr(l), unichr(h)))
            else:
                ranges.append(None)
                break
        return format_char_ranges(ranges)

    def __hash__(self):
        return hash(tuple(self.ranges))

    def __eq__(self, other):
        return isinstance(other, CharSet) and self.ranges == other.ranges

    def __ne__(self, other):
        return not (self == other)

    def __le__(self, other):
        assert isinstance(other, CharSet)
        return self.ranges < other.ranges

    def __contains__(self, char):
        """
        Return whether a character is in this set.

        :type char: unicode
        :rtype: bool
        """
        char = ord(char)
        found, _ = self._lookup(char)
        return found

    def __or__(self, other):
        """
        Return the union of two character sets.

        :type other: CharSet
        :rtype: CharSet
        """
        assert isinstance(other, CharSet)
        result = CharSet()
        for cs in (self, other):
            for l, h in cs.ranges:
                result.add_int_range(l, h)
        return result

    @property
    def is_empty(self):
        return not self.ranges

    @property
    def ada_ranges(self):
        """
        Return an Ada code excerpt to check that a character belong to this
        set. This returns 'X' so that this check can be implemented the
        following way::

            if Char in X then

        :rtype: str
        """

        def format_char(char):
            return ("Character_Type'Val (16#{:0x}#)".format(char)
                    if char < ord(' ') or ord('~') < char else
                    "'{}'".format(unichr(char)))

        return ' | '.join(
            (format_char(l) if l == h else
             '{} .. {}'.format(format_char(l), format_char(h)))
            for l, h in self.ranges
        )

    @classmethod
    def any_char(cls):
        result = cls()
        result.add_int_range(0, MAXUNICODE)
        return result

    @property
    def negation(self):
        """
        Return a character set that contains everything that is not in
        ``self``.

        :rtype: CharSet
        """
        result = CharSet()

        last = None
        for l, h in self.ranges:
            if last is None:
                if l > 0:
                    result.add_int_range(0, l - 1)
            else:
                result.add_int_range(last + 1, l - 1)
            last = h
        result.add_int_range(last + 1, MAXUNICODE)
        return result

    @property
    def split_ascii_subsets(self):
        """
        Return two character sets: one for the ASCII subset in self, and the
        other for the non-ASCII subset.

        :rtype: (CharSet, CharSet)
        """
        ascii = CharSet()
        non_ascii = CharSet()

        def add_range(char_set, l, h):
            char_set.add_int_range(l, h)

        for l, h in self.ranges:
            if h < 128:
                ascii.add_int_range(l, h)
            elif l < 128:
                ascii.add_int_range(l, 127)
                non_ascii.add_int_range(128, h)
            else:
                non_ascii.add_int_range(l, h)

        return (ascii, non_ascii)

    def overlaps_with(self, other):
        """
        Return whether this overlaps with ``other``.

        :type other: CharSet
        :rtype: bool
        """
        assert isinstance(other, CharSet)

        def overlap(r1, r2):
            return r1[0] <= r2[1] and r1[1] >= r2[0]

        self_r = list(self.ranges)
        other_r = list(other.ranges)

        while self_r and other_r:
            # Remove the first item from one list if it precedes (without
            # overlapping) the first item from the other list.
            if self_r[0][0] > other_r[0][1]:
                other_r.pop(0)
            elif self_r[0][1] < other_r[0][0]:
                self_r.pop(0)
            else:
                return True
        return False

    def _lookup(self, char):
        """
        Look for the range that contains ``char``.

        If found, return ``(True, index)`` where ``index`` is the index of the
        range that contains ``char``. Otherwise, return ``(False, index)``
        where ``index`` is the index of the range right after the position for
        ``char`` in the range list, plus 1, or 0 if it comes before the first
        range.

        :rtype: (bool, int)
        """
        # Inclusive bounds for the potential index we are looking for
        low = 0
        high = len(self.ranges) - 1

        while low <= high:
            # Inspect the range right in the middle of [low; high]
            i = (low + high) // 2
            r = self.ranges[i]

            # If the range does not include ``char``, update the corresponding
            # bound. Otherwise, just return the result.
            if char < r[0]:
                high = i - 1
            elif char > r[1]:
                low = i + 1
            else:
                return (True, i)

        return (False, low)

    def add(self, char):
        """
        Add a single character to this set.

        :type char: unicode
        """
        self.add_range(char, char)

    def add_int_range(self, low, high):
        """
        Add a range of characters to this set.

        :type low: int
        :type righ: int
        """
        assert low <= MAXUNICODE and high <= MAXUNICODE

        # Look for a range that contains the low bound
        found, index = self._lookup(low)

        # If we could find one, try to extend its high bound to include
        # ``high``.
        if found:
            if high <= self.ranges[index][1]:
                # If there is nothing to extend, just stop there
                return
            self.ranges[index] = (self.ranges[index][0], high)

        # If the high bound of the range before is next to our low bound,
        # extend its high bound.
        elif index > 0 and self.ranges[index - 1][1] == low - 1:
            self.ranges[index - 1] = (self.ranges[index - 1][0], high)
            index = index - 1

        # Otherwise just insert a new range
        else:
            self.ranges.insert(index, (low, high))

        # Convenient access to the range we added/extended
        r0 = self.ranges[index]

        # Merge ranges that can overlap with the modified entries
        to_merge_index = index + 1
        while to_merge_index < len(self.ranges):
            r = self.ranges[to_merge_index]

            if r[1] <= high:
                # This range is completely included in the range we are adding,
                # so we can remove it from our list.
                self.ranges.pop(to_merge_index)

            elif r[0] <= high + 1:
                # The low part of this range overlaps/is adjacent with the
                # range we are adding, so we can extend the latter and remove
                # the former.
                self.ranges[index] = (r0[0], r[1])
                self.ranges.pop(to_merge_index)

            else:
                # This range appears after and cannot be merged with the range
                # we added, so there is nothing else to do.
                break

    def add_range(self, low, high):
        """
        Add a range of characters to this set.

        :type low: unicode
        :type righ: unicode
        """
        self.add_int_range(ord(low), ord(high))

    @staticmethod
    def for_category(category):
        """
        Return the character set corresponding to the given Unicode general
        category. Raise a KeyError if ``category`` is invalid.

        :param str category: Name of the Unicode general category (see
            unicodedata.category).
        :rtype: CharSet
        """
        # The unicode_data module is auto-generated, so import is only when
        # required.
        from langkit.lexer.unicode_data import unicode_categories_char_sets
        return unicode_categories_char_sets[category]


def compute_unicode_categories_char_sets():
    # We assume here that the Python interpreter is built to use UCS-4 to
    # represent strings. It's fine because this code runs only to precompute
    # data that will be cached in source code, not on every script using
    # Langkit.
    sets = {}
    for i in range(MAXUNICODE + 1):
        char = unichr(i)
        cat = unicodedata.category(unichr(i))
        for subcat in (cat, cat[0]):
            sets.setdefault(subcat, CharSet())
            sets[subcat].add(char)

    lines = [
        'from __future__ import absolute_import, division, print_function',
        '',
        'from langkit.lexer.char_set import CharSet',
        '',
        '# Character sets for Unicode general categories. The following',
        '# literal is precomputed from',
        '# langkit.lexer.char_set.compute_unicode_categories_char_sets to',
        '# avoid taking 6s at startup, even on modern hardware.',
        '',
        'unicode_categories_char_sets = {',
    ]
    for cat, char_set in sorted(sets.iteritems()):
        lines.append('    {}: CharSet.from_int_ranges(*['.format(repr(cat)))
        for low, high in char_set.ranges:
            lines.append('        ({}, {}),'.format(low, high))
        lines.append('    ]),')
    lines.append('}')

    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            'unicode_data.py')
    with open(filename, 'wb') as f:
        for l in lines:
            f.write(l)
            f.write('\n')


if __name__ == '__main__':
    # When executed as the main script, regenerate the unicode_data.py file
    compute_unicode_categories_char_sets()
