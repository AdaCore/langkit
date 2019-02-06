from __future__ import absolute_import, division, print_function

import os.path
import sys
import unicodedata


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

    def __repr__(self):
        ranges = []
        for l, h in self.ranges:
            if not self._repr_ellipsis or l <= 127:
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
                result.add_range(unichr(l), unichr(h))
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
        return cls((unichr(0), unichr(sys.maxunicode)))

    @property
    def negation(self):
        """
        Return a character set that contains everything that is not in
        ``self``.

        :rtype: CharSet
        """
        result = CharSet()

        def add_range(low, high):
            result.add_range(unichr(low), unichr(high))

        last = None
        for l, h in self.ranges:
            if last is None:
                if l > 0:
                    add_range(0, l - 1)
            else:
                add_range(last + 1, l - 1)
            last = h
        add_range(last + 1, sys.maxunicode)
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
            char_set.add_range(unichr(l), unichr(h))

        for l, h in self.ranges:
            if h < 128:
                add_range(ascii, l, h)
            elif l < 128:
                add_range(ascii, l, 127)
                add_range(non_ascii, 128, h)
            else:
                add_range(non_ascii, l, h)

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

    def add_range(self, low, high):
        """
        Add a range of characters to this set.

        :type low: unicode
        :type righ: unicode
        """
        low = ord(low)
        high = ord(high)

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
    sets = {}
    for i in range(sys.maxunicode + 1):
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
        lines.append('    {}: CharSet(*['.format(repr(cat)))
        for low, high in char_set.ranges:
            lines.append('        ({}, {}),'.format(
                repr(unichr(low)), repr(unichr(high))
            ))
        lines.append('    ]),')
    lines.append('}')

    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            'unicode_data.py')
    with open(filename, 'wb') as f:
        for l in lines:
            f.write(l)
            f.write('\n')


if __name__ == '__main__':
    compute_unicode_categories_char_sets()

    def check_ranges(cs, ranges):
        cs_ranges = [(unichr(l), unichr(h))
                     for l, h in cs.ranges]
        assert cs_ranges == ranges, (
            'Expected: {}\n'
            'but got:  {}\n'.format(format_char_ranges(ranges),
                                    format_char_ranges(cs_ranges))
        )

    check_ranges(
        CharSet('a'),
        [('a', 'a')])
    check_ranges(
        CharSet('a', 'b'),
        [('a', 'b')])
    check_ranges(
        CharSet('a', 'c'),
        [('a', 'a'), ('c', 'c')])
    check_ranges(
        CharSet('c', 'a'),
        [('a', 'a'), ('c', 'c')])
    check_ranges(
        CharSet('a', 'c', 'b'),
        [('a', 'c')])

    check_ranges(
        CharSet(('a', 'c'), ('d', 'c')),
        [('a', 'c')])
    check_ranges(
        CharSet(('a', 'c'), 'b'),
        [('a', 'c')])
    check_ranges(
        CharSet(('i', 'o'), ('a', 'c')),
        [('a', 'c'), ('i', 'o')])
    for c in ('h', 'i', 'j', 'k'):
        check_ranges(
            CharSet(('i', 'o'), ('a', c)),
            [('a', 'o')])
    check_ranges(
        CharSet(('i', 'o'), ('a', 'o')),
        [('a', 'o')])
    check_ranges(
        CharSet(('i', 'o'), ('a', 'p')),
        [('a', 'p')])
    for c in ('i', 'j', 'k'):
        check_ranges(
            CharSet(('i', 'o'), (c, 'p')),
            [('i', 'p')])

    check_ranges(
        CharSet(('\x00', '\x10'), ('b', 'y')).negation,
        [('\x11', 'a'), ('z', unichr(sys.maxunicode))])
