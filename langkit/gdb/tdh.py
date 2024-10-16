from __future__ import annotations

from bisect import bisect_right

import gdb

from langkit.utils import memoized


class TDH:
    """
    Helper to deal with tokens data handlers.
    """

    def __init__(self, value: gdb.Value):
        self.value = value

    def _vector_item(self, vector: gdb.Value, index: int) -> gdb.Value:
        last = int(vector['size'])
        if index < 1 or last < index:
            raise gdb.error('Out of bounds index')

        array = vector['e'].dereference()
        return array[index]

    def get(self, token_no: int, trivia_no: int) -> Token:
        """
        Retreive the token or trivia in this TDH corresponding to the given
        indices.
        """
        return (self.trivia(token_no, trivia_no)
                if trivia_no else
                self.token(token_no))

    def token(self, token_no: int) -> Token:
        """
        Retreive the token number "token_no" in this TDH.
        """
        return Token(self, self._vector_item(self.value['tokens'], token_no),
                     token_no, 0)

    def trivia(self, token_no: int, trivia_no: int) -> Token:
        """
        Retreive the trivia number "trivia" in this TDH.
        """
        return Token(self,
                     self._vector_item(self.value['trivias'], trivia_no)['t'],
                     token_no, trivia_no)

    @property  # type: ignore
    @memoized
    def _line_starts(self) -> list[int]:
        """
        Return a python list corresponding to the Lines_Starts vectors in
        token data handlers. Note that the index of the python list is 0-based
        whereas the Ada counterpart is 1-based.
        """
        lines_starts = self.value['lines_starts']
        elems = lines_starts['e'].dereference()
        last = int(lines_starts['size'])
        return [int(elems[i]) for i in range(1, last + 1)]

    def get_sloc(self, char_index: int) -> Sloc:
        """
        Return the Sloc (1-based line and column number) of the character at
        the given 1-based character index.
        """
        # Get the line number in which this character lies
        line = int(bisect_right(self._line_starts, char_index))

        # Get the character index of the first character of the line
        line_offset = int(self._line_starts[line - 1])

        # Get the last character index of the buffer
        source_last = int(self.value['source_last'])

        # Compute the column number as being the given character index minus
        # the character index of the first character of the line. Make sure
        # the character index is not higher than the index of the last
        # character of the buffer. Note that we don't support tab expansion
        # here.
        column = min(char_index, source_last) - line_offset + 1

        # Return the Sloc with 1-based line and columber numbers
        return Sloc(line, column)


class Token:
    """
    Helper to deal with tokens.
    """

    def __init__(self,
                 tdh: TDH,
                 value: gdb.Value,
                 token_no: int,
                 trivia_no: int):
        self.tdh = tdh
        self.value = value
        self.token_no = token_no
        self.trivia_no = trivia_no

    @property
    def kind(self) -> gdb.Value:
        return self.value['kind']

    @property
    def sloc_range(self) -> SlocRange:
        first = int(self.value['source_first'])
        last = int(self.value['source_last'])
        return SlocRange(
            self.tdh.get_sloc(first),
            self.tdh.get_sloc(last if last < first else last + 1)
        )

    @property
    def text(self) -> str:
        # Fetch the fat pointer, the bounds and then go subscript the
        # underlying array ourselves.
        src_buffer = self.tdh.value['source_buffer']
        first = int(self.value['source_first'])
        last = int(self.value['source_last'])

        length = last - first + 1
        if length <= 0:
            return u''

        # It does not seem possible to get an architecture from a gdb.Value, so
        # take the architecture for the current selected frame and hope for the
        # best.
        uint32_t = gdb.selected_frame().architecture().integer_type(
            32, signed=False
        )
        text_addr = (src_buffer['P_ARRAY'].cast(uint32_t.pointer()) +
                     (first - int(src_buffer['P_BOUNDS']['LB0'])))

        char = gdb.lookup_type('character').pointer()
        return text_addr.cast(char).string('utf32', length=4 * length)

    def __repr__(self) -> str:
        return '<Token {} {}/{} at {} {}>'.format(
            self.kind, self.token_no, self.trivia_no, self.sloc_range,
            repr(self.text)
        )


class Sloc:
    def __init__(self, line: int, column: int):
        self.line = line
        self.column = column

    def __repr__(self) -> str:
        return '{}:{}'.format(self.line, self.column)


class SlocRange:
    def __init__(self, start: Sloc, end: Sloc):
        self.start = start
        self.end = end

    def __repr__(self) -> str:
        return '{}-{}'.format(self.start, self.end)
