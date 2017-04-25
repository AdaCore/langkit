from __future__ import absolute_import, division, print_function

import gdb


class TDH(object):
    """
    Helper to deal with tokens data handlers.
    """

    def __init__(self, value):
        self.value = value

    def token(self, token_no):
        """
        Retreive the token number "token_no" in this TDH.

        :rtype: Token
        """
        last_token = int(self.value['tokens']['size'])
        if token_no < 1 or last_token < token_no:
            raise gdb.error('Out of bounds token number')

        tokens_array = self.value['tokens']['e'].dereference()
        return Token(self, tokens_array[token_no])


class Token(object):
    """
    Helper to deal with tokens.
    """

    def __init__(self, tdh, value):
        self.tdh = tdh
        self.value = value

    @property
    def kind(self):
        return self.value['kind']

    @property
    def sloc_range(self):
        return SlocRange(self.value['sloc_range'])


class Sloc(object):
    def __init__(self, line, column):
        self.line = line
        self.column = column

    def __repr__(self):
        return '{}:{}'.format(self.line, self.column)


class SlocRange(object):
    def __init__(self, value):
        self.start = Sloc(int(value['start_line']),
                          int(value['start_column']))
        self.end = Sloc(int(value['end_line']),
                        int(value['end_column']))

    def __repr__(self):
        return '{}-{}'.format(self.start, self.end)
