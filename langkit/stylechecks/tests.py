"""
Nose-based testcases to make sure the stylechecker works as intended. It is
also a way to make it explicit how it is intended to work...
"""

import dataclasses
import os
from typing import List, Tuple

import pytest

from langkit.stylechecks import Report, check_file_content


@dataclasses.dataclass
class Testcase:
    """Type to instantiate for each testcase."""

    filename: str
    """
    Filename passed to the style checker.
    """

    content: str
    """
    File content to pass to the style checker.

    For layout convenience: if ``reindent_content`` is True, "content" is
    assumed to be a string whose first line is empty. This first line is then
    stripped, as well as the maximum common indentation. See
    "reindent_content".
    """

    records: List[Tuple[int, int, str]]
    """
    List of expected diagnostics from the style checker when working on
    ``content``.
    """

    reindent_content: bool = True
    """
    See ``content``.
    """


testcases = (
    #
    # Line-wrapping testing
    #

    Testcase('line_wrap_1.py', '''
        {}
    '''.format('a' * 80), []),
    Testcase('line_wrap_2.py', '''
        {}
    '''.format('a' * 81), [(1, 0, 'Too long line')]),
    Testcase('line_wrap_3.py', '''
        print("http://{}")
    '''.format('a' * 81), []),
    Testcase("line_wrap_4.py", "", [], reindent_content=False),
    Testcase(
        "line_wrap_5.py",
        "foo\nbar",
        [(2, 0, "No newline at end of file")],
        reindent_content=False,
    ),

    #
    # ASCII testing
    #

    Testcase('ascii_1.py', '''
        # Comment with a special char: \xa0
    ''', [(1, 0, 'Non-ASCII characters')]),
    Testcase('ascii_2.py', '''
        # style: non-ascii

        # Comment with a special char: \xa0
    ''', []),

    #
    # Comment box testing
    #

    Testcase('comment_box.py', '''
        #######
        # Box #
        #######
    ''', []),
    Testcase('comment_box_space.py', '''
        ########
        # Box  #
        ########
    ''', []),
    Testcase('comment_box.adb', '''
        ---------
        -- Box --
        ---------
    ''', []),
    Testcase('comment_box_middle_line_shorter.py', '''
        #######
        # Box#
        #######
    ''', [(2, 0, 'Badly formatted comment box')]),
    Testcase('comment_box_last_line_shorter.py', '''
        #######
        # Box #
        ######
    ''', [(1, 0, 'First and last lines are not identical in comment box')]),
    Testcase('comment_box_space.py', '''
        ########
        # Box ##
        ########
    ''', [(2, 0, 'Badly formatted comment box')]),
    Testcase('comment_fake_box.py', '''
        #
        # Header comment
        #
    ''', []),

    #
    # Packages sorting testing
    #

    Testcase('package_1.py', '''
        import foo
    ''', []),
    Testcase('package_2.py', '''
        import bar
        import foo
    ''', []),
    Testcase('package_3.py', '''
        import foo
        import bar
    ''', [(2, 0, 'Imported package "foo" must appear after "bar"')]),
    Testcase('package_4.py', '''
        import foo

        import bar
    ''', []),
    Testcase('package_5.py', '''
        import foo
        import bar as zoo
    ''', [(2, 0, 'Imported package "foo" must appear after "bar"')]),

    Testcase('package_6.adb', '''
        with Foo;
        with Bar; use Bar;
    ''', [(2, 0, 'Imported package "Foo" must appear after "Bar"')]),
    Testcase('package_7.adb', '''
        with Foo;
        with ${blah};
        with Bar;
    ''', [(3, 0, 'Imported package "Foo" must appear after "Bar"')]),
    Testcase('package_8.adb', '''
        with AB;
        with Aa;
    ''', [(2, 0, 'Imported package "AB" must appear after "Aa"')]),

    #
    # Mako-specific testing
    #

    Testcase('makopython_1.mako', '''
        ## vim: ft=makopython
        import foo
        import bar
    ''', [(3, 0, 'Imported package "foo" must appear after "bar"')]),
    Testcase('makoada_1.mako', '''
        ## vim: ft=makoada
        with Foo;
        with Bar;
    ''', [(3, 0, 'Imported package "Foo" must appear after "Bar"')]),

    #
    # Comments testing
    #

    Testcase('comment_single_1.py', '''
        # This is a single-line comment
    ''', []),
    Testcase('comment_single_2.py', '''
        # This is a single-line comment.
    ''', [(1, 0, 'Single-line comment must not have a final period')]),
    Testcase('comment_single_3.py', '''
        # This is a single-line comment...
    ''', []),
    Testcase('comment_single_4.py', '''
        # This is a single-line comment!
    ''', []),
    Testcase('comment_single_5.py', '''
        foo # This a trailing single-line comment.
            # This is an autonomous single-line comment.
    ''', [(1, 0, 'Single-line comment must not have a final period'),
          (2, 0, 'Single-line comment must not have a final period')]),
    Testcase('comment_single_6.py', '''
        # Invalid !
    ''', [(1, 0, 'Extra space before double punctuation')]),

    Testcase('comment_multi_1.py', '''
        # This is a multi-line comment.
        # Yes?
    ''', []),
    Testcase('comment_multi_2.py', '''
        # This is a multi-line comment.
        # Yes
    ''', [(2, 0, 'Multi-line comment must have a final period')]),
    Testcase('comment_multi_3.py', '''
        # This is a multi-line comment.
        #
        # But with an empty line.
    ''', []),
    Testcase('comment_multi_4.py', '''
        # This is a multi-line comment
        #
        # But with an empty line.
    ''', [(1, 0, 'Multi-line comment must have a final period')]),
    Testcase('comment_multi_5.py', '''
        # This is a multi-line comment.
        #
        # But with an empty line
    ''', [(3, 0, 'Multi-line comment must have a final period')]),
    Testcase('comment_multi_6.py', '''
        # This is a multi-line comment::
        #
        #     Blah : Invalid ! No dot, free style
    ''', []),
    Testcase('comment_multi_7.py', '''
        # This is a multi-line comment::
        #
        # No dot, free style
    ''', [(3, 0, 'Multi-line comment must have a final period')]),
    Testcase('comment_multi_8.py', '''
        # This is a multi-line comment::
        #
        #     Blah : Invalid ! No dot, free style
        #
        # and it is correctly formatted.
    ''', []),
    Testcase('comment_multi_9.py', '''
        # This is a multi-line comment::
        #
        #     Blah : Invalid ! No dot, free style
        #
        # and it is badly formatted
    ''', [(5, 0, 'Multi-line comment must have a final period')]),
    Testcase('comment_multi_10.py', '''
        # This is a multi-line comment:
        #
        # >>> Blah : Invalid ! No dot, free style
    ''', []),
    Testcase('comment_multi_11.py', '''
        # This is a multi-line comment:
        #
        # >>> Blah : Invalid ! No dot, free style
        #
        # This is the end of the comment.
    ''', []),
    Testcase('comment_multi_12.py', '''
        # Allow empty comments: they are useful in Mako templates

        #
    ''', []),

    #
    # Docstring testing
    #

    # Line numbers are sometimes imprecise, but that's because we loose precise
    # track of line numbers when getting docstrings out of AST nodes.

    Testcase('docstring_single_1.py', '''
        def foo():
            """This is a single-line docstring."""
    ''', []),
    Testcase('docstring_single_2.py', '''
        def foo():
            """This is a single-line docstring"""
    ''', [(3, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_single_3.py', '''
        def foo():
            """This is a single-line docstring..."""
    ''', []),
    Testcase('docstring_single_4.py', '''
        def foo():
            """This is a single-line docstring!"""
    ''', []),

    Testcase('docstring_multi_1.py', '''
        def foo():
            """
            This is a multi-line docstring.
            Yes?
            """
    ''', []),
    Testcase('docstring_multi_2.py', '''
        def foo():
            """
            This is a multi-line docstring.
            Yes
            """
    ''', [(4, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_multi_3.py', '''
        def foo():
            """
            This is a multi-line docstring.

            But with an empty line.
            """
    ''', []),
    Testcase('docstring_multi_4.py', '''
        def foo():
            """
            This is a multi-line docstring

            But with an empty line.
            """
    ''', [(3, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_multi_5.py', '''
        def foo():
            """
            This is a multi-line docstring.

            But with an empty line
            """
    ''', [(5, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_multi_6.py', '''
        def foo():
            """
            This is a multi-line docstring::

                Blah : Invalid ! No dot, free style
            """
    ''', []),
    Testcase('docstring_multi_7.py', '''
        def foo():
            """
            This is a multi-line docstring::

            No dot, free style
            """
    ''', [(5, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_multi_8.py', '''
        def foo():
            """
            This is a multi-line docstring:

            >>> Blah : Invalid ! No dot, free style
            """
    ''', []),
    Testcase('docstring_multi_9.py', '''
        def foo():
            """
            This is a multi-line docstring:

            >>> Blah : Invalid ! No dot, free style

            This is the end of the docstring.
            """
    ''', []),
    Testcase('docstring_multi_10.py', '''
        def foo():
            """
            Documenting some function.

            :param arg: Argument.
            :type arg: str
            """
    ''', []),
    Testcase('docstring_multi_11.py', '''
        def foo():
            """
            Documenting some function.

            :param arg: Argument1.
            :type arg: str

            :param arg: Argument2.
            :type arg: int
            """
    ''', []),
    Testcase('docstring_multi_12.py', '''
        def foo():
            """
            Documenting some function.

            :param arg: Argument
            :type arg: str
            """
    ''', [(5, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_multi_13.py', '''
        def foo():
            """
            Documenting some function.

            :param str arg: Long description for this argument which is
                supposed to be a string.
            """
    ''', []),
    Testcase('docstring_multi_14.py', '''
        def foo():
            """
            Documenting some function.

            :param str arg: Long description for this argument which is
                supposed to be a string
            """
    ''', [(6, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_multi_15.py', '''
        def foo():
            """
            Documenting some function.

            .. code:: python

                foo + bar(True)
            """
    ''', []),
    Testcase('docstring_multi_16.py', '''
        def foo():
            """
            Documenting some :ref:`function <foo>`.

            Inline code: `A := 1`.
            """
    ''', []),
    Testcase('docstring_multi_17.py', '''
        def foo():
            """
            Documenting some function

            :rtype: bool
            """
    ''', [(3, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_multi_18.py', '''
        def foo():
            a = True
            """
            Meaningless local variable

            :rtype: bool
            """
    ''', [(4, 0, 'Docstring sentences must end with periods')]),
    Testcase('docstring_multi_19.py', '''
        def foo():
            """
            Documenting some :ref:ada:`function <foo>`.
            """
    ''', []),

    #
    # "from __future__ testing
    #

    Testcase('future_1.py', '''
        from __future__ import annotations


        dummy = 1
    ''', []),
    Testcase('future_3.py', '''
        from __future__ import division


        dummy = 1
    ''', [(1, 0, 'Forbidden annotation: division')]),

    #
    # from X import Y testing
    #

    Testcase('from_import_1.py', """
        from X import A, B
    """, []),

    Testcase('from_import_1.py', """
        from X import B, A
    """, [(1, 0, 'Imported entity "B" should appear after "A"')]),

    #
    # Python imports spacing testing
    #
    Testcase('spacing_1.py', """
        import foo
        a = 1
    """, [(2, 0, 'Two empty lines required between the last import statement'
                 ' and this line.')]),
    Testcase('spacing_2.py', """
        import foo

        a = 1
    """, [(3, 0, 'Two empty lines required between the last import statement'
                 ' and this line.')]),
    Testcase('spacing_3.py', """
        import foo


        a = 1
    """, []),
    Testcase('spacing_4.py', """
        from foo import (
            bar
        )


        a = 1
    """, []),
    Testcase('spacing_5.py', """
        # pyflakes off
        from foo import (
            bar
        )
        # pyflakes on


        a = 1
    """, []),
    Testcase('spacing_6.py', """
        # pyflakes off
        from foo import (
            bar
        )
        # pyflakes on

        a = 1
    """, [(7, 0, 'Two empty lines required between the last import statement'
                 ' and this line.')]),
)


def reindent_content(tc: Testcase) -> str:
    """
    Return a stripped version of "tc.content".

    The first line (which must be empty) is stripped and the common identation
    from the other lines is stripped as well.

    :param Testcase tc: Testcase to process.
    :rtype: str
    """
    # Do not strip non-ASCII whitespaces
    lines = tc.content.rstrip(' \n').split('\n')
    assert not lines[0], (
        'First content line for {} must be empty'.format(tc.filename)
    )
    result = []
    indent = ' ' * 8
    for i, line in enumerate(lines[1:], 1):
        assert not line.strip() or line[:len(indent)] == indent, (
            'Badly indented line {} for {}'.format(i, tc.filename)
        )
        result.append(line[len(indent):])
    if result:
        result.append("")
    return '\n'.join(result)


@pytest.mark.parametrize("tc", testcases)
def test_all(tc: Testcase) -> None:
    # Pre-process content
    report = Report(enable_colors=False)
    content = reindent_content(tc) if tc.reindent_content else tc.content
    check_file_content(report, tc.filename, content)
    records = [
        Report.Record(tc.filename, line, col, msg)
        for line ,col, msg in tc.records
    ]

    def fmt_records(records: list[Report.Record]) -> str:
        return (
            '\n'.join(
                f"  {r.filename}:{r.line}:{r.col}: {r.message}"
                for r in records
            )
            if records else
            '  <no report>'
        )

    assert report.records == records, (
        'For the following source:\n'
        '{}\n'
        'Got the following report:\n'
        '{}\n'
        'But the following was expected instead:\n'
        '{}'.format(
            '\n'.join('    {}'.format(line)
                      for line in content.split('\n')),
            fmt_records(report.records), fmt_records(records)
        )
    )


def test_report_output() -> None:
    """
    Just to cover output-related code, this trivial feature itself is not
    tested otherwise.
    """
    with open(os.devnull, 'w') as f:
        for enable_colors in (False, True):
            r = Report(enable_colors, f)
            r.set_context("foo.txt", 1)
            r.add('Foobar')
            r.output()
