#! /usr/bin/env python

"""
Style-checker engine for the Langkit project.

This engine, which checks comments and docstrings is meant to be used in
addition to PEP8/GNAT checks. As a quick-n-dirty script, the details of the
algoritms are not decently commented: in order to see what this is supposed to
handle, have a look at the testsuite in the stylechecks.tests module.
"""

import argparse
import ast
import os
import os.path
import re
import sys
from typing import Pattern


TERM_CODE_RE = re.compile('(\x1b\\[[^m]*m)')
RESET = '\x1b[0m'
RED = '\x1b[31m'
GREEN = '\x1b[32m'
YELLOW = '\x1b[33m'

punctuation_re = re.compile(' [!?:;]')

accepted_chars = [chr(c) for c in range(0x20, 0x80)]


def colored(msg, color):
    """Return a string that displays "msg" in "color" inside a terminal."""
    return '{}{}{}'.format(color, msg, RESET)


def strip_colors(msg):
    """Return "msg" with all the terminal control codes stripped."""
    while True:
        m = TERM_CODE_RE.search(msg)
        if m:
            start, end = m.span()
            msg = msg[:start] + msg[end:]
        else:
            return msg


class Report:

    """Container for diagnostic messages."""

    def __init__(self, enable_colors=False, file=None):
        """Create a report.

        :param bool enable_colors: Whether diagnostics should be printed with
            colors.
        :param file|None file: File in which the "output" method should write
            the report. Standard output if None.
        """
        self.file = file or sys.stdout
        self.enable_colors = enable_colors

        self.filename = None
        self.lineno = None

        self.records = []

    @property
    def context(self):
        """Return the context for the next diagnostics."""
        return (self.filename, self.lineno)

    def set_context(self, filename, lineno):
        """Set the context for the next diagnostics."""
        self.filename = filename
        self.lineno = lineno

    def add(self, message, filename=None, line=None, col=None):
        """Add a diagnostic record."""
        line = line or self.lineno
        col = col or 0
        filename = filename or self.filename
        if not self.enable_colors:
            message = strip_colors(message)
        self.records.append((
            filename, line, col, message
        ))

    def output(self):
        """Write all diagnostics to the output file."""
        for filename, lineno, colno, message in sorted(set(self.records)):
            line = '{}:{}:{} {}\n'.format(
                colored(filename, RED),
                colored(lineno, YELLOW),
                "{}:".format(colored(colno, YELLOW)) if colno else "",
                message
            )
            if not self.enable_colors:
                line = strip_colors(line)
            self.file.write(line)


def iter_lines(content):
    """Return a generator yielding (line no., string) for each line."""
    return enumerate(content.splitlines(), 1)


def indent_level(line):
    """Return the number of prefix spaces in "line"."""
    return len(line) - len(line.lstrip(' '))


def preprocess_docstring(text):
    """
    Strip expected whitespaces in a Python docstring.

    Return the preprocessed docstring, plus the number of leading lines
    stripped.
    """
    lineno_offset = 0
    lines = text.splitlines()
    if not len(lines):
        return ('', lineno_offset)

    # Remove the first and the last lines if they are empty
    if not lines[0].strip():
        lines.pop(0)
        lineno_offset += 1
        first_line = None
    else:
        # Consider the first line specifically, as we allow it to contain
        # meaningful content and not to be indented.
        first_line = lines.pop(0)

    if lines and not lines[-1].strip():
        lines.pop()

    # Remove the minimum indentation level on all non-empty lines
    min_indent = min(indent_level(line)
                     for line in lines
                     if line.strip()) if lines else 0
    lines = [line[min_indent:] for line in lines]
    if first_line:
        lines.insert(0, first_line)
    return ('\n'.join(lines), lineno_offset)


class PackageChecker:
    """Helper to check the order of imported packages."""

    def __init__(self, report):
        self.report = report
        self.reset()

    def add(self, name):
        if self.last_package and self.last_package.lower() > name.lower():
            self.report.add(
                'Imported package "{}" must appear after "{}"'.format(
                    colored(self.last_package, GREEN),
                    colored(name, GREEN),
                )
            )
        self.last_package = name

    def reset(self):
        self.last_package = None


def check_text(report, filename, lang, first_line, text, is_comment):
    """
    Check various rules related to comments and docstrings.

    :param Report report: The report in which diagnostics must be emitted.
    :param str filename: Filename from which the text to check comes.
    :param LanguageChecker lang: language checker corresponding to "text".
    :param int first_line: Line number for the first line in "text".
    :param str text: Text on which the checks must be performed.
    :param bool is_comment: True if "text" is a comment, False if it's a
        docstring.
    """
    lines = text.split('\n')
    chars = set(lines[0])
    if len(chars) == 1 and chars == set(lang.comment_start):
        # This is a comment box

        # Each line must have the same length
        if lines[0] != lines[-1]:
            report.set_context(filename, first_line)
            report.add('First and last lines are not identical in comment box')

        # Each line must start and end with language comment start
        for i, line in enumerate(lines[1:-1], 1):
            report.set_context(filename, first_line + i)
            if (not line.endswith(' ' + lang.comment_start) or
                    len(lines[0]) != len(line)):
                report.add('Badly formatted comment box')
        return

    # Otherwise, assume this is regular text
    class State:

        """Helper for checking state-tracking."""

        def __init__(self):
            # If in a "quote" (i.e. an indented chunk of arbitrary content),
            # this is the minium number of columns for the quoted content. None
            # otherwise.
            self.quote_indent = None

            self.first_block = True
            self.lines_count = 0
            self.last_line = None
            self.last_end = ''

            self.is_sphinx = False
            self.is_prompt = False

            self.may_be_header = False
            self.header_context = None

        def end_block(self, is_last):
            """To be called at the end of each hunk of text."""
            if (not self.last_line or
                    not self.last_line.strip() or
                    self.quote_indent is not None):
                return

            if self.may_be_header:
                if self.last_line.strip() or not is_last:
                    report.set_context(*self.header_context)
                    report.add('Multi-line comment must have a final period')
                else:
                    return

            ends = ('.', '?', '!', ':', '...', '::')

            if is_comment:
                if ((self.lines_count > 1 or not is_last) and
                        self.last_end not in ends):
                    if self.lines_count == 1 and not is_last:
                        self.may_be_header = True
                        self.header_context = report.context
                    else:
                        report.add('Multi-line comment must have a final'
                                   ' period')
                elif (is_last and
                        self.lines_count == 1 and
                        self.first_block and
                        self.last_end == '.' and
                        len([c for c in self.last_line if c == '.']) == 1):
                    report.add('Single-line comment must not have a final'
                               ' period')
            elif (not self.is_sphinx and
                    not self.is_prompt and
                    self.last_end not in ends):
                report.add('Docstring sentences must end with periods')

            self.first_block = False
            self.is_sphinx = False

    def has_prompt(line):
        """Return whether "line" starts with a Python prompt."""
        return line.lstrip().startswith('>>> ')

    s = State()

    for i, line in iter_lines(text):
        empty_line = not line.strip()

        if s.quote_indent is not None:
            if line.startswith(' ' * s.quote_indent) or empty_line:
                continue
            else:
                s.quote_indent = None
        elif s.is_prompt:
            if has_prompt(line):
                continue
            s.is_prompt = False

        if (line.startswith(':type')
                or line.startswith(':rtype:')
                or line.startswith('.. code')):
            s.end_block(False)
            s.is_sphinx = True
        elif line.startswith(':param'):
            s.end_block(False)
        elif has_prompt(line):
            s.is_prompt = True
            continue
        elif not empty_line:
            s.lines_count += 1
        elif s.lines_count > 0:
            s.end_block(False)

        report.set_context(filename, first_line + i - 1)

        # Report extra space before double punctuation. As soon as there is a
        # backquote on the line, disable this check, as this we must note
        # report Sphinx inline markup (e.g. :ref:`foo`) and anything inside
        # inline code (`A := 1`). Detecting extra spaces without false positive
        # is not worth the effort.
        if '`' not in line and punctuation_re.search(line):
            report.add('Extra space before double punctuation')

        if line.endswith('::'):
            s.last_end = '::'
            s.quote_indent = indent_level(line) + 1
        elif line.endswith('...'):
            s.last_end = '...'
        elif line.startswith('.. '):
            s.quote_indent = indent_level(line) + 1
        elif not empty_line:
            s.last_end = line[-1:]
        s.last_line = line

    s.end_block(True)


def check_generic(report, filename, content, lang):
    """
    Perform language-agnostic ("generic") style checks.

    :param Report report: The report in which diagnostics must be emitted.
    :param str filename: Filename from which the text to check comes.
    :param LanguageChecker lang: language checker corresponding to "text".
    :param str content: Text on which the checks must be performed.
    """
    # Line list for the current block of comments
    comment_block = []

    # Line number for the first comment line
    comment_first_line = None

    # Column number for the comment block. If we are not in a block but a
    # single line of comment (i.e. we have a comment on the same line as
    # regular code), this is still None.
    comment_column = None

    def check_comment():
        """Helper to invoke check_text on the text in "comment_block".

        Reset "comment_block" afterwards.
        """
        nonempty_lines = [l for l in comment_block if l.strip()]
        if nonempty_lines:
            # Remove common indentation for this block of comment
            indent = min(len(l) - len(l.lstrip())
                         for l in nonempty_lines)

            check_text(report, filename, lang,
                       comment_first_line,

                       # Ignored lines starting with '%': they are directives
                       # for documentation generators.
                       '\n'.join(l[indent:] for l in comment_block
                                 if not l.startswith('%')),
                       True)
        comment_block[:] = []

    def start_comment():
        """
        Return (comment_column, comment_first_line) (see above) for the current
        "line".
        """
        column = None if line[:comment_start].strip() else comment_start
        first_line = i
        return (column, first_line)

    for i, line in iter_lines(content):
        report.set_context(filename, i)

        for c in line:
            if c not in accepted_chars:
                report.add('Non-ASCII characters')
                break

        if (len(line) > 80 and
                'http://' not in line and
                'https://' not in line):
            report.add('Too long line')
        comment_start = line.find(lang.comment_start)

        def get_comment_text():
            """Return the text contained in the comment in "line"."""
            first = comment_start + len(lang.comment_start)
            return line[first:]

        if comment_start != -1:
            if not comment_block:
                comment_column, comment_first_line = start_comment()
                comment_first_line = i
            elif (comment_column is None or
                    comment_start != comment_column):
                check_comment()
                comment_column, comment_first_line = start_comment()
            comment_block.append(get_comment_text())

        elif comment_block:
            check_comment()

    if comment_block:
        check_comment()


class LanguageChecker:

    """Base class for language-specific checkers."""

    # String for single-line comments starters
    comment_start: str

    # Regular expression that matches package imports
    with_re: Pattern

    def check(self, report, filename, content, parse):
        """
        Perform style checks.

        :param Report report: The report in which diagnostics must be emitted.
        :param str filename: Filename from which the text to check comes.
        :param str content: Text on which the checks must be performed.
        :param bool parse: Whether we expect "content" to be syntactically
            correct (i.e. if we can parse it without error).
        """
        raise NotImplementedError()


class AdaLang(LanguageChecker):
    comment_start = '--'
    with_re = re.compile('^with (?P<name>[a-zA-Z0-9_.]+);.*')

    def check(self, report, filename, content, parse):
        pcheck = PackageChecker(report)
        for i, line in iter_lines(content):
            report.set_context(filename, i)
            if not line.strip():
                pcheck.reset()

            m = self.with_re.match(line)
            if m:
                pcheck.add(m.group('name'))


class PythonLang(LanguageChecker):
    comment_start = '#'
    import_re = re.compile('^import (?P<name>[a-zA-Z0-9_.]+)'
                           '( as [a-zA-Z0-9_.]+)?'
                           '(?P<remaining>.*)')
    from_import_re = re.compile('^from (?P<name>[a-zA-Z0-9_.]+) import.*')

    def check(self, report, filename, content, parse):
        self.custom_check(report, filename, content, parse)
        if os.path.exists(filename):
            self.pep8_check(report, filename)
            self.pyflakes_check(report, filename, content)

    def pep8_check(self, report, filename):
        """
        Run pep8 checks on given filename, adding pep8 reports to report.
        """
        # Try to use pycodestyle, and fallback on pep8 if not available. If
        # nothing is available, don't do anything.
        try:
            try:
                import pycodestyle as pep8
            except ImportError:
                import pep8
        except ImportError:
            return

        class CustomReport(pep8.BaseReport):
            def error(self, line_number, offset, text, check):
                # Due to the great architecture of PEP8/pycodestyle, we have to
                # duplicate this check here in order to not show certain (but
                # not all) errors that should be ignored.
                code = text[:4]
                if self._ignore_code(code):
                    return
                report.add(text, filename, line_number, offset)

        sg = pep8.StyleGuide(
            quiet=True,
            ignore=["W503", "E121", "E123", "E126", "E226", "E24",
                    "E704", "E402", "E721", "W504", "E741"]
        )
        sg.init_report(CustomReport)
        sg.check_files([filename])

    def pyflakes_check(self, report, filename, content):
        """
        Run pyflakes on given file with given content. Add pyflakes reports to
        report.
        """

        # Just exit silently if pyflakes is not available
        try:
            from pyflakes import api, reporter
        except ImportError:
            return

        lines_map = [(None, None)]
        current = True
        for line in content.splitlines():
            if line.strip() == "# pyflakes off":
                current = False
            elif line.strip() == "# pyflakes on":
                current = True
            lines_map.append((current, line))

        class CustomReporter(reporter.Reporter):
            def syntaxError(self, _, msg, lineno, offset, text):
                pass

            def unexpectedError(self, filename, msg):
                pass

            def flake(self, msg):
                if lines_map[msg.lineno][0]:
                    report.add(
                        msg.message % msg.message_args, filename, msg.lineno, 0
                    )

        api.checkPath(
            filename, reporter=CustomReporter(sys.stdout, sys.stderr)
        )

    def custom_check(self, report, filename, content, parse):
        pcheck = PackageChecker(report)
        last_import_line = None
        for i, line in iter_lines(content):
            report.set_context(filename, i)
            if not line.strip():
                pcheck.reset()
                continue

            m = self.import_re.match(line)
            if m:
                if m.group('remaining'):
                    report.add('Import is more complex than'
                               ' "import PACKAGE [as NAME]"')
                pcheck.add(m.group('name'))

            m = self.from_import_re.match(line)
            if m:
                pcheck.add(m.group('name'))

            # Expect exactly two blank lines between the last import statement
            # and the next statement.
            if (
                line.startswith('import ') or
                line.startswith('from ') or
                (
                    # Hack to match continuation lines for long ImportFrom
                    # statements.
                    last_import_line is not None and
                    (line.startswith('    ') or line == ')')
                )
            ):
                last_import_line = i
            else:
                if last_import_line and i != last_import_line + 3:
                    # Consider that comments that appear too close to import
                    # lines are part of these imports, so they must themselves
                    # be separated from regular statements with two empty
                    # lines.
                    if line.startswith('#'):
                        last_import_line = i
                    else:
                        report.add('Two empty lines required between the last'
                                   ' import statement and this line.')
                        last_import_line = None
                else:
                    last_import_line = None

        if parse:
            try:
                root = ast.parse(content)
            except (SyntaxError, TypeError) as exc:
                report.add('Could not parse: {}'.format(exc))
            else:

                def node_lineno(node):
                    return getattr(node, 'lineno', 0) + 1

                for node in ast.walk(root):
                    if isinstance(node, ast.ImportFrom):
                        report.set_context(filename, node_lineno(node) - 1)
                        if node.module == '__future__':
                            for alias in node.names:
                                if alias.name != 'annotations':
                                    report.add('Forbidden annotation: {}'
                                               .format(alias.name))
                        else:
                            self._check_imported_entities(report, node)

                    elif (
                        isinstance(node, ast.stmt) and
                        isinstance(node, ast.Expr) and
                        isinstance(node.value, ast.Str)
                    ):
                        # Sometimes we use docstrings on local variables, and
                        # ast.get_docstring does not allow us to catch that.
                        # Instead, process all string literals that appear at
                        # the root of a statement.
                        raw_docstring = node.value.s
                        docstring, lineno_offset = preprocess_docstring(
                            raw_docstring
                        )

                        check_text(report, filename, self, node_lineno(node),
                                   docstring, False)

    def _check_imported_entities(self, report, import_node):
        last = None
        for alias in import_node.names:
            name = alias.name
            if last and last > name:
                report.add('Imported entity "{}" should appear after "{}"'
                           .format(last, name))
            last = name


class MakoLang(LanguageChecker):
    comment_start = '##'

    def check(self, report, filename, content, parse):
        first_line = content.split('\n', 1)[0]
        if 'makoada' in first_line:
            ada_lang.check(report, filename, content, parse=False)
            check_generic(report, filename, content, ada_lang)
        elif 'makopython' in first_line:
            python_lang.custom_check(report, filename, content, parse=False)
            check_generic(report, filename, content, python_lang)


ada_lang = AdaLang()
python_lang = PythonLang()
mako_lang = MakoLang()


langs = {
    'ads': ada_lang,
    'adb': ada_lang,
    'py':  python_lang,
    'mako': mako_lang,
}


def check_file_content(report, filename, content):
    """
    Perform generic and language-specific style checks.

    :param Report report: The report in which diagnostics must be emitted.
    :param str filename: Filename from which the text to check comes.
    :param str content: Text on which the checks must be performed.
    """
    ext = filename.split('.')[-1]
    lang = langs[ext]
    check_generic(report, filename, content, lang)
    lang.check(report, filename, content, parse=True)


def check_file(report, filename):  # pragma: no cover
    """
    Perform generic and language-specific style checks.

    :param Report report: The report in which diagnostics must be emitted.
    :param str filename: Filename from which the text to check comes.
    """
    ext = filename.split('.')[-1]
    if ext not in langs:
        return

    with open(filename, 'r', encoding='utf-8') as f:
        try:
            content = f.read()
        except UnicodeDecodeError as exc:
            print("{}: cannot decode as UTF-8: {}".format(filename, exc))
            return
    check_file_content(report, filename, content)


def excludes_match(path, excludes):
    """
    Return whether at least one item in `excludes` matches the `path`.

    :type path: str
    :type excludes: list[str]
    :rtype: bool
    """
    path = os.path.sep + path
    return any(path.endswith(os.path.sep + e)
               for e in excludes)


def traverse(report, root, excludes):  # pragma: no cover
    """
    Perform generic and language-specific style checks.

    :param Report report: The report in which diagnostics must be emitted.
    :param str root: Root directory in which the files to stylecheck are looked
        for.
    :param [str] excludes: List of path to exclude from the search of files to
        check.
    """
    for item in sorted(os.listdir(root)):
        path = os.path.join(root, item)
        if excludes_match(path, excludes):
            continue

        if os.path.isdir(path):
            traverse(report, path, excludes)
        else:
            check_file(report, os.path.relpath(path))


def main(src_root, files, dirs, excludes):
    """
    Global purpose main procedure.

    :param str langkit_root: Root directory for the Langkit source repository.
    :param list[str] files: Source files to analyze. If empty, look for all
        sources in the Langkit repository.
    :param list[str] dirs: List of directories in which to find sources to
        check.
    :param list[str] excludes: List of directories to exclude from the search.
    """
    report = Report(enable_colors=os.isatty(sys.stdout.fileno()))

    if files:
        for f in args.files:
            check_file(report, f)
    else:
        os.chdir(src_root)
        for root in dirs:
            traverse(report, root, excludes)

    report.output()


def langkit_main(langkit_root, files=[]):
    """
    Run main() on Langkit sources.
    """
    dirs = [os.path.join('contrib', 'python'),
            os.path.join('contrib', 'lkt'),
            os.path.join('langkit'),
            os.path.join('scripts'),
            os.path.join('testsuite'),
            os.path.join('utils')]
    excludes = ['__pycache__',
                os.path.join('contrib', 'python', 'build'),
                os.path.join('contrib', 'lkt', 'build'),
                os.path.join('langkit', 'support', 'obj'),
                os.path.join('langkit', 'dsl_unparse.py'),
                'out',
                os.path.join('stylechecks', 'tests.py'),
                os.path.join('testsuite', 'out')]
    main(langkit_root, files, dirs, excludes)


args_parser = argparse.ArgumentParser(description="""
    Check the coding style for the Langkit code base.
""")
args_parser.add_argument(
    '--langkit-root',
    default=os.path.dirname(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    ),
    help='Root directory for the Langkit source repository. Used to'
         ' automatically look for source files to analyze. If not provided,'
         ' default to a path relative to the `langkit.stylechecks` package.')
args_parser.add_argument(
    'files', nargs='*',
    help='Source files to analyze. If none is provided, look for all sources'
         ' in the Langkit repository.')


if __name__ == '__main__':
    args = args_parser.parse_args()
    langkit_main(args.langkit_root, args.files)
