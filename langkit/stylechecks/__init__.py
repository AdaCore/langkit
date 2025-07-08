#! /usr/bin/env python

"""
Style-checker engine for the Langkit project.

This engine, which checks comments and docstrings is meant to be used in
addition to PEP8/GNAT checks. As a quick-n-dirty script, the details of the
algoritms are not decently commented: in order to see what this is supposed to
handle, have a look at the testsuite in the stylechecks.tests module.
"""

from __future__ import annotations

import abc
import argparse
import ast
import dataclasses
import os
import os.path
import re
import sys
from typing import Any, IO, Iterator, Pattern


TERM_CODE_RE = re.compile("(\x1b\\[[^m]*m)")
RESET = "\x1b[0m"
RED = "\x1b[31m"
GREEN = "\x1b[32m"
YELLOW = "\x1b[33m"

punctuation_re = re.compile(" [!?:;]")

accepted_chars = [chr(c) for c in range(0x20, 0x80)]


def colored(msg: str, color: str) -> str:
    """Return a string that displays "msg" in "color" inside a terminal."""
    return "{}{}{}".format(color, msg, RESET)


def strip_colors(msg: str) -> str:
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

    @dataclasses.dataclass(frozen=True, order=True)
    class Record:
        filename: str
        line: int
        col: int
        message: str

    def __init__(
        self,
        enable_colors: bool = False,
        file: IO[str] | None = None,
    ):
        """Create a report.

        :param enable_colors: Whether diagnostics should be printed with
            colors.
        :param file: File in which the "output" method should write the report.
            Standard output if None.
        """
        self.file = file or sys.stdout
        self.enable_colors = enable_colors

        self.filename: str | None = None
        self.lineno: int | None = None

        self.records: list[Report.Record] = []

    @property
    def context(self) -> tuple[str | None, int | None]:
        """Return the context for the next diagnostics."""
        return (self.filename, self.lineno)

    def set_context(self, filename: str | None, lineno: int | None) -> None:
        """Set the context for the next diagnostics."""
        self.filename = filename
        self.lineno = lineno

    def add(
        self,
        message: str,
        filename: str | None = None,
        line: int | None = None,
        col: int | None = None,
    ) -> None:
        """Add a diagnostic record."""
        lineno = line or self.lineno
        assert lineno is not None
        colno = col or 0
        f = filename or self.filename
        assert f is not None
        if not self.enable_colors:
            message = strip_colors(message)
        self.records.append(self.Record(f, lineno, colno, message))

    def output(self) -> None:
        """Write all diagnostics to the output file."""
        for r in sorted(set(self.records)):
            line = "{}:{}:{} {}\n".format(
                colored(r.filename, RED),
                colored(str(r.line), YELLOW),
                "{}:".format(colored(str(r.col), YELLOW)) if r.col else "",
                r.message,
            )
            if not self.enable_colors:
                line = strip_colors(line)
            self.file.write(line)


def iter_lines(content: str) -> Iterator[tuple[int, str]]:
    """Return a generator yielding (line no., string) for each line."""
    return enumerate(content.splitlines(), 1)


def indent_level(line: str) -> int:
    """Return the number of prefix spaces in "line"."""
    return len(line) - len(line.lstrip(" "))


def preprocess_docstring(text: str) -> tuple[str, int]:
    """
    Strip expected whitespaces in a Python docstring.

    Return the preprocessed docstring, plus the number of leading lines
    stripped.
    """
    lineno_offset = 0
    lines = text.splitlines()
    if not len(lines):
        return ("", lineno_offset)

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
    non_empty_lines = [line for line in lines if line.strip()]
    min_indent = (
        min(indent_level(line) for line in non_empty_lines)
        if non_empty_lines
        else 0
    )
    lines = [line[min_indent:] for line in lines]
    if first_line:
        lines.insert(0, first_line)
    return ("\n".join(lines), lineno_offset)


class PackageChecker:
    """Helper to check the order of imported packages."""

    def __init__(self, report: Report):
        self.report = report
        self.last_package: str | None = None

    def add(self, name: str) -> None:
        if self.last_package and self.last_package.lower() > name.lower():
            self.report.add(
                'Imported package "{}" must appear after "{}"'.format(
                    colored(self.last_package, GREEN),
                    colored(name, GREEN),
                )
            )
        self.last_package = name

    def reset(self) -> None:
        self.last_package = None


def check_text(
    report: Report,
    filename: str,
    lang: LanguageChecker,
    first_line: int,
    text: str,
    is_comment: bool,
) -> None:
    """
    Check various rules related to comments and docstrings.

    :param report: The report in which diagnostics must be emitted.
    :param filename: Filename from which the text to check comes.
    :param lang: language checker corresponding to "text".
    :param first_line: Line number for the first line in "text".
    :param text: Text on which the checks must be performed.
    :param is_comment: True if "text" is a comment, False if it's a docstring.
    """
    lines = text.split("\n")
    chars = set(lines[0])
    if (
        lang.comment_start is not None
        and len(chars) == 1
        and chars == set(lang.comment_start)
    ):
        # This is a comment box

        # Each line must have the same length
        if lines[0] != lines[-1]:
            report.set_context(filename, first_line)
            report.add("First and last lines are not identical in comment box")

        # Each line must start and end with language comment start
        for i, line in enumerate(lines[1:-1], 1):
            report.set_context(filename, first_line + i)
            if not line.endswith(" " + lang.comment_start) or len(
                lines[0]
            ) != len(line):
                report.add("Badly formatted comment box")
        return

    # Otherwise, assume this is regular text
    class State:
        """Helper for checking state-tracking."""

        def __init__(self) -> None:
            # If in a "quote" (i.e. an indented chunk of arbitrary content),
            # this is the minium number of columns for the quoted content. None
            # otherwise.
            self.quote_indent: int | None = None

            self.first_block = True
            self.lines_count = 0
            self.last_line = ""
            self.last_end = ""

            self.is_sphinx = False
            self.is_prompt = False
            self.sphinx_heading_trailer = False

            self.may_be_header = False
            self.header_context: tuple[str | None, int | None] = (None, None)

        def end_block(self, is_last: bool) -> None:
            """To be called at the end of each hunk of text."""
            if (
                not self.last_line
                or not self.last_line.strip()
                or self.quote_indent is not None
            ):
                return

            if self.may_be_header:
                if self.last_line.strip() or not is_last:
                    report.set_context(*self.header_context)
                    report.add("Multi-line comment must have a final period")
                else:
                    return

            ends = (".", "?", "!", ":", "...", "::")

            if is_comment:
                if (
                    self.lines_count > 1 or not is_last
                ) and self.last_end not in ends:
                    if self.lines_count == 1 and not is_last:
                        self.may_be_header = True
                        self.header_context = report.context
                    else:
                        report.add(
                            "Multi-line comment must have a final" " period"
                        )
                elif (
                    is_last
                    and self.lines_count == 1
                    and self.first_block
                    and self.last_end == "."
                    and len([c for c in self.last_line if c == "."]) == 1
                ):
                    report.add(
                        "Single-line comment must not have a final" " period"
                    )
            elif (
                not self.is_sphinx
                and not self.is_prompt
                and self.last_end not in ends
            ):
                report.add("Docstring sentences must end with periods")

            self.first_block = False
            self.is_sphinx = False

    def has_prompt(line: str) -> bool:
        """Return whether "line" starts with a Python prompt."""
        return line.lstrip().startswith(">>> ")

    s = State()

    for i, line in iter_lines(text):
        empty_line = not line.strip()

        if empty_line and s.sphinx_heading_trailer:
            s.sphinx_heading_trailer = False
            continue
        elif s.quote_indent is not None:
            if line.startswith(" " * s.quote_indent) or empty_line:
                continue
            else:
                s.quote_indent = None
        elif s.is_prompt:
            if has_prompt(line):
                continue
            s.is_prompt = False

        if (
            line.startswith(":type")
            or line.startswith(":rtype:")
            or line.startswith(".. code")
            or (not is_comment and line.startswith("#"))
        ):
            s.end_block(False)
            s.is_sphinx = True
        elif line.startswith(":param"):
            s.end_block(False)
        elif set(line.strip()) in ({"="}, {"-"}, {"~"}):
            s.sphinx_heading_trailer = True
            continue
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
        if "`" not in line and punctuation_re.search(line):
            report.add("Extra space before double punctuation")

        if line.endswith("::"):
            s.last_end = "::"
            s.quote_indent = indent_level(line) + 1
        elif line.endswith("..."):
            s.last_end = "..."
        elif line.startswith(".. "):
            s.quote_indent = indent_level(line) + 1
        elif not empty_line:
            s.last_end = line[-1:]
        s.last_line = line

    s.end_block(True)


def check_generic(
    report: Report,
    filename: str,
    content: str,
    lang: LanguageChecker,
) -> None:
    """
    Perform language-agnostic ("generic") style checks.

    :param report: The report in which diagnostics must be emitted.
    :param filename: Filename from which the text to check comes.
    :param content: Text on which the checks must be performed.
    :param lang: language checker corresponding to "text".
    """
    if content and not content.endswith("\n"):
        report.set_context(filename, 1 + content.count("\n"))
        report.add(
            "No newline at end of file",
            filename=filename,
            line=1 + content.count("\n"),
            col=0,
        )

    # Whether non-ASCII characters are allowed
    non_ascii_allowed = "style: non-ascii" in content

    # Line list for the current block of comments
    comment_block: list[str] = []

    # Line number for the first comment line
    comment_first_line: int | None = None

    # Column number for the comment block. If we are not in a block but a
    # single line of comment (i.e. we have a comment on the same line as
    # regular code), this is still None.
    comment_column: int | None = None

    def check_comment() -> None:
        """Helper to invoke check_text on the text in "comment_block".

        Reset "comment_block" afterwards.
        """
        nonempty_lines = [l for l in comment_block if l.strip()]
        if nonempty_lines:
            # Remove common indentation for this block of comment.  Ignored
            # lines starting with '%': they are directives for documentation
            # generators.
            indent = min(len(l) - len(l.lstrip()) for l in nonempty_lines)
            clean_lines = [
                l[indent:] for l in comment_block if not l.startswith("%")
            ]

            # Copyright notices have a special formatting
            if (
                comment_first_line == 1
                and len(clean_lines) == 4
                and not clean_lines[0]
                and not clean_lines[3]
            ):
                report.set_context(filename, 1)
                if not (
                    clean_lines[1].startswith("Copyright (C) ")
                    and clean_lines[1].endswith(", AdaCore")
                ):
                    report.add("Invalid copyright line")
                if clean_lines[2] != "SPDX-License-Identifier: Apache-2.0":
                    report.add("Invalid license")
            else:
                assert comment_first_line is not None
                check_text(
                    report,
                    filename,
                    lang,
                    comment_first_line,
                    "\n".join(clean_lines),
                    True,
                )
        comment_block[:] = []

    def start_comment() -> tuple[None | int, int]:
        """
        Return (comment_column, comment_first_line) (see above) for the current
        "line".
        """
        column = None if line[:comment_start].strip() else comment_start
        first_line = i
        return (column, first_line)

    for i, line in iter_lines(content):
        report.set_context(filename, i)

        if not non_ascii_allowed:
            for c in line:
                if c not in accepted_chars:
                    report.add("Non-ASCII characters")
                    break

        if len(line) > 80 and "http://" not in line and "https://" not in line:
            report.add("Too long line")

        if lang.comment_start is not None:
            lang_comment_start = lang.comment_start
            comment_start = line.find(lang_comment_start)

            def get_comment_text() -> str:
                """Return the text contained in the comment in "line"."""
                first = comment_start + len(lang_comment_start)
                return line[first:]

            if comment_start != -1:
                if not comment_block:
                    comment_column, comment_first_line = start_comment()
                    comment_first_line = i
                elif comment_column is None or comment_start != comment_column:
                    check_comment()
                    comment_column, comment_first_line = start_comment()
                comment_block.append(get_comment_text())

            elif comment_block:
                check_comment()

    if comment_block:
        check_comment()


class LanguageChecker(abc.ABC):
    """Base class for language-specific checkers."""

    # String for single-line comments starters
    comment_start: str | None

    # Regular expression that matches package imports, if applicable
    with_re: Pattern | None

    @abc.abstractmethod
    def check(
        self,
        report: Report,
        filename: str,
        content: str,
        parse: bool,
    ) -> None:
        """
        Perform style checks.

        :param report: The report in which diagnostics must be emitted.
        :param filename: Filename from which the text to check comes.
        :param content: Text on which the checks must be performed.
        :param parse: Whether we expect "content" to be syntactically correct
            (i.e. if we can parse it without error).
        """
        ...


class AdaLang(LanguageChecker):
    comment_start = "--"
    with_re = re.compile("^with (?P<name>[a-zA-Z0-9_.]+);.*")

    def check(
        self,
        report: Report,
        filename: str,
        content: str,
        parse: bool,
    ) -> None:
        pcheck = PackageChecker(report)
        for i, line in iter_lines(content):
            report.set_context(filename, i)
            if not line.strip():
                pcheck.reset()

            m = self.with_re.match(line)
            if m:
                pcheck.add(m.group("name"))


class JavaLang(LanguageChecker):
    comment_start = None
    with_re = re.compile("^import (?P<name>[a-zA-Z0-9_.]+);")

    def check(
        self,
        report: Report,
        filename: str,
        content: str,
        parse: bool,
    ) -> None:
        pcheck = PackageChecker(report)
        for i, line in iter_lines(content):
            report.set_context(filename, i)
            if not line.strip():
                pcheck.reset()

            m = self.with_re.match(line)
            if m:
                pcheck.add(m.group("name"))


class PythonLang(LanguageChecker):
    comment_start = "#"
    import_re = re.compile(
        "^import (?P<name>[a-zA-Z0-9_.]+)"
        "( as [a-zA-Z0-9_.]+)?"
        "(?P<remaining>.*)"
    )
    from_import_re = re.compile("^from (?P<name>[a-zA-Z0-9_.]+) import.*")

    def check(
        self,
        report: Report,
        filename: str,
        content: str,
        parse: bool,
    ) -> None:
        self.custom_check(report, filename, content, parse)
        if os.path.exists(filename):
            self.pep8_check(report, filename)
            self.pyflakes_check(report, filename, content)

    def pep8_check(self, report: Report, filename: str) -> None:
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
            def error(
                self,
                line_number: int,
                offset: int,
                text: str,
                check: object,
            ) -> None:
                # Due to the great architecture of PEP8/pycodestyle, we have to
                # duplicate this check here in order to not show certain (but
                # not all) errors that should be ignored.
                code = text[:4]
                if self._ignore_code(code):
                    return
                report.add(text, filename, line_number, offset)

        sg = pep8.StyleGuide(
            quiet=True,
            ignore=[
                "W503",
                "E121",
                "E123",
                "E126",
                "E203",
                "E226",
                "E24",
                "E704",
                "E402",
                "E721",
                "W504",
                "E741",
            ],
        )
        sg.init_report(CustomReport)
        sg.check_files([filename])

    def pyflakes_check(
        self,
        report: Report,
        filename: str,
        content: str,
    ) -> None:
        """
        Run pyflakes on given file with given content. Add pyflakes reports to
        report.
        """

        # Just exit silently if pyflakes is not available
        try:
            from pyflakes import api, reporter
        except ImportError:
            return

        lines_map: list[tuple[bool | None, str | None]] = [(None, None)]
        current = True
        for line in content.splitlines():
            if line.strip() == "# pyflakes off":
                current = False
            elif line.strip() == "# pyflakes on":
                current = True
            lines_map.append((current, line))

        class CustomReporter(reporter.Reporter):
            def syntaxError(
                self,
                _: object,
                msg: Any,
                lineno: int,
                offset: int,
                text: str,
            ) -> None:
                pass

            def unexpectedError(self, filename: str, msg: Any) -> None:
                pass

            def flake(self, msg: Any) -> None:
                if lines_map[msg.lineno][0]:
                    report.add(
                        msg.message % msg.message_args, filename, msg.lineno, 0
                    )

        api.checkPath(
            filename, reporter=CustomReporter(sys.stdout, sys.stderr)
        )

    def custom_check(
        self,
        report: Report,
        filename: str,
        content: str,
        parse: bool,
    ) -> None:
        pcheck = PackageChecker(report)
        last_import_line: int | None = None
        for i, line in iter_lines(content):
            report.set_context(filename, i)
            if not line.strip():
                pcheck.reset()
                continue

            import_m = self.import_re.match(line)
            if import_m:
                if import_m.group("remaining"):
                    report.add(
                        "Import is more complex than"
                        ' "import PACKAGE [as NAME]"'
                    )
                pcheck.add(import_m.group("name"))

            from_import_m = self.from_import_re.match(line)
            if from_import_m:
                pcheck.add(from_import_m.group("name"))

            # Expect exactly two blank lines between the last import statement
            # and the next statement.
            if (
                import_m
                or from_import_m
                or (
                    # Hack to match continuation lines for long ImportFrom
                    # statements.
                    last_import_line is not None
                    and (line.startswith("    ") or line == ")")
                )
            ):
                last_import_line = i
            else:
                if last_import_line and i != last_import_line + 3:
                    # Consider that comments that appear too close to import
                    # lines are part of these imports, so they must themselves
                    # be separated from regular statements with two empty
                    # lines.
                    if line.startswith("#"):
                        last_import_line = i
                    else:
                        report.add(
                            "Two empty lines required between the last"
                            " import statement and this line."
                        )
                        last_import_line = None
                else:
                    last_import_line = None

        if parse:
            try:
                root = ast.parse(content)
            except (SyntaxError, TypeError) as exc:
                report.add("Could not parse: {}".format(exc))
            else:

                def node_lineno(node: ast.AST) -> int:
                    return getattr(node, "lineno", 0) + 1

                for node in ast.walk(root):
                    if isinstance(node, ast.ImportFrom):
                        report.set_context(filename, node_lineno(node) - 1)
                        if node.module == "__future__":
                            for alias in node.names:
                                if alias.name != "annotations":
                                    report.add(
                                        "Forbidden annotation: {}".format(
                                            alias.name
                                        )
                                    )
                        else:
                            self._check_imported_entities(report, node)

                    elif (
                        isinstance(node, ast.stmt)
                        and isinstance(node, ast.Expr)
                        and isinstance(node.value, ast.Str)
                    ):
                        # Sometimes we use docstrings on local variables, and
                        # ast.get_docstring does not allow us to catch that.
                        # Instead, process all string literals that appear at
                        # the root of a statement.
                        raw_docstring = node.value.s
                        docstring, lineno_offset = preprocess_docstring(
                            raw_docstring
                        )

                        check_text(
                            report,
                            filename,
                            self,
                            node_lineno(node),
                            docstring,
                            False,
                        )

    def _check_imported_entities(
        self,
        report: Report,
        import_node: ast.ImportFrom,
    ) -> None:
        last = None
        for alias in import_node.names:
            name = alias.name
            if last and last > name:
                report.add(
                    'Imported entity "{}" should appear after "{}"'.format(
                        last, name
                    )
                )
            last = name


class LktLang(LanguageChecker):
    comment_start = "#"
    with_re = re.compile("^import (?P<name>[a-zA-Z0-9_]+)")

    def check(
        self,
        report: Report,
        filename: str,
        content: str,
        parse: bool,
    ) -> None:
        pcheck = PackageChecker(report)
        for i, line in iter_lines(content):
            report.set_context(filename, i)
            if not line.strip():
                pcheck.reset()

            m = self.with_re.match(line)
            if m:
                pcheck.add(m.group("name"))


class MakoLang(LanguageChecker):
    comment_start = "##"

    def check(
        self,
        report: Report,
        filename: str,
        content: str,
        parse: bool,
    ) -> None:
        first_line = content.split("\n", 1)[0]
        if "makoada" in first_line:
            ada_lang.check(report, filename, content, parse=False)
            check_generic(report, filename, content, ada_lang)
        elif "makopython" in first_line:
            python_lang.custom_check(report, filename, content, parse=False)
            check_generic(report, filename, content, python_lang)


class YAMLLang(LanguageChecker):
    comment_start = "#"
    with_re = None

    def check(
        self,
        report: Report,
        filename: str,
        content: str,
        parse: bool,
    ) -> None:
        pass


ada_lang = AdaLang()
java_lang = JavaLang()
lkt_lang = LktLang()
mako_lang = MakoLang()
python_lang = PythonLang()
yaml_lang = YAMLLang()


langs = {
    "adb": ada_lang,
    "ads": ada_lang,
    "java": java_lang,
    "lkt": lkt_lang,
    "mako": mako_lang,
    "py": python_lang,
    "yaml": yaml_lang,
}


def check_file_content(report: Report, filename: str, content: str) -> None:
    """
    Perform generic and language-specific style checks.

    :param report: The report in which diagnostics must be emitted.
    :param filename: Filename from which the text to check comes.
    :param content: Text on which the checks must be performed.
    """
    ext = filename.split(".")[-1]
    lang = langs[ext]
    check_generic(report, filename, content, lang)
    lang.check(report, filename, content, parse=True)


def check_file(report: Report, filename: str) -> None:  # pragma: no cover
    """
    Perform generic and language-specific style checks.

    :param report: The report in which diagnostics must be emitted.
    :param filename: Filename from which the text to check comes.
    """
    ext = filename.split(".")[-1]
    if ext not in langs:
        return

    with open(filename, "r", encoding="utf-8") as f:
        try:
            content = f.read()
        except UnicodeDecodeError as exc:
            print("{}: cannot decode as UTF-8: {}".format(filename, exc))
            return
    check_file_content(report, filename, content)


def excludes_match(path: str, excludes: list[str]) -> bool:
    """
    Return whether at least one item in `excludes` matches the `path`.
    """
    path = os.path.sep + path
    return any(path.endswith(os.path.sep + e) for e in excludes)


def traverse(report: Report, root: str, excludes: list[str]) -> None:
    """
    Perform generic and language-specific style checks.

    :param report: The report in which diagnostics must be emitted.
    :param root: Root directory in which the files to stylecheck are looked
        for. Filenames are accepted as well.
    :param excludes: List of path to exclude from the search of files to check.
    """

    def process(path: str) -> None:
        """
        Do nothing if ``path`` must be excluded. Otherwise, traverse it if it
        is a directory, or run style checks on it if it is a file.
        """
        if excludes_match(path, excludes):
            return

        if os.path.isdir(path):
            traverse(report, path, excludes)
        else:
            check_file(report, os.path.relpath(path))

    if os.path.isdir(root):
        for item in sorted(os.listdir(root)):
            process(os.path.join(root, item))
    else:
        process(root)


def main(
    src_root: str,
    files: list[str],
    dirs: list[str],
    excludes: list[str],
) -> None:
    """
    Global purpose main procedure.

    :param langkit_root: Root directory for the Langkit source repository.
    :param files: Source files to analyze. If empty, look for all sources in
        the Langkit repository.
    :param dirs: List of directories in which to find sources to check.
    :param excludes: List of directories to exclude from the search.
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


def langkit_main(langkit_root: str, files: list[str] = []) -> None:
    """
    Run main() on Langkit sources.
    """

    def test(*args: str) -> str:
        return os.path.join("testsuite", "tests", *args)

    dirs = [
        os.path.join("contrib", "python"),
        os.path.join("langkit"),
        os.path.join("lkt"),
        os.path.join("manage.py"),
        os.path.join("pyproject.toml"),
        os.path.join("scripts"),
        os.path.join("testsuite"),
        os.path.join("utils"),
    ]
    excludes = [
        "__pycache__",
        "expected_concrete_syntax.lkt",
        os.path.join("contrib", "python", "build"),
        os.path.join("langkit", "adasat"),
        os.path.join("langkit", "support", "obj"),
        os.path.join("lkt", "bootstrap"),
        os.path.join("lkt", "build"),
        "out",
        os.path.join("stylechecks", "tests.py"),
        os.path.join("testsuite", "python_support", "expect.py"),
        os.path.join("testsuite", "python_support", "quotemeta.py"),
        os.path.join("testsuite", "out"),
    ]
    main(langkit_root, files, dirs, excludes)


args_parser = argparse.ArgumentParser(
    description="""
    Check the coding style for the Langkit code base.
"""
)
args_parser.add_argument(
    "--langkit-root",
    default=os.path.dirname(
        os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    ),
    help="Root directory for the Langkit source repository. Used to"
    " automatically look for source files to analyze. If not provided,"
    " default to a path relative to the `langkit.stylechecks` package.",
)
args_parser.add_argument(
    "files",
    nargs="*",
    help="Source files to analyze. If none is provided, look for all sources"
    " in the Langkit repository.",
)


if __name__ == "__main__":
    args = args_parser.parse_args()
    langkit_main(args.langkit_root, args.files)
