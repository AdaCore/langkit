"""
Various helpers to format text.
"""

from __future__ import annotations


def append_paragraph(text: str, paragraph: str) -> str:
    """
    Append a paragraph to `text`.

    :param text: Potentially empty block of text.
    :param paragraph: Non-empty paragraph to add.
    """
    return ('{}\n\n{}'.format(text.rstrip(), paragraph)
            if text else paragraph)


def indent(text: str, level: int) -> str:
    """
    Return `text` with all non-empty lines indented by `level` columns.

    :param text: Text to process.
    :param level: Number of columns for the indentation to insert.
    """
    prefix = ' ' * level
    return '\n'.join((prefix + line) if line.strip() else ''
                     for line in text.splitlines())


def first_line_indentation(text: str) -> int:
    """
    Return the indentation level of the first non-empty line in `text`.

    :param text: Text to process.
    """
    lines = text.splitlines()

    # Look for the first line that is not empty
    for line in lines:
        if line.strip():
            return len(line) - len(line.lstrip())
    return 0
