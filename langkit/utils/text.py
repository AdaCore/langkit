"""
Various helpers to format text.
"""


def append_paragraph(text, paragraph):
    """
    Append a paragraph to `text`.

    :param str text: Potentially empty block of text.
    :param str paragraph: Non-empty paragraph to add.
    :rtype: str
    """
    return ('{}\n\n{}'.format(text.rstrip(), paragraph)
            if text else paragraph)


def indent(text, level):
    """
    Return `text` with all non-empty lines indented by `level` columns.

    :param str text: Text to process.
    :param int level: Number of columns for the indentation to insert.
    :rtype: str
    """
    prefix = ' ' * level
    return '\n'.join((prefix + line) if line.strip() else ''
                     for line in text.splitlines())


def first_line_indentation(text):
    """
    Return the indentation level of the first non-empty line in `text`.

    :param str text: Text to process.
    :rtype: int
    """
    lines = text.splitlines()

    # Look for the first line that is not empty
    for line in lines:
        if line.strip():
            return len(line) - len(line.lstrip())
    return 0
