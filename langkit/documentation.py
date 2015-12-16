"""
This module provides the various documentation parts to be part of the
generated code.

This is useful in the context of bindings: many docstrings are very similar,
there, leading to the usual maintenance problem with code duplication. This
module is an attempt to reduce code duplication and thus to avoid the
corresponding maintenance problems.

In order to achieve this, we consider that there are entities to document in
various places and that some entities appear in multiple contexts (for instance
in the Ada code and in all bindings). We assign these entities unique names
("documentation entity name"), assign them a documentation chunk here and refer
to them in code generation.

Because some documentations must vary depending on the context (for instance,
the interface of entities can depend on the language binding that exposes
them), these chunks are implemented as Mako templates.
"""

from __future__ import absolute_import

import textwrap

from mako.template import Template


# Mapping: documentation entity name (str) -> mako.template.Template.  All
# templates can use the "lang" parameter, which contains "ada", "c" or "python"
# depending on the binding for which we generate documentation.
documentations = {
    'langkit.initialize': Template("""
        Initialize the library. Must be called before anything else from this
        library and from Langkit_Support.
    """),

    #
    # Main analysis types
    #

    'langkit.analysis_context_type': Template("""
        Context for all source analysis.
    """),
    'langkit.analysis_unit_type': Template("""
        Context for the analysis of a single compilation unit.
        % if lang != 'python':
            References are ref-counted.
        % endif
    """),
    'langkit.node_type': Template("""
        Data type for all AST nodes. AST nodes are assembled to make up a tree.
        See the AST node primitives below to inspect such trees.
        % if lang != 'python':
            References are ref-counted.
        % endif
    """),
    'langkit.node_kind_type': Template("""
        Kind of AST nodes in parse trees.
    """),
    'langkit.token_type': Template("""
        Data type for tokens.
        % if lang != 'python':
            Tokens always come from AST node and have the same lifetime than
            the AST node they come from.
        % endif
    """),
    'langkit.text_type': Template("""
        String encoded in UTF-32 (native endianness).
    """),
    'langkit.text_type.chars': Template("""
        Address for the content of the string.
    """),
    'langkit.text_type.length': Template("""
        Size of the string (in characters).
    """),
    'langkit.diagnostic_type': Template("""
        Analysis unit diagnostics.
    """),

    #
    # Analysis primitives
    #

    'langkit.create_context': Template("""
        Create a new Analysis_Context.
        % if lang != 'python':
            When done with it, invoke Destroy on it.
        % endif

        Charset will be used as a default charset to decode input sources in
        analysis units. Please see GNATCOLL.Iconv for a couple of supported
        charsets. Be careful: passing an unsupported charset here is not
        guaranteed to raise an error here.

        ${TODO} Passing an unsupported charset here is not guaranteed to raise
        an error right here, but this would be really helpful for users.
    """),
    'langkit.destroy_context': Template("""
        Invoke Remove on all the units Context contains and free Context. Thus,
        any analysis unit it contains may survive if there are still references
        to it elsewhere.
    """),

    'langkit.get_unit_from_file': Template("""
        Create a new analysis unit for Filename or return the existing one if
        any. If Reparse is true and the analysis unit already exists, reparse
        it from Filename.

        % if lang != 'python':
            The result is owned by the context: the caller must increase its
            ref-count in order to keep a reference to it.
        % endif

        Use Charset in order to decode the content of Filename. If Charset is
        empty then use the last charset used for this unit, or use the
        context's default if creating this unit.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics.

        When With_Trivia is true, the parsed analysis unit will contain
        trivias. Already existing analysis units are reparsed if needed.
    """),
    'langkit.get_unit_from_buffer': Template("""
        Create a new analysis unit for Filename or return the existing one if
        any. Whether the analysis unit already exists or not, (re)parse it from
        the source code in Buffer.

        % if lang != 'python':
            The result is owned by the context: the caller must increase its
            ref-count in order to keep a reference to it.
        % endif

        Use Charset in order to decode the content of Filename. If Charset is
        empty then use the last charset used for this unit, or use the
        context's default if creating this unit.

        If any failure occurs, such as decoding, lexing or parsing failure,
        return an analysis unit anyway: errors are described as diagnostics.

        When With_Trivia is true, the parsed analysis unit will contain
        trivias. Already existing analysis units are reparsed if needed.
    """),
    'langkit.remove_unit': Template("""
        Remove the corresponding analysis unit from this context. If someone
        still owns a reference to it, it remains available but becomes
        context-less.

        % if lang == 'ada':
            If there is no such analysis unit, raise a Constraint_Error
            exception.
        % elif lang == 'c':
            Return whether the removal was successful (i.e. whether the
            analysis unit existed).
        % elif lang == 'python':
            If there is no such analysis unit, raise a KeyError exception.
        % endif
    """),

    'langkit.unit_reparse_file': Template("""
        Reparse an analysis unit from the associated file. If Charset is empty
        or ${null}, use the last charset successfuly used for this unit,
        otherwise use it to decode the content of Filename.

        If any failure occurs, such as decoding, lexing or parsing
        failure, diagnostic are emitted to explain what happened.
    """),
    'langkit.unit_reparse_buffer': Template("""
        Reparse an analysis unit from a buffer. If Charset is empty or ${null},
        use the last charset successfuly used for this unit, otherwise use it
        to decode the content of Filename.

        If any failure occurs, such as decoding, lexing or parsing
        failure, diagnostic are emitted to explain what happened.
    """),
    'langkit.unit_reparse_generic': Template("""
        Reparse an analysis unit from a buffer, if provided, or from the
        original file otherwise. If Charset is empty or ${null}, use the last
        charset successfuly used for this unit, otherwise use it to decode the
        content of the source file.

        If any failure occurs, such as decoding, lexing or parsing
        failure, diagnostic are emitted to explain what happened.
    """),
    'langkit.unit_root': Template("""
        Return the root AST node for this unit, or ${null} if there is none.
    """),
    'langkit.unit_diagnostic_count': Template("""
        Return the number of diagnostics associated to this unit.
    """),
    'langkit.unit_diagnostic': Template("""
        Get the Nth diagnostic in this unit and store it into *DIAGNOSTIC_P.
        Return zero on failure (when N is too big).
    """),
    'langkit.unit_incref': Template("""
        Increase the reference count to an analysis unit.
        % if lang == 'c':
            Return the reference for convenience.
        % endif
    """),
    'langkit.unit_decref': Template("""
        Decrease the reference count to an analysis unit.
    """),

    #
    # General AST node primitives
    #

    'langkit.node_kind': Template("""
        Get the kind of an AST node.
    """),
    'langkit.kind_name': Template("""
        Helper for textual dump: return the name of a node kind. The returned
        string is a copy and thus must be free'd by the caller.
    """),
    'langkit.node_sloc_range': Template("""
        Get the spanning source location range for an AST node.
    """),
    'langkit.lookup_in_node': Template("""
        Return the bottom-most AST node from NODE that contains SLOC, or
        ${null} if there is none.
    """),
    'langkit.node_parent': Template("""
        Return the lexical parent of NODE, if any. Return ${null} for the root
        AST node or for AST nodes for which no one has a reference to the
        parent.
    """),
    'langkit.node_child_count': Template("""
        Return the number of AST node in NODE's fields.
    """),
    'langkit.node_child': Template("""
        Get the Nth child AST node in NODE's fields and store it into *CHILD_P.
        Return zero on failure (when N is too big).
    """),

    'langkit.token_text': Template("""
        Get the text of the given token.
    """),
    'langkit.text_to_locale_string': Template("""
        Encode some text using the current locale. The result is dynamically
        allocated: it is up to the caller to free it when done with it.

        This is a development helper to make it quick and easy to print token
        and diagnostic text: it ignores errors (when the locale does not
        support some characters). Production code should use real conversion
        routines such as libiconv's in order to deal with UTF-32 texts.
    """),
    'langkit.free': Template("""
        Free dynamically allocated memory.

        This is a helper to free objects from dynamic languages.
    """),

    #
    # Extensions handling
    #

    'langkit.extensions_handling': Template("""
        The following functions makes it possible to attach arbitrary data to
        AST nodes: these are extensions.  Each data is associated with both an
        extension ID and a destructor.  AST nodes can have either none or only
        one extension for a given ID.  The destructor is called when the AST
        node is about to be destroyed itself.

        This mechanism is inteded to ease annotating trees with analysis data
        but also to host node wrappers for language bindings.
    """),
    'langkit.node_extension_destructor': Template("""
        Type for extension destructors.  The parameter are the "node" the
        extension was attached to and the "extension" itself.
    """),
    'langkit.register_extension': Template("""
        Register an extension and return its identifier.  Multiple calls with
        the same name will return the same identifier.
    """),
    'langkit.node_extension': Template("""
        Create an extension slot in "node".  If this node already contains an
        extension for "ext_id", return the existing slot.  If not, create such
        a slot, associate the "dtor" destructor to it and initialize the slot
        to ${null}.  Return a pointer to the slot.

        Note that the pointer is not guaranteed to stay valid after further
        calls to this function.
    """),
}


null_names = {
    'ada':    'null',
    'c':      'NULL',
    'python': 'None',
}
todo_markers = {
    'ada':    '???',
    'c':      'TODO:',
    'python': 'TODO:',
}


def split_paragraphs(text):
    """
    Split arbitrary text into paragraphs.

    :param str text: Text to split. Paragraphs are separated by empty lines.
    :rtype: [str]
    """
    paragraphs = []
    current_paragraph = []

    def end_paragraph():
        """Move the current paragraph (if any) to "paragraphs"."""
        if current_paragraph:
            paragraphs.append(
                ' '.join(current_paragraph)
            )
            current_paragraph[:] = []

    for line in text.split('\n'):
        if line.strip():
            current_paragraph.append(line.strip())
        else:
            end_paragraph()
    end_paragraph()

    return paragraphs


def _render(entity, **kwargs):
    """
    Render a documentation template.

    :param entity: Name for the entity to document, or entity to document.
    :type entity: str|compiled_types.CompiledType
    :param dict kwargs: Additional parameters to pass to the Mako template
        rendering. Must at least contain a "lang" entry to specify the binding
        language.
    :rtype: str
    """
    if isinstance(entity, str):
        lang = kwargs['lang']
        kwargs['null'] = null_names[lang]
        kwargs['TODO'] = todo_markers[lang]
        text = documentations[entity].render(**kwargs)
    else:
        text = entity.doc()
    return text


def get_available_width(indent_level):
    """
    Return the number of available columns on source code lines.

    :param indent_level: Identation level of the source code lines.
    """
    return 79 - indent_level


def format_text(text, column):
    """
    Format some text as mere indented text.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :rtype: str
    """
    available_width = get_available_width(column)
    lines = []
    for i, paragraph in enumerate(split_paragraphs(text)):
        if i > 0:
            lines.append('')
        for line in textwrap.wrap(paragraph, available_width,
                                  drop_whitespace=True):
            lines.append(' ' * column + line)

    return '\n'.join(lines)


def format_ada(text, column):
    """
    Format some text as an Ada comment.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :rtype: str
    """
    available_width = get_available_width(column)
    lines = []
    for i, paragraph in enumerate(split_paragraphs(text)):
        if i > 0:
            lines.append('--')
        for line in textwrap.wrap(paragraph, available_width - 4,
                                  drop_whitespace=True):
            lines.append('--  {}'.format(line))

    return '\n{}'.format(' ' * column).join(lines)


def format_c(text, column):
    """
    Format some text as a C multi-line comment.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :rtype str:
    """
    available_width = get_available_width(column)
    lines = []
    for i, paragraph in enumerate(split_paragraphs(text)):
        if i > 0:
            lines.append('')
        for j, line in enumerate(textwrap.wrap(paragraph, available_width - 3,
                                               drop_whitespace=True)):
            prefix = '/* ' if i == 0 and j == 0 else '   '
            lines.append('{}{}'.format(prefix, line))

    if available_width - len(lines[-1]) >= 4:
        lines[-1] += '  */'
    else:
        line, last_word = lines[-1].rsplit(None, 1)
        lines[-1] = line
        lines.append('   {}   */'.format(last_word))
    return '\n{}'.format(' ' * column).join(lines)


def format_python(text, column):
    """
    Format some text as Python docstring.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :rtype: str
    """
    available_width = get_available_width(column)
    indent = ' ' * column
    lines = ['"""']
    for i, paragraph in enumerate(split_paragraphs(text)):
        if i > 0:
            lines.append('')
        for line in textwrap.wrap(paragraph, available_width,
                                  drop_whitespace=True):
            lines.append(indent + line)

    lines.append(indent + '"""')
    return '\n'.join(lines)


def create_doc_printer(lang, formatter):
    """
    Return a function that prints documentation.

    :param str lang: The default language for which we generate documentation.
    :param formatter: Function that formats text into source code
        documentation. See the ``format_*`` functions above.
    :type formatter: (str, int) -> str
    :rtype: function
    """

    def func(entity, column=0, **kwargs):
        """
        :type entity: str|compiled_types.CompiledType
        :type column: int
        """

        # Tell _render for which binding we are generating documentation
        kwargs.setdefault('lang', lang)

        doc = _render(entity, **kwargs)
        return formatter(doc, column) if doc else ''

    func.__name__ = '{}_doc'.format(lang)
    return func

# The following are functions which return formatted source code documentation
# for an entity. Their arguments are:
#
#   * an entity (string or compiled_types.CompiledType subclass) from which the
#     documentation is retreived;
#
#   * a column number (zero if not provided) used to indent the generated
#     documentation;
#
#   * arbitrary keyword arguments to pass to the documentation Mako templates.

ada_doc = create_doc_printer('ada', format_ada)
c_doc = create_doc_printer('c', format_c)
py_doc = create_doc_printer('python', format_python)
