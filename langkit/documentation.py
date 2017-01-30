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

All templates can use the "lang" parameter, which contains "ada", "c" or
"python" depending on the binding for which we generate documentation.
"""

from __future__ import absolute_import

import textwrap

from mako.template import Template


def instantiate_templates(doc_dict):
    """
    Turn a pure text documentation database into a Mako template one.

    :param doc_dict: Documentation database to convert.
    :type doc_dict: dict[str, str]

    :rtype: dict[str, mako.template.Template]
    """
    return {key: Template(val) for key, val in doc_dict.items()}


base_langkit_docs = {
    'langkit.initialize': """
        Initialize the library. Must be called before anything else from this
        library and from Langkit_Support.
    """,

    #
    # Main analysis types
    #

    'langkit.analysis_context_type': """
        Context for all source analysis.
    """,
    'langkit.analysis_unit_type': """
        Context for the analysis of a single compilation unit.
        % if lang != 'python':
            References are ref-counted.
        % endif
    """,
    'langkit.grammar_rule_type': """
        Gramar rule to use for parsing.
    """,
    'langkit.node_type': """
        Data type for all AST nodes. AST nodes are assembled to make up a tree.
        See the AST node primitives below to inspect such trees.
        % if lang != 'python':
            References are ref-counted.
        % endif
    """,
    'langkit.node_kind_type': """
        Kind of AST nodes in parse trees.
    """,
    'langkit.lexical_env_type': """
        Data type for lexical environments.
    """,
    'langkit.env_rebindings': """
        Data type for env rebindings. For internal use only.
    """,
    'langkit.token_kind': """
        Type for individual tokens.
    """,
    'langkit.token_type': """
        Reference to a token in an analysis unit.
    """,
    'langkit.text_type': """
        String encoded in UTF-32 (native endianness).
    """,
    'langkit.text_type.chars': """
        Address for the content of the string.
    """,
    'langkit.text_type.length': """
        Size of the string (in characters).
    """,
    'langkit.diagnostic_type': """
        Analysis unit diagnostics.
    """,
    'langkit.exception_type': """
        Holder for native exceptions-related information.  Memory management
        for this and all the fields is handled by the library: one just has to
        make sure not to keep references to it.

        TODO: For the moment, this structure contains already formatted
        information, but depending on possible future Ada runtime improvements,
        this might change.
    """,
    'langkit.exception_type.is_fatal': """
        Whether this exception is fatal for this process. If it is fatal, then
        process sanity is no longer guaranteed by Libadalang. If it is not,
        performing further processing is safe.
    """,
    'langkit.exception_type.information': """
        Message and context information associated with this exception.
    """,
    'langkit.invalid_unit_name_error': """
        Raised when an invalid unit name is provided.
    """,
    'langkit.property_error': """
        Raised when an error occurs while evaluating a property.
    """,

    #
    # Analysis primitives
    #

    'langkit.create_context': """
        Create a new Analysis_Context.
        % if lang != 'python':
            The returned value has a ref-count set to 1. If you use shared
            ownership, use ref-counting primitives (Inc_Ref and Dec_Ref).
            Otherwise, just invoke Destroy when you are done with it: the
            ref-count will be ignored.
        % endif

        Charset will be used as a default charset to decode input sources in
        analysis units. Please see GNATCOLL.Iconv for a couple of supported
        charsets. Be careful: passing an unsupported charset here is not
        guaranteed to raise an error here.

        % if lang != 'ada':
            If no charset is provided, take ${ctx.default_charset} as the
            default.
        % endif

        ${TODO} Passing an unsupported charset here is not guaranteed to raise
        an error right here, but this would be really helpful for users.

        % if ctx.default_unit_file_provider:
            If provided, Unit_File_Provider will be used to query the file name
            that corresponds to an unit reference during semantic analysis. If
            it is ${null}, the default one is used instead. It is up to the
            caller to free resources allocated to it when done with the
            analysis context.
        % endif
    """,
    'langkit.context_incref': """
        Increase the reference count to an analysis context.
        % if lang == 'c':
            Return the reference for convenience.
        % endif
    """,
    'langkit.context_decref': """
        Decrease the reference count to an analysis context. Destruction
        happens when the ref-count reaches 0.
    """,
    'langkit.destroy_context': """
        Invoke Remove on all the units Context contains and free Context. Thus,
        any analysis unit it contains may survive if there are still references
        to it elsewhere.
    """,

    'langkit.get_unit_from_file': """
        Create a new analysis unit for Filename or return the existing one if
        any. If Reparse is true and the analysis unit already exists, reparse
        it from Filename.

        % if lang != 'python':
            The result is owned by the context: the caller must increase its
            ref-count in order to keep a reference to it.
        % endif

        % if lang == 'ada':
            Rule controls which grammar rule is used to parse the unit.

            ${TODO} export this feature to the C and Python APIs.
        % endif

        Use Charset in order to decode the content of Filename. If Charset is
        empty then use the last charset used for this unit, or use the
        context's default if creating this unit.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics.

        When With_Trivia is true, the parsed analysis unit will contain
        trivias. Already existing analysis units are reparsed if needed.
    """,
    'langkit.get_unit_from_buffer': """
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

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of
        the returned analysis unit.

        When With_Trivia is true, the parsed analysis unit will contain
        trivias. Already existing analysis units are reparsed if needed.
    """,
    'langkit.get_unit_from_provider': """
        Create a new analysis unit for Name/Kind or return the existing one if
        any. If Reparse is true and the analysis unit already exists, reparse
        it from Filename.

        % if lang != 'python':
            The result is owned by the context: the caller must increase its
            ref-count in order to keep a reference to it.
        % endif

        % if lang == 'ada':
            Rule controls which grammar rule is used to parse the unit.

            ${TODO} export this feature to the C and Python APIs.
        % endif

        Use Charset in order to decode the content of Filename. If Charset is
        empty then use the last charset used for this unit, or use the
        context's default if creating this unit.

        If the unit name cannot be tuned into a file name,
        % if lang == 'ada':
            raise an Invalid_Unit_Name_Error exception.
        % elif lang == 'python':
            raise an InvalidUnitNameError exception.
        % else:
            return ${null}.
        % endif
        If any other failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.

        When With_Trivia is true, the parsed analysis unit will contain
        trivias. Already existing analysis units are reparsed if needed.
    """,
    'langkit.remove_unit': """
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
    """,

    'langkit.unit_reparse_file': """
        Reparse an analysis unit from the associated file. If Charset is empty
        or ${null}, use the last charset successfuly used for this unit,
        otherwise use it to decode the content of Filename.

        If any failure occurs, such as decoding, lexing or parsing
        failure, diagnostic are emitted to explain what happened.
    """,
    'langkit.unit_reparse_buffer': """
        Reparse an analysis unit from a buffer. If Charset is empty or ${null},
        use the last charset successfuly used for this unit, otherwise use it
        to decode the content of Filename.

        If any failure occurs, such as decoding, lexing or parsing
        failure, diagnostic are emitted to explain what happened.
    """,
    'langkit.unit_reparse_generic': """
        Reparse an analysis unit from a buffer, if provided, or from the
        original file otherwise. If Charset is empty or ${null}, use the last
        charset successfuly used for this unit, otherwise use it to decode the
        content of the source file.

        If any failure occurs, such as decoding, lexing or parsing
        failure, diagnostic are emitted to explain what happened.
    """,
    'langkit.unit_root': """
        Return the root AST node for this unit, or ${null} if there is none.
    """,
    'langkit.node_unit': """
        Return the unit that owns an AST node.
    """,
    'langkit.unit_first_token': """
        Return a reference to the first token scanned in this unit.
    """,
    'langkit.unit_last_token': """
        Return a reference to the last token scanned in this unit.
    """,
    'langkit.unit_token_count': """
        Return the number of tokens in this unit.
    """,
    'langkit.unit_trivia_count': """
        Return the number of trivias in this unit. This is 0 for units that
        were parsed with trivia analysis disabled.
    """,
    'langkit.unit_filename': """
        Return the filename an unit is associated to.

        % if lang == 'c':
            The returned string is dynamically allocated and the caller must
            free it when done with it.
        % endif
    """,
    'langkit.unit_diagnostic_count': """
        Return the number of diagnostics associated to this unit.
    """,
    'langkit.unit_diagnostic': """
        Get the Nth diagnostic in this unit and store it into *DIAGNOSTIC_P.
        Return zero on failure (when N is too big).
    """,
    'langkit.unit_has_diagnostics': """
        Return whether this unit has associated diagnostics.
    """,
    'langkit.unit_diagnostics': """
        Return an array that contains the diagnostics associated to this unit.
    """,
    'langkit.unit_incref': """
        Increase the reference count to an analysis unit.
        % if lang == 'c':
            Return the reference for convenience.
        % endif
    """,
    'langkit.unit_decref': """
        Decrease the reference count to an analysis unit.
    """,
    'langkit.unit_context': """
        Return the context that owns this unit.
    """,

    'langkit.unit_populate_lexical_env': """
        Populate the lexical environments for this analysis unit, according to
        the specifications given in the language spec.

        % if lang == 'c':
            Return 0 on failure and 1 on success.
        % else:
            Raise a Property_Error on failure.
        % endif
    """,

    #
    # General AST node primitives
    #

    'langkit.node_kind': """
        Get the kind of an AST node.
    """,
    'langkit.kind_name': """
        Helper for textual dump: return the name of a node kind. The returned
        string is a copy and thus must be free'd by the caller.
    """,
    'langkit.node_sloc_range': """
        Get the spanning source location range for an AST node.
    """,
    'langkit.lookup_in_node': """
        Return the bottom-most AST node from NODE that contains SLOC, or
        ${null} if there is none.
    """,
    'langkit.node_child_count': """
        Return the number of AST node in NODE's fields.
    """,
    'langkit.node_child': """
        Get the Nth child AST node in NODE's fields and store it into *CHILD_P.
        Return zero on failure (when N is too big).
    """,
    'langkit.node_short_image': """
        Return a representation of NODE as a string.
    """,

    'langkit.token_text': """
        Get the text of the given token.
    """,
    'langkit.token_sloc_range': """
        Get the source location range of the given token.
    """,
    'langkit.text_to_locale_string': """
        Encode some text using the current locale. The result is dynamically
        allocated: it is up to the caller to free it when done with it.

        This is a development helper to make it quick and easy to print token
        and diagnostic text: it ignores errors (when the locale does not
        support some characters). Production code should use real conversion
        routines such as libiconv's in order to deal with UTF-32 texts.
    """,
    'langkit.free': """
        Free dynamically allocated memory.

        This is a helper to free objects from dynamic languages.
    """,
    'langkit.destroy_text': """
        If this text object owns the buffer it references, free this buffer.

        Note that even though this accepts a pointer to a text object, it does
        not deallocates the text object itself but rather the buffer it
        references.
    """,

    #
    # Lexical environment primitives
    #

    'langkit.lexical_env_parent': """
        Get the ENV's parent lexical environment. This returns ${null} for the
        root lexical environment.
    """,
    'langkit.lexical_env_node': """
        Get the AST node for which this environment was created.
    """,
    'langkit.lexical_env_get': """
        Look for elements in ENV corresponding to NAME.

        % if lang != 'python':
        The result is a dynamically allocated array. The caller is responsible
        for deallocating it afterwards. The content of the array is owned by
        the corresponding analysis unit, however.
        % endif
    """,

    #
    # Extensions handling
    #

    'langkit.extensions_handling': """
        The following functions makes it possible to attach arbitrary data to
        AST nodes: these are extensions.  Each data is associated with both an
        extension ID and a destructor.  AST nodes can have either none or only
        one extension for a given ID.  The destructor is called when the AST
        node is about to be destroyed itself.

        This mechanism is inteded to ease annotating trees with analysis data
        but also to host node wrappers for language bindings.
    """,
    'langkit.node_extension_destructor': """
        Type for extension destructors.  The parameter are the "node" the
        extension was attached to and the "extension" itself.
    """,
    'langkit.register_extension': """
        Register an extension and return its identifier.  Multiple calls with
        the same name will return the same identifier.
    """,
    'langkit.node_extension': """
        Create an extension slot in "node".  If this node already contains an
        extension for "ext_id", return the existing slot.  If not, create such
        a slot, associate the "dtor" destructor to it and initialize the slot
        to ${null}.  Return a pointer to the slot.

        Note that the pointer is not guaranteed to stay valid after further
        calls to this function.
    """,

    #
    # Unit file providers
    #

    'langkit.unit_kind_type': """
        Specify a kind of analysis unit. Specification units provide an
        interface to the outer world while body units provide an implementation
        for the corresponding interface.
    """,
    'langkit.unit_file_provider_type': """
        Interface type for an object that can turn an analysis unit reference
        represented as an AST node into a file name. This is used get
        inter-unit analysis working.
    """,
    'langkit.unit_file_provider_get_file_from_node': """
        Turn an analysis unit reference represented as an AST node into a file
        name.
        % if lang == 'ada':
            Raise a Property_Error
        % else:
            Return ${null}
        % endif
        if Node is not a valid unit name representation.

        % if lang == 'c':
            The result is heap allocated and the caller must free it when done
            with it.
        % endif
    """,
    'langkit.unit_file_provider_get_file_from_name': """
        Turn an analysis unit reference represented as a textual name into a
        file name.
        % if lang == 'ada':
            Raise a Property_Error
        % else:
            Return ${null}
        % endif
        if Name is not a valid unit name.

        % if lang == 'c':
            The result is heap allocated and the caller must free it when done
            with it.
        % endif
    """,
    'langkit.unit_file_provider_destroy': """
        Free any resources that needs to be free'd in "data".
    """,

    'langkit.create_unit_file_provider': """
        Create an unit file provider. When done with it, the result must be
        passed to ${capi.get_name('destroy_unit_file_provider')}.

        Pass as "data" a pointer to hold your private data: it will be passed
        to all callbacks below.

        "destroy" is a callback that is called by
        ${capi.get_name('destroy_unit_file_provider')} to leave a chance to
        free resources that "data" may hold.

        "get_file_from_node" is a callback. It turns an analysis unit reference
        represented as an AST node into a file name. It should return ${null}
        if Node is not a valid unit name representation.  Its result is heap
        allocated and the caller must free it when done with it.

        "get_file_from_name" is a callback similar to "get_file_from_node",
        except it takes an analysis unit reference represented as a string.
    """,
    'langkit.destroy_unit_file_provider': """
        Destroy an unit file provider. This calls the "destroy" callback: see
        ${capi.get_name('create_unit_file_provider')} for more information.
    """,

    'langkit.unit_file_provider_destroy_type': """
        Callback type for functions that are called when destroying an unit
        file provider type.
    """,
    'langkit.unit_file_provider_get_file_from_node_type': """
        Callback type for functions that are called to turn an unit reference
        encoded as an AST node into a file name.
    """,
    'langkit.unit_file_provider_get_file_from_name_type': """
        Callback type for functions that are called to turn an unit reference
        encoded as an AST node into a file name.
    """,

    #
    # Misc
    #

    'langkit.get_last_exception': """
        Return exception information for the last error that happened in the
        current thread. Will be automatically allocated on error and free'd on
        the next error.
    """,
    'langkit.token_kind_name': """
        Return a human-readable name for a token kind.

        % if lang == 'c':
            The returned string is dynamically allocated and the caller must
            free it when done with it.

            If the given kind is invalid, return NULL and set the last
            exception accordingly.
        % endif
    """,
    'langkit.token_next': """
        Return a reference to the next token in the corresponding analysis
        unit.
    """,
    'langkit.token_is_equivalent': """
        Return whether L and R are structurally equivalent tokens. This means
        that their position in the stream won't be taken into account, only the
        kind and text of the token.
    """,
    'langkit.token_previous': """
        Return a reference to the previous token in the corresponding analysis
        unit.
    """,
    'langkit.token_range_text': """
        Compute the source buffer slice corresponding to the text that spans
        between the First and Last tokens. This yields an empty slice if Last
        actually appears before First.
        % if lang == 'c':
            Put the result in RESULT.
        % endif

        % if lang == 'ada':
            This raises a Constraint_Error
        % elif lang == 'c':
            This returns 0
        % elif lang == 'python':
            This raises a ValueError
        % endif
        if First and Last don't belong to the same analysis unit.
        % if lang == 'c':
            Return 1 if successful.
        % endif
    """
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


def _render(ctx, entity, **kwargs):
    """
    Render a documentation template.

    :param langkit.compile_context.CompileContext: Context for the rendering.
    :param entity: Name for the entity to document, or entity to document.
    :type entity: str|compiled_types.CompiledType
    :param dict kwargs: Additional parameters to pass to the Mako template
        rendering. Must at least contain a "lang" entry to specify the binding
        language.
    :rtype: str
    """
    if isinstance(entity, str):
        kwargs['ctx'] = ctx
        kwargs['capi'] = ctx.c_api_settings

        lang = kwargs['lang']
        kwargs['null'] = null_names[lang]
        kwargs['TODO'] = todo_markers[lang]
        text = ctx.documentations[entity].render(**kwargs)
    else:
        text = entity.doc()
    return text


def get_available_width(indent_level):
    """
    Return the number of available columns on source code lines.

    :param indent_level: Identation level of the source code lines.
    """
    return 79 - indent_level


text_wrapper = textwrap.TextWrapper(drop_whitespace=True)


def format_text(text, column):
    """
    Format some text as mere indented text.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :rtype: str
    """
    text_wrapper.available_width = get_available_width(column)
    lines = []
    for i, paragraph in enumerate(split_paragraphs(text)):
        if i > 0:
            lines.append('')
        for line in text_wrapper.wrap(paragraph):
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

        from langkit.compile_context import get_context
        ctx = get_context()

        # Tell _render for which binding we are generating documentation
        kwargs.setdefault('lang', lang)

        doc = _render(ctx, entity, **kwargs)
        return formatter(doc, column) if doc else ''

    func.__name__ = '{}_doc'.format(lang)
    return func

# The following are functions which return formatted source code documentation
# for an entity. Their arguments are:
#
#   * An entity (string or compiled_types.CompiledType subclass) from which the
#     documentation is retreived.
#
#   * A column number (zero if not provided) used to indent the generated
#     documentation.
#
#   * Arbitrary keyword arguments to pass to the documentation Mako templates.


ada_doc = create_doc_printer('ada', format_ada)
c_doc = create_doc_printer('c', format_c)
py_doc = create_doc_printer('python', format_python)


def ada_c_doc(entity, column=0, **kwargs):
    """
    Shortcut to render documentation for a C entity with an Ada doc syntax.

    :type entity: str|compiled_types.CompiledType
    :type column: int
    """
    return ada_doc(entity, column, lang='c', **kwargs)
