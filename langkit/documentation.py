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

from __future__ import absolute_import, division, print_function

import textwrap

from mako.template import Template


class DocDatabase(object):
    """
    Database for documentation entries.
    """

    def __init__(self, dict):
        self._dict = dict
        """
        Documentation database.

        :type: dict[str, str]
        """

        self._used = set()
        """
        Set of names for documentation database that were actually used.

        :type: set[str]
        """

    def __getitem__(self, key):
        self._used.add(key)
        return self._dict[key]

    def report_unused(self):
        """
        Report all documentation entries that have not been used on the
        standard output. Either they should be used, or they should be removed.
        """
        unused = set(self._dict) - self._used
        if unused:
            print('The following documentation entries were not used in code'
                  ' generation:')
            for k in sorted(unused):
                print('   ', k)


def instantiate_templates(doc_dict):
    """
    Turn a pure text documentation database into a Mako template one.

    :param doc_dict: Documentation database to convert.
    :type doc_dict: dict[str, str]

    :rtype: dict[str, mako.template.Template]
    """
    return DocDatabase({key: Template(val) for key, val in doc_dict.items()})


base_langkit_docs = {
    'langkit.initialize': """
        Initialize the library. Must be called before anything else from this
        library and from Langkit_Support.
    """,

    #
    # Main analysis types
    #

    'langkit.analysis_context_type': """
        This type represents a context for all source analysis. This is the
        first type you need to create to use ${ctx.lib_name}. It will contain
        the results of all analysis, and is the main holder for all the data.

        You can create several analysis contexts if you need to, which enables
        you, for example to:

        - Analyze several different projects at the same time
        - Analyze different parts of the same projects in parallel

        In its current design, ${ctx.lib_name} will keep all the data it
        analyzes for-ever, so if you need to get memory back, the only option
        at your disposal is to destroy your ``Analysis_Context`` instance.

        % if lang == 'c':
        This structure is partially opaque: some fields are exposed to allow
        direct access, for performance concerns.
        % endif
    """,

    'langkit.analysis_unit_type': """
        This type represents the analysis of a single file.

        % if lang != 'python':
        References are ref-counted.
        % endif

        % if lang == 'c':
        This structure is partially opaque: some fields are exposed to allow
        direct access, for performance concerns.
        % endif
    """,
    'langkit.node_type': """
        Data type for all nodes. Nodes are assembled to make up a tree.  See
        the node primitives below to inspect such trees.
    """,
    'langkit.node_kind_type': """
        Kind of AST nodes in parse trees.
    """,
    'langkit.symbol_type': """
        Reference to a symbol. Symbols are owned by analysis contexts, so they
        must not outlive them. This type exists only in the C API, and roughly
        wraps the corresponding Ada type (an array fat pointer).
    """,
    'langkit.env_rebindings_type': """
        Data type for env rebindings. For internal use only.
    """,
    'langkit.sloc_type': """
        Location in a source file. Line and column numbers are one-based.
    """,
    'langkit.sloc_range_type': """
        Location of a span of text in a source file.
    """,
    'langkit.token_kind': """
        Kind for this token.
    """,
    'langkit.token_reference_type': """
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
    'langkit.big_integer_type': """
        Arbitrarily large integer.
    """,
    'langkit.diagnostic_type': """
        Diagnostic for an analysis unit: cannot open the source file, parsing
        error, ...
    """,
    'langkit.exception_kind_type': """
        Enumerated type describing all possible exceptions that need to be
        handled in the C bindings.
    """,
    'langkit.exception_type': """
        Holder for native exceptions-related information.  Memory management
        for this and all the fields is handled by the library: one just has to
        make sure not to keep references to it.

        .. todo:: For the moment, this structure contains already formatted
           information, but depending on possible future Ada runtime
           improvements, this might change.
    """,
    'langkit.exception_type.kind': """
        The kind of this exception.
    """,
    'langkit.exception_type.information': """
        Message and context information associated with this exception.
    """,
    'langkit.invalid_unit_name_error': """
        Raised when an invalid unit name is provided.
    """,
    'langkit.native_exception': """
        Exception raised in language bindings when the underlying C API reports
        an unexpected error that occurred in the library.

        This kind of exception is raised for internal errors: they should never
        happen in normal situations and if they are raised at some point, it
        means the library state is potentially corrupted.

        Nevertheless, the library does its best not to crash the program,
        materializing internal errors using this kind of exception.
    """,
    'langkit.precondition_failure': """
        Exception raised when an API is called while its preconditions are not
        satisfied.
    """,
    'langkit.property_error': """
        Exception that is raised when an error occurs while evaluating any
        ${'function' if lang == 'ada' else 'AST node method'}
        whose name starts with
        ``${'P_' if lang == 'ada' else 'p_'}``. This is the only exceptions
        that such functions can raise.
    """,
    'langkit.invalid_symbol_error': """
        Exception raise when an invalid symbol is passed to a subprogram.
    """,
    'langkit.stale_reference_error': """
        Exception raised while trying to access data that was deallocated. This
        happens when one tries to use a node whose unit has been reparsed, for
        instance.
    """,
    'langkit.unknown_charset': """
        Raised by lexing functions (``${ctx.lib_name}.Lexer``) when the input
        charset is not supported.
    """,
    'langkit.invalid_input': """
        Raised by lexing functions (``${ctx.lib_name}.Lexer``) when the input
        contains an invalid byte sequence.
    """,
    'langkit.introspection.invalid_field': """
        Raised when introspection functions
        (``${ctx.lib_name}.Introspection``) are requested an invalid field.
    """,
    'langkit.introspection.node_data_evaluation_error': """
        Raised when introspection functions (``${ctx.lib_name}.Introspection``)
        are improperly used to evaluate a node data (field or property).
    """,
    'langkit.rewriting.template_format_error': """
        Exception raised when a template has an invalid syntax, such as badly
        formatted placeholders.
    """,
    'langkit.rewriting.template_args_error': """
        Exception raised when the provided arguments for a template don't match
        what the template expects.
    """,
    'langkit.rewriting.template_instantiation_error': """
        Exception raised when the instantiation of a template cannot be parsed.
    """,

    #
    # Analysis primitives
    #

    'langkit.create_context': """
        Create a new analysis context.

        ``Charset`` will be used as a default charset to decode input sources
        in analysis units. Please see ``GNATCOLL.Iconv`` for several supported
        charsets. Be careful: passing an unsupported charset is not guaranteed
        to raise an error here. If no charset is provided,
        ``"${ctx.default_charset}"`` is the default.

        .. todo:: Passing an unsupported charset here is not guaranteed to
           raise an error right here, but this would be really helpful for
           users.

        When ``With_Trivia`` is true, the parsed analysis units will contain
        trivias.

        If provided, ``Unit_Provider`` will be used to query the file name
        that corresponds to a unit reference during semantic analysis. If
        it is ``${null}``, the default one is used instead.

        ``Tab_Stop`` is a positive number to describe the effect of tabulation
        characters on the column number in source files.
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
    'langkit.context_hash': """
        Return a hash for this context, to be used in hash tables.
    """,
    'langkit.context_symbol': """
        If the given string is a valid symbol, yield it as a symbol and return
        true. Otherwise, return false.
    """,
    'langkit.context_discard_errors_in_populate_lexical_env': """
        Debug helper. Set whether ``Property_Error`` exceptions raised in
        ``Populate_Lexical_Env`` should be discarded. They are by default.
    """,
    'langkit.context_set_logic_resolution_timeout': """
        If ``Timeout`` is greater than zero, set a timeout for the resolution
        of logic equations. The unit is the number of steps in ANY/ALL
        relations.  If ``Timeout`` is zero, disable the timeout. By default,
        the timeout is ``100 000`` steps.
    """,

    'langkit.get_unit_from_file': """
        Create a new analysis unit for ``Filename`` or return the existing one
        if any. If ``Reparse`` is true and the analysis unit already exists,
        reparse it from ``Filename``.

        ``Rule`` controls which grammar rule is used to parse the unit.

        Use ``Charset`` in order to decode the source file. If ``Charset`` is
        empty then use the last charset used for this unit, or use the
        context's default if creating this unit.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.
    """,
    'langkit.get_unit_from_buffer': """
        Create a new analysis unit for ``Filename`` or return the existing one
        if any. Whether the analysis unit already exists or not, (re)parse it
        from the source code in ``Buffer``.

        ``Rule`` controls which grammar rule is used to parse the unit.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the last charset used for this unit, or use the context's
        default if creating this unit.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.
    """,
    'langkit.get_unit_from_provider': """
        Create a new analysis unit for ``Name``/``Kind`` or return the existing
        one if any. If ``Reparse`` is true and the analysis unit already
        exists, reparse it from ``Filename``.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the last charset used for this unit, or use the context's
        default if creating this unit.

        If the unit name cannot be tuned into a file name,
        % if lang == 'ada':
            raise an ``Invalid_Unit_Name_Error`` exception.
        % elif lang == 'python':
            raise an ``InvalidUnitNameError`` exception.
        % else:
            return ``${null}``.
        % endif
        If any other failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.
    """,

    'langkit.unit_context': """
        Return the context that owns this unit.
    """,
    'langkit.unit_hash': """
        Return a hash for this unit, to be used in hash tables.
    """,
    'langkit.unit_reparse_file': """
        Reparse an analysis unit from the associated file. If ``Charset`` is
        empty or ``${null}``, use the last charset successfuly used for this
        unit, otherwise use it to decode the source file.

        If any failure occurs, such as decoding, lexing or parsing failure,
        diagnostic are emitted to explain what happened.
    """,
    'langkit.unit_reparse_buffer': """
        Reparse an analysis unit from a buffer. If ``Charset`` is empty or
        ``${null}, use the last charset successfuly used for this unit,
        otherwise use it to decode the source.

        If any failure occurs, such as decoding, lexing or parsing failure,
        diagnostic are emitted to explain what happened.
    """,
    'langkit.unit_reparse_generic': """
        Reparse an analysis unit from a buffer, if provided, or from the
        original file otherwise. If ``Charset`` is empty or ``${null}``, use
        the last charset successfuly used for this unit, otherwise use it to
        decode the content of the source file.

        If any failure occurs, such as decoding, lexing or parsing failure,
        diagnostic are emitted to explain what happened.
    """,
    'langkit.unit_root': """
        Return the root node for this unit, or ``${null}`` if there is none.
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
    'langkit.unit_text': """
        Return the source buffer associated to this unit.
    """,
    'langkit.unit_lookup_token': """
        Look for a token in this unit that contains the given source location.
        If this falls before the first token, return the first token. If this
        falls between two tokens, return the token that appears before. If this
        falls after the last token, return the last token. If there is no token
        in this unit, return no token.
    """,
    'langkit.unit_filename': """
        Return the filename this unit is associated to.

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

    'langkit.unit_populate_lexical_env': """
        Create lexical environments for this analysis unit, according to the
        specifications given in the language spec.

        If not done before, it will be automatically called during semantic
        analysis. Calling it before enables one to control where the latency
        occurs.

        Depending on whether errors are discarded (see
        ``Discard_Errors_In_Populate_Lexical_Env``),
        % if lang == 'c':
            return 0 on failure and 1 on success.
        % else:
            raise a ``Property_Error`` on failure.
        % endif
    """,

    #
    # General AST node primitives
    #

    'langkit.node_kind': """
        Return the kind of this node.
    """,
    'langkit.kind_name': """
        Helper for textual dump: return the kind name for this node.
        % if lang == 'c':
        The returned string is a copy and thus must be free'd by the caller.
        % endif
    """,
    'langkit.node_unit': """
        Return the analysis unit that owns this node.
    """,
    'langkit.node_text': """
        Return the source buffer slice corresponding to the text that spans
        between the first and the last tokens of this node.

        Note that this returns the empty string for synthetic nodes.
    """,
    'langkit.node_sloc_range': """
        Return the spanning source location range for this node.

        Note that this returns the sloc of the parent for synthetic nodes.
    """,
    'langkit.lookup_in_node': """
        Return the bottom-most node from in ``Node`` and its children which
        contains ``Sloc``, or ``${null}`` if there is none.
    """,
    'langkit.node_children_count': """
        Return the number of children in this node.
    """,
    'langkit.node_child': """
        Return the Nth child for in this node's fields and store it into
        *CHILD_P.  Return zero on failure (when N is too big).
    """,
    'langkit.node_is_null': """
        Return whether this node is a null node reference.
    """,
    'langkit.node_is_token_node': """
        Return whether this node is a node that contains only a single token.
    """,
    'langkit.node_is_synthetic': """
        Return whether this node is synthetic.
    """,
    'langkit.node_short_image': """
        Return a representation of this node as a string.
    """,
    'langkit.entity_image': """
        Return a representation of this entity as a string.
    """,

    'langkit.token_text': """
        Return the text of the given token.
    """,
    'langkit.token_sloc_range': """
        Return the source location range of the given token.
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
    'langkit.symbol_text': """
        Return the text associated to this symbol.
    """,
    'langkit.create_big_integer': """
        Create a big integer from its string representation (in base 10).
    """,
    'langkit.big_integer_text': """
        Return the string representation (in base 10) of this big integer.
    """,
    'langkit.big_integer_decref': """
        Decrease the reference count for this big integer.
    """,

    #
    # Unit providers
    #

    'langkit.unit_provider_type': """
        Interface to fetch analysis units from a name and a unit kind.

        The unit provider mechanism provides an abstraction which assumes that
        to any couple (unit name, unit kind) we can associate at most one
        source file. This means that several couples can be associated to the
        same source file, but on the other hand, only one one source file can
        be associated to a couple.

        This is used to make the semantic analysis able to switch from one
        analysis units to another.
    """,
    'langkit.unit_provider_get_unit_filename': """
        Return the filename corresponding to the given unit name/unit kind.
        % if lang == 'ada':
            Raise a ``Property_Error``
        % else:
            Return ``${null}``
        % endif
        if the given unit name is not valid.
    """,
    'langkit.unit_provider_get_unit_from_name': """
        Fetch and return the analysis unit referenced by the given unit name.
        % if lang == 'ada':
            Raise a ``Property_Error``
        % else:
            Return ``${null}``
        % endif
        if the given unit name is not valid.
    """,
    'langkit.unit_provider_destroy': """
        Free any resources that needs to be free'd in ``data``.
    """,

    'langkit.create_unit_provider': """
        Create a unit provider. When done with it, the result must be passed to
        ``${capi.get_name('destroy_unit_provider')}``.

        Pass as ``data`` a pointer to hold your private data: it will be passed
        to all callbacks below.

        ``destroy`` is a callback that is called by
        ``${capi.get_name('destroy_unit_provider')}`` to leave a chance to free
        resources that ``data`` may hold.

        ``get_unit_from_node`` is a callback. It turns an analysis unit
        reference represented as a node into an analysis unit. It should return
        ``${null}`` if the node is not a valid unit name representation.

        ``get_unit_from_name`` is a callback similar to ``get_unit_from_node``
        except it takes an analysis unit reference represented as a string.
    """,
    'langkit.destroy_unit_provider': """
        Destroy a unit provider. This calls the ``destroy`` callback: see
        ``${capi.get_name('create_unit_provider')}`` for more information.
    """,

    'langkit.unit_provider_destroy_type': """
        Callback type for functions that are called when destroying a unit file
        provider type.
    """,
    'langkit.unit_provider_get_unit_filename_type': """
        Callback type for functions that are called to turn a unit reference
        encoded as a unit name into an analysis unit.
    """,
    'langkit.unit_provider_get_unit_from_name_type': """
        Callback type for functions that are called to turn a unit reference
        encoded as a unit name into an analysis unit.
    """,

    #
    # Misc
    #

    'langkit.get_last_exception': """
        Return exception information for the last error that happened in the
        current thread. Will be automatically allocated on error and free'd on
        the next error.
    """,
    'langkit.synthetic_nodes': """
        Set of nodes that are synthetic.

        Parsers cannot create synthetic nodes, so these correspond to no source
        text. These nodes are created dynamically for convenience during
        semantic analysis.
    """,
    'langkit.token_kind_name': """
        Return a human-readable name for a token kind.

        % if lang == 'c':
        The returned string is dynamically allocated and the caller must free
        it when done with it.

        If the given kind is invalid, return ``NULL`` and set the last
        exception accordingly.
        % endif
    """,
    'langkit.token_next': """
        Return a reference to the next token in the corresponding analysis
        unit.
    """,
    'langkit.token_previous': """
        Return a reference to the previous token in the corresponding analysis
        unit.
    """,
    'langkit.token_range_until': """
        Return ${'an iterator on' if lang == 'python' else ''} the list of
        tokens that spans between
        % if lang == 'python':
            `self` and `other`
        % else:
            the two input tokens
        % endif
        (included). This returns an empty list if the first token appears after
        the other one in the source code.
        % if lang == 'python':
            Raise a ``ValueError`` if both tokens come from different analysis
            units.
        % endif
    """,
    'langkit.token_is_equivalent': """
        Return whether ``L`` and ``R`` are structurally equivalent tokens. This
        means that their position in the stream won't be taken into account,
        only the kind and text of the token.
    """,
    'langkit.token_range_text': """
        Compute the source buffer slice corresponding to the text that spans
        between the ``First`` and ``Last`` tokens (both included). This yields
        an empty slice if ``Last`` actually appears before ``First``.
        % if lang == 'c':
        Put the result in ``RESULT``.
        % endif

        % if lang == 'ada':
            This raises a ``Constraint_Error``
        % elif lang == 'c':
            This returns 0
        % elif lang == 'python':
            This raises a ``ValueError``
        % endif
        if ``First`` and ``Last`` don't belong to the same analysis unit.
        % if lang == 'c':
            Return 1 if successful.
        % endif
    """,
    'langkit.token_is_trivia': """
        Return whether this token is a trivia. If it's not, it's a regular
        token.
    """,
    'langkit.token_index': """
        % if lang == 'ada':
            One-based
        % else:
            Zero-based
        % endif
        index for this token/trivia. Tokens and trivias get their own index
        space.
    """,

    #
    # Misc
    #

    'langkit.rewriting.rewriting_handle_type': """
        Handle for an analysis context rewriting session
    """,
    'langkit.rewriting.unit_rewriting_handle_type': """
        Handle for the process of rewriting an analysis unit. Such handles are
        owned by a Rewriting_Handle instance.
    """,
    'langkit.rewriting.node_rewriting_handle_type': """
        Handle for the process of rewriting an AST node. Such handles are owned
        by a Rewriting_Handle instance.
    """,
    'langkit.rewriting.context_handle': """
        Return the rewriting handle associated to Context, or
        No_Rewriting_Handle if Context is not being rewritten.
    """,
    'langkit.rewriting.handle_context': """
        Return the analysis context associated to Handle
    """,
    'langkit.rewriting.start_rewriting': """
        Start a rewriting session for Context.

        This handle will keep track of all changes to do on Context's analysis
        units. Once the set of changes is complete, call the Apply procedure to
        actually update Context. This makes it possible to inspect the "old"
        Context state while creating the list of changes.

        There can be only one rewriting session per analysis context, so this
        will raise an Existing_Rewriting_Handle_Error exception if Context
        already has a living rewriting session.
    """,
    'langkit.rewriting.abort_rewriting': """
        Discard all modifications registered in Handle and close Handle
    """,
    'langkit.rewriting.apply': """
        Apply all modifications to Handle's analysis context. If that worked,
        close Handle and return (Success => True). Otherwise, reparsing did not
        work, so keep Handle and its Context unchanged and return details about
        the error that happened.
    """,
    'langkit.rewriting.unit_handles': """
        Return the list of unit rewriting handles in the given context handle
        for units that the Apply primitive will modify.
    """,
    'langkit.rewriting.unit_handle': """
        Return the rewriting handle corresponding to Unit
    """,
    'langkit.rewriting.handle_unit': """
        Return the unit corresponding to Handle
    """,
    'langkit.rewriting.root': """
        Return the node handle corresponding to the root of the unit which
        Handle designates.
    """,
    'langkit.rewriting.set_root': """
        Set the root node for the unit Handle to Root. This unties the previous
        root handle. If Root is not No_Node_Rewriting_Handle, this also ties
        Root to Handle.

        Root must not already be tied to another analysis unit handle.
    """,
    'langkit.rewriting.node_handle': """
        Return the rewriting handle corresponding to Node.

        The owning unit of Node must be free of diagnostics.
    """,
    'langkit.rewriting.handle_node': """
        Return the node which the given rewriting Handle relates to. This can
        be the null entity if this handle designates a new node.
    """,
    'langkit.rewriting.node_context': """
        Return a handle for the rewriting context to which Handle belongs
    """,
    'langkit.rewriting.unparse': """
        Turn the given rewritten node Handles designates into text. This is the
        text that is used in Apply in order to re-create an analysis unit.
    """,
    'langkit.rewriting.kind': """
        Return the kind corresponding to Handle's node
    """,
    'langkit.rewriting.tied': """
        Return whether this node handle is tied to an analysis unit. If it is
        not, it can be passed as the Child parameter to Set_Child.
    """,
    'langkit.rewriting.parent': """
        Return a handle for the node that is the parent of Handle's node. This
        is No_Rewriting_Handle for a node that is not tied to any tree yet.
    """,
    'langkit.rewriting.children_count': """
        Return the number of children the node represented by Handle has
    """,
    'langkit.rewriting.child': """
        Return a handle corresponding to the Index'th child of the node that
        Handle represents. Index is 1-based.
    """,
    'langkit.rewriting.set_child': """
        If Child is No_Rewriting_Node, untie the Handle's Index'th child to
        this tree, so it can be attached to another one. Otherwise, Child must
        have no parent as it will be tied to Handle's tree.
    """,
    'langkit.rewriting.text': """
        Return the text associated to the given token node
    """,
    'langkit.rewriting.set_text': """
        Override text associated to the given token node
    """,
    'langkit.rewriting.replace': """
        If Handle is the root of an analysis unit, untie it and set New_Node as
        its new root. Otherwise, replace Handle with New_Node in Handle's
        parent node.

        Note that:
        * Handle must be tied to an existing analysis unit handle.
        * New_Node must not already be tied to another analysis unit handle.
    """,
    'langkit.rewriting.insert_child': """
        Assuming Handle refers to a list node, insert the given Child node to
        be in the children list at the given index.

        The given Child node must not be tied to any analysis unit.
    """,
    'langkit.rewriting.append_child': """
        Assuming Handle refers to a list node, append the given Child node to
        the children list.

        The given Child node must not be tied to any analysis unit.
    """,
    'langkit.rewriting.remove_child': """
        Assuming Handle refers to a list node, remove the child at the given
        Index from the children list.
    """,
    'langkit.rewriting.clone': """
        Create a clone of the Handle node tree. The result is not tied to any
        analysis unit tree.
    """,
    'langkit.rewriting.create_node': """
        Create a new node of the given Kind, with empty text (for token nodes)
        or children (for regular nodes).
    """,
    'langkit.rewriting.create_token_node': """
        Create a new token node with the given Kind and Text
    """,
    'langkit.rewriting.create_regular_node': """
        Create a new regular node of the given Kind and assign it the given
        Children.

        Except for lists, which can have any number of children, the
        size of Children must match the number of children associated to the
        given Kind. Besides, all given children must not be tied.
    """,
    'langkit.rewriting.create_from_template': """
        Create a tree of new nodes from the given Template string, replacing
        placeholders with nodes in Arguments and parsed according to the given
        grammar Rule.
    """,
}


null_names = {
    'ada':    'null',
    'c':      'NULL',
    'python': 'None',
    'ocaml':  'None',
}
todo_markers = {
    'ada':    '???',
    'c':      'TODO:',
    'python': 'TODO:',
    'ocaml':  'TODO:',
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
        line = line.strip()
        is_bullet = line.startswith('- ') or line.startswith('* ')
        if line and not is_bullet:
            current_paragraph.append(line)
        elif is_bullet:
            end_paragraph()
            current_paragraph.append(line)
        else:
            end_paragraph()
    end_paragraph()

    return paragraphs


def get_available_width(indent_level, width=None):
    """
    Return the number of available columns on source code lines.

    :param int indent_level: Identation level of the source code lines.
    :param int|None width: Total available width on source code lines. By
        default, use 79.
    """
    if width is None:
        width = 79
    return width - indent_level


text_wrapper = textwrap.TextWrapper(drop_whitespace=True)


def wrap(paragraph, width):
    result = textwrap.wrap(paragraph, width)

    # If this is a Sphinx admonition, preserve its formatting so that it's
    # still an admonition.
    if result and result[0].startswith('.. '):
        first_line, rest = result[0], result[1:]
        result = [first_line] + [
            '   ' + line
            for line in textwrap.wrap('\n'.join(rest), width - 3)
        ]

    return result


def format_text(text, column, width=None):
    """
    Format some text as mere indented text.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :param int|None width: See get_available_width.
    :rtype: str
    """
    lines = []
    for i, paragraph in enumerate(split_paragraphs(text)):
        if i > 0:
            lines.append('')
        for line in wrap(paragraph, get_available_width(column, width)):
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
        for line in wrap(paragraph, available_width - 4):
            lines.append('--  {}'.format(line))

    return '\n{}'.format(' ' * column).join(lines)


def format_c(text, column):
    """
    Format some text as a C multi-line comment.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :rtype str:
    """
    if not text.strip():
        return ''

    available_width = get_available_width(column)
    lines = []
    for i, paragraph in enumerate(split_paragraphs(text)):
        if i > 0:
            lines.append('')
        for j, line in enumerate(wrap(paragraph, available_width - 3)):
            prefix = '/* ' if i == 0 and j == 0 else '   '
            lines.append('{}{}'.format(prefix, line))

    if available_width - len(lines[-1]) >= 4:
        lines[-1] += '  */'
    else:
        line, last_word = lines[-1].rsplit(None, 1)
        lines[-1] = line
        lines.append('   {}   */'.format(last_word))
    return '\n{}'.format(' ' * column).join(lines)


def format_python(text, column, argtypes=[], rtype=None):
    """
    Format some text as Python docstring.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :param list[(str, langkit.compiled_types.CompiledType)]: List of argument
        names and argument types, to be appended as ``:type:`` Sphinx
        annotations.
    :param None|langkit.compiled_types.CompiledType rtype: If non-None, append
        to the formatted docstring a Sphinx-style ``:rtype:`` annotation, whose
        type is the given ``rtype``.
    :rtype: str
    """
    from langkit.compile_context import get_context

    def fmt_type(t):
        return get_context().python_api_settings.type_public_name(t)

    # Number of columns for the documentation text itself, per line
    available_width = get_available_width(column)

    # Identation to prepend to every line, except the first
    indent = ' ' * column

    lines = []

    # Add the given documentation text
    if text.strip():
        for i, paragraph in enumerate(split_paragraphs(text)):
            if i > 0:
                lines.append('')
            for line in textwrap.wrap(paragraph, available_width,
                                      drop_whitespace=True):
                lines.append(line)
        lines.append('')

    # Add types for arguments, if provided. Note that the ":param:" directive
    # is required in order for the type to appear in the Sphinx autodoc.
    for argname, argtype in argtypes:
        lines.append(':param {}:'.format(argname))
        lines.append(':type {}: {}'.format(argname, fmt_type(argtype)))

    # Add the return type, if provided
    if rtype:
        lines.append(':rtype: {}'.format(fmt_type(rtype)))

    # Remove any trailing empty line
    if lines and not lines[-1]:
        lines.pop()

    # Append indentation and multi-line string delimiters
    lines = ['"""'] + [
        '{}{}'.format(indent, line) if line else ''
        for line in lines
    ] + [indent + '"""']
    return '\n'.join(lines)


def format_ocaml(text, column):
    """
    Format some text as a OCaml multi-line comment.

    :param str text: Text to format.
    :param int column: Indentation level for the result.
    :rtype str:
    """
    if not text.strip():
        return ''

    available_width = get_available_width(column)
    lines = ['(**']
    for i, paragraph in enumerate(split_paragraphs(text)):
        if i > 0:
            lines.append('')
        for line in wrap(paragraph, available_width - 3):
            lines.append(' * {}'.format(line))

    lines.append(' *)')
    return '\n{}'.format('  ' * column).join(lines)


def create_doc_printer(lang, formatter):
    """
    Return a function that prints documentation.

    :param str lang: The default language for which we generate documentation.

    :param formatter: Function that formats text into source code
        documentation. See the ``format_*`` functions above.
    :type formatter: (str, int) -> str

    :rtype: function
    """

    def func(entity, column=0, lang=lang, **kwargs):
        """
        :param str|compiled_types.CompiledType entity: Name for the entity to
            document, or entity to document.
        :param int column: Indentation level for the result.
        :param str lang: Language for the documentation.
        :param kwargs: Parameters to be passed to the specific formatter.

        :rtype: str
        """

        from langkit.compile_context import get_context
        ctx = get_context()

        template_ctx = dict()
        template_ctx['ctx'] = get_context()
        template_ctx['capi'] = ctx.c_api_settings
        template_ctx['null'] = null_names[lang]
        template_ctx['TODO'] = todo_markers[lang]

        if isinstance(entity, str):
            doc = ctx.documentations[entity].render(
                ctx=get_context(),
                capi=ctx.c_api_settings,
                lang=lang,
                null=null_names[lang],
                TODO=todo_markers[lang]
            )
        else:
            doc = entity.doc or ''

        return formatter(doc, column, **kwargs)

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
ocaml_doc = create_doc_printer('ocaml', format_ocaml)


def ada_c_doc(entity, column=0):
    """
    Shortcut to render documentation for a C entity with an Ada doc syntax.

    :type entity: str|compiled_types.CompiledType
    :type column: int
    """
    return ada_doc(entity, column, lang='c')
