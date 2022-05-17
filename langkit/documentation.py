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

from __future__ import annotations

import inspect
import textwrap

from dataclasses import dataclass, replace
from typing import (
    Any, Callable, Dict, List, Optional, Set, TYPE_CHECKING, Union, cast
)

import docutils.frontend
import docutils.nodes
import docutils.parsers
import docutils.parsers.rst
import docutils.parsers.rst.roles
import docutils.utils
from mako.template import Template

from langkit.diagnostics import (
    Severity, check_source_language,
    diagnostic_context, get_current_location
)
from langkit.utils import memoized


if TYPE_CHECKING:
    from typing import Protocol
    from langkit.compile_context import CompileCtx
    from langkit.compiled_types import ASTNodeType, CompiledType

    NodeNameGetter = Callable[[CompileCtx, ASTNodeType], str]

else:
    # We want to support Python 3.7, and typing.Protocol was introduced in
    # Python 3.8. Our only use of this, as a base class, is to type-check the
    # codebase using Mypy, so we can just use object at runtime.
    Protocol = object


class DocDatabase:
    """
    Database for documentation entries.
    """

    def __init__(self, dict: Dict[str, Template]) -> None:
        self._dict = dict
        """
        Documentation database.
        """

        self._used: Set[str] = set()
        """
        Set of names for documentation database that were actually used.
        """

    def __getitem__(self, key: str) -> Template:
        self._used.add(key)
        return self._dict[key]

    def report_unused(self) -> None:
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


def instantiate_templates(doc_dict: Dict[str, str]) -> DocDatabase:
    """
    Turn a pure text documentation database into a Mako template one.

    :param doc_dict: Documentation database to convert.
    """
    return DocDatabase({key: Template(val) for key, val in doc_dict.items()})


base_langkit_docs = {
    #
    # Main analysis types
    #

    'langkit.analysis_context_type': """
        This type represents a context for all source analysis. This is the
        first type you need to create to use ${ctx.lib_name}. It will contain
        the results of all analysis, and is the main holder for all the data.

        You can create several analysis contexts if you need to, which enables
        you, for example to:

        * analyze several different projects at the same time;
        * analyze different parts of the same projects in parallel.

        In the current design, contexts always keep all of their analysis units
        allocated. If you need to get this memory released, the only option at
        your disposal is to destroy your analysis context instance.

        % if lang == 'c':
        This structure is partially opaque: some fields are exposed to allow
        direct access, for performance concerns.
        % endif
    """,

    'langkit.analysis_unit_type': """
        This type represents the analysis of a single file.

        % if lang != 'python':
        This type has strong-reference semantics and is ref-counted.
        Furthermore, a reference to a unit contains an implicit reference to
        the context that owns it. This means that keeping a reference to a unit
        will keep the context and all the unit it contains allocated.
        % endif

        % if lang == 'c':
        This structure is partially opaque: some fields are exposed to allow
        direct access, for performance concerns.
        % endif
    """,
    'langkit.node_type': """
        Data type for all nodes. Nodes are assembled to make up a tree.  See
        the node primitives below to inspect such trees.

        % if lang != 'python':
        Unlike for contexts and units, this type has weak-reference semantics:
        keeping a reference to a node has no effect on the decision to keep the
        unit that it owns allocated. This means that once all references to the
        context and units related to a node are dropped, the context and its
        units are deallocated and the node becomes a stale reference: most
        operations on it will raise a ``Stale_Reference_Error``.

        Note that since reparsing an analysis unit deallocates all the nodes it
        contains, this operation makes all reference to these nodes stale as
        well.
        % endif
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

        .. TODO: For the moment, this structure contains already formatted
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
    'langkit.syntax_error': """
        Subprograms may raise this when they try to parse invalid syntax.
        % if lang == "ocaml":
        Also raised if a field in a parsing node is null due to a syntax
        error.
        % else:
        Note that this does *not* concern analysis unit getters, which
        create diagnostic vectors for such errors.
        % endif
    """,
    'langkit.file_read_error': """
        Subprograms may raise this when they cannot open a source file. Note
        that this does *not* concern analysis unit getters, which create
        diagnostic vectors for such errors.
    """,
    'langkit.introspection.bad_type_error': """
        Raised when introspection functions (``${ctx.lib_name}.Introspection``)
        are provided mismatching types/values.
    """,
    'langkit.introspection.out_of_bounds_error': """
        Raised when introspection functions (``${ctx.lib_name}.Introspection``)
        are passed an out of bounds index.
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

        .. TODO: Passing an unsupported charset here is not guaranteed to
           raise an error right here, but this would be really helpful for
           users.

        When ``With_Trivia`` is true, the parsed analysis units will contain
        trivias.

        If provided, ``File_Reader`` will be used to fetch the contents of
        source files instead of the default, which is to just read it from the
        filesystem and decode it using the regular charset rules. Note that if
        provided, all parsing APIs that provide a buffer are forbidden, and any
        use of the rewriting API with the returned context is rejected.

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

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.

        % if lang == 'ada':
        It is invalid to pass ``True`` to ``Reparse`` if a rewriting context is
        active.
        % endif
    """,
    'langkit.get_unit_from_buffer': """
        Create a new analysis unit for ``Filename`` or return the existing one
        if any. Whether the analysis unit already exists or not, (re)parse it
        from the source code in ``Buffer``.

        ``Rule`` controls which grammar rule is used to parse the unit.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.

        % if lang == 'ada':
        Calling this is invalid if a rewriting context is active.
        % endif
    """,
    'langkit.get_unit_from_provider': """
        Create a new analysis unit for ``Name``/``Kind`` or return the existing
        one if any. If ``Reparse`` is true and the analysis unit already
        exists, reparse it from the on-disk source file.

        The ``Name`` and ``Kind`` arguments are forwarded directly to query the
        context's unit provider and get the filename for the returned unit.
        % if lang == 'python':
        ``Name`` must be a string, while ``Kind`` must be an
        ``AnalysisUnitKind`` enumeration value.
        % endif
        See the documentation of the relevant unit provider for their exact
        semantics.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

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

        % if lang == 'ada':
        It is invalid to pass ``True`` to ``Reparse`` if a rewriting context is
        active.
        % endif
    """,

    'langkit.unit_context': """
        Return the context that owns this unit.
    """,
    'langkit.unit_hash': """
        Return a hash for this unit, to be used in hash tables.
    """,
    'langkit.unit_reparse_file': """
        Reparse an analysis unit from the associated file.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

        If any failure occurs, such as decoding, lexing or parsing failure,
        diagnostic are emitted to explain what happened.
    """,
    'langkit.unit_reparse_buffer': """
        Reparse an analysis unit from a buffer.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

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
    'langkit.unit_dump_lexical_env': """
        Debug helper: output the lexical envs for the given analysis unit.
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
        Get the Nth diagnostic in this unit and store it into
        ``*diagnostic_p``. Return zero on failure (when N is too big).
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
        return ``0`` on failure and ``1`` on success.
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
        ``*child_p``.  Return zero on failure (when ``N`` is too big).
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
    'langkit.node_image': """
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
    'langkit.get_versions': """
        Allocate strings to represent the library version number and build date
        and put them in Version/Build_Date. Callers are expected to call free()
        on the returned string once done.
    """,
    'langkit.string_type': """
        Type to contain Unicode text data.
    """,
    'langkit.create_string': """
        Create a string value from its content (UTF32 with native endianity).

        Note that the CONTENT buffer argument is copied: the returned value
        does not contain a reference to it.
    """,
    'langkit.string_dec_ref': """
        Decrease the reference count for this string.
    """,

    #
    # Iterators
    #

    'langkit.iterator_type': """
        % if lang == 'python':
        Base class for Ada iterator bindings.

        % endif;
        An iterator provides a mean to retrieve values one-at-a-time.

        % if lang == 'ada':
        Resource management for iterators is automatic.
        % endif

        Currently, each iterator is bound to the analysis context used to
        create it. Iterators are invalidated as soon as any unit of that
        analysis is reparsed. Due to the nature of iterators (lazy
        computations), this invalidation is necessary to avoid use of
        inconsistent state, such as an iterator trying to use analysis context
        data that is stale.
    """,

    'langkit.iterator_next': """
        % if lang == 'c':
        Set the next value from the iterator in the given element pointer.
        Return ``1`` if successful, otherwise ``0``.
        % elif lang == 'ada':
        Set the next value from the iterator in the given out argument.
        Return True if successful, otherwise False.
        % elif lang == 'python':
        Return the next value from the iterator. Raises ``StopIteration`` if
        there is no more element to retrieve.
        % endif

        This raises a ``Stale_Reference_Error`` exception if the iterator is
        invalidated.
    """,

    #
    # File readers
    #
    'langkit.file_reader_type': """
        Interface to override how source files are fetched and decoded.
    """,
    'langkit.create_file_reader': """
        Create a file reader. When done with it, the result must be passed to
        ``${capi.get_name('dec_ref_file_reader')}``.

        Pass as ``data`` a pointer to hold your private data: it will be passed
        to all callbacks below.

        ``destroy`` is a callback that is called by
        ``${capi.get_name('dec_ref_file_reader')}`` to leave a chance to free
        resources that ``data`` may hold.

        ``read`` is a callback. For a given filename/charset and whether to
        read the BOM (Byte Order Mark), it tries to fetch the contents of the
        source file, returned in ``Contents``. If there is an error, it must
        return it in ``Diagnostic`` instead.
    """,
    'langkit.file_reader_read': """
        Read the content of the source at the given filename, decoding it using
        the given charset and decoding the byte order mark if ``Read_BOM`` is
        true.

        If there is an error during this process, append an error message to
        Diagnostics. In that case, Contents is considered uninitialized.

        Otherwise, allocate a Text_Type buffer, fill it and initialize Contents
        to refer to it.
    """,
    'langkit.file_reader_inc_ref': """
        Create an ownership share for this file reader.
    """,
    'langkit.file_reader_dec_ref': """
        Release an ownership share for this file reader. This destroys the file
        reader if there are no shares left.

        % if lang == 'ada':
        Return whether there are no ownership shares left.
        % endif
    """,
    'langkit.file_reader_destroy_type': """
        Callback type for functions that are called when destroying a file
        reader.
    """,
    'langkit.file_reader_read_type': """
        Callback type for functions that are called to fetch the decoded source
        buffer for a requested filename.
    """,

    #
    # Event handlers
    #
    'langkit.create_event_handler': """
        Create an event handler. When done with it, the result must be passed
        to ``${capi.get_name('dec_ref_event_handler')}``.

        Pass as ``data`` a pointer to hold your private data: it will be passed
        to all callbacks below.

        ``destroy`` is a callback that is called by
        ``${capi.get_name('dec_ref_event_handler')}`` to leave a chance to
        free resources that ``data`` may hold.

        ``unit_requested`` is a callback that will be called when a unit is
        requested.

        .. warning:: Please note that the unit requested callback can be called
            *many* times for the same unit, so in all likeliness, those events
            should be filtered if they're used to forward diagnostics to the
            user.

        ``unit_parsed`` is a callback that will be called when a unit is
        parsed.
    """,
    'langkit.event_handler_type': """
        Interface to handle events sent by the analysis context.
    """,
    'langkit.event_handler_unit_requested_type': """
        Callback type for functions that are called when a unit is requested.

        ``name`` is the name of the requested unit.

        ``from`` is the unit from which the unit was requested.

        ``found`` indicates whether the requested unit was found or not.

        ``is_not_found_error`` indicates whether the fact that the unit was not
        found is an error or not.

        .. warning:: The interface of this callback is probably subject to
            change, so should be treated as experimental.
    """,
    'langkit.event_handler_unit_parsed_type': """
        Callback type for functions that are called when a unit is parsed.

        ``unit`` is the resulting unit.

        ``reparsed`` indicates whether the unit was reparsed, or whether it was
        the first parse.
    """,
    'langkit.event_handler_destroy_type': """
        Callback type for functions that are called when destroying an event
        handler.
    """,
    'langkit.event_handler_inc_ref': """
        Create an ownership share for this event handler.
    """,
    'langkit.event_handler_dec_ref': """
        Release an ownership share for this event handler. This destroys the
        event handler if there are no shares left.

        % if lang == 'ada':
        Return whether there are no ownership shares left.
        % endif
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

        See the documentation of each unit provider for the exact semantics of
        the unit name/kind information.
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
    'langkit.unit_provider_inc_ref': """
        Create an ownership share for this unit provider.
    """,
    'langkit.unit_provider_dec_ref': """
        Release an ownership share for this unit provider. This destroys the
        unit provider if there are no shares left.

        % if lang == 'ada':
            Return whether there are no ownership shares left.
        % endif
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
        ``self`` and ``other``
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
        This returns ``0``
        % elif lang == 'python':
        This raises a ``ValueError``
        % endif
        if ``First`` and ``Last`` don't belong to the same analysis unit.
        % if lang == 'c':
        Return ``1`` if successful.
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
    'langkit.rewriting.unit_unparse': """
        Return the text associated to the given unit.
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
        is ``No_Rewriting_Handle`` for a node that is not tied to any tree yet.
    """,
    'langkit.rewriting.children_count': """
        Return the number of children the node represented by Handle has
    """,
    'langkit.rewriting.child': """
        Return a handle corresponding to the Index'th child of the node that
        Handle represents. Index is 1-based.
    """,
    'langkit.rewriting.set_child': """
        If Child is ``No_Rewriting_Node``, untie the Handle's ``Index``'th
        child to this tree, so it can be attached to another one. Otherwise,
        Child must have no parent as it will be tied to ``Handle``'s tree.
    """,
    'langkit.rewriting.text': """
        Return the text associated to the given token node.
    """,
    'langkit.rewriting.set_text': """
        Override text associated to the given token node.
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

    #
    # Python-specific
    #

    'langkit.python.AnalysisUnit.TokenIterator': """
        Iterator over the tokens in an analysis unit.
    """,
    'langkit.python.AnalysisUnit.iter_tokens': """
        Iterator over the tokens in an analysis unit.
    """,
    'langkit.python.AnalysisUnit.diagnostics': """
        Diagnostics for this unit.
    """,
    'langkit.python.Token.__eq__': """
        Return whether the two tokens refer to the same token in the same unit.

        Note that this does not actually compares the token data.
    """,
    'langkit.python.Token.__lt__': """
        Consider that None comes before all tokens. Then, sort by unit, token
        index, and trivia index.
    """,
    'langkit.python.Token.to_data': """
        Return a dict representation of this Token.
    """,
    'langkit.python.FileReader.__init__': """
        This constructor is an implementation detail, and is not meant to be
        used directly.
    """,
    'langkit.python.UnitProvider.__init__': """
        This constructor is an implementation detail, and is not meant to be
        used directly.
    """,
    'langkit.python.root_node.__bool__': """
        Return always True so that checking a node against None can be done as
        simply as::

            if node:
                ...
    """,
    'langkit.python.root_node.__iter__': """
        Return an iterator on the children of this node.
    """,
    'langkit.python.root_node.__len__': """
        Return the number of ${pyapi.root_astnode_name} children this node has.
    """,
    'langkit.python.root_node.__getitem__': """
        Return the Nth ${pyapi.root_astnode_name} child this node has.

        This handles negative indexes the same way Python lists do. Raise an
        IndexError if "key" is out of range.
    """,
    'langkit.python.root_node.iter_fields': """
        Iterate through all the fields this node contains.

        Return an iterator that yields (name, value) couples for all abstract
        fields in this node. If "self" is a list, field names will be
        "item_{n}" with "n" being the index.
    """,
    'langkit.python.root_node.dump_str': """
        Dump the sub-tree to a string in a human-readable format.
    """,
    'langkit.python.root_node.dump': """
        Dump the sub-tree in a human-readable format on the given file.

        :param str indent: Prefix printed on each line during the dump.
        :param file file: File in which the dump must occur.
    """,
    'langkit.python.root_node.findall': """
        Helper for finditer that will return all results as a list. See
        finditer's documentation for more details.
    """,
    'langkit.python.root_node.find': """
        Helper for finditer that will return only the first result. See
        finditer's documentation for more details.
    """,
    'langkit.python.root_node.finditer': """
        Find every node corresponding to the passed predicates.

        :param ast_type_or_pred: If supplied with a subclass of
            ${pyapi.root_astnode_name}, will constrain the resulting collection
            to only the instances of this type or any subclass. If supplied
            with a predicate, it will apply the predicate on every node and
            keep only the ones for which it returns True. If supplied with a
            list of subclasses of ${pyapi.root_astnode_name}, it will match all
            instances of any of them.

        :param kwargs: Allows the user to filter on attributes of the node. For
            every key value association, if the node has an attribute of name
            key that has the specified value, then the child is kept.
    """,
    'langkit.python.root_node.parent_chain': """
        Return the parent chain of self. Self will be the first element,
        followed by the first parent, then this parent's parent, etc.
    """,
    'langkit.python.root_node.tokens': """
        Return an iterator on the range of tokens that self encompasses.
    """,
    'langkit.python.root_node.to_data': """
        Return a nested python data-structure, constituted only of standard
        data types (dicts, lists, strings, ints, etc), and representing the
        portion of the AST corresponding to this node.
    """,
    'langkit.python.root_node.to_json': """
        Return a JSON representation of this node.
    """,
    'langkit.python.root_node.is_a': """
        Shortcut for isinstance(self, types).
        :rtype: bool
    """,
    'langkit.python.root_node.cast': """
        Fluent interface style method. Return ``self``, raise an error if self
        is not of type ``typ``.

        :type typ: () -> T
        :rtype: T
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


class Formatter(Protocol):
    def __call__(self,
                 text: str,
                 column: int,
                 width: int = 79) -> str: ...


def get_line(node: Any) -> Optional[int]:
    """
    Utility function to get the closest line for a given rst node (since
    not all nodes have line information).
    """
    if node is None:
        return None
    if node.line is not None:
        return node.line
    else:
        return get_line(node.parent)


class LangkitTypeRef(docutils.nodes.reference):
    """
    Specific langkit node for a reference to a Langkit CompiledType. Meant to
    be replaced by our visitor by a type ref node that is understandable in the
    role of the language for which we generate documentation, or if there isn't
    such a role, a simple text reference.
    """

    @memoized
    def get_type(self) -> Optional[CompiledType]:
        """
        Return the langkit type this node references.
        """
        from langkit.compiled_types import resolve_type

        c = self['compiled_type']
        if c:
            return resolve_type(c)

        return None

    @staticmethod
    def role_fn(
        name: Any, rawtext: Any, text: Any, lineno: Any,
        inliner: Any, options: Any = {}, content: Any = []
    ) -> Any:
        """
        Role function to create a ``LangkitTypeRef`` node.
        """
        from langkit.compiled_types import T
        ct = getattr(T, text, None)

        node = LangkitTypeRef(rawtext, text, compiled_type=ct, **options)
        return [node], []


docutils.parsers.rst.roles.register_local_role(
    "typeref", LangkitTypeRef.role_fn
)


#
# Global data used by docutils visitors
#

TAGNAMES_WITH_SURROUNDINGS = {
    "literal": "``",
    "emphasis": "*",
    "strong": "**",
}

SUPPORTED_ADMONITIONS = [
    "attention", "caution", "danger", "error", "hint", "important", "note",
    "tip", "warning", "admonition"
]

EXPLICITLY_FORBIDDEN_TAGS = ['title_reference']

SUPPORTED_TAGS = [
    "#text", "comment", "field", "paragraph", "list_item", "literal_block",
    "enumerated_list", "field_name", "document", "bullet_list",
    "system_message", "problematic", "warning", "field_list",
    "field_name", "field_body", "block_quote"
] + SUPPORTED_ADMONITIONS + list(TAGNAMES_WITH_SURROUNDINGS.keys())

SKIP_CHILDREN = ["field_name", "literal_block"]


class RstCommentChecker(docutils.nodes.GenericNodeVisitor):
    """
    Visitor that will be run on docstrings to check that they're correct,
    e.g. that they respect the subset of ReST that we're supposed to use,
    and the restrictions that we impose on docstrings.
    """

    def default_visit(self, node: Any) -> None:
        # Forward error messages from the parser itself
        if node.tagname in "system_message":
            self.check(node[0], False, node[0].astext())

        # Forbid title references, because they're useless in docstrings,
        # and they're a commonly occuring error in our docstrings.
        elif node.tagname == 'title_reference':
            self.check(
                node, False,
                "title_reference nodes are forbidden in docstrings. You "
                "probably meant to use double backquotes.",
            )

        # Warn for all node types that are not explicitly supported
        elif node.tagname not in SUPPORTED_TAGS:
            self.check(
                node, False,
                f"Unsupported Rst tag: {node.tagname}. Will be excluded "
                "from output."
            )

        # Skip children of nodes that need to be skipped, so that we don't
        # mistakenly encounter an unsupported node that we would have skipped
        # anyway.
        if node.tagname in SKIP_CHILDREN:
            raise docutils.nodes.SkipChildren()

    def unknown_visit(self, node: docutils.nodes.node) -> None:

        if isinstance(node, LangkitTypeRef):
            ct = node.get_type()
            self.check(node, ct is not None, "Wrong type reference")
            raise docutils.nodes.SkipChildren()

    def unknown_departure(self, node: docutils.nodes.node) -> None:
        pass

    def check(self, node: Any, condition: bool, message: str) -> None:
        """
        Utility method, to run a language level langkit check with a proper
        sloc inside of the rst docstring, if possible.
        """

        loc = get_current_location()

        if loc is not None:
            node_line = get_line(node)
            if node_line is not None:
                loc = replace(loc, line=loc.line + node_line + 1)

        with diagnostic_context(loc):
            check_source_language(
                condition,
                message,
                severity=Severity.warning,
                ok_for_codegen=True
            )

    @staticmethod
    def check_doc(doc: Optional[str]) -> None:
        """
        Shortcut to run this visitor on a given (potentially ``None``)
        docstring.
        """
        if doc:
            rst_doc = rst_document(doc)
            visitor = RstCommentChecker(rst_doc)
            rst_doc.walk(visitor)


class RstCommentFormatter(docutils.nodes.GenericNodeVisitor):
    """
    Docutils ``NodeVisitor``, meant to output a formatted rst docstring, with
    text properly wrapped for the given indentation/prefix.
    """

    @dataclass
    class BlockContext:
        node: docutils.nodes.node
        """
        The node for the block.
        """

        initial_prefix: str
        """
        The prefix string for the first line of the block.
        """

        subsequent_prefix: str
        """
        The prefix string for the subsequent lines of the block.
        """

        parts: List[str]
        """
        The list of text parts that make up the block, and that will be
        populated in the visit function.
        """

    def __init__(
        self,
        document: docutils.nodes.document,
        prefix: str,
        get_node_name: NodeNameGetter,
        type_role_name: str = '',
        width: int = 79
    ):
        """
        Construct a new ``RstCommentFormatter`` visitor.

        :param document: The document this visitor will iterate on.

        :param prefix: The string prefix with which we want to prefix every
            line of the resulting output. Typically constituted of the
            whitespace for the desired indentation, plus the prefix for the
            comment style of the output language.

        :param get_node_name: Callable that will return the formatted name of a
            langkit node type, in the desired style for the output language.

        :param type_role_name: String that represents the name of the role for
            type references in the doc for the output language.

        :param width: Maximum width to which to wrap the output.
        """

        super().__init__(document)

        # Instantiation data, used to parametrize the output
        self.width = width
        self.prefix = prefix
        self.get_node_name = get_node_name
        self.type_role_name = type_role_name

        # Context variables
        self.surrounding = ""
        """
        For text parts that require to be surrounded (like literal blocks),
        this will be set to the appropriate surrounding text.
        """

        self.in_enumerated_list = False
        self.enumerated_list_item_no = 1

        # State variables for the visitor
        self.parts: List[str] = []
        """
        List of toplevel parts to be concatenated at the end of the visit.
        """

        self.block_context_stack: List[RstCommentFormatter.BlockContext] = []
        """
        Stack of block contexts. Contains what is needed to format a block, in
        order:
        """

    @property
    def current_parts(self) -> List[str]:
        """
        Shortcut property to return the list of current parts for the topmost
        entry on the block context stack.
        """
        return self.block_context_stack[-1].parts

    def append_part(self, part: str) -> None:
        """
        Append a part to the list of toplevel parts.
        """
        if part:
            self.parts.append(part)

    def append_context(
        self,
        node: docutils.nodes.Node,
        initial_prefix: str = '',
        subsequent_prefix: str = ''
    ) -> None:
        """
        Append a new block context to the block context stack.
        """
        if self.block_context_stack:
            self.append_part(self.wrap(''.join(self.current_parts)))
            self.current_parts.clear()

        self.block_context_stack.append(
            RstCommentFormatter.BlockContext(
                node, initial_prefix, subsequent_prefix, []
            )
        )

    @property
    def text(self) -> str:
        """
        Get the constructed docstring's text out of this visitor.
        """
        lines = f"\n{self.prefix}\n".join(self.parts).splitlines()
        return "\n".join(l.rstrip() for l in lines)

    @property
    def initial_indent(self) -> str:
        """
        Helper property to get the indent text for the first line of a
        formatted block in the current context.
        """
        if self.block_context_stack:
            return ''.join(
                t.subsequent_prefix for t in self.block_context_stack[:-1]
            ) + self.block_context_stack[-1].initial_prefix
        else:
            return ''

    @property
    def subsequent_indent(self) -> str:
        """
        Helper property to get the indent text for the subsequent lines of a
        formatted block in the current context.
        """
        return ''.join(t.subsequent_prefix for t in self.block_context_stack)

    def wrap(self, text: str) -> str:
        """
        Helper method to wrap text with the desired settings.
        """
        return "\n".join(textwrap.wrap(
            text, self.width,
            initial_indent=self.prefix + self.initial_indent,
            subsequent_indent=self.prefix + self.subsequent_indent
        ))

    def unknown_visit(self, node: docutils.nodes.node) -> None:
        """
        Visit function for langkit specific nodes.
        """

        from langkit.compile_context import get_context
        from langkit.compiled_types import ASTNodeType

        if isinstance(node, LangkitTypeRef):
            ct = node.get_type()
            if not ct:
                return
            # TODO: For the moment ``:typeref:`` will only work for AST node
            # types.
            assert isinstance(ct, ASTNodeType)
            type_name = self.get_node_name(get_context(), ct)
            if self.type_role_name:
                self.current_parts.append(
                    f"{self.type_role_name}`{type_name}`"
                )
            else:
                self.current_parts.append(f"``{type_name}``")

            raise docutils.nodes.SkipChildren()

    def unknown_departure(self, node: docutils.nodes.node) -> None:
        pass

    def default_visit(self, node: docutils.nodes.node) -> None:
        """
        Visit function for generic docutils/sphinx nodes.
        """
        if node.tagname == "#text":
            # Text nodes are added to parts, and will be later built via the
            # builder.
            self.current_parts.append(
                f"{self.surrounding}{node.astext()}{self.surrounding}"
            )

        elif node.tagname == "comment":
            # Comments are kept, but maybe we should get rid of them?
            self.append_context(node, ".. ", "   ")

        elif node.tagname == "field":
            self.append_context(node, f":{node[0].astext()}: ", '   ')

        elif node.tagname == "paragraph":
            # If a paragraph is not part of a larger block that has a
            # builder, then add a simple builder for the paragraph that will
            # simply wrap.
            if not self.block_context_stack:
                self.append_context(node, '', '')

        elif node.tagname == "list_item":
            if self.in_enumerated_list:
                initial_indent = f'{self.enumerated_list_item_no}. '
                self.enumerated_list_item_no += 1
            else:
                initial_indent = '* '
            subsequent_indent = ' ' * len(initial_indent)
            self.append_context(node, initial_indent, subsequent_indent)

        elif node.tagname in SUPPORTED_ADMONITIONS:
            self.append_context(node, f".. {node.tagname}:: ", '   ')

        elif node.tagname in TAGNAMES_WITH_SURROUNDINGS.keys():

            # If we have a node that will "surround" the text inside with
            # some special character, set the ``surrounding`` variable,
            # which will be used when we get to the inside text.
            # NOTE: Inline tags with surrounding characters are handled via a
            # separate mechanism than builders, but we might be able to have
            # a stack of builders and only use builders, if we do some
            # adjustments.
            self.surrounding = TAGNAMES_WITH_SURROUNDINGS[node.tagname]

        elif node.tagname == "literal_block":
            # Literal blocks are code blocks. For those we want to bypass
            # the mechanism we use for every other block that will wrap the
            # resulting text, and instead preserve the original formatting.
            classes = set(node["classes"])
            classes = classes - {"code"}
            try:
                lang = classes.pop()
            except KeyError:
                lang = ""
            self.append_part(
                f"{self.prefix}{self.subsequent_indent}.. code:: {lang}"
                .rstrip()
            )

            self.append_part("\n".join(
                f"{self.prefix}{self.subsequent_indent}   {l}"
                for l in node.astext().splitlines()
            ))
        elif node.tagname == "enumerated_list":
            # TODO: Add support for nested enumerated lists
            self.in_enumerated_list = True
            self.enumerated_list_item_no = 1

        if node.tagname not in SUPPORTED_TAGS or node.tagname in SKIP_CHILDREN:
            # Skip nodes that are not supported: we know that we have warned
            # the user previously, now we can just ignore the content. It
            # will be stripped from the output.
            raise docutils.nodes.SkipChildren()

    def default_departure(self, node: Any) -> None:
        """
        Departure (post children traversal) visit function for generic
        docutils/sphinx nodes.
        """
        if node.tagname == "enumerated_list":
            self.in_enumerated_list = False
        elif node.tagname in [
            "field", "list_item", "paragraph", "comment"
        ] + SUPPORTED_ADMONITIONS:
            if self.block_context_stack[-1].node == node:
                self.append_part(self.wrap(''.join(self.current_parts)))
                # Reset data
                self.block_context_stack.pop()
        elif node.tagname in TAGNAMES_WITH_SURROUNDINGS:
            self.surrounding = ""


default_settings = docutils.frontend.OptionParser(
    components=(docutils.parsers.rst.Parser,)
).get_default_values()
# Don't emit any report on stdout/stderr
default_settings.report_level = 4


@memoized
def rst_document(text: str) -> docutils.nodes.document:
    """
    From a given docstring, return a docutils document.

    .. note:: This might have large strings as inputs, and as such is maybe
        not optimal. We will probably be able to get rid of that by directly
        storing the docutils document in entities at some stage, but this is a
        good first step solution.
    """

    document = docutils.utils.new_document("<input>", default_settings)
    parser = docutils.parsers.rst.Parser()
    parser.parse(text, document)
    return document


def make_formatter(
    prefix: str = '',
    suffix: str = '',
    line_prefix: str = '',
    get_node_name: NodeNameGetter = lambda c, n: n.name.lower,
    type_role_name: str = ''
) -> Formatter:
    """
    Create a formatter function which, given a text that contains a list of
    paragraphs, return a list of lines that are formatted correctly, with
    wrapped paragraphs, the given ``line_prefix`` for each line, and given
    ``prefix`` and ``suffix``.

    If the ``:typeref:`` Langkit directive is used in the docstring,
    ``get_node_name`` will be used to translate the name to the proper name in
    the given language, and ``type_role_name`` will be used as the name for the
    type reference role in the given language.

    The first line of the outputted text will not be indented, since that's our
    need in templates.

    The resulting function has the following parameters:

    * ``text``, which is the original text of the docstring.

    * ``column``, which is the starting column at which the resulting docstring
      must be indented.

    * ``width``, which is an optional parameter which defaults to ``79``,
      specifying the maximum width the text must be wrapped to.
    """

    def formatter(text: str, column: int, width: int = 79) -> str:
        text = inspect.cleandoc(text)
        indent = ' ' * column
        pfx = indent + line_prefix

        document = rst_document(text)
        visitor = RstCommentFormatter(
            document, prefix=pfx, get_node_name=get_node_name,
            type_role_name=type_role_name
        )
        document.walkabout(visitor)

        return "\n".join([prefix, visitor.text, indent + suffix]).strip()

    return formatter


class DocPrinter(Protocol):
    def __call__(self,
                 entity: Union[str, CompiledType],
                 column: int = 0,
                 lang: str = '',
                 **kwargs: Any) -> str: ...


def create_doc_printer(
    lang: str,
    formatter: Formatter,
) -> DocPrinter:
    """
    Return a function that prints documentation.

    :param lang: The default language for which we generate documentation.
    :param formatter: Function that formats text into source code
        documentation. See the ``format_*`` functions above.
    """

    def func(entity:
             Union[str, CompiledType],
             column: int = 0,
             lang: str = lang,
             **kwargs: Any) -> str:
        """
        :param entity: Name for the entity to document, or entity to document.
        :param column: Indentation level for the result.
        :param lang: Language for the documentation.
        :param kwargs: Parameters to be passed to the specific formatter.
        """
        from langkit.compile_context import get_context
        from langkit.compiled_types import T

        ctx = get_context()

        doc: str

        if isinstance(entity, str):
            doc = ctx.documentations[entity].render(
                ctx=get_context(),
                capi=ctx.c_api_settings,
                pyapi=ctx.python_api_settings,
                lang=lang,
                null=null_names[lang],
                TODO=todo_markers[lang],
                T=T,
            )
        elif entity.doc:
            doc = entity.doc
        else:
            doc = ""

        return formatter(doc, column, **kwargs)

    func.__name__ = '{}_doc'.format(lang)
    return func


# The following are functions which return a docstring as formatted text for
# the given language. See ``make_formatter``'s documentation for the arguments.

format_text = make_formatter()
format_ada = make_formatter(
    line_prefix='--  ',
    get_node_name=lambda ctx, node: node.entity.api_name,
    type_role_name=':ada:ref:'
)
format_c = make_formatter(
    prefix='/*', line_prefix=' * ', suffix=' */',

    # In the C header, there is only one node type, so use kind enumerators
    # instead.
    get_node_name=(lambda ctx, node:
                   ctx.c_api_settings.get_name(node.kwless_raw_name)),
)
format_python = make_formatter(
    prefix='"""', suffix='"""',
    get_node_name=(lambda ctx, node:
                   ctx.python_api_settings.type_public_name(node)),
    type_role_name=':py:class:'
)
format_ocaml = make_formatter(
    prefix='(**', line_prefix=' * ', suffix=' *)',
    get_node_name=(lambda ctx, node:
                   ctx.ocaml_api_settings
                   .type_public_name(node.entity))
)


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

ada_doc = create_doc_printer(
    'ada', cast(Formatter, format_ada),
)
c_doc = create_doc_printer(
    'c', cast(Formatter, format_c),
)
py_doc = create_doc_printer(
    'python', cast(Formatter, format_python),
)
ocaml_doc = create_doc_printer(
    'ocaml', cast(Formatter, format_ocaml),
)


def ada_c_doc(entity: Union[str, CompiledType], column: int = 0) -> str:
    """
    Shortcut to render documentation for a C entity with an Ada doc syntax.

    :type entity: str|compiled_types.CompiledType
    :type column: int
    """
    return ada_doc(entity, column, lang='c')
