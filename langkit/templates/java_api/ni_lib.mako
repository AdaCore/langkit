<%def name="generate()">
    <%namespace name="ast_node" file="ast_node.mako" />
    <%namespace name="struct" file="struct.mako" />
    <%namespace name="array" file="array.mako" />
    <%namespace name="iterator" file="iterator.mako" />
    <%namespace name="exts" file="/extensions.mako" />
    <%
    api = java_api
    nat = c_api.get_name
    %>

    /** This class contains the directives for the shared lib loading */
    public static final class LibDirectives implements CContext.Directives {
        @Override
        public List<String> getHeaderFiles() {
            List<String> res = new ArrayList<>();
            res.add("<${ctx.lib_name.lower}.h>");
            res.add("<stdlib.h>");
            return res;
        }

        @Override
        public List<String> getLibraries() {
            List<String> res = new ArrayList<>();
            res.add("${ctx.lang_name.lower}lang");
            return res;
        }
    }

    // ===== Language specific structures =====

    ${exts.include_extension(ctx.ext("java_api", "ni_defs"))}

    // ===== Constant structures =====

    /** The structure for the langkit exceptions */
    @CContext(LibDirectives.class)
    @CStruct("${exception_type}")
    public interface LangkitExceptionNative extends PointerBase {
        @CField("kind") public int get_kind();
        @CField("kind") public void set_kind(
            int kind
        );

        @CField("information") public CCharPointer get_information();
        @CField("information") public void set_information(
            CCharPointer information
        );
    }

    /** The big integers are just pointers */
    public interface BigIntegerNative extends Pointer {}

    /** The structure for the symbols */
    @CContext(LibDirectives.class)
    @CStruct("${symbol_type}")
    public interface SymbolNative extends PointerBase {
        @CField("data") public VoidPointer get_data();
        @CField("data") public void set_data(
            VoidPointer data
        );

        @CField("bounds") public VoidPointer get_bounds();
        @CField("bounds") public void set_bounds(
            VoidPointer bounds
        );
    }

    /** The string wrappers are just pointers */
    public interface StringNative extends Pointer {}

    /** The structure for the text */
    @CContext(LibDirectives.class)
    @CStruct("${text_type}")
    public interface TextNative extends PointerBase {
        @CField("chars") public CIntPointer get_chars();
        @CField("chars") public void set_chars(
            CIntPointer chars
        );

        @CField("length") public long get_length();
        @CField("length") public void set_length(
            long length
        );

        @CField("is_allocated") public int get_is_allocated();
        @CField("is_allocated") public void set_is_allocated(
            int is_allocated
        );
    }

    /** The structure for the source locations */
    @CContext(LibDirectives.class)
    @CStruct("${sloc_type}")
    public interface SourceLocationNative extends PointerBase {
        @CField("line") public int get_line();
        @CField("line") public void set_line(
            int line
        );

        @CField("column") public short get_column();
        @CField("column") public void set_column(
            short column
        );
    }

    /** The structure for the source location ranges */
    @CContext(LibDirectives.class)
    @CStruct("${sloc_range_type}")
    public interface SourceLocationRangeNative extends PointerBase {
        @CField("start.line") public int get_start_line();
        @CField("start.line") public void set_start_line(
            int start_line
        );

        @CField("start.column") public short get_start_column();
        @CField("start.column") public void set_start_column(
            short start_column
        );

        @CField("end.line") public int get_end_line();
        @CField("end.line") public void set_end_line(
            int end_line
        );

        @CField("end.column") public short get_end_column();
        @CField("end.column") public void set_end_column(
            short end_column
        );
    }

    /** The structure for the diagnostic */
    @CContext(LibDirectives.class)
    @CStruct("${diagnostic_type}")
    public interface DiagnosticNative extends PointerBase {
        @CField("sloc_range.start.line") public int get_start_line();
        @CField("sloc_range.start.line") public void set_start_line(
            int start_line
        );

        @CField("sloc_range.start.column") public short get_start_column();
        @CField("sloc_range.start.column") public void set_start_column(
            short start_column
        );

        @CField("sloc_range.end.line") public int get_end_line();
        @CField("sloc_range.end.line") public void set_end_line(
            int end_line
        );

        @CField("sloc_range.end.column") public short get_end_column();
        @CField("sloc_range.end.column") public void set_end_column(
            short end_column
        );

        @CField("message.chars") public CIntPointer get_message_chars();
        @CField("message.chars") public void set_message_chars(
            CIntPointer chars
        );

        @CField("message.length") public long get_message_length();
        @CField("message.length") public void set_message_length(
            long length
        );

        @CField("message.is_allocated") public int get_message_is_allocated();
        @CField("message.is_allocated") public void set_message_is_allocated(
            int is_allocated
        );
    }


    /** The file reader is just a pointer */
    public interface FileReaderNative extends Pointer {}

    /** The unit provider is just a pointer */
    public interface UnitProviderNative extends Pointer {}

    /** The event handler is just a pointer */
    public interface EventHandlerNative extends Pointer {}

    /** The event handler unit requested callback type */
    public interface UnitRequestedFunctionPointer extends CFunctionPointer {
        @InvokeCFunctionPointer
        void invoke(
            VoidPointer data,
            AnalysisContextNative context,
            TextNative name,
            AnalysisUnitNative from,
            boolean found,
            boolean is_not_found_error
        );
    }

    /** The event handler unit parsed callback type */
    public interface UnitParsedFunctionPointer extends CFunctionPointer {
        @InvokeCFunctionPointer
        void invoke(
            VoidPointer data,
            AnalysisContextNative context,
            AnalysisUnitNative unit,
            boolean reparsed
        );
    }

    /** Anonymous structure for the token data handler */
    @RawStructure
    public interface TokenDataHandlerNative extends PointerBase {
        @RawField public long version();
    }

    /** The structure representing a token */
    @CContext(LibDirectives.class)
    @CStruct("${token_type}")
    public interface TokenNative extends PointerBase {
        @CField("context") public AnalysisContextNative get_context();
        @CField("context") public void set_context(
            AnalysisContextNative context
        );

        @CField("token_data") public TokenDataHandlerNative get_data();
        @CField("token_data") public void set_data(
            TokenDataHandlerNative data
        );

        @CField("token_index") public int get_token_index();
        @CField("token_index") public void set_token_index(
            int token_index
        );

        @CField("trivia_index") public int get_trivia_index();
        @CField("trivia_index") public void set_trivia_index(
            int trivia_index
        );

        @CField("kind") public int get_kind();
        @CField("kind") public void set_kind(
            int kind
        );

        @CField("text.chars") public CIntPointer get_text_chars();
        @CField("text.chars") public void set_text_chars(
            CIntPointer chars
        );

        @CField("text.length") public long get_text_length();
        @CField("text.length") public void set_text_length(
            long length
        );

        @CField("text.is_allocated") public int get_text_is_allocated();
        @CField("text.is_allocated") public void set_text_is_allocated(
            int is_allocated
        );

        @CField("sloc_range.start.line") public int get_start_line();
        @CField("sloc_range.start.line") public void set_start_line(
            int start_line
        );

        @CField("sloc_range.start.column") public short get_start_column();
        @CField("sloc_range.start.column") public void set_start_column(
            short start_column
        );

        @CField("sloc_range.end.line") public int get_end_line();
        @CField("sloc_range.end.line") public void set_end_line(
            int end_line
        );

        @CField("sloc_range.end.column") public short get_end_column();
        @CField("sloc_range.end.column") public void set_end_column(
            short end_column
        );
    }

    /** Anonymous strucutre for analysis context */
    @RawStructure
    public interface AnalysisContextNative extends PointerBase {
        @RawField public long serial_number();
    }

    /** Anonymous strucutre for analysis unit */
    @RawStructure
    public interface AnalysisUnitNative extends PointerBase {
        @RawField public long version_number();
    }

    // ===== Generated structures =====

    % for struct_type in ctx.struct_types:
        % if struct_type.is_entity_type:
            % if struct_type is root_entity:
    ${struct.ni_def(struct_type)}
            % endif
        % else:
        <%
        emit_struct = (
            struct_type is T.entity_info
            or struct_type is T.env_md
            or struct_type.exposed
        )
        %>
            % if emit_struct:
    ${struct.ni_def(struct_type)}
            % endif
        % endif
    % endfor

    // ===== Generated arrays =====

    % for array_type in ctx.array_types:
        % if array_type.exposed and array_type.emit_c_type:
    ${array.ni_def(array_type)}
        % endif
    % endfor

    // ===== Generated iterators =====

    % for iterator_type in ctx.iterator_types:
        % if iterator_type.exposed and iterator_type.emit_c_type:
    ${iterator.ni_def(iterator_type)}
        % endif
    % endfor

    // ===== Native function definitions =====

    /** This class contains all native function definitions for NI */
    @CContext(LibDirectives.class)
    public static final class NI_LIB {

        // ----- Language specific functions -----

        ${exts.include_extension(ctx.ext("java_api", "ni_funcs"))}

        // ----- Entry point literals -----

        /**
         * This entry point literal provide a pointer to the unit requested
         * callback.
         */
        public static final CEntryPointLiteral<UnitRequestedFunctionPointer>
            unitRequestedFunction = CEntryPointLiteral.create(
                ${ctx.lib_name.camel}.class,
                "unitRequested",
                IsolateThread.class,
                AnalysisContextNative.class,
                TextNative.class,
                AnalysisUnitNative.class,
                byte.class,
                byte.class
            );

        /**
         * This entry point literal provide a pointer to the unit parsed
         * callback.
         */
        public static final CEntryPointLiteral<UnitParsedFunctionPointer>
            unitParsedFunction = CEntryPointLiteral.create(
                ${ctx.lib_name.camel}.class,
                "unitParsed",
                IsolateThread.class,
                AnalysisContextNative.class,
                AnalysisUnitNative.class,
                byte.class
            );

        // ----- Util functions -----

        /** Util function to free langkit side allocated memory */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("free")}(
            PointerBase pointer
        );

        // ----- Exception functions -----

        /** Get the last exception raised by langkit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native LangkitExceptionNative
        ${nat("get_last_exception")}();

        // ----- Big integer functions -----

        /** Create a big integer from a text */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native BigIntegerNative ${nat("create_big_integer")}(
            TextNative text
        );

        /** Get the text representation of a big integer */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("big_integer_text")}(
            BigIntegerNative big_integer,
            TextNative text
        );

        /** Decrease the reference counter of the big integer */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("big_integer_decref")}(
            BigIntegerNative big_integer
        );

        // ----- Symbol functions -----

        /** Create a new symbol in the given context */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("context_symbol")}(
            AnalysisContextNative context,
            TextNative text,
            SymbolNative res
        );

        /** Get the text of a given symbol */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("symbol_text")}(
            SymbolNative symbol,
            TextNative text
        );

        // ----- String functions -----

        /** Create a new string wrapper in langkit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native StringNative ${nat("create_string")}(
            CIntPointer content,
            int length
        );

        /** Decrease the reference counter of a string */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("string_dec_ref")}(
            StringNative string
        );

        // ----- Text functions -----

        /** Destroy a text in the memory */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("destroy_text")}(
            TextNative text
        );

        // ----- File reader functions -----

        /** Decrease the reference counter of the given file reader */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("dec_ref_file_reader")}(
            FileReaderNative fileReader
        );

        // ----- Unit provider functions -----

        /** Decrease the ref counter of the unit provider */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("dec_ref_unit_provider")}(
            UnitProviderNative unitProvider
        );

        // ----- Event handler functions -----

        /** Create a new event handler */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native EventHandlerNative ${nat("create_event_handler")}(
            VoidPointer data,
            VoidPointer destroy_callback,
            UnitRequestedFunctionPointer unit_requested_func,
            UnitParsedFunctionPointer unit_parsed_func
        );

        /** Decrease the ref counter of the event handler */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("dec_ref_event_handler")}(
            EventHandlerNative eventHandler
        );

        // ----- Token functions -----

        /** Get the next token */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("token_next")}(
            TokenNative token,
            TokenNative res
        );

        /** Get the previous token */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("token_previous")}(
            TokenNative token,
            TokenNative res
        );

        /** Get if two tokens are equivalent */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native byte ${nat("token_is_equivalent")}(
            TokenNative left,
            TokenNative right
        );

        /** Get the text in a token range */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("token_range_text")}(
            TokenNative start,
            TokenNative end,
            TextNative res
        );

        // ----- Analysis context functions -----

        /** Allocate a new analysis context */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native AnalysisContextNative
        ${nat("allocate_analysis_context")}();

        /** Create a new analysis context */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void
        ${nat("initialize_analysis_context")}(
            AnalysisContextNative context,
            CCharPointer charset,
            FileReaderNative file_reader,
            UnitProviderNative unit_provider,
            EventHandlerNative event_handler,
            int with_trivia,
            int tab_stop
        );

        /** Increase the reference counter of a context */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("context_incref")}(
            AnalysisContextNative context
        );

        /** Decrease the reference counter of a context */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("context_decref")}(
            AnalysisContextNative context
        );

        // ----- Analysis unit functions -----

        /** Get a unit from a file */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native AnalysisUnitNative
        ${nat("get_analysis_unit_from_file")}(
            AnalysisContextNative context,
            CCharPointer file_name,
            CCharPointer charset,
            int reparse,
            int rule
        );

        /** Get a unit from a buffer */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native AnalysisUnitNative
        ${nat("get_analysis_unit_from_buffer")}(
            AnalysisContextNative context,
            CCharPointer file_name,
            CCharPointer charset,
            CCharPointer buffer,
            long buffer_size,
            int rule
        );

        /** Get the root of an analysis unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("unit_root")}(
            AnalysisUnitNative unit,
            EntityNative res
        );

        /** Get the file name for a given unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native CCharPointer ${nat("unit_filename")}(
            AnalysisUnitNative unit
        );

        /** Get the token count of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("unit_token_count")}(
            AnalysisUnitNative unit
        );

        /** Get the trivia count of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("unit_trivia_count")}(
            AnalysisUnitNative unit
        );

        /** Get the first token of an analysis unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("unit_first_token")}(
            AnalysisUnitNative unit,
            TokenNative res
        );

        /** Get the last token of an analysis unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("unit_last_token")} (
            AnalysisUnitNative unit,
            TokenNative res
        );

        /** Get the context for a given unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native AnalysisContextNative ${nat("unit_context")}(
            AnalysisUnitNative unit
        );

        /** Get the diagnostic count of the unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("unit_diagnostic_count")}(
            AnalysisUnitNative unit
        );

        /** Get the nth diagnostic for the unit */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("unit_diagnostic")}(
            AnalysisUnitNative unit,
            int n,
            DiagnosticNative diagnostic
        );

        // ----- Array functions -----

        % for array_type in ctx.array_types:
            % if array_type.exposed and array_type.emit_c_type:
        ${array.ni_funcs(array_type)}
            % endif
        % endfor

        // ----- Structure functions -----

        % for struct_type in ctx.struct_types:
            % if struct_type.is_entity_type:
                % if struct_type is root_entity:
        ${struct.ni_funcs(struct_type)}
                % endif
            % else:
            <%
            emit_struct = (
                struct_type is T.entity_info
                or struct_type is T.env_md
                or struct_type.exposed
            )
            %>
                % if emit_struct:
        ${struct.ni_funcs(struct_type)}
                % endif
            % endif
        % endfor

        // ----- Node functions -----

        /** Return whether the two given entities are equal */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("node_is_equivalent")}(
            EntityNative entity_left,
            EntityNative entity_right
        );

        /** Get the hash of a node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("node_hash")}(
            EntityNative entity
        );

        /** Get the type of a node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("node_kind")}(
            EntityNative entity
        );

        /** Get the text from a node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("node_text")}(
            EntityNative entity,
            TextNative text
        );

        /** Get the source location range for a node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("node_sloc_range")}(
            EntityNative entity,
            SourceLocationRangeNative slocr
        );

        /** Get the number of children for a node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("node_children_count")}(
            EntityNative entity
        );

        /** Get the nth child for the node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("node_child")}(
            EntityNative entity,
            int n,
            EntityNative res
        );

        /** Get if the node is a token node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${nat("node_is_token_node")}(
            EntityNative entity
        );

        /** Get the unit of the node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native AnalysisUnitNative ${nat("node_unit")}(
            EntityNative entity
        );

        /** Get the image of a node */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("node_image")}(
            EntityNative entity,
            TextNative text
        );

        // ----- Node fields accessors and properties -----

        % for astnode in ctx.astnode_types:
        ${ast_node.ni_funcs(astnode)}
        % endfor

    }
</%def>
