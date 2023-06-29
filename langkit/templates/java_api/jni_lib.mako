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

    /** This class contains all native function definitions for JNI */
    public static final class JNI_LIB {

        // ----- Static initializer -----

        static {
            if(!ImageInfo.inImageCode()) {
                if(OS.indexOf("win") < 0) {
                    System.loadLibrary("langkit_sigsegv_handler");
                }
                System.loadLibrary("${ctx.lang_name.lower}lang_jni");
                ${nat("initialize")}();
            }
        }

        public static native void ${nat("initialize")}();

        // ----- Language specific functions -----

        ${exts.include_extension(ctx.ext("java_api", "jni_funcs"))}

        // ----- Exception functions ------

        /** Get the last langkit exception */
        @CompilerDirectives.TruffleBoundary
        public static native LangkitException ${nat("get_last_exception")}();

        // ----- Text functions -----

        /** Create a new text from its content */
        @CompilerDirectives.TruffleBoundary
        public static native Text ${nat("create_text")}(
            byte[] utf32Content
        );

        /** Destroy the given text */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("destroy_text")}(
            Text text
        );

        // ----- File reader functions -----

        /** Decrease the reference counter of the given file reader */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("dec_ref_file_reader")}(
            FileReader fileReader
        );

        // ----- Unit provider functions -----

        /** Decrease the ref counter of the unit provider */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("dec_ref_unit_provider")}(
            UnitProvider unitProvider
        );

        // ----- Event handler functions -----

        /** Create a new event handler */
        @CompilerDirectives.TruffleBoundary
        public static native PointerWrapper ${nat("create_event_handler")}(
            EventHandler.UnitRequestedCallback unitRequestedCallback,
            EventHandler.UnitParsedCallback unitParsedCallback
        );

        /** Decrease the ref counter of the event handler */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("dec_ref_event_handler")}(
            EventHandler eventHandler
        );

        // ----- Token functions -----

        /** Get the next token */
        @CompilerDirectives.TruffleBoundary
        public static native Token ${nat("token_next")}(
            Token token
        );

        /** Get the previous token */
        @CompilerDirectives.TruffleBoundary
        public static native Token ${nat("token_previous")}(
            Token token
        );

        /** Get if the tokens are equivalent */
        @CompilerDirectives.TruffleBoundary
        public static native boolean ${nat("token_is_equivalent")}(
            Token left,
            Token right
        );

        /** Get text between the two tokens */
        @CompilerDirectives.TruffleBoundary
        public static native Text ${nat("token_range_text")}(
            Token start,
            Token end
        );

        // ----- Analysis context functions -----

        /** Create a new analysis context */
        @CompilerDirectives.TruffleBoundary
        public static native PointerWrapper ${nat("create_analysis_context")}(
            String charset,
            FileReader fileReader,
            UnitProvider unitProvider,
            EventHandler eventHandler,
            boolean withTrivia,
            int tabstop
        );

        /** Increase the reference counter of a context */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("context_incref")}(
            long context
        );

        /** Decrease the reference counter of a context */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("context_decref")}(
            long context
        );

        // ----- Analysis unit functions -----

        /** Get the analysis unit from a file */
        @CompilerDirectives.TruffleBoundary
        public static native AnalysisUnit
        ${nat("get_analysis_unit_from_file")}(
            AnalysisContext context,
            String fileName,
            String charset,
            boolean reparse,
            int grammarRule
        );

        /** Get the analysis unit from a buffer */
        @CompilerDirectives.TruffleBoundary
        public static native AnalysisUnit
        ${nat("get_analysis_unit_from_buffer")}(
            AnalysisContext context,
            String fileName,
            String charset,
            String buffer,
            long bufferSize,
            int grammarRule
        );

        /** Get the root of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native Entity ${nat("unit_root")}(
            AnalysisUnit unit
        );

        /** Get the file name of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native String ${nat("unit_filename")}(
            AnalysisUnit unit
        );

        /** Get the token count of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native int ${nat("unit_token_count")}(
            AnalysisUnit unit
        );

        /** Get the trivia count of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native int ${nat("unit_trivia_count")}(
            AnalysisUnit unit
        );

        /** Get the first token of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native Token ${nat("unit_first_token")}(
            AnalysisUnit unit
        );

        /** Get the last token of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native Token ${nat("unit_last_token")}(
            AnalysisUnit unit
        );

        /** Get the context of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native AnalysisContext ${nat("unit_context")}(
            AnalysisUnit unit
        );

        /** Get the number of diagnostic in the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native int ${nat("unit_diagnostic_count")}(
            AnalysisUnit unit
        );

        /** Get the nth diagnostic of the analysis unit */
        @CompilerDirectives.TruffleBoundary
        public static native Diagnostic ${nat("unit_diagnostic")}(
            AnalysisUnit unit,
            int n
        );

        // ----- Node functions -----

        /** Return whether the two given entities are equal */
        @CompilerDirectives.TruffleBoundary
        public static native int ${nat("node_is_equivalent")}(
            Entity entity_left,
            Entity entity_right
        );

        /** Get the hash of a node */
        @CompilerDirectives.TruffleBoundary
        public static native int ${nat("node_hash")}(
            Entity entity
        );

        /** Get the node kind */
        @CompilerDirectives.TruffleBoundary
        public static native int ${nat("node_kind")}(
            Entity entity
        );

        /** Get the node text */
        @CompilerDirectives.TruffleBoundary
        public static native Text ${nat("node_text")}(
            Entity entity
        );

        /** Get the node source location range */
        @CompilerDirectives.TruffleBoundary
        public static native SourceLocationRange ${nat("node_sloc_range")}(
            Entity entity
        );

        /** Get the node children count */
        @CompilerDirectives.TruffleBoundary
        public static native int ${nat("node_children_count")}(
            Entity entity
        );

        /** Get the node nth child */
        @CompilerDirectives.TruffleBoundary
        public static native Entity ${nat("node_child")}(
            Entity entity,
            int n
        );

        /** Get if the node is a token node */
        @CompilerDirectives.TruffleBoundary
        public static native boolean ${nat("node_is_token_node")}(
            Entity entity
        );

        /** Get the unit of the node */
        @CompilerDirectives.TruffleBoundary
        public static native AnalysisUnit ${nat("node_unit")}(
            Entity entity
        );

        /** Get the entity image of the node */
        @CompilerDirectives.TruffleBoundary
        public static native Text ${nat("node_image")}(
            Entity entity
        );

        // ----- Node fields accessors and properties -----

        % for astnode in ctx.astnode_types:
        ${ast_node.jni_funcs(astnode)}
        % endfor

    }
</%def>
