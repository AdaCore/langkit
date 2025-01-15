## vim: ft=makojava

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
                // Load the needed libraries
                if(OS.indexOf("win") < 0) {
                    System.loadLibrary("langkit_sigsegv_handler");
                }
                System.loadLibrary(
                    "${cfg.library.language_name.lower}lang_jni"
                );

                // Initialize the JNI library
                ${nat("initialize")}();

                // Register the library finalizer
                Runtime.getRuntime().addShutdownHook(
                    new Thread(JNI_LIB::${nat("finalize")})
                );
            }
        }

        // ----- Language specific functions -----

        ${exts.include_extension(ctx.ext("java_api", "jni_funcs"))}

        // ----- Lifecycle functions ------

        /** Function to initialize the JNI library */
        public static native void ${nat("initialize")}();

        /** Function to finalize the JNI library */
        public static native void ${nat("finalize")}();

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

        % if ctx.generate_unparsers:
        // ----- Rewriting apply result functions -----

        /** Get the diagnostics from the rewriting apply result */
        @CompilerDirectives.TruffleBoundary
        public static native Diagnostic[]
        ${nat("rewriting_get_result_diagnostics")}(
            int diagnosticsCount,
            long diagnosticsReference
        );

        /** Free the rewriting apply result structure */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_free_apply_result")}(
            RewritingApplyResult applyResult
        );
        % endif

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

        % if cfg.library.defaults.unit_provider:
        /** Get the analysis unit from the unit provider. */
        @CompilerDirectives.TruffleBoundary
        public static native AnalysisUnit
        ${nat("get_analysis_unit_from_provider")}(
            AnalysisContext context,
            Text name,
            int kind,
            String charset,
            boolean reparse
        );
        % endif

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

        % if ctx.generate_unparsers:
        // ----- Rewriting context functions -----

        /** Start a rewriting session and return the new context */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingContext
        ${nat("rewriting_start_rewriting")}(
            AnalysisContext analysisContext
        );

        /** Get the analysis context from the given rewriting context */
        @CompilerDirectives.TruffleBoundary
        public static native AnalysisContext
        ${nat("rewriting_handle_to_context")}(
            RewritingContext rewritingContext
        );

        /** Get a pointer to the rewriting units owned by the context */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingUnit[] ${nat("rewriting_unit_handles")}(
            RewritingContext rewritingContext
        );

        /** Create a node in the rewriting context and return it */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_create_node")}(
            RewritingContext rewritingContext,
            int nodeKind
        );

        /** Create a node in the rewriting context with the given children */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode
        ${nat("rewriting_create_regular_node")}(
            RewritingContext rewritingContext,
            int nodeKind,
            RewritingNode[] children
        );

        /** Create a token node in the rewriting context and return it */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode
        ${nat("rewriting_create_token_node")}(
            RewritingContext rewritingContext,
            int nodeKind,
            Text nodeText
        );

        /** Create a new node tree from the given template */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode
        ${nat("rewriting_create_from_template")}(
            RewritingContext rewriting_context,
            Text template_text,
            RewritingNode[] arguments,
            int rule
        );

        /** Apply the rewriting session and close it fi success */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingApplyResult ${nat("rewriting_apply")}(
            RewritingContext rewritingContext
        );

        /** Abort the rewriting session */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_abort_rewriting")}(
            RewritingContext rewritingContext
        );

        // ----- Rewriting unit functions -----

        /** Get the rewriting unit associated */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingUnit ${nat("rewriting_unit_to_handle")}(
            AnalysisUnit analysisUnit
        );

        /** Get the analysis unit corresponding to the given rewriting unit */
        @CompilerDirectives.TruffleBoundary
        public static native AnalysisUnit ${nat("rewriting_handle_to_unit")}(
            RewritingUnit rewritingUnit
        );

        /** Get the root of the given rewriting unit */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_unit_root")}(
            RewritingUnit rewritingUnit
        );

        /** Set the root of the rewriting unit to the rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_unit_set_root")}(
            RewritingUnit rewritingUnit,
            RewritingNode rewritingNode
        );

        /** Unparse the given rewriting unit and return its textual value */
        @CompilerDirectives.TruffleBoundary
        public static native Text ${nat("rewriting_unit_unparse")}(
            RewritingUnit rewritingUnit
        );

        // ----- Rewriting node functions -----

        /** Get the rewriting node from the given parsed node */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_node_to_handle")}(
            Entity entity
        );

        /** Get the parsed node from the given rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native Entity ${nat("rewriting_handle_to_node")}(
            RewritingNode rewritingNode
        );

        /** Get the rewriting context from the given rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingContext
        ${nat("rewriting_node_to_context")}(
            RewritingNode rewriting_node
        );

        /** Clone the given rewriting node and return the copy */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_clone")}(
            RewritingNode toClone
        );

        /** Unparse the given rewriting node in the given text */
        @CompilerDirectives.TruffleBoundary
        public static native Text ${nat("rewriting_node_unparse")}(
            RewritingNode rewritingNode
        );

        /** Get the kind of the rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native int ${nat("rewriting_kind")}(
            RewritingNode rewritingNode
        );

        /** Get the rewriting node image */
        @CompilerDirectives.TruffleBoundary
        public static native Text ${nat("rewriting_node_image")}(
            RewritingNode rewritingNode
        );

        /** Return whether the node is tied to a rewriting unit */
        @CompilerDirectives.TruffleBoundary
        public static native boolean ${nat("rewriting_tied")}(
            RewritingNode rewritingNode
        );

        /** Return the parent of the given rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_parent")}(
            RewritingNode rewritingNode
        );

        /** Get the rewriting node children */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode[] ${nat("rewriting_children")}(
            RewritingNode rewritingNode
        );

        /** Get the child at the given member reference */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_child")}(
            RewritingNode parent,
            int childMemberReference
        );

        /** Set the given child at the given member reference */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_set_child")}(
            RewritingNode parent,
            int childMemberReference,
            RewritingNode newChild
        );

        /** Replace the rewriting node by the new one */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_replace")}(
            RewritingNode rewritingNode,
            RewritingNode newNode
        );

        /** Get the first child of the rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_first_child")}(
            RewritingNode parent
        );

        /** Get the last child of the rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_last_child")}(
            RewritingNode parent
        );

        /** Get the next child from the given rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_next_child")}(
            RewritingNode rewritingNode
        );

        /** Get the previous child from the given rewriting node */
        @CompilerDirectives.TruffleBoundary
        public static native RewritingNode ${nat("rewriting_previous_child")}(
            RewritingNode rewritingNode
        );

        /** Insert the provided rewriting node before the other node */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_insert_before")}(
            RewritingNode rewritingNode,
            RewritingNode toInsert
        );

        /** Insert the provided rewriting node after the other node */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_insert_after")}(
            RewritingNode rewritingNode,
            RewritingNode toInsert
        );

        /**
         * Insert the provided rewriting node at the beginning of the
         * children
         */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("rewriting_insert_first")}(
            RewritingNode rewritingNode,
            RewritingNode toInsert
        );

        /** Insert the provided rewriting node at the end of the children */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${nat("rewriting_insert_last")}(
            RewritingNode rewritingNode,
            RewritingNode toInsert
        );

        /** Remove the given rewriting node from its list parent */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_remove_child")}(
            RewritingNode toRemove
        );

        /** Get the text of the rewriting token node */
        @CompilerDirectives.TruffleBoundary
        public static native Text ${nat("rewriting_text")}(
            RewritingNode rewritingNode
        );

        /** Set the text of the rewriting token node */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat("rewriting_set_text")}(
            RewritingNode rewritingNode,
            Text text
        );
        % endif

        // ----- Iterator functions -----

        % for iterator_type in ctx.iterator_types:
            % if iterator_type.exposed and iterator_type.emit_c_type:
        ${iterator.jni_funcs(iterator_type)}
            % endif
        % endfor

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
