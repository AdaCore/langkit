## vim: ft=makojava

<%def name="wrapping_class(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls)
    ni_type = api.ni_type(cls)
    c_type = cls.c_type(capi).name

    elem_java_type = api.wrapping_type(cls.element_type)
    elem_java_unw_type = api.wrapping_type(cls.element_type, False)
    elem_ni_type = api.ni_type(cls.element_type)
    elem_ni_ref_type = api.ni_reference_type(cls.element_type)
    %>

    /**
     * This class represents the ${c_type} Java wrapping class
     */
    public static final class
    ${java_type} extends ArrayBase<${elem_java_type}> {

        // ----- Class attributes -----

        /** Singleton that represents the none array. */
        public static final ${java_type} NONE = new ${java_type}(
            new ${elem_java_type}[0]
        );

        // ----- Constructors -----

        /**
         * Create a new array with the given content.
         *
         * @param content The content of the array.
         */
        ${java_type}(
            final ${elem_java_type}[] content
        ) {
            super(content);
        }

        /**
         * Create a new array from the JNI stub.
         *
         * @param content The unwrapped JNI content.
         */
        private static ${java_type} jniCreate(
            final ${elem_java_unw_type}[] jniContent
        ) {
            final ${elem_java_type}[] content =
                new ${elem_java_type}[jniContent.length];
            for(int i = 0 ; i < content.length ; i++) {
                content[i] =
                    ${api.java_jni_wrap(cls.element_type, "jniContent[i]")};
            }
            return new ${java_type}(content);
        }

        /**
         * Create a sized array.
         *
         * @param size The size of the array you want to create.
         * @return The newly created array.
         */
        public static ${java_type} create(
            final int size
        ) {
            return new ${java_type}(
                new ${elem_java_type}[size]
            );
        }

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer to an array native value in the Java class.
         *
         * @param pointer The pointer to the array NI native value.
         * @return The newly wrapped array.
         */
        static ${java_type} wrap(
            final WordPointer pointer
        ) {
            return wrap((${ni_type}) pointer.read());
        }

        /**
         * Wrap an array native value in the Java class.
         *
         * @param nativeArray The NI array native value to wrap.
         * @return The newly wrapped array.
         */
        static ${java_type} wrap(
            final ${ni_type} nativeArray
        ) {
            // Get the size and prepare the working variables
            final int size = nativeArray.get_n();
            final ${elem_java_type}[] content = new ${elem_java_type}[size];
            final Pointer nativeItems = nativeArray.address_items();
            Pointer nativeItem;
            ${elem_ni_ref_type} toRead;

            // Iterate over all array elements
            final int elemSize = SizeOf.get(${elem_ni_type}.class);
            for(int i = 0 ; i < size ; i++) {
                nativeItem = nativeItems.add(i * elemSize);
                toRead = WordFactory.unsigned(nativeItem.rawValue());
                content[i] = ${
                    api.ni_wrap(cls.element_type, "toRead", [])
                };
            }

            // Return the new langkit array
            return new ${java_type}(content);
        }

        /**
         * Unwrap the array in the given pointer
         *
         * @param pointer The pointer to place the native array pointer
         * in.
         */
        void unwrap(
            final WordPointer pointer
            ${(
                ", final AnalysisContext currentContext"
                if cls.element_type.is_symbol_type else
                ""
            )}
        ) {
            // Create a new native array with the size
            final ${ni_type} resNative = this.unwrap(
                ${(
                    "currentContext"
                    if cls.element_type.is_symbol_type else
                    ""
                )}
            );

            // Place the result in the pointer
            pointer.write(resNative);
        }

        /**
         * Allocate a new native array and unwrap inside.
         *
         * @return The newly allocated unwraped array.
         */
        ${ni_type} unwrap(
            ${(
                "final AnalysisContext currentContext"
                if cls.element_type.is_symbol_type else
                ""
            )}
        ) {
            // Create a new native array with the size
            final ${ni_type} res = NI_LIB.${cls.c_create(capi)}(
                this.content.length
            );

            // Prepare the working vars
            final Pointer nativeItems = res.address_items();
            Pointer nativeItem;
            ${elem_ni_ref_type} toWrite;

            // Place all elements in the native array
            final int elemSize = SizeOf.get(${elem_ni_type}.class);
            for(int i = 0 ; i < this.content.length ; i++) {
                nativeItem = nativeItems.add(i * elemSize);
                toWrite = WordFactory.unsigned(
                    nativeItem.rawValue()
                );
                ${api.ni_write(
                    cls.element_type,
                    "this.content[i]",
                    "toWrite"
                )}
            }

            // Return the result
            return res;
        }

        /**
         * Release native array pointer by the given pointer.
         *
         * @param The pointer to the array to release.
         */
        static void release(
            final WordPointer pointer
        ) {
            release((${ni_type}) pointer.read());
        }

        /**
         * Release the given native array.
         *
         * @param arrayNative The native array to release.
         */
        static void release(
            final ${ni_type} arrayNative
        ) {
            NI_LIB.${cls.c_dec_ref(c_api)}(arrayNative);
        }

        // ----- Getters -----

        /**
         * Get the content in an array unwrapped for the JNI stubs.
         *
         * @return The content unwrapped.
         */
        private ${elem_java_unw_type}[] jniContent() {
            final ${elem_java_unw_type}[] res =
                new ${elem_java_unw_type}[this.content.length];
            for(int i = 0 ; i < res.length ; i++) {
                res[i] = ${api.java_jni_unwrap(
                    cls.element_type,
                    "this.content[i]"
                )};
            }
            return res;
        }

    }
</%def>

<%def name="ni_def(cls)">
    <%
    api = java_api

    ni_type = api.ni_type(cls)
    c_type = cls.c_type(capi).name
    %>

    /**
     * The native structure of the ${c_type} langkit array.
     */
    @CContext(LibDirectives.class)
    @CStruct(
        value = "${c_type}_record",
        addStructKeyword = true,
        isIncomplete = true
    )
    public interface ${ni_type} extends PointerBase {
        @CField("n") public int get_n();
        @CField("ref_count") public int get_ref_count();
        @CFieldAddress("items")
        public <T extends PointerBase> T address_items();
    }
</%def>

<%def name="ni_funcs(cls)">
    <%
    api = java_api

    ni_type = api.ni_type(cls)
    %>

        /**
         * Create a new sized array.
         *
         * @param size The size of the array to create.
         * @return The native pointer to the created array.
         */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native ${ni_type} ${cls.c_create(capi)}(int size);

        /**
         * Decrease reference counter of the given array
         *
         * @param array The array to decrease the reference counter.
         */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${cls.c_dec_ref(c_api)}(${ni_type} array);
</%def>

<%def name="jni_c_decl(cls)">
    <%
    api = java_api

    c_type = cls.c_type(capi).name
    java_type = api.wrapping_type(cls)
    %>

${c_type} ${java_type}_new_value();
jobject ${java_type}_wrap(JNIEnv *, ${c_type});
${c_type} ${java_type}_unwrap(
    JNIEnv *,
    jobject
    ${(
        ", jobject"
        if cls.element_type.is_symbol_type else
        ""
    )});
void ${java_type}_release(${c_type});

jclass ${java_type}_class_ref = NULL;
jmethodID ${java_type}_create_method_id = NULL;
jmethodID ${java_type}_content_method_id = NULL;
</%def>

<%def name="jni_init_global_refs(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls, False)

    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
    sig = f"L{sig_base}${java_type};"

    elem_java_type = api.wrapping_type(cls.element_type, False)
    elem_sig = f"L{sig_base}${elem_java_type};"
    %>

    ${java_type}_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig}")
    );

    ${java_type}_create_method_id = (*env)->GetStaticMethodID(
        env,
        ${java_type}_class_ref,
        "jniCreate",
        "([${elem_sig})${sig}"
    );

    ${java_type}_content_method_id = (*env)->GetMethodID(
        env,
        ${java_type}_class_ref,
        "jniContent",
        "()[${elem_sig}"
    );
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api

    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
    ptr_sig = f"{sig_base}$PointerWrapper"

    c_type = cls.c_type(capi).name
    java_type = api.wrapping_type(cls)
    sig = f"L{sig_base}${java_type};"

    elem_c_type = cls.element_type.c_type(capi).name
    elem_java_type = api.wrapping_type(cls.element_type, False)
    elem_sig = f"L{sig_base}${elem_java_type};"
    %>

// Create a new value for a ${c_type}
${c_type} ${java_type}_new_value() {
    return NULL;
}

// Wrap a native ${c_type} in the Java wrapping class
jobject ${java_type}_wrap(
    JNIEnv *env,
    ${c_type} array_native
) {
    // Get the size of the array
    int array_size = array_native->n;

    // Create a new Java array of object of the element type
    jobjectArray array_content = (*env)->NewObjectArray(
        env,
        (jsize) array_size,
        ${elem_java_type}_class_ref,
        NULL
    );

    // Put the elements in the Java array
    for(int i = 0 ; i < array_size ; i++) {
        ${elem_c_type} elem = array_native->items[i];
        (*env)->SetObjectArrayElement(
            env,
            array_content,
            (jsize) i,
            ${api.jni_wrap(cls.element_type, "elem", [], ast_wrapping=False)}
        );
    }

    // Return the new array
    return (*env)->CallStaticObjectMethod(
        env,
        ${java_type}_class_ref,
        ${java_type}_create_method_id,
        array_content
    );
}

// Get a native ${c_type} from a Java wrapping instance
${c_type} ${java_type}_unwrap(
    JNIEnv *env,
    jobject array
    ${(
        ", jobject context"
        if cls.element_type.is_symbol_type else
        ""
    )}
) {
    % if cls.element_type.is_symbol_type:
    // Unwrap the analysis context
    ${analysis_context_type} context_native =
        AnalysisContext_unwrap(env, context);
    % endif

    //  Retrieve the array's content
    jobjectArray content = (jobjectArray) (*env)->CallObjectMethod(
        env,
        array,
        ${java_type}_content_method_id
    );

    // Get the content size
    int size = (int) (*env)->GetArrayLength(env, content);

    // Create a new native array
    ${c_type} res = ${cls.c_create(capi)}(size);

    // Fill the new native array
    for(int i = 0 ; i < size ; i++) {
        jobject elem = (*env)->GetObjectArrayElement(
            env,
            content,
            (jsize) i
        );
        ${api.jni_unwrap(
            cls.element_type,
            "elem",
            "elem_native",
            [],
            ast_wrapping=False
        )}
        res->items[i] = elem_native;
    }

    // Return the array
    return res;
}

// Release the given native array
void ${java_type}_release(
    ${c_type} array_native
) {
    ${cls.c_dec_ref(c_api)}(array_native);
}
</%def>
