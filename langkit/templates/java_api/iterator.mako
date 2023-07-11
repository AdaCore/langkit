## vim: ft=makojava

<%def name="wrapping_class(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls)
    ni_type = api.ni_type(cls)

    elem_java_wrapped_type = api.wrapping_type(
        cls.element_type,
        java_wrapping=True
    )
    elem_java_unw_type = api.wrapping_type(
        cls.element_type,
        ast_wrapping=False
    )
    elem_ni_ref_type = api.ni_reference_type(cls.element_type)

    wrap_release = []
    %>

    ${java_doc(cls, 4)}
    public static final class ${java_type} implements AutoCloseable,
    Iterator<${elem_java_wrapped_type}> {

        // ----- Class attributes -----

        /** Singleton that represents the none iterator. */
        public static final ${java_type} NONE = new ${java_type}(
            PointerWrapper.nullPointer()
        );

        // ----- Instance attributes -----

        /** The wrapped reference to the native iterator. */
        public final PointerWrapper reference;

        /**
         * A cache telling whether there is a next value in the iterator. If
         * this member value is `null`, it means either it isn't initialized or
         * the current value has been consumed by calling the `next` method.
         */
        private Boolean hasNextCache;

        /** A cache for the next value to return. */
        private ${elem_java_wrapped_type} nextCache;

        // ----- Constructors -----

        /** Create a new ${java_type} from the reference to its native value. */
        private ${java_type}(
            final PointerWrapper reference
        ) {
            this.reference = reference;
            this.hasNextCache = null;
            this.nextCache = null;
        }

        // ----- Graal C API methods -----

        /** Wrap the given pointer to a native iterator instance. */
        static ${java_type} wrap(
            final WordPointer niPointer
        ) {
            return wrap((${ni_type}) niPointer.read());
        }

        /** Wrap the given native iterator instance. */
        static ${java_type} wrap(
            final ${ni_type} iteratorNative
        ) {
            return new ${java_type}(
                new PointerWrapper(iteratorNative)
            );
        }

        /** Get the native value of the wrapped iterator. */
        ${ni_type} unwrap() {
            return (${ni_type}) this.reference.ni();
        }

        // ----- Instance methods -----

        /**
         * Internal method to help JNI C stubs accessing the "has next" cache.
         */
        private void jniSetHasNextCache(final boolean value) {
            this.hasNextCache = value;
        }

        /** Internal method to help JNI C stubs accessing the next cache. */
        private void jniSetNextCache(
            final ${elem_java_unw_type} value
        ) {
            this.nextCache = ${api.java_jni_wrap(cls.element_type, "value")};
        }

        /**
         * Internal method to move the iterator pointer to the next element,
         * placing it in the `nextCache` instance member. This method returns
         * whether there is a next element.
         */
        private boolean moveToNext() {
            final int success;

            if(ImageInfo.inImageCode()) {
                final ${elem_ni_ref_type} nextNative =
                    ${api.ni_stack_value(cls.element_type)};
                success = NI_LIB.${cls.c_next(capi)}(
                    this.reference.ni(),
                    nextNative
                );
                if (success == 1) {
                    this.nextCache = ${api.ni_wrap(
                        cls.element_type,
                        "nextNative",
                        wrap_release
                    )};
                    % for to_release in wrap_release:
                    ${api.wrapper_class(to_release.public_type)}.release(
                        ${to_release.name}
                    );
                    % endfor
                }
            } else {
                success = JNI_LIB.${cls.c_next(capi)}(this);
            }

            return success == 1;
        }

        /** @see java.util.Iterator#hasNext() */
        @Override
        public boolean hasNext() {
            if(this.hasNextCache == null) {
                this.hasNextCache = this.moveToNext();
            }

            return this.hasNextCache;
        }

        /** @see java.util.Iterator#next() */
        @Override
        public ${elem_java_wrapped_type} next() {
            if (this.hasNextCache == null) {
                this.hasNextCache = this.moveToNext();
            }

            if (this.hasNextCache) {
                this.hasNextCache = null;
                return this.nextCache;
            } else {
                return null;
            }
        }

        /** @see java.lang.AutoCloseable#close() */
        @Override
        public void close() {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${cls.c_dec_ref(capi)}(
                    this.reference.ni()
                );
            } else {
                JNI_LIB.${cls.c_dec_ref(capi)}(this);
            }

            // Check the closing exception
            checkException();

        }

        // ----- Override methods -----

        /** @see java.lang.Object#equals(java.lang.Object) */
        public boolean equals(
            final Object o
        ) {
            if(o == this) return true;
            if(!(o instanceof ${java_type})) return false;
            ${java_type} other = (${java_type}) o;
            return this.reference.equals(other.reference);
        }

    }
</%def>

<%def name="ni_def(cls)">
    <%
    api = java_api
    ni_type = api.ni_type(cls)
    c_type = cls.c_type(capi).name
    %>

    /** This interface represents the '${c_type}' native type. */
    public interface ${ni_type} extends Pointer {}
</%def>

<%def name="ni_funcs(cls)">
    <%
    api = java_api
    ni_type = api.ni_type(cls)
    elem_ni_ref_type = api.ni_reference_type(cls.element_type)
    c_type = cls.c_type(capi).name
    %>

        /** Get the next element of the iterator (for ${c_type}). */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${cls.c_next(capi)}(
            ${ni_type} iterator,
            ${elem_ni_ref_type} elem_ref
        );

        /** Decrease the reference counter of the iterator (for ${c_type}). */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${cls.c_dec_ref(capi)}(
            ${ni_type} iterator
        );
</%def>

<%def name="jni_funcs(cls)">
    <%
    api = java_api
    java_type = api.wrapping_type(cls)
    c_type = cls.c_type(capi).name
    %>

        /** Get the next element of the iterator (for ${c_type}). */
        @CompilerDirectives.TruffleBoundary
        public static native int ${cls.c_next(capi)}(
            ${java_type} iterator
        );

        /** Decrease the reference counter of the iterator (for ${c_type}). */
        @CompilerDirectives.TruffleBoundary
        public static native void ${cls.c_dec_ref(capi)}(
            ${java_type} iterator
        );
</%def>

<%def name="jni_c_decl(cls)">
    <%
    api = java_api
    java_type = api.wrapping_type(cls)
    c_type = cls.c_type(capi).name
    %>

${c_type} ${java_type}_new_value();
jobject ${java_type}_wrap(JNIEnv *, ${c_type});
${c_type} ${java_type}_unwrap(JNIEnv *, jobject);

jclass ${java_type}_class_ref = NULL;
jmethodID ${java_type}_constructor_id = NULL;
jmethodID ${java_type}_set_next_cache_id = NULL;
jfieldID ${java_type}_reference_field_id = NULL;
</%def>

<%def name="jni_init_global_refs(cls)">
    <%
    api = java_api
    java_type = api.wrapping_type(cls)

    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
    ptr_sig = f"{sig_base}$PointerWrapper"

    elem_sig = api.jni_sig_type(cls.element_type, sig_base, ast_wrapping=False)
    %>

    ${java_type}_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$${java_type}")
    );

    ${java_type}_constructor_id = (*env)->GetMethodID(
        env,
        ${java_type}_class_ref,
        "<init>",
        "(L${ptr_sig};)V"
    );

    ${java_type}_set_next_cache_id = (*env)->GetMethodID(
        env,
        ${java_type}_class_ref,
        "jniSetNextCache",
        "(${elem_sig})V"
    );

    ${java_type}_reference_field_id = (*env)->GetFieldID(
        env,
        ${java_type}_class_ref,
        "reference",
        "L${ptr_sig};"
    );
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api
    c_type = cls.c_type(capi).name
    java_type = api.wrapping_type(cls, ast_wrapping=False)

    elem_c_type = cls.element_type.c_type(capi).name
    elem_jni_type = api.jni_c_type(cls.element_type)

    wrap_release = []
    %>

${c_type} ${java_type}_new_value() {
    return NULL;
}

jobject ${java_type}_wrap(
    JNIEnv *env,
    ${c_type} iterator_native
) {
    return (*env)->NewObject(
        env,
        ${java_type}_class_ref,
        ${java_type}_constructor_id,
        PointerWrapper_wrap(env, (void *) iterator_native)
    );
}

${c_type} ${java_type}_unwrap(
    JNIEnv *env,
    jobject iterator
) {
    return (${c_type}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            iterator,
            ${java_type}_reference_field_id
        )
    );
}

// JNI stub to get the next element of the given iterator
${api.jni_func_sig(cls.c_next(capi), "jint", do_nat=False)}(
    JNIEnv *env,
    jclass jni_lib,
    jobject iterator
) {
    // Call the native 'next' function
    ${elem_c_type} res_native = ${api.jni_new_value(cls.element_type)};
    int success = ${cls.c_next(capi)}(
        ${java_type}_unwrap(env, iterator),
        &res_native
    );

    // If the previous call succeded, set the next element cache in the Java
    // iterator wrapper.
    if(success) {
        ${elem_jni_type} res = ${api.jni_wrap(
            cls.element_type,
            "res_native",
            wrap_release,
            ast_wrapping=False
        )};

        % for to_release in wrap_release:
        ${api.wrapping_type(
            to_release.public_type,
            ast_wrapping=False
        )}_release(
            ${to_release.name}
        );
        % endfor

        (*env)->CallVoidMethod(
            env,
            iterator,
            ${java_type}_set_next_cache_id,
            res
        );
    }
    return (jint) success;
}

// Decrease the reference counter of the given iterator
${api.jni_func_sig(cls.c_dec_ref(capi), "void", do_nat=False)}(
    JNIEnv *env,
    jclass jni_lib,
    jobject iterator
) {
    ${cls.c_dec_ref(capi)}(
        ${java_type}_unwrap(env, iterator)
    );
}
</%def>
