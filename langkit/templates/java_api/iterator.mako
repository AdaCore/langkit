## vim: ft=makojava

<%def name="wrapping_class(cls)">
    <%
    api = java_api

    class_name = api.wrapping_type(cls)
    ni_name = api.ni_type(cls)
    c_name = cls.c_type(capi).name
    %>

    ${java_doc(cls, 4)}
    public static final class ${class_name} implements AutoCloseable {

        // ----- Class attributes -----

        /** Singleton for the iterator NONE value. */
        public static final ${class_name} NONE = new ${class_name}(
            PointerWrapper.nullPointer()
        );

        // ----- Attributes -----

        /** The pointer to the native iterator. */
        public final PointerWrapper reference;

        // ----- Constructors -----

        /**
         * Create a new ${class_name} from the reference to its native value.
         *
         * @param reference The reference to the native iterator value.
         */
        private ${class_name}(
            PointerWrapper reference
        ) {
            this.reference = reference;
        }

        /**
         * Wrap a pointer to the iterator native value in the Java class.
         *
         * @param niPointer The pointer to the NI iterator native value.
         * @return The newly created iterator or null if the given pointer
         * is null.
         */
        static ${class_name} wrap(
            final WordPointer niPointer
        ) {
            if(niPointer.isNull()) return null;
            else return wrap((${ni_name}) niPointer.read());
        }

        /**
         * Wrap the iterator native value in the Java class.
         *
         * @param iteratorNative The NI iterator native value.
         * @return The newly created iterator or null if the iterator value
         * is null.
         */
        static ${class_name} wrap(
            ${ni_name} iteratorNative
        ) {
            if(((PointerBase) iteratorNative).isNull()) return null;
            else return new ${class_name}(
                new PointerWrapper(iteratorNative)
            );
        }

        // ----- Instance methods -----

        /** @see java/lang/AutoCloseable#close() */
        public void close() {
            System.err.println("HANDLE THE ITERATOR CLOSING !!!");
        }

    }
</%def>

<%def name="ni_def(cls)">
    <%
    api = java_api
    ni_name = api.ni_type(cls)
    c_name = cls.c_type(capi).name
    %>

    /** This interface represents the langkit ${c_name} iterator */
    public interface ${ni_name} extends Pointer {}
</%def>

<%def name="jni_c_decl(cls)">
    <%
    api = java_api
    j_name = api.wrapping_type(cls, False)
    c_name = cls.c_type(capi).name
    %>

${c_name} ${j_name}_new_value();
jobject ${j_name}_wrap(JNIEnv *, ${c_name});
${c_name} ${j_name}_unwrap(JNIEnv *, jobject);

jclass ${j_name}_class_ref = NULL;
jmethodID ${j_name}_constructor_id = NULL;
jfieldID ${j_name}_reference_field_id = NULL;
</%def>

<%def name="jni_init_global_refs(cls)">
    <%
    api = java_api
    j_name = api.wrapping_type(cls, False)

    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
    ptr_sig = f"{sig_base}$PointerWrapper"
    %>

    ${j_name}_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$${j_name}")
    );

    ${j_name}_constructor_id = (*env)->GetMethodID(
        env,
        ${j_name}_class_ref,
        "<init>",
        "(L${ptr_sig};)V"
    );

    ${j_name}_reference_field_id = (*env)->GetFieldID(
        env,
        ${j_name}_class_ref,
        "reference",
        "L${ptr_sig};"
    );
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api
    j_name = api.wrapping_type(cls, False)
    c_name = cls.c_type(capi).name
    %>

// Create a new value for a ${c_name}
${c_name} ${j_name}_new_value() {
    return NULL;
}

// Wrap a native ${c_name} in the Java wrapping class
jobject ${j_name}_wrap(
    JNIEnv *env,
    ${c_name} struct_native
) {
    // Verify the iterator nullity
    if(struct_native == NULL) return NULL;

    // Return the new iterator
    return (*env)->NewObject(
        env,
        ${j_name}_class_ref,
        ${j_name}_constructor_id,
        PointerWrapper_wrap(env, (void *) struct_native)
    );
}

// Get a native ${c_name} from a Java wrapping instance
${c_name} ${j_name}_unwrap(
    JNIEnv *env,
    jobject object
) {
    return (${c_name}) PointerWrapper_unwrap(
        env,
        (*env)->GetObjectField(
            env,
            object,
            ${j_name}_reference_field_id
        )
    );
}
</%def>
