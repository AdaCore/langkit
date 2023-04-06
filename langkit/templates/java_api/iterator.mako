<%def name="wrapping_class(cls)">
    <%
    api = java_api

    class_name = api.wrapping_type(cls)
    ni_name = api.ni_type(cls)
    c_name = cls.c_type(capi).name
    %>

    ${java_doc(cls, 4)}
    public static final class ${class_name} implements AutoCloseable {

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
            Pointer niPointer
        ) {
            if(niPointer.isNull()) return null;
            else return wrap((${ni_name}) niPointer.readWord(0));
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
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api
    j_name = api.wrapping_type(cls, False)
    c_name = cls.c_type(capi).name

    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
    ptr_sig = f"{sig_base}$PointerWrapper"
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

    // Get the class
    jclass clazz = (*env)->FindClass(
        env,
        "${sig_base}$${j_name}"
    );

    // Get the construtor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};)V"
    );

    // Return the new iterator
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) struct_native)
    );
}

// Get a native ${c_name} from a Java wrapping instance
${c_name} ${j_name}_unwrap(
    JNIEnv *env,
    jobject object
) {
    return (${c_name}) get_reference(env, object);
}
</%def>
