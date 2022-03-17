<%def name="wrapping_class(cls)">
    <%
    api = java_api
    nat = c_api.get_name

    class_name = api.wrapping_type(cls)
    ni_name = api.java_ni_type(cls)

    elem_class_name = api.wrapping_type(cls.element_type)
    elem_ni_name = api.java_ni_type(cls.element_type)
    elem_ni_ref_name = api.ni_reference_type(cls.element_type)
    elem_jni_name = api.java_jni_type(cls.element_type)

    c_name = cls.c_type(capi).name
    %>

    ${java_doc(cls, 4)}
    public static final class ${class_name}
    extends ArrayBase<${elem_class_name}> {

        // ----- Attributes -----

        /** The content of the array. */
        protected final ${elem_jni_name}[] content;

        /** The cleanable instance. */
        private final Cleaner.Cleanable cleanable;

        // ----- Constructors -----

        /**
         * Create a new array wrapper from a pointer.
         *
         * @param reference The pointer to the native structure.
         */
        private ${class_name}(
            PointerWrapper reference
        ) {
            this(reference, null);
        }

        /**
         * Create a new array wrapper from its pointer and its cached
         * content. This constructor is only used by JNI bindings.
         *
         * @param reference The pointer to the native structure.
         * @param content The content of the array.
         */
        private ${class_name}(
            PointerWrapper reference,
            ${elem_jni_name}[] content
        ) {
            super(reference);
            this.content = content;
            this.cleanable = CLEANER.register(
                this,
                new ReleaseTask(reference)
            );
        }

        /**
         * Create a sized array.
         *
         * @param size The size of the array you want to create.
         * @return The newly created array.
         */
        public static ${class_name} create(int size) {

            if(ImageInfo.inImageCode()) {
                return ${class_name}.wrap(
                    NI_LIB.${cls.c_create(capi)}(size)
                );
            } else {
                return JNI_LIB.${cls.c_create(capi)}(size);
            }

        }

        // ----- Cleaning methods/classes -----

        /**
         * The release task for the array
         */
        private static final class ReleaseTask implements Runnable {

            // ----- Attributes -----

            /** The reference to the array */
            private final PointerWrapper arrayRef;

            // ----- Constructors -----

            /**
             * Create a new release task for an array.
             *
             * @param arrayRef The referenc to the array.
             */
            ReleaseTask(
                PointerWrapper arrayRef
            ) {
                this.arrayRef = arrayRef;
            }

            // ----- Instance methods -----

            /** @see java.lang.Runnable#run() */
            @Override
            public void run() {
                ${class_name}.release(this.arrayRef);
            }

        }

        /**
         * Release the given array reference.
         *
         * @pararm arrayRef The reference to the array to release.
         */
        public static void release(
            PointerWrapper arrayRef
        ) {

            if(ImageInfo.inImageCode()) {
                NI_LIB.${cls.c_dec_ref(c_api)}(arrayRef.ni());
            } else {
                JNI_LIB.${cls.c_dec_ref(c_api)}(arrayRef.jni());
            }

        }

        /** @see java.lang.AutoCloseable#close() */
        @Override
        public void close() {
            this.cleanable.clean();
        }

        // ----- Class methods -----

        /**
         * Wrap a pointer to an array native value in the Java class.
         *
         * @param niPointer The pointer to the array NI native value.
         * @return The newly created array or null if the pointer is null.
         */
        static ${class_name} wrap(
            Pointer niPointer
        ) {
            if(niPointer.isNull()) return null;
            else return wrap((${ni_name}) niPointer.readWord(0));
        }

        /**
         * Wrap an array native value in the Java class.
         *
         * @param arrayNative The NI array native value to wrap.
         * @return The newly created array or null of the given native
         * value is null.
         */
        static ${class_name} wrap(
            ${ni_name} arrayNative
        ) {
            if(((PointerBase) arrayNative).isNull()) return null;
            else return new ${class_name}(
                new PointerWrapper(arrayNative)
            );
        }

        // ----- Instance methods -----

        /** @see ArrayBase#size() */
        @Override
        public int size() {

            if(ImageInfo.inImageCode()) {
                return ((${ni_name}) this.reference.ni()).get_n();
            } else {
                return this.content.length;
            }

        }

        /** @see ArrayBase#get(int) */
        @Override
        public ${elem_class_name} get(int i) {

            if(i < this.size() && i >= 0) {

                if(ImageInfo.inImageCode()) {
                    ${ni_name} nativeArray = this.reference.ni();
                    Pointer toNativeRes = nativeArray.addressOfItems().add(
                        i * SizeOf.get(${elem_ni_name}.class)
                    );
                    ${elem_ni_ref_name} nativeRes = WordFactory.unsigned(
                        toNativeRes.rawValue()
                    );
                    return ${
                        api.ni_wrap(cls.element_type, "nativeRes")
                    };
                } else {
                    ${elem_jni_name} res = this.content[i];
                    return ${api.java_jni_wrap(cls.element_type, "res")};
                }

            } else {
                throw new IndexOutOfBoundsException(
                    "Index " + i + " is invalid for array of size "
                        + this.size()
                );
            }

        }

        /**
         * Set the given element at the given place.
         *
         * @param i The index to put the element at.
         * @param elem The element to place in the array.
         */
        public void set(
            int i,
            ${elem_class_name} elem
            ${(
                ", AnalysisContext currentContext"
                if cls.element_type.is_symbol_type else
                ""
            )}
        ) {

            if(i < this.size() && i >= 0) {

                if(ImageInfo.inImageCode()) {
                    ${ni_name} nativeArray = this.reference.ni();
                    Pointer toNativeRes = nativeArray.addressOfItems().add(
                        i * SizeOf.get(${elem_ni_name}.class)
                    );
                    ${elem_ni_ref_name} nativeRes = WordFactory.unsigned(
                        toNativeRes.rawValue()
                    );
                    ${api.java_ni_write(
                        cls.element_type,
                        "elem",
                        "nativeRes"
                    )};
                } else {
                    ${elem_jni_name} elemJni =
                        ${api.java_jni_unwrap(cls.element_type, "elem")};
                    this.content[i] = elemJni;
                    JNI_LIB.${nat(f"{c_name}_set")}(
                        this,
                        i,
                        elemJni
                        ${(
                            ", currentContext"
                            if cls.element_type.is_symbol_type else
                            ""
                        )}
                    );
                }

            } else {
                throw new IndexOutOfBoundsException(
                    "Index "
                    + i
                    + " is invalid for array of size "
                    + this.size()
                );
            }

        }

        /** @see java.util.Iterable#iterator() */
        @Override
        public Iterator<${elem_class_name}> iterator() {
            return new ${class_name}Iterator(this);
        }

        // ----- Inner classes -----

        /**
         * The iterator class for the array.
         */
        private static final class ${class_name}Iterator
        implements Iterator<${elem_class_name}> {

            // ----- Attributes -----

            /** The array to iterate on. */
            private final ${class_name} array;

            /** The current index. */
            private int index;

            // ----- Constructors -----

            /**
             * Create an iterator class for the given array.
             *
             * @param array The array to create the iterator for.
             */
            public ${class_name}Iterator(
                ${class_name} array
            ) {
                this.array = array;
                this.index = 0;
            }

            // ----- Instance methods -----

            @Override
            public boolean hasNext() {
                return this.index < this.array.size();
            }

            @Override
            public ${elem_class_name} next() {

                if(ImageInfo.inImageCode()) {
                    ${ni_name} nativeArray = this.array.reference.ni();
                    Pointer toNativeRes = nativeArray.addressOfItems().add(
                        this.index * SizeOf.get(${elem_ni_name}.class)
                    );
                    ${elem_ni_name} nativeRes = WordFactory.unsigned(
                        toNativeRes.rawValue()
                    );
                    this.index += 1;
                    return ${api.ni_wrap(cls.element_type, "nativeRes")};
                } else {
                    ${elem_jni_name} res = this.array.content[this.index++];
                    return ${api.java_jni_wrap(cls.element_type, "res")};
                }

            }

        }

    }
</%def>

<%def name="ni_def(cls)">
    <%
    api = java_api
    ni_name = api.java_ni_type(cls)
    c_name = cls.c_type(capi).name
    %>

    /**
     * The native structure of the ${c_name} langkit array.
     */
    @CContext(LibDirectives.class)
    @CStruct(
        value = "${c_name}_record",
        addStructKeyword = true,
        isIncomplete = true
    )
    public interface ${ni_name} extends PointerBase {
        @CField("n") public int get_n();
        @CField("ref_count") public int get_ref_count();
        @CFieldAddress("items") public Pointer addressOfItems();
    }
</%def>

<%def name="ni_funcs(cls)">
    <%
    api = java_api
    ni_name = api.java_ni_type(cls)
    %>

        /**
         * Decrease reference counter of the given array
         *
         * @param array The array to decrease the reference counter.
         */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${cls.c_dec_ref(c_api)}(${ni_name} array);

        /**
         * Create a new sized array.
         *
         * @param size The size of the array to create.
         * @return The native pointer to the created array.
         */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native ${ni_name} ${cls.c_create(capi)}(int size);
</%def>

<%def name="jni_funcs(cls)">
    <%
    api = java_api
    nat = c_api.get_name

    c_name = cls.c_type(capi).name
    class_name = api.wrapping_type(cls)
    elem_jni_name = api.java_jni_type(cls.element_type)
    %>

        /**
         * Decrease reference counter of the given array.
         *
         * @param array The array to decrease the reference counter from
         */
        @CompilerDirectives.TruffleBoundary
        public static native void ${cls.c_dec_ref(c_api)}(
            long arrayRef
        );

        /**
         * Create a new sized array.
         *
         * @param size The size of the array to create.
         * @return The newly created array.
         */
        @CompilerDirectives.TruffleBoundary
        public static native ${class_name} ${cls.c_create(capi)}(
            int size
        );

        /**
         * Set the wanted element of the array.
         *
         * @param array The array to set the element in.
         * @param i The index of the element to set.
         * @param elem The element to place in the array.
         */
        @CompilerDirectives.TruffleBoundary
        public static native void ${nat(f"{c_name}_set")}(
            ${class_name} array,
            int i,
            ${elem_jni_name} elem
            ${(
                ", AnalysisContext context"
                if cls.element_type.is_symbol_type else
                ""
            )}
        );
</%def>

<%def name="jni_c_decl(cls)">
    <%
    api = java_api

    c_name = cls.c_type(capi).name
    j_name = api.wrapping_type(cls)
    %>

${c_name} ${j_name}_new_value();
jobject ${j_name}_wrap(JNIEnv *, ${c_name});
${c_name} ${j_name}_unwrap(JNIEnv *, jobject);
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api
    c_name = cls.c_type(capi).name
    j_name = api.java_jni_type(cls)

    elem_c_name = cls.element_type.c_type(capi).name
    elem_j_name = api.java_jni_type(cls.element_type)

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
    ${c_name} native_struct
) {
    // Verify the nullity of the native value
    if(native_struct == NULL) return NULL;

    // Get the size of the array
    int array_size = native_struct->n;

    // Create a new Java array of object of the element type
    jclass element_clazz = (*env)->FindClass(
        env,
        "${sig_base}$${elem_j_name}"
    );
    jobjectArray result_array = (*env)->NewObjectArray(
        env,
        (jsize) array_size,
        element_clazz,
        NULL
    );

    // Put the elements in the Java array
    for(int i = 0 ; i < array_size ; i++) {
        ${elem_c_name} elem = native_struct->items[i];
        (*env)->SetObjectArrayElement(
            env,
            result_array,
            (jsize) i,
            ${elem_j_name}_wrap(env, elem)
        );
    }

    // Get the array class
    jclass clazz = (*env)->FindClass(
        env,
        "${sig_base}$${j_name}"
    );

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(L${ptr_sig};[L${sig_base}$${elem_j_name};)V"
    );

    // Return the new array
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        PointerWrapper_wrap(env, (void *) native_struct),
        result_array
    );
}

// Get a native ${c_name} from a Java wrapping instance
${c_name} ${j_name}_unwrap(
    JNIEnv *env,
    jobject object
) {
    return (${c_name}) get_reference(env, object);
}

// Decrease the reference counter of an array
${api.jni_func_sig(cls.c_dec_ref(c_api), "void", do_nat=False)}(
    JNIEnv *env,
    jclass jni_lib,
    jlong array_ref
) {
    ${cls.c_dec_ref(c_api)}((${c_name}) array_ref);
}

// Create a new sized array
${api.jni_func_sig(cls.c_create(capi), "jobject", do_nat=False)}(
    JNIEnv *env,
    jclass jni_lib,
    jint size
) {
    ${c_name} res = ${cls.c_create(capi)}(
        (int) size
    );
    return ${j_name}_wrap(env, res);
}

// Set the element of an array
${api.jni_func_sig(f"{c_name}_set", "void")}(
    JNIEnv *env,
    jclass jni_lib,
    jobject array,
    jint i,
    jobject elem
    ${(
        ", jobject context"
        if cls.element_type.is_symbol_type else
        ""
    )}
) {
    ${c_name} array_native = ${j_name}_unwrap(env, array);
    % if cls.element_type.is_symbol_type:
    ${analysis_context_type} context_native =
        AnalysisContext_unwrap(env, context);
    % endif
    array_native->items[i] = ${api.jni_unwrap(cls.element_type, "elem")};
}
</%def>
