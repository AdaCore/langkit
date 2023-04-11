<%def name="decl(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls)
    %>

    ${java_doc(cls, 4)}
    public enum ${java_type} {

        // ----- Enum values -----

        % for i in range(len(cls.values)):
        ${api.mangle_enum(cls.values[i].name.upper)}(${i}),
        % endfor
        ;

        // ----- Attributes -----

        /** Singleton that represents the none enum value. */
        public static final ${java_type} NONE =
            ${api.mangle_enum(cls.values[0].name.upper)};

        /** The value of the enum instance. */
        private final int value;

        /** The map from int to enum values. */
        private static final Map<Integer, ${java_type}> map = new HashMap<>();

        // ----- Constructors -----

        static {
            // Initialise the lookup map
            for(${java_type} elem : ${java_type}.values()) {
                map.put(elem.value, elem);
            }
        }

        /** Private constructor. */
        private ${java_type}(
            final int value
        ) {
            this.value = value;
        }

        // ----- Enum methods -----

        /**
         * Get the enum instance for the given C value.
         *
         * @param cValue The C int value to get the enum instance from.
         * @return The enum instance which correspond to the int value.
         * @throws EnumException When the int value doesn't map to any enum
         * instance.
         */
        public static ${java_type} fromC(
            final int cValue
        ) throws EnumException {
            if(!map.containsKey(cValue))
                throw new EnumException(
                    "Cannot get ${java_type} from " + cValue
                );
            return (${java_type}) map.get(cValue);
        }

        /**
         * Get the C value from the enum instance.
         *
         * @return The int C value of the enum instance.
         */
        public int toC() {
            return this.value;
        }

    }
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
jmethodID ${java_type}_from_c_method_id = NULL;
jmethodID ${java_type}_to_c_method_id = NULL;
</%def>

<%def name="jni_init_global_refs(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls, False)

    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
    %>

    ${java_type}_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(env, "${sig_base}$${java_type}")
    );

    ${java_type}_from_c_method_id = (*env)->GetStaticMethodID(
        env,
        ${java_type}_class_ref,
        "fromC",
        "(I)L${sig_base}$${java_type};"
    );

    ${java_type}_to_c_method_id = (*env)->GetMethodID(
        env,
        ${java_type}_class_ref,
        "toC",
        "()I"
    );
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls)
    c_type = cls.c_type(capi).name
    %>

// Get a new value for the enumeration
${c_type} ${java_type}_new_value() {
    return 0;
}

// Wrap the native enum value in a Java class
jobject ${java_type}_wrap(
    JNIEnv *env,
    ${c_type} enum_value_native
) {
    // Call the static method
    return (*env)->CallStaticObjectMethod(
        env,
        ${java_type}_class_ref,
        ${java_type}_from_c_method_id,
        (jint) enum_value_native
    );
}

// Unwrap a Java enum object in a native enum value
${c_type} ${java_type}_unwrap(
    JNIEnv *env,
    jobject enum_value
) {
    // Call the Java method
    return (${c_type}) (*env)->CallIntMethod(
        env,
        enum_value,
        ${java_type}_to_c_method_id
    );
}
</%def>
