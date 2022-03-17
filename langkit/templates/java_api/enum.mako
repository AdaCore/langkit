<%def name="decl(cls)">
    <%
    api = java_api
    enum_name = api.wrapping_type(cls)
    %>

    ${java_doc(cls, 4)}
    public enum ${enum_name} {

        // ----- Enum values -----

        % for i in range(len(cls.values)):
        ${cls.values[i].name.upper}(${i}),
        % endfor
        ;

        // ----- Attributes -----

        /** The value of the enum instance. */
        private final int value;

        /** The map from int to enum values. */
        private static final Map<Integer, ${enum_name}> map = new HashMap<>();

        // ----- Constructors -----

        static {
            // Initialise the lookup map
            for(${enum_name} elem : ${enum_name}.values()) {
                map.put(elem.value, elem);
            }
        }

        /** Private constructor. */
        private ${enum_name}(int value) {
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
        public static ${enum_name} fromC(int cValue) throws EnumException {
            if(!map.containsKey(cValue))
                throw new EnumException(
                    "Cannot get ${enum_name} from " + cValue
                );
            return (${enum_name}) map.getOrDefault(cValue, null);
        }

        /**
         * Get the C value from the enum instance.
         *
         * @return The int C value of the enum instance.
         */
        public int toC() { return this.value; }

    }
</%def>

<%def name="jni_c_decl(cls)">
    <%
    api = java_api
    enum_name = api.wrapping_type(cls)
    c_name = cls.c_type(capi).name
    %>

${c_name} ${enum_name}_new_value();
jobject ${enum_name}_wrap(JNIEnv *, ${c_name});
${c_name} ${enum_name}_unwrap(JNIEnv *, jobject);
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api
    enum_name = api.wrapping_type(cls)
    c_name = cls.c_type(capi).name

    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
    %>

// Get a new value for the enumeration
${c_name} ${enum_name}_new_value() {
    return 0;
}

// Wrap the native enum value in a Java class
jobject ${enum_name}_wrap(
    JNIEnv *env,
    ${c_name} enum_value_native
) {
    // Get the enum class
    jclass clazz = (*env)->FindClass(env, "${sig_base}$${enum_name}");

    // Get the constructing static method
    jmethodID from_c_method = (*env)->GetStaticMethodID(
        env,
        clazz,
        "fromC",
        "(I)L${sig_base}$${enum_name};"
    );

    // Call the static method
    return (*env)->CallStaticObjectMethod(
        env,
        clazz,
        from_c_method,
        (jint) enum_value_native
    );
}

// Unwrap a Java enum object in a native enum value
${c_name} ${enum_name}_unwrap(
    JNIEnv *env,
    jobject enum_value
) {
    // Get the object class
    jclass clazz = (*env)->GetObjectClass(env, enum_value);

    // Get the method
    jmethodID to_c_method = (*env)->GetMethodID(
        env,
        clazz,
        "toC",
        "()I"
    );

    // Call the Java method
    return (${c_name}) (*env)->CallIntMethod(
        env,
        enum_value,
        to_c_method
    );
}
</%def>
