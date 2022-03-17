<%def name="wrapping_class(cls)">
    <%
    api = java_api
    class_name = api.wrapping_type(cls, ast_wrapping=False)
    ni_name = api.java_ni_type(cls)
    c_name = cls.c_type(capi).name

    fields = api.get_struct_fields(cls)
    flatten_fields = api.flatten_struct_fields(fields)
    %>

    % if len(cls.get_fields()) > 0:
    ${java_doc(cls, 4)}
    public static class ${class_name} {

        // ----- Attributes -----

        % for field in fields:
        public final
        ${api.wrapping_type(field.public_type, ast_wrapping=False)}
        ${field.name};
        % endfor

        // ----- Constructors -----

        /**
         * Create a new structure object from the value if its fields.
         */
        ${class_name}(
            ${','.join([
                f"{api.wrapping_type(field.public_type, ast_wrapping=False)}"
                f" {field.name}"
                for field in fields
            ])}
        ) {
            % for field in fields:
            this.${field.name} = ${field.name};
            % endfor
        }

        /**
         * Wrap a pointer to the native structure value in the Java class.
         *
         * @param niPointer The pointer to the NI structure native value.
         * @return The newly created structure or null if the given pointer
         * is null.
         */
        static ${class_name} wrap(
            Pointer niPointer
        ) {
            if(niPointer.isNull()) return null;
            else return wrap((${ni_name}) niPointer.readWord(0));
        }

        /**
         * Wrap the given structure native value in the Java class.
         *
         * @param structNative The NI structure native value.
         * @return The newly created structure or null if the given native
         * value is null.
         */
        static ${class_name} wrap(
            ${ni_name} structNative
        ) {
            if(((PointerBase) structNative).isNull()) return null;
            else return new ${class_name}(
                ${','.join([
                    api.ni_field_wrap(field)
                    for field in fields
                ])}
            );
        }

        /**
         * Create a new structure with the field values.
         */
        public static ${class_name} create(
            ${','.join([
                f"{api.wrapping_type(field.public_type, ast_wrapping=False)}"
                f" {field.name}"
                for field in fields
            ])}
        ) {
            return new ${class_name}(
                ${','.join([field.name for field in fields])}
            );
        }

        // ----- Instance methods -----

        /**
         * Unwrap the structure in the given native value.
         *
         * @param structNative The NI structure native value to fill.
         */
        public void unwrap(${ni_name} structNative) {
            % for flat in flatten_fields:
            structNative.set_${flat.native_access}(
                this.${api.ni_field_unwrap(flat)}
            );
            % endfor
        }

        % if cls.is_ada_record:
        /**
         * Get an empty value for the structure.
         *
         * @param structNative The NI structure native value to initialize.
         */
        public static void defaultValue(${ni_name} structNative) {
            % for flat in flatten_fields:
            structNative.set_${flat.native_access}(
                ${api.ni_null_value(flat.public_type)}
            );
            % endfor
        }
        % endif

    }
    % endif
</%def>

<%def name="ni_def(cls)">
    <%
    api = java_api
    ni_name = api.java_ni_type(cls)
    c_name = cls.c_type(capi).name
    flatten_fields = api.flatten_struct_fields(api.get_struct_fields(cls))
    %>

    % if len(cls.get_fields()) > 0:
    /** The structure for the langkit ${c_name} */
    @CContext(LibDirectives.class)
    @CStruct("${c_name}")
    public interface ${ni_name} extends PointerBase {
        % for field in flatten_fields:
        @CField("${field.custom_access('.')}")
        public ${api.java_ni_type(field.public_type)} 
        get_${field.native_access}();

        @CField("${field.custom_access('.')}") 
        public void 
        set_${field.native_access}(
            ${api.java_ni_type(field.public_type)} val
        );
        % endfor
    }
    % endif
</%def>

<%def name="jni_c_decl(cls)">
    <%
    api = java_api
    c_name = cls.c_type(capi).name
    j_name = api.java_jni_type(cls)
    %>

${c_name} ${j_name}_new_value();
% if len(cls.get_fields()) > 0:
jobject ${j_name}_wrap(JNIEnv *, ${c_name});
${c_name} ${j_name}_unwrap(JNIEnv *, jobject);
% endif
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api
    c_name = cls.c_type(capi).name
    j_name = api.java_jni_type(cls)

    fields = api.get_struct_fields(cls)
    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"
    constructor_sig = "".join([
        api.jni_sig_type(field.public_type, sig_base) for field in fields
    ])
    %>

// Create a new value for a langkit ${c_name}
${c_name} ${j_name}_new_value() {
    ${c_name} res = {
        % for f in cls.get_fields():
        ${api.jni_new_value(f.type)},
        % endfor
    };
    return res;
}

% if len(cls.get_fields()) > 0:
// Wrap a native ${c_name} in the Java wrapping class
jobject ${j_name}_wrap(
    JNIEnv *env,
    ${c_name} native_struct
) {
    // Get the Java class
    jclass clazz = (*env)->FindClass(
        env,
        "${sig_base}$${j_name}"
    );

    // Get the constructor
    jmethodID constructor = (*env)->GetMethodID(
        env,
        clazz,
        "<init>",
        "(${constructor_sig})V"
    );

    // Return the new Java instance
    return (*env)->NewObject(
        env,
        clazz,
        constructor,
        ${", ".join([
            api.jni_wrap(
                field.public_type,
                f"native_struct.{field.native_name}"
            )
            for field in fields
        ])}
    );
}

// Get a native ${c_name} from a Java wrapping instance
${c_name} ${j_name}_unwrap(
    JNIEnv *env,
    jobject object
) {
    // Prepare the result structure
    ${c_name} res = ${j_name}_new_value();

    // Check the the Java object is not null
    if(object == NULL) {
        return res;
    }

    // Get the class
    jclass clazz = (*env)->GetObjectClass(env, object);

    // Get the field ids
    % for field in fields:
    jfieldID ${field.native_name}_field = (*env)->GetFieldID(
        env,
        clazz,
        "${field.name}",
        "${api.jni_sig_type(field.public_type, sig_base)}"
    );
    % endfor

    // Get the field values
    % for field in fields:
    ${api.jni_c_type(field.public_type)} ${field.native_name}_value =
        (*env)->${api.jni_field_access(field.public_type)}(
        env,
        object,
        ${field.native_name}_field
    );
    % endfor

    // Fill the result structure
    % for field in fields:
    res.${field.native_name} =
        ${api.jni_unwrap(field.public_type, f"{field.native_name}_value")};
    % endfor

    // Return the native result
    return res;
}
% endif
</%def>
