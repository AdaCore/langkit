<%def name="wrapping_class(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls, ast_wrapping=False)
    ni_type = api.ni_type(cls)
    c_type = cls.c_type(capi).name

    fields = api.get_struct_fields(cls)
    flatten_fields = api.flatten_struct_fields(fields)
    %>

    % if len(cls.get_fields()) > 0:
    ${java_doc(cls, 4)}
    public static class ${java_type} {

        // ----- Attributes -----

        /** Singleton that represents the none value for the structure. */
        public static final ${java_type} NONE = new ${java_type}(
            ${','.join([
                api.none_value(field.public_type, False)
                for field in fields
            ])}
        );

        % for field in fields:
        public final
        ${api.wrapping_type(field.public_type, ast_wrapping=False)}
        ${field.name};
        % endfor

        // ----- Constructors -----

        /**
         * Create a new structure object from the value if its fields.
         */
        ${java_type}(
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
         * Create a new structure with the field values.
         */
        public static ${java_type} create(
            ${','.join([
                f"{api.wrapping_type(field.public_type, ast_wrapping=False)}"
                f" {field.name}"
                for field in fields
            ])}
        ) {
            return new ${java_type}(
                ${','.join([field.name for field in fields])}
            );
        }

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer to the native structure value in the Java class.
         *
         * @param niPointer The pointer to the NI structure native value.
         * @return The newly wrapped structure.
         */
        static ${java_type} wrap(
            Pointer niPointer
        ) {
            return wrap((${ni_type}) niPointer.readWord(0));
        }

        /**
         * Wrap the given structure native value in the Java class.
         *
         * @param structNative The NI structure native value.
         * @return The newly wrapped structure.
         */
        static ${java_type} wrap(
            ${ni_type} structNative
        ) {
            return new ${java_type}(
                ${','.join([
                    api.ni_field_wrap(field)
                    for field in fields
                ])}
            );
        }

        /**
         * Unwrap the structure in the given native value.
         *
         * @param structNative The NI structure native value to fill.
         */
        void unwrap(${ni_type} structNative) {
            % for flat in flatten_fields:
            ${api.ni_field_unwrap(flat)}
            % endfor
        }

        % if cls.is_ada_record:
        /**
         * Get an empty value for the structure.
         *
         * @param structNative The NI structure native value to initialize.
         */
        static void defaultValue(${ni_type} structNative) {
            % for flat in flatten_fields:
            structNative.set_${flat.native_access}(
                ${api.ni_default_value(flat.public_type)}
            );
            % endfor
        }
        % endif

        % if cls.is_refcounted:
        /**
         * Release the structure.
         *
         * @param structNative The native structure to release.
         */
        static void release(${ni_type} structNative) {
            NI_LIB.${cls.c_dec_ref(capi)}(structNative);
        }
        % endif

    }
    % endif
</%def>

<%def name="ni_def(cls)">
    <%
    api = java_api

    ni_type = api.ni_type(cls)
    c_type = cls.c_type(capi).name

    flatten_fields = api.flatten_struct_fields(api.get_struct_fields(cls))
    %>

    % if len(cls.get_fields()) > 0:
    /** The structure for the langkit ${c_type} */
    @CContext(LibDirectives.class)
    @CStruct("${c_type}")
    public interface ${ni_type} extends PointerBase {
        % for field in flatten_fields:
        @CField("${field.custom_access('.')}")
        public ${api.ni_type(field.public_type)}
        get_${field.native_access}();

        @CFieldAddress("${field.custom_access('.')}")
        public <T extends PointerBase> T address_${field.native_access}();

        @CField("${field.custom_access('.')}")
        public void
        set_${field.native_access}(
            ${api.ni_type(field.public_type)} val
        );
        % endfor
    }
    % endif
</%def>

<%def name="ni_funcs(cls)">
    <%
    api = java_api

    ni_type = api.ni_type(cls)
    %>

        % if cls.is_refcounted:
        /**
         * Decreate the reference counter of the given struct.
         *
         * @param structNative The structure to decrease the reference counter.
         */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native void ${cls.c_dec_ref(capi)}(
            ${ni_type} structNative
        );
        % endif
</%def>

<%def name="jni_c_decl(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls, False)
    c_type = cls.c_type(capi).name
    %>

${c_type} ${java_type}_new_value();
% if len(cls.get_fields()) > 0:
jobject ${java_type}_wrap(JNIEnv *, ${c_type});
${c_type} ${java_type}_unwrap(JNIEnv *, jobject);
    % if cls.is_refcounted:
    void ${java_type}_release(${c_type});
    % endif
% endif
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api

    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"

    java_type = api.wrapping_type(cls, False)
    c_type = cls.c_type(capi).name

    fields = api.get_struct_fields(cls)
    constructor_sig = "".join([
        api.jni_sig_type(field.public_type, sig_base) for field in fields
    ])
    %>

// Create a new value for a langkit ${c_type}
${c_type} ${java_type}_new_value() {
    ${c_type} res = {
        % for f in cls.get_fields():
        ${api.jni_new_value(f.type)},
        % endfor
    };
    return res;
}

% if len(cls.get_fields()) > 0:
// Wrap a native ${c_type} in the Java wrapping class
jobject ${java_type}_wrap(
    JNIEnv *env,
    ${c_type} native_struct
) {
    // Get the Java class
    jclass clazz = (*env)->FindClass(
        env,
        "${sig_base}$${java_type}"
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
                f"native_struct.{field.native_name}",
                []
            )
            for field in fields
        ])}
    );
}

// Get a native ${c_type} from a Java wrapping instance
${c_type} ${java_type}_unwrap(
    JNIEnv *env,
    jobject object
) {
    // Prepare the result structure
    ${c_type} res = ${java_type}_new_value();

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
    ${api.jni_unwrap(
        field.public_type,
        f"{field.native_name}_value",
        f"{field.native_name}_native",
        []
    )}
    res.${field.native_name} = ${field.native_name}_native;
    % endfor

    // Return the native result
    return res;
}

    % if cls.is_refcounted:
// Decrease the reference counter of the given structure
void ${java_type}_release(
    ${c_type} struct_native
) {
    ${cls.c_dec_ref(capi)}(&struct_native);
}
    % endif
% endif
</%def>
