## vim: ft=makojava

<%def name="wrapping_class(cls)">
    <%
    api = java_api

    wrap_nodes = not (cls.is_entity_type or cls == T.Metadata)

    java_type = api.wrapping_type(cls, ast_wrapping=wrap_nodes)
    ni_type = api.ni_type(cls)
    c_type = cls.c_type(capi).name

    fields = api.get_struct_fields(cls)
    flatten_fields = api.flatten_struct_fields(fields)
    %>

    % if not cls.is_empty:
    ${java_doc(cls, 4)}
    public static final class ${java_type} {

        // ----- Class attributes -----

        /** Singleton that represents the none value for the structure. */
        public static final ${java_type} NONE = new ${java_type}(
            ${','.join([
                api.none_value(field.public_type, ast_wrapping=wrap_nodes)
                for field in fields
            ])}
        );

        // ----- Instance attributes -----

        % for field in fields:
        public final
        ${api.wrapping_type(field.public_type, ast_wrapping=wrap_nodes)}
        ${field.name};
        % endfor

        // ----- Constructors -----

        /**
         * Create a new structure object from the value if its fields.
         */
        ${java_type}(
            ${','.join([
                f"final "
                f"{api.wrapping_type(field.public_type, wrap_nodes)}"
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
                f"final "
                f"{api.wrapping_type(field.public_type, wrap_nodes)}"
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
            final WordPointer pointer
        ) {
            return wrap((${ni_type}) pointer.read());
        }

        /**
         * Wrap the given structure native value in the Java class.
         *
         * @param structNative The NI structure native value.
         * @return The newly wrapped structure.
         */
        static ${java_type} wrap(
            final ${ni_type} structNative
        ) {
            return new ${java_type}(
                ${','.join([
                    api.ni_field_wrap(field, ast_wrapping=wrap_nodes)
                    for field in fields
                ])}
            );
        }

        % if cls.is_entity_type:
        /**
         * Special wrapping method to construct a new entity structure
         * from a native bare node pointer.
         */
        static ${java_type} wrapBareNode(
            final Pointer bareNode
        ) {
            return new ${java_type}(
                ${','.join([
                    "PointerWrapper.wrap(bareNode)"
                    if field.lower_name == "node" else
                    api.none_value(
                        field.public_type,
                        ast_wrapping=False
                    )
                    for field in fields
                ])}
            );
        }
        % endif

        /**
         * Unwrap the structure in the given native value.
         *
         * @param structNative The NI structure native value to fill.
         */
        void unwrap(
            final ${ni_type} structNative
        ) {
            % for field in fields:
            ${api.ni_field_unwrap(field, ast_wrapping=wrap_nodes)}
            % endfor
        }

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
    % else:
    public static final class ${java_type} {

        // ----- Class attributes -----

        /** Singleton that represents the none value for the structure. */
        public static final ${java_type} NONE = new ${java_type}();

        // ----- Instance attributes ------

        /** The dummy field is always false (0) */
        final boolean dummy = false;

        // ----- Constructors -----

        /**
         * An empty constructor because the structure is empty
         */
        ${java_type}() {}

        // ----- Graal C API methods -----

        /**
         * Wrap a pointer to the native structure value in the Java class.
         *
         * @param niPointer The pointer to the NI structure native value.
         * @return The None instance because the structure is empty.
         */
        static ${java_type} wrap(
            final WordPointer pointer
        ) {
            return ${java_type}.NONE;
        }

        /**
         * Unwrap the structure in the given native value.
         *
         * @param structNative The NI structure native value to fill.
         */
        void unwrap(
            final ${ni_type} structNative
        ) {
            // Do nothing because the dummy field is useless
        }

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

    fields = api.get_struct_fields(cls)
    flatten_fields = api.flatten_struct_fields(fields)
    %>

    % if not cls.is_empty:
    /** The structure for the langkit ${c_type} */
    @CContext(LibDirectives.class)
    @CStruct("${c_type}")
    public interface ${ni_type} extends PointerBase {
        % for field in flatten_fields:
        @CField("${field.custom_access('.')}")
        public ${api.ni_type(field.public_type, ast_wrapping=False)}
        get_${field.native_access}();

        @CField("${field.custom_access('.')}")
        public void
        set_${field.native_access}(
            ${api.ni_type(field.public_type)} val
        );

        @CFieldAddress("${field.custom_access('.')}")
        public <T extends PointerBase> T address_${field.native_access}();

        % endfor

        % for field in [f for f in fields if f.fields is not None]:
            <%
            n = lambda f:(
                [f.lower_name] + n(f.fields[0])
                if f.fields else
                [f.lower_name]
            )
            first_field = n(field)
            %>
        @CFieldAddress("${'.'.join(first_field)}")
        public ${api.ni_type(field.public_type)}
        address_${field.lower_name}();

        % endfor
    }
    % else:
    @CContext(LibDirectives.class)
    @CStruct("${c_type}")
    public interface ${ni_type} extends PointerBase {
        @CField("dummy") public byte get_dummy();
        @CField("dummy") public void set_dummy(byte dummy);
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

    wrap_nodes = not (cls.is_entity_type or cls == T.Metadata)

    java_type = api.wrapping_type(cls, ast_wrapping=wrap_nodes)
    c_type = cls.c_type(capi).name

    fields = api.get_struct_fields(cls)
    %>

${c_type} ${java_type}_new_value();
jobject ${java_type}_wrap(JNIEnv *, ${c_type});
jobject ${java_type}_wrap_bare_node(JNIEnv *, ${node_type});
${c_type} ${java_type}_unwrap(JNIEnv *, jobject);

% if cls.is_refcounted:
void ${java_type}_release(${c_type});
% endif

jclass ${java_type}_class_ref = NULL;

% if len(fields) > 0:
jmethodID ${java_type}_constructor_id = NULL;
% endif

jfieldID ${java_type}_none_field_id = NULL;

% for field in fields:
jfieldID ${java_type}_${field.native_name}_field_id = NULL;
% endfor
</%def>

<%def name="jni_init_global_refs(cls)">
    <%
    api = java_api

    wrap_nodes = not (cls.is_entity_type or cls == T.Metadata)

    java_type = api.wrapping_type(cls, ast_wrapping=wrap_nodes)
    sig_base = f"com/adacore/{ctx.lib_name.lower}/{ctx.lib_name.camel}"

    fields = api.get_struct_fields(cls)

    constructor_sig = "".join([
        api.jni_sig_type(
            field.public_type,
            sig_base,
            ast_wrapping=wrap_nodes
        )
        for field in fields
    ])
    %>

    ${java_type}_class_ref = (jclass) (*env)->NewGlobalRef(
        env,
        (*env)->FindClass(
            env,
            "${sig_base}$${java_type}"
        )
    );

    % if len(fields) > 0:
    ${java_type}_constructor_id = (*env)->GetMethodID(
        env,
        ${java_type}_class_ref,
        "<init>",
        "(${constructor_sig})V"
    );
    % endif

    ${java_type}_none_field_id = (*env)->GetStaticFieldID(
        env,
        ${java_type}_class_ref,
        "NONE",
        "L${sig_base}$${java_type};"
    );

    % for field in fields:
    ${java_type}_${field.native_name}_field_id = (*env)->GetFieldID(
        env,
        ${java_type}_class_ref,
        "${field.name}",
        "${api.jni_sig_type(
            field.public_type,
            sig_base,
            ast_wrapping=wrap_nodes
        )}"
    );
    % endfor
</%def>

<%def name="jni_c_impl(cls)">
    <%
    api = java_api

    wrap_nodes = not (cls.is_entity_type or cls == T.Metadata)

    java_type = api.wrapping_type(cls, ast_wrapping=wrap_nodes)
    c_type = cls.c_type(capi).name

    fields = api.get_struct_fields(cls)
    %>

// Create a new value for a langkit ${c_type}
${c_type} ${java_type}_new_value() {
    % if len(fields) > 0:
    ${c_type} res = {
        % for f in cls.get_fields():
        ${api.jni_new_value(f.type)},
        % endfor
    };
    % else:
    ${c_type} res = {
        0,
    };
    % endif
    return res;
}

// Wrap a native ${c_type} in the Java wrapping class
jobject ${java_type}_wrap(
    JNIEnv *env,
    ${c_type} native_struct
) {
    % if len(fields) > 0:
    // Return the new Java instance
    return (*env)->NewObject(
        env,
        ${java_type}_class_ref,
        ${java_type}_constructor_id,
        ${",".join([
            api.jni_wrap(
                field.public_type,
                f"native_struct.{field.native_name}",
                [],
                ast_wrapping=wrap_nodes
            )
            for field in fields
        ])}
    );
    % else:
    // Return the None instance because there is no need of new one
    return (*env)->GetStaticObjectField(
        env,
        ${java_type}_class_ref,
        ${java_type}_none_field_id
    );
    % endif
}

% if cls.is_entity_type:
jobject ${java_type}_wrap_bare_node(
    JNIEnv *env,
    ${node_type} bare_node
) {
    ${entity_type} struct_native = ${java_type}_new_value();
    struct_native.node = bare_node;
    return ${java_type}_wrap(env, struct_native);
}
% endif

// Get a native ${c_type} from a Java wrapping instance
${c_type} ${java_type}_unwrap(
    JNIEnv *env,
    jobject object
) {
    // Prepare the result structure
    ${c_type} res = ${java_type}_new_value();

    // Get the field values
    % for field in fields:
    ${api.jni_c_type(field.public_type)} ${field.native_name}_value =
        (*env)->${api.jni_field_access(field.public_type)}(
            env,
            object,
            ${java_type}_${field.native_name}_field_id
        );
    % endfor

    // Fill the result structure
    % for field in fields:
    ${api.jni_unwrap(
        field.public_type,
        f"{field.native_name}_value",
        f"{field.native_name}_native",
        [],
        ast_wrapping=wrap_nodes
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
</%def>
