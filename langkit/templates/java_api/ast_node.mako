<%def name="wrapping_class(cls)">
    <%
    nat = c_api.get_name
    api = java_api
    class_name = api.wrapping_type(cls)
    base_name = api.wrapping_type(cls.base)
    c_name = cls.c_type(capi).name
    %>

    ${java_doc(cls, 4)}
    public static ${"abstract" if cls.abstract else ""}
    class ${class_name} extends ${base_name} {

        // ----- Static -----

        ${static_decl(cls)}

        // ----- Constructors -----

        protected ${class_name}(
            Entity entity
        ) {
            super(entity);
        }

        public static ${class_name} fromEntity(
            Entity entity
        ) {
            return (${class_name}) ${base_name}.fromEntity(entity);
        }

        // ----- Instance methods -----

        @Override
        public String getKindName() {
            return ${class_name}.kindName;
        }

        @Override
        public String[] getFieldNames() {
            return ${class_name}.fieldNames;
        }

        @Override
        public boolean isListType() {
            return ${class_name}.isListType;
        }

        @Override
        @CompilerDirectives.TruffleBoundary
        public ${ctx.lib_name.camel}Field getFieldDescription(String name) {
            return ${class_name}.fieldDescriptions.getOrDefault(name, null);
        }

        % if not cls.abstract:
        // ----- Visitor methods -----

        public <T> T accept(BasicVisitor<T> visitor) {
            return visitor.visit(this);
        }

        public <T, P> T accept(ParamVisitor<T, P> visitor, P param) {
            return visitor.visit(this, param);
        }
        % endif

        // ----- Field accessors -----

        % for field in cls.fields_with_accessors():
        ${field_accessor(field)}
        % endfor
    }

</%def>

<%def name="static_decl(cls)">
    <%
    api = java_api
    class_name = api.wrapping_type(cls)
    base_name = api.wrapping_type(cls.base) if cls.base else None
    field_names = api.get_node_formatted_fields(cls)
    %>

        /** The name of the node kind */
        public static final String kindName = "${class_name}";

        /** The names of the fields associated to the node */
        public static final String[] fieldNames = {
            ${",".join([f'"{name}"' for name in field_names])}
        };

        /** If the node is a list node */
        public static final boolean isListType =
            ${"true" if cls.is_list_type else "false"};

        /** The map containing the node's fields description. */
        public static final Map<String, ${ctx.lib_name.camel}Field>
        fieldDescriptions = new HashMap<>(
            ${f"{base_name}.fieldDescriptions" if base_name else ""}
        );

        // Initialisation of the method map
        static {
            % if len(cls.fields_with_accessors()) > 0:
            try {
                % for field in cls.fields_with_accessors():
                <%
                method = api.get_java_method(field)
                param_classes = [
                    f"{api.wrapping_type(param.public_type)}.class"
                    for param in method.params
                ]
                %>
                % if not method.name in api.excluded_fields:
                {
                    // Get the Java method of the field
                    Method method = ${class_name}.class.getMethod(
                        "${method.name}",
                        new Class[]{${",".join(param_classes)}}
                    );

                    // Create the parameter list
                    List<Param> parameters = new ArrayList<>();
                    % for param in method.params:
                        % if param.default_value_expr is None:
                    parameters.add(new Param(
                        ${api.wrapping_type(param.public_type)}.class,
                        "${param.name}"
                    ));
                        % else:
                    parameters.add(new ParamWithDefaultValue(
                        ${api.wrapping_type(param.public_type)}.class,
                        "${param.name}",
                        ${param.default_value_expr}
                    ));
                        % endif
                    % endfor

                    // Add the method and the parameters in maps
                    fieldDescriptions.put(
                        "${method.native_name}",
                        new ${ctx.lib_name.camel}Field(method, parameters)
                    );
                }
                % endif
                % endfor
            } catch (Exception e) {
                e.printStackTrace();
            }
            % endif
        }
</%def>

<%def name="field_accessor(field)">
    <%
    api = java_api
    nat = c_api.get_name
    method = api.get_java_method(field)
    native_function = nat(field.accessor_basename.lower)
    return_type = api.wrapping_type(method.public_type)
    return_jni = api.java_jni_type(method.public_type)
    ref_ni = api.ni_reference_type(method.public_type)
    %>

        % if not method.name in api.excluded_fields:
        ${java_doc(field, 8)}
        public ${return_type} ${method.name}(
            ${','.join([
                f"{api.wrapping_type(param.public_type)} {param.name}"
                for param in method.params
            ])}
        ) {

            if(ImageInfo.inImageCode()) {
                // Unwrap the current node
                EntityNative thisNative = StackValue.get(EntityNative.class);
                this.entity.unwrap(thisNative);

                % if api.field_need_context(field):
                // Get the node context
                AnalysisContext currentContext = this.getUnit().getContext();
                % endif

                // Unwrap the arguments
                % for param in method.params:
                ${api.ni_unwrap(
                    param.public_type,
                    param.name,
                    f"{param.name}Native"
                )};
                % endfor

                // Create the result native
                ${ref_ni} resNative = ${api.ni_new_value(field.public_type)};

                // Call the native function
                NI_LIB.${native_function}(
                    thisNative,
                    % for param in method.params:
                    ${f"{param.name}Native"},
                    % endfor
                    resNative
                );

                // Check the langkit exceptions
                LangkitException exception = checkException();
                if(exception != null) {
                    throw exception;
                }

                % if api.field_need_context(field):
                // Close the context
                currentContext.close();
                % endif

                // Wrap and return the result
                return ${api.ni_wrap(field.public_type, "resNative")};
            } else {
                // Call the native function
                ${return_jni} res = JNI_LIB.${native_function}(
                    % for param in method.params:
                    ${api.java_jni_unwrap(param.public_type, param.name)},
                    % endfor
                    this.entity
                );

                // Check the langkit exceptions
                LangkitException exception = checkException();
                if(exception != null) {
                    throw exception;
                }

                // Wrap and return the result
                return ${api.java_jni_wrap(field.public_type, "res")};
            }

        }
        % endif
</%def>

<%def name="ni_funcs(cls)">
    <%
    api = java_api
    nat = c_api.get_name
    %>

        % for field in cls.fields_with_accessors():
            <%
            native_function = nat(field.accessor_basename.lower)
            ni_ref = api.ni_reference_type(field.public_type)

            arg_list = []
            for arg in field.arguments:
                arg_list.append(
                    f"{api.java_ni_type(arg.public_type)} {arg.name.lower}"
                )
            %>

        % if not field.accessor_basename.lower in api.excluded_fields:
        /** Isomethod of ${native_function} langkit function */
        @CompilerDirectives.TruffleBoundary
        @CFunction
        public static native int ${native_function}(
            EntityNative node,
            % for arg in arg_list:
            ${arg},
            % endfor
            ${ni_ref} result
        );
        % endif
        % endfor
</%def>

<%def name="jni_funcs(cls)">
    <%
    api = java_api
    nat = c_api.get_name
    %>

        % for field in cls.fields_with_accessors():
            <%
            native_function = nat(field.accessor_basename.lower)
            jni_type = api.java_jni_type(field.public_type)
            %>

        % if not field.accessor_basename.lower in api.excluded_fields:
        /** Isomethod of ${native_function} langkit function */
        @CompilerDirectives.TruffleBoundary
        public static native ${jni_type} ${native_function}(
            % for arg in field.arguments:
            ${api.java_jni_type(arg.public_type)} ${arg.name.lower},
            % endfor
            Entity node
        );
        % endif
        % endfor
</%def>

<%def name="jni_field_accessor(field)">
    <%
    api = java_api
    nat = c_api.get_name
    c_type = field.c_type_or_error(capi).name
    jni_type = api.java_jni_type(field.public_type)
    func_sig = api.jni_func_sig(
        field.accessor_basename.lower,
        api.jni_c_type(field.public_type)
    )
    %>

% if not field.accessor_basename.lower in api.excluded_fields:
${func_sig}(
    JNIEnv *env,
    jclass jni_lib,
    % for arg in field.arguments:
    ${api.jni_c_type(arg.public_type)} ${arg.name.lower}_j,
    % endfor
    jobject entity
) {
    // Unwrap the node
    ${entity_type} native_entity = Entity_unwrap(env, entity);

    % if api.field_need_context(field):
    // Get the node context
    ${analysis_unit_type} unit_native = ${nat("node_unit")}(&native_entity);
    ${analysis_context_type} context_native =
        ${nat("unit_context")}(unit_native);
    % endif

    // Unwrap the arguments
    % for arg in field.arguments:
    const ${arg.public_type.c_type(capi).name} ${arg.name.lower}_n =
        ${api.jni_unwrap(arg.public_type, f"{arg.name.lower}_j")};
    % endfor

    // Prepare the result structure
    ${c_type} res = ${api.jni_new_value(field.public_type)};

    // Call the native function
    ${nat(field.accessor_basename.lower)}(
        &native_entity,
        % for arg in field.arguments:
            % if arg.public_type.is_ada_record:
        &${arg.name.lower}_n,
            % else:
        ${arg.name.lower}_n,
            % endif
        % endfor
        &res
    );

    // Return the wrapped value
    return ${api.jni_wrap(field.public_type, "res")};
}
% endif
</%def>
