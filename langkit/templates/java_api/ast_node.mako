<%def name="wrapping_class(cls)">
    <%
    api = java_api
    nat = c_api.get_name

    java_type = api.wrapping_type(cls)

    base_type = api.wrapping_type(cls.base)
    %>

    ${java_doc(cls, 4)}
    public static ${"abstract" if cls.abstract else ""}
    class ${java_type} extends ${base_type} {

        // ----- Static -----

        ${static_decl(cls)}

        // ----- Constructors -----

        protected ${java_type}(
            Entity entity
        ) {
            super(entity);
        }

        public static ${java_type} fromEntity(
            Entity entity
        ) {
            return (${java_type}) ${base_type}.fromEntity(entity);
        }

        // ----- Instance methods -----

        @Override
        public String getKindName() {
            return ${java_type}.kindName;
        }

        @Override
        public String[] getFieldNames() {
            return ${java_type}.fieldNames;
        }

        @Override
        public boolean isListType() {
            return ${java_type}.isListType;
        }

        @Override
        @CompilerDirectives.TruffleBoundary
        public ${ctx.lib_name.camel}Field getFieldDescription(String name) {
            return ${java_type}.fieldDescriptions.getOrDefault(name, null);
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

    java_type = api.wrapping_type(cls)
    base_type = api.wrapping_type(cls.base) if cls.base else None

    field_names = api.get_node_formatted_fields(cls)
    %>

        /** The name of the node kind */
        public static final String kindName = "${java_type}";

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
            ${f"{base_type}.fieldDescriptions" if base_type else ""}
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
                    Method method = ${java_type}.class.getMethod(
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
    return_unw_type = api.wrapping_type(method.public_type, False)
    return_ni_ref_type = api.ni_reference_type(method.public_type)

    need_unit = api.field_need_context(field) or api.field_need_unit(field)
    need_context = api.field_need_context(field)

    wrap_release = []
    unwrap_release = []
    %>

        % if not method.name in api.excluded_fields:
        ${java_doc(field, 8)}
        public ${return_type} ${method.name}(
            ${','.join([
                f"{api.wrapping_type(param.public_type)} {param.name}"
                for param in method.params
            ])}
        ) {

            // Verify that arguments are not null
            % for param in method.params:
                % if api.is_java_nullable(param.public_type):
            if(${param.name} == null) throw new NullPointerException(
                "Argument ${param.name} cannot be 'null'"
            );
                % endif
            % endfor

            if(ImageInfo.inImageCode()) {
                // Unwrap the current node
                EntityNative thisNative = StackValue.get(EntityNative.class);
                this.entity.unwrap(thisNative);

                % if need_unit:
                // Get the node unit
                AnalysisUnit currentUnit = this.getUnit();
                % endif

                % if need_context:
                // Get the node context
                AnalysisContext currentContext = currentUnit.getContext();
                % endif

                // Unwrap the arguments
                % for param in method.params:
                ${api.ni_unwrap(
                    param.public_type,
                    param.name,
                    f"{param.name}Native",
                    unwrap_release
                )}
                % endfor

                // Create the result native
                ${return_ni_ref_type} resNative =
                    ${api.ni_stack_value(field.public_type)};

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

                // Wrap the result
                ${return_type} res = ${api.ni_wrap(
                    field.public_type,
                    "resNative",
                    wrap_release
                )};

                // Release the allocated ressources
                % for to_release in unwrap_release:
                ${api.wrapper_class(to_release.public_type)}.release(
                    ${to_release.name}
                );
                % endfor
                % for to_release in wrap_release:
                ${api.wrapper_class(to_release.public_type)}.release(
                    ${to_release.name}
                );
                % endfor

                // Return the result
                return res;
            } else {
                // Call the native function
                ${return_unw_type} res = JNI_LIB.${native_function}(
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
                    f"{api.ni_type(arg.public_type)} {arg.name.lower}"
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
            jni_type = api.wrapping_type(field.public_type, False)
            %>

        % if not field.accessor_basename.lower in api.excluded_fields:
        /** Isomethod of ${native_function} langkit function */
        @CompilerDirectives.TruffleBoundary
        public static native ${jni_type} ${native_function}(
            % for arg in field.arguments:
            ${api.wrapping_type(arg.public_type, False)} ${arg.name.lower},
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
    return_type = api.jni_c_type(field.public_type)

    func_sig = api.jni_func_sig(
        field.accessor_basename.lower,
        api.jni_c_type(field.public_type)
    )

    release_list = []
    %>

% if not field.accessor_basename.lower in api.excluded_fields:
${func_sig}(
    JNIEnv *env,
    jclass jni_lib,
    % for arg in field.arguments:
    ${api.jni_c_type(arg.public_type)} ${arg.name.lower}_java,
    % endfor
    jobject entity
) {
    // Unwrap the node
    ${entity_type} entity_native = Entity_unwrap(env, entity);

    % if api.field_need_context(field) or api.field_need_unit(field):
    // Get the node unit
    ${analysis_unit_type} unit_native = ${nat("node_unit")}(&entity_native);
    % endif

    % if api.field_need_context(field):
    // Get the node context
    ${analysis_context_type} context_native =
        ${nat("unit_context")}(unit_native);
    % endif

    // Unwrap the arguments
    % for arg in field.arguments:
    ${api.jni_unwrap(
        arg.public_type,
        f"{arg.name.lower}_java",
        f"{arg.name.lower}_native",
        release_list
    )}
    % endfor

    // Prepare the result structure
    ${c_type} res_native = ${api.jni_new_value(field.public_type)};

    // Call the native function
    ${nat(field.accessor_basename.lower)}(
        &entity_native,
        % for arg in field.arguments:
            % if arg.public_type.is_ada_record:
        &${arg.name.lower}_native,
            % else:
        ${arg.name.lower}_native,
            % endif
        % endfor
        &res_native
    );

    // Return the wrapped value
    ${return_type} res = ${api.jni_wrap(
        field.public_type,
        "res_native",
        release_list
    )};

    // Release the memory
    % for to_release in release_list:
    ${api.wrapping_type(to_release.public_type, False)}_release(
        ${to_release.name}
    );
    % endfor

    // Return the result
    return res;
}
% endif
</%def>
