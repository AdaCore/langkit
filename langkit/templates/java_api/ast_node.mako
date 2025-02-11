## vim: ft=makojava

<%def name="wrapping_class(cls)">
    <%
    from langkit.java_api import format_name
    api = java_api
    nat = c_api.get_name

    java_type = api.wrapping_type(cls)

    base_type = api.wrapping_type(cls.base)
    root_node_type = api.wrapping_type(T.root_node)
    implements = api.make_implements(cls.implements(include_parents=False))
    %>

    ${java_doc(cls, 4)}
    public static ${"abstract" if cls.abstract else ""}
    class ${java_type} extends ${base_type} ${implements}{

        // ----- Static -----

        ${static_decl(cls)}

        // ----- Attributes -----

        /** Singleton that represents the none node. */
        public static final ${java_type} NONE =
        % if cls.abstract:
            new ${java_type}None();
        % else:
            new ${java_type}(
                Entity.NONE
            );
        % endif

        // ----- Constructors -----

        protected ${java_type}(
            final Entity entity
        ) {
            super(entity);
        }

        public static ${java_type} fromEntity(
            final Entity entity
        ) {
            return entity.node.isNull() ?
                ${java_type}.NONE :

                % if cls.abstract:
                (${java_type}) ${root_node_type}.dispatchNodeCreation(entity);
                % else:
                new ${java_type}(entity);
                % endif
        }

        // ----- Instance methods -----

        @Override
        public Reflection.Node getDescription() {
            return ${java_type}.description;
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

        // ----- Inner classes -----

        % if cls.abstract:
        /**
         * This class represents the none value of the abstract node
         * ${java_type}.
         */
        private static final class ${java_type}None extends ${java_type} {
            ${java_type}None() {super(Entity.NONE);}
        }
        % endif

    }

</%def>

<%def name="static_decl(cls)">
    <%
    api = java_api

    java_type = api.wrapping_type(cls)
    base_type = api.wrapping_type(cls.base) if cls.base else None

    field_names = api.get_node_formatted_fields(cls)
    kind = (f"NodeKind.{cls.kwless_raw_name.upper}"
            if not cls.abstract else
            "null")
    %>

        /** Full description of the node (kind, fields, class...) */
        public static final Reflection.Node description =
            new Reflection.Node(
                ${kind},
                ${"true" if cls.is_token_node else "false"},
                ${"true" if cls.is_list_type else "false"},
                ${java_type}.class,
                "${java_type}",
                new String[] {
                    ${",".join([f'"{name}"' for name in field_names])}
                },
                new HashMap<>(
                    ${f"{base_type}.description.fieldDescriptions" \
                      if base_type else \
                      ""}
                )
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
                member_ref_name = (
                    f"{capi.symbol_prefix.upper()}_"
                    f"{generic_api.member_name(field).upper()}"
                )
                %>
                % if not method.name in api.excluded_fields:
                {
                    // Get the Java method of the field
                    Method method = ${java_type}.class.getMethod(
                        "${method.name}",
                        new Class[]{${",".join(param_classes)}}
                    );

                    // Create the parameter list
                    List<Reflection.Param> parameters = new ArrayList<>();
                    % for param in method.params:
                        % if param.default_value_expr is None:
                    parameters.add(new Reflection.Param(
                        ${api.wrapping_type(param.public_type)}.class,
                        "${param.name}"
                    ));
                        % else:
                    parameters.add(new Reflection.Param(
                        ${api.wrapping_type(param.public_type)}.class,
                        "${param.name}",
                        ${param.default_value_expr}
                    ));
                        % endif
                    % endfor

                    // Add the method and the parameters in maps
                    description.fieldDescriptions.put(
                        "${method.native_name}",
                        new Reflection.Field(
                            method,
                            parameters,
                            MemberReference.${member_ref_name}
                        )
                    );
                }
                % endif
                % endfor
            } catch (Exception e) {
                // This catch block handles exceptions from the Java reflection
                // API. Since calls to this API are generated, those exceptions
                // cannot be raised unless the Java bindings are erroneous.
                System.err.println(
                    "ERROR DURING ${ctx.lib_name.upper} STATIC INITIALISATION"
                );
                e.printStackTrace();
                System.exit(1);
            }
            % endif
        }
</%def>

<%def name="field_accessor(field)">
    <%
    from langkit.java_api import format_name

    api = java_api
    nat = c_api.get_name

    method = api.get_java_method(field)
    native_function = nat(field.accessor_basename.lower)

    return_type = api.wrapping_type(method.public_type)
    return_unw_type = api.wrapping_type(method.public_type, ast_wrapping=False)
    return_ni_ref_type = api.ni_reference_type(method.public_type)

    need_unit = api.field_needs_context(field) or api.field_needs_unit(field)
    need_context = api.field_needs_context(field)

    wrap_release = []
    unwrap_release = []

    implements = field.implements
    %>
        % if not method.name in api.excluded_fields:

        % if implements is not None:
        public ${api.support_type_name(implements.return_type, True)}
        ${format_name((Name("G") + implements.name).lower)} (
            ${api.create_support_prototype_args(implements.args, prefix=True)}
        ) {
            return ${method.name}(
                ${api.cast_arguments_from_interface(method.params)}
            );
        }
        % endif

        ${java_doc(field, 8)}
        public ${return_type} ${method.name}(
            ${','.join([
                f"final {api.wrapping_type(param.public_type)} {param.name}"
                for param in method.params
            ])}
        ) {

            // Verify that arguments are not null
            % for param in method.params:
                % if not api.is_java_primitive(param.public_type):
            if(${param.name} == null) throw new IllegalArgumentException(
                "Argument '${param.name}' of type " +
                "${api.wrapping_type(param.public_type)} cannot be null"
            );
                % endif
            % endfor

            if(ImageInfo.inImageCode()) {
                // Unwrap the current node
                final EntityNative thisNative = StackValue.get(
                    EntityNative.class
                );
                this.entity.unwrap(thisNative);

                % if need_unit:
                // Get the node unit
                final AnalysisUnit currentUnit = this.getUnit();
                % endif

                % if need_context:
                // Get the node context
                final AnalysisContext currentContext =
                    currentUnit.getContext();
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
                final ${return_ni_ref_type} resNative =
                    ${api.ni_stack_value(field.public_type)};

                // Call the native function
                NI_LIB.${native_function}(
                    thisNative,
                    % for param in method.params:
                    ${f"{param.name}Native"},
                    % endfor
                    resNative
                );

                // Get the potential exception of the native property call
                final LangkitException propException = getLastException();

                // Wrap the result if the property returned normally
                ${return_type} res = ${api.none_value(method.public_type)};
                if(propException == null) {
                    res = ${api.ni_wrap(
                        field.public_type,
                        "resNative",
                        wrap_release
                    )};

                    % for to_release in wrap_release:
                    ${api.wrapper_class(to_release.public_type)}.release(
                        ${to_release.name}
                    );
                    % endfor
                }

                // Release the unwrapped parameters
                % for to_release in unwrap_release:
                ${api.wrapper_class(to_release.public_type)}.release(
                    ${to_release.name}
                );
                % endfor

                % if api.field_needs_context(field):
                // Close the context
                currentContext.close();
                % endif

                // If the property exception is not null throw it
                if(propException != null) {
                    throw propException;
                }

                // Return the result
                return res;
            } else {
                // Call the native function
                final ${return_unw_type} res = JNI_LIB.${native_function}(
                    % for param in method.params:
                    ${api.java_jni_unwrap(param.public_type, param.name)},
                    % endfor
                    this.entity
                );

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
                    f"{api.ni_type(arg.public_type, ast_wrapping=False)} "
                    f"{arg.name.lower}"
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
            jni_type = api.wrapping_type(field.public_type, ast_wrapping=False)
            %>

        % if not field.accessor_basename.lower in api.excluded_fields:
        /** Isomethod of ${native_function} langkit function */
        @CompilerDirectives.TruffleBoundary
        public static native ${jni_type} ${native_function}(
            % for arg in field.arguments:
            ${api.wrapping_type(arg.public_type, ast_wrapping=False)}
            ${arg.name.lower},
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

    args_release_list = []
    return_release_list = []
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

    % if api.field_needs_context(field) or api.field_needs_unit(field):
    // Get the node unit
    ${analysis_unit_type} unit_native = ${nat("node_unit")}(&entity_native);
    % endif

    % if api.field_needs_context(field):
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
        args_release_list,
        ast_wrapping=False
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

    /* If successful, wrap the return value.  */
    const ${exception_type} *exc_c = ${nat("get_last_exception")} ();
    ${return_type} res = ${api.jni_c_none(field.public_type)};
    if (exc_c == NULL)
      {
        res = ${api.jni_wrap(
            field.public_type,
            "res_native",
            return_release_list,
            ast_wrapping=False
        )};

        /* Release resources used to wrap the result.  */
        % for to_release in return_release_list:
        ${api.wrapper_class(
            to_release.public_type,
            ast_wrapping=False
        )}_release(
            ${to_release.name}
        );
        % endfor
      }

    /* Release resources used to unwrap the arguments.  */
    % for to_release in args_release_list:
    ${api.wrapper_class(
        to_release.public_type,
        ast_wrapping=False
    )}_release(
        ${to_release.name}
    );
    % endfor

    /* Raise the exception if any, return the result otherwise.  */
    if (exc_c != NULL)
      {
        jthrowable exc_java = LangkitException_wrap (env, *exc_c);
        (*env)->Throw (env, exc_java);
      }
    return res;
}
% endif
</%def>
