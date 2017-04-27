## vim: filetype=makopython

<%def name="accessor_body(field, with_implicit_args)">

        <% c_accessor = '_{}'.format(field.accessor_basename.lower) %>

        % if field.type.is_ast_node and not field.exposed_arguments:
        return self._eval_astnode_field(${c_accessor})

        % else:
        <%
            # Expression to create a holder for the C result
            c_result_constructor = '{}()'.format(
                pyapi.type_internal_name(field.type),
            )

            # Expression for the C value for field evaluation
            def unwrap_arg(arg_type, arg_expr):
               return pyapi.unwrap_value(arg_expr, arg_type)

            explicit_args = [
                unwrap_arg(arg.type, arg.name.lower)
                for arg in field.explicit_arguments
            ]
            implicit_args = [
                # If we must use implicit args, unwrap them from the "kwargs"
                # local variable, otherwise just use default values for them.
                unwrap_arg(
                    arg.type,
                    'kwargs.get({}, {})'.format(
                        repr(arg.name.lower),
                        arg.type.py_nullexpr()
                    )
                 ) if with_implicit_args else unwrap_arg(
                    arg.type, arg.type.py_nullexpr()
                )
                for arg in field.exposed_implicit_arguments
            ]
            eval_args = ([c_result_constructor, c_accessor]
                         + explicit_args + implicit_args)
            c_result = 'self._eval_field({})'.format(', '.join(eval_args))

            # What comes next is the unwrapping of this C result for the
            # caller.
        %>
        return ${pyapi.wrap_value(c_result, field.type)}
        % endif

</%def>

<%def name="subclass_decls(cls)">
    <%
        # Parent class for "cls", or None if "cls" is actually the root AST node
        # (if we called .base() on it, it would return ASTNode).
        parent_cls = cls.base() if T.root_node != cls else None

        # Python expression that yield a tuple that contains the names for all
        # fields that "cls" inherits.
        parent_fields = ('{}._field_names'.format(parent_cls.name().camel)
                         if parent_cls else
                         '()')
    %>

    ## First, emit public properties/methods for field accessors. Accessors
    ## with no implicit argument will implement C calls themselves, but those
    ## with some will just delegate to the private methods below.

    % for field in cls.fields_with_accessors():
    <% arg_list = ['self'] + [a.name.lower
                              for a in field.explicit_arguments] %>
    % if not field.explicit_arguments:
    @property
    % endif
    def ${field.name.lower}(${', '.join(arg_list)}):
        ${py_doc(field, 8)}
        % if field.exposed_implicit_arguments:
        return self._${field.name.lower}(${', '.join(arg_list[1:])})
        % else:
        ${accessor_body(field, False)}
        % endif
    % endfor

    ## Then, for properties with implicit arguments, emit private methods

    % for field in cls.fields_with_accessors():
        % if field.exposed_implicit_arguments:
    <% arg_list = ['self'] + [a.name.lower
                              for a in field.explicit_arguments] %>
    def _${field.name.lower}(${', '.join(arg_list)}, **kwargs):
        ${accessor_body(field, True)}
        % endif
    % endfor

    _field_names = ${parent_fields} + (
        % for field in cls.fields_with_accessors():
        "${field.name.lower}",
        % endfor
    )

    % if not cls.abstract:
    _kind_name = ${repr(cls.name().camel)}
    % endif

    % if cls.is_list_type:
    is_list_type = True
    % endif
</%def>

<%def name="decl(cls)">

class ${cls.name().camel}(${cls.base().name().camel}):
    ${py_doc(cls, 4)}
${subclass_decls(cls)}

</%def>
