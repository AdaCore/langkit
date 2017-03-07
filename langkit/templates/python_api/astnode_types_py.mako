## vim: filetype=makopython

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

    % for field in cls.fields_with_accessors():

    <%
      arg_list = ['self'] + [a.name.lower for a in field.explicit_arguments]
    %>

    % if not field.explicit_arguments:
    @property
    % endif
    def ${field.name.lower}(${', '.join(arg_list)}):
        ${py_doc(field, 8)}
        <% c_accessor = '_{}'.format(field.accessor_basename.lower) %>

        % if is_ast_node(field.type) and not field.explicit_arguments:
        return self._eval_astnode_field(${c_accessor})

        % else:
        <%
            # Expression to create a holder for the C result
            c_result_args = (
                (['{}=None'.format(fld.name.lower)
                  for fld in field.type.get_fields()]
                 + ['_uninitialized=True'])
                if is_struct(field.type) and not is_ast_node(field.type) else
                []
            )
            c_result_constructor = '{}({})'.format(
                pyapi.type_internal_name(field.type),
                ', '.join(c_result_args)
            )

            # Expression for the C value for field evaluation
            eval_args = [c_result_constructor, c_accessor] + [
                pyapi.unwrap_value(arg.name.lower, arg.type)
                for arg in field.explicit_arguments
            ]
            c_result = 'self._eval_field({})'.format(', '.join(eval_args))

            # What comes next is the unwrapping of this C result for the
            # caller.
        %>
        return ${pyapi.wrap_value(c_result, field.type)}
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
