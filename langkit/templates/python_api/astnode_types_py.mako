## vim: filetype=makopython

<%def name="accessor_body(field)">

        <% c_accessor = '_{}'.format(field.accessor_basename.lower) %>

        % if field.type.is_ast_node and not field.arguments:
        result = self._eval_astnode_field(${c_accessor})

        % else:
        <%
            # Expression to create a holder for the C result
            c_result_constructor = '{}()'.format(
                pyapi.type_internal_name(field.public_type),
            )

            # Expression for the C value for field evaluation
            explicit_args = [
                pyapi.unwrap_value(arg.name.lower, arg.public_type)
                for arg in field.arguments
            ]
            eval_args = [c_result_constructor, c_accessor] + explicit_args
            c_result = 'self._eval_field({})'.format(', '.join(eval_args))

            # What comes next is the unwrapping of this C result for the
            # caller.
        %>
        result = ${pyapi.wrap_value(c_result, field.public_type)}
        % endif

</%def>

<%def name="subclass_decls(cls)">
    <%
        # Parent class for "cls", or None if "cls" is actually the root AST
        # node (if we called .base on it, it would return ASTNodeType).
        parent_cls = cls.base if T.root_node != cls else None

        # Python expression that yield a tuple that contains the names for all
        # fields that "cls" inherits.
        parent_fields = (
            '{}._field_names'.format(parent_cls.kwless_raw_name.camel)
            if parent_cls else '()'
        )
    %>

    % for field in cls.fields_with_accessors():
    <%
        arg_list = ['self'] + [
            (a.name.lower
             if a.default_value is None else
             '{}={}'.format(a.name.lower,
                            a.default_value.render_python_constant()))
            for a in field.arguments
        ]
    %>
    % if not field.arguments:
    @property
    % endif
    def ${field.name.lower}(${', '.join(arg_list)}):
        ${py_doc(field, 8)}

        ## If this field/property takes no argument, use the cache
        <% field_name = repr(field.name.lower) %>
        % if not field.arguments:
        try:
            return self._field_cache[${field_name}]
        except KeyError:
            pass
        % endif

        ${accessor_body(field)}

        ## Corresponding cache store
        % if not field.arguments:
        self._field_cache[${field_name}] = result
        % endif

        return result
    % endfor

    _field_names = ${parent_fields} + (
        % for field in cls.fields_with_accessors():
        "${field.name.lower}",
        % endfor
    )

    % if not cls.abstract:
    _kind_name = ${repr(cls.kwless_raw_name.camel)}
    % endif

    % if cls.is_list_type:
    is_list_type = True
    % endif
</%def>

<%def name="decl(cls)">

class ${cls.kwless_raw_name.camel}(${cls.base.kwless_raw_name.camel}):
    ${py_doc(cls, 4)}
${subclass_decls(cls)}

</%def>
