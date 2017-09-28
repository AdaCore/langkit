## vim: filetype=makopython

<%def name="accessor_body(field)">

        <%
            c_accessor = '_{}'.format(field.accessor_basename.lower)
            e_info = field.entity_info_name.lower
        %>

        % if field.type.is_ast_node and not field.arguments:
        return self._eval_astnode_field(${c_accessor}, ${e_info})

        % else:
        <%
            # Expression to create a holder for the C result
            c_result_constructor = '{}()'.format(
                pyapi.type_internal_name(field.type),
            )

            # Expression for the C value for field evaluation
            explicit_args = [pyapi.unwrap_value(arg.name.lower, arg.type)
                             for arg in field.arguments]
            eval_args = ([c_result_constructor, c_accessor, e_info]
                         + explicit_args)
            c_result = 'self._eval_field({})'.format(', '.join(eval_args))

            # What comes next is the unwrapping of this C result for the
            # caller.
        %>
        return ${pyapi.wrap_value(c_result, field.type)}
        % endif

</%def>

<%def name="subclass_decls(cls)">
    <%
        # Parent class for "cls", or None if "cls" is actually the root AST
        # node (if we called .base() on it, it would return ASTNodeType).
        parent_cls = cls.base() if T.root_node != cls else None

        # Python expression that yield a tuple that contains the names for all
        # fields that "cls" inherits.
        parent_fields = ('{}._field_names'.format(parent_cls.name.camel)
                         if parent_cls else
                         '()')
    %>

    ## First, emit public properties/methods for field accessors. They are
    ## basically wrappers that call the private method that accepts an entity
    ## information argument.

    % for field in cls.fields_with_accessors():
    <% arg_list = ['self'] + [a.name.lower for a in field.arguments] %>
    % if not field.arguments:
    @property
    % endif
    def ${field.name.lower}(${', '.join(arg_list)}):
        ${py_doc(field, 8)}
        <% args = arg_list[1:] + [T.entity_info.py_nullexpr] %>
        return self._ENT_${field.name.lower}(${', '.join(args)})
    % endfor

    ## Then, emit private methods that accept an entity information argument

    % for field in cls.fields_with_accessors():
    <% arg_list = (['self'] +
                   [a.name.lower for a in field.arguments] +
                   [field.entity_info_name.lower]) %>
    def _ENT_${field.name.lower}(${', '.join(arg_list)}):
        ${accessor_body(field)}
    % endfor

    _field_names = ${parent_fields} + (
        % for field in cls.fields_with_accessors():
        "${field.name.lower}",
        % endfor
    )

    % if not cls.abstract:
    _kind_name = ${repr(cls.name.camel)}
    % endif

    % if cls.is_list_type:
    is_list_type = True
    % endif
</%def>

<%def name="decl(cls)">

class ${cls.name.camel}(${cls.base().name.camel}):
    ${py_doc(cls, 4)}
${subclass_decls(cls)}

</%def>
