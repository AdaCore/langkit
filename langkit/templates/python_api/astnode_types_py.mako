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

    % for field in cls.fields_with_accessors():

    <%
      arg_list = ['self'] + [a.name.lower for a in field.explicit_arguments]
    %>

    % if not field.explicit_arguments:
    @property
    % endif
    def ${field.name.lower}(${', '.join(arg_list)}):
        ${py_doc(field, 8)}
        ## Declare a variable of the type
        result = ${pyapi.type_internal_name(field.type)}(
        % if is_struct(field.type) and not is_ast_node(field.type):
            % for fld in field.type.get_fields():
            ${fld.name.lower}=None,
            % endfor
            _uninitialized=True
        % endif
        )

        ## Get it via the C field accessor. Note that "unwrap_value" already
        ## takes care of type checking so we should keep memory safety.
        if not _${field.accessor_basename.lower}(
            self._c_value,
            % for arg in field.explicit_arguments:
            ${pyapi.unwrap_value(arg.name.lower, arg.type)},
            % endfor
            ctypes.byref(result)
        ):
            exc = _get_last_exception()
            if exc:
                raise PropertyError(*exc.contents._wrap().args)
            else:
                raise PropertyError()

        return ${pyapi.wrap_value('result', field.type)}
    % endfor
</%def>

<%def name="decl(cls)">

class ${cls.name().camel}(${cls.base().name().camel}):
    ${py_doc(cls, 4)}
${subclass_decls(cls)}

</%def>
