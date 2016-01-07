## vim: filetype=makopython

<%def name="decl(cls)">

   <% parent_cls = cls.base() %>

class ${cls.name().camel}(${parent_cls.name().camel}):
    ${py_doc(cls, 4)}

    _field_names = ${parent_cls.name().camel}._field_names + (
        % for field in cls.fields_with_accessors():
        "${field.name.lower}",
        % endfor
    )

    % if not cls.abstract:
    _kind_name = ${repr(cls.name().camel)}
    % endif

    % for field in cls.fields_with_accessors():

    @property
    def ${field.name.lower}(self):
        ${py_doc(field, 8)}
        ## Declare a variable of the type
        result = ${pyapi.type_internal_name(field.type)}()

        ## Get it via the C field accessor
        if not _${field.accessor_basename.lower}(self._c_value,
                                                 ctypes.byref(result)):
           raise PropertyError()

        return ${pyapi.wrap_value('result', field.type)}
    % endfor

</%def>
