## vim: filetype=makopython


class ${cls.name().camel}(${parent_cls.name().camel}):
    ${py_doc(cls, 4)}

    _field_names = ${parent_cls.name().camel}._field_names + (
        % for primitive in primitives:
        "${primitive.field.name.lower}",
        % endfor
    )

    % if not cls.abstract:
    _kind_name = ${repr(cls.name().camel)}
    % endif
    % for primitive in primitives:

    @property
    def ${primitive.field.name.lower}(self):
        ${py_doc(primitive.field, 8)}
        result = ${primitive.field.type.py_type(pyapi).name_low}()
        assert _${primitive.name.lower}(self._c_value, ctypes.byref(result))
        return ${pyapi.wrap_value('result', primitive.field.type)}
    % endfor
