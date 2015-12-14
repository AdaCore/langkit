## vim: filetype=makopython

class ${cls.name().camel}(ctypes.Structure):
    ${py_doc(cls, 4)}
    _fields_ = [
    % for primitive in primitives:
       ('_${primitive.field.name.lower}',
        ${pyapi.type_internal_name(primitive.field.type)}),
    % endfor
    ]
    % for primitive in primitives:

    @property
    def ${primitive.field.name.lower}(self):
        ${py_doc(primitive.field, 8)}
        result = self._${primitive.field.name.lower}
        return ${pyapi.wrap_value('result', primitive.field.type)}
    % endfor
