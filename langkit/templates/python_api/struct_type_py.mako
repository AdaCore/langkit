## vim: filetype=makopython

class ${cls.name().camel}(ctypes.Structure):
    ${py_doc(cls, 4)}
    _fields_ = [
    % for field in cls.get_fields():
       ('_${field.name.lower}',
        ${pyapi.type_internal_name(field.type)}),
    % endfor
    ]
    % for primitive in primitives:

    @property
    def ${field.name.lower}(self):
        ${py_doc(field, 8)}
        result = self._${field.name.lower}
        return ${pyapi.wrap_value('result', field.type)}
    % endfor
