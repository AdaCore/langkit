## vim: filetype=makopython

<%def name="decl(cls)">

class ${cls.name().camel}(ctypes.Structure):
    ${py_doc(cls, 4)}
    _fields_ = [
    % for field in cls.get_fields():
       ('_${field.name.lower}',
        ${pyapi.type_internal_name(field.type)}),
    % endfor
    ]
    % for field in cls.get_fields():

    @property
    def ${field.name.lower}(self):
        ${py_doc(field, 8)}
        result = self._${field.name.lower}
        return ${pyapi.wrap_value('result', field.type)}
    % endfor

    def __getitem__(self, key):
      if not isinstance(key, int):
         raise TypeError('Tuples items are indexed by integers, not {}'.format(
            type(key)
         ))

      if 0 <= key < len(self._fields_):
         field_name = self._fields_[key][0]
         return getattr(self, field_name[1:])
      else:
         raise IndexError('There is no {}th field'.format(key))

</%def>
