## vim: filetype=makopython

<%def name="decl(cls)">

<%
   type_name = cls.name().camel
   ptr_name = '{}_Ptr'.format(type_name)
   dec_ref = '_{}_dec_ref'.format(type_name)
%>

class ${type_name}(ctypes.Structure):
    ${py_doc(cls, 4)}
    _fields_ = [
    % for field in cls.get_fields():
        ('_${field.name.lower}',
         ${pyapi.type_internal_name(field.type)}),
    % endfor
        ('is_null', ctypes.c_uint8),
    ]

    def copy(self):
        """
        Return a copy of this structure.
        """
        return ${type_name}(
            % for field in cls.get_fields():
                self._${field.name.lower},
            % endfor
        )

    % for field in cls.get_fields():

    @property
    def ${field.name.lower}(self):
        ${py_doc(field, 8)}
        result = self._${field.name.lower}
        return ${pyapi.wrap_value('result', field.type,
                                  from_field_access=True)}
    % endfor

    def __getitem__(self, key):
      if not isinstance(key, int):
         raise TypeError('Tuples items are indexed by integers, not {}'.format(
            type(key)
         ))
      ## Do not expose the "is_null" internal field
      fields = self._fields_[:-1]
      if 0 <= key < len(fields):
         field_name = fields[key][0]
         return getattr(self, field_name[1:])
      else:
         raise IndexError('There is no {}th field'.format(key))

    def __repr__(self):
        field_names = [name[1:] for name, _ in self._fields_[:-1]]
        return '<{} {}>'.format(
            type(self).__name__,
            ' '.join('{}={}'.format(name, getattr(self, name))
                      for name in field_names)
        )

    % if cls.is_refcounted():
    def __del__(self):
        ${dec_ref}(ctypes.byref(self))
    % endif

% if cls.is_refcounted():
${ptr_name} = ctypes.POINTER(${type_name})

${dec_ref} = _import_func(
   '${cls.c_dec_ref(capi)}',
   [${ptr_name}], None
)
% endif

</%def>
