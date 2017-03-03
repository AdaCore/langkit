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
         ## At this point in the binding, no array type has been emitted yet,
         ## so use a generic pointer: we will do the conversion later for
         ## users.
         % if is_array_type(field.type):
             ctypes.c_void_p
         % else:
            ${pyapi.type_internal_name(field.type)}
         % endif
         ),
    % endfor
        ('is_null', ctypes.c_uint8),
    ]

    def __init__(self,
        % for field in cls.get_fields():
        ${field.name.lower},
        % endfor
        _uninitialized=False
    ):
        if _uninitialized:
            super(${type_name}, self).__init__()
            return

        super(${type_name}, self).__init__(
        % for field in cls.get_fields():
            _${field.name.lower}=${
                pyapi.unwrap_value(field.name.lower, field.type)
            },
        % endfor
        )

    def copy(self):
        """
        Return a copy of this structure.
        """
        return ${type_name}(
            % for field in cls.get_fields():
            <%
                fld = 'self._{}'.format(field.name.lower)
                copy = pyapi.wrap_value(fld, field.type,
                                        from_field_access=True,
                                        inc_ref=True)
            %>${copy},
            % endfor
        )

    % for field in cls.get_fields():

    @property
    def ${field.name.lower}(self):
        ${py_doc(field, 8)}
        result = self._${field.name.lower}
        % if is_array_type(field.type):
        result = ctypes.cast(result, ${pyapi.type_internal_name(field.type)})
        % endif
        ## "self" has an ownership share for this field, but we have to create
        ## a new one for the caller, so both can live (and die) independently.
        return ${pyapi.wrap_value('result', field.type,
                                  from_field_access=True,
                                  inc_ref=True)}
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

</%def>


<%def name="low_level_decl(cls)">

<%
   type_name = cls.name().camel
   ptr_name = '{}_Ptr'.format(type_name)
   dec_ref = '_{}_dec_ref'.format(type_name)
%>

% if cls.is_refcounted():
${ptr_name} = ctypes.POINTER(${type_name})

${dec_ref} = _import_func(
   '${cls.c_dec_ref(capi)}',
   [${ptr_name}], None
)
% endif

</%def>
