## vim: filetype=makopython

<%def name="base_decls()">

class _BaseStruct(object):
    """
    Mixin for Ada struct wrappers.
    """

    def __getitem__(self, key):
      if not isinstance(key, int):
         raise TypeError('Tuples items are indexed by integers, not {}'.format(
            type(key)
         ))

      # Do not expose the "is_null" internal field
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


class _BaseEnvElement(_BaseStruct):
    """
    Specialized mixin for env elements.
    """

    def __getattr__(self, name):
        """
        Evaluate the "name" attribute on the wrapped AST node. This
        automatically passes parents environment rebindings.
        """
        node = self.el
        unbound_public_method = getattr(type(node), name)

        # If there is no private method for this accessor, it means there are
        # no implicit arguments to pass, so just return it.
        try:
            unbound_private_method = getattr(type(node), '_' + name)
        except AttributeError:
            return getattr(node, name)

        def bound_method(*args, **kwargs):
            kwargs[${repr(PropertyDef.env_rebinding_name.lower)}] = \
                self.parents_bindings
            return unbound_private_method(node, *args, **kwargs)

        # If the public method is actually a property, the caller will not
        # expect a callable to be returned: in this case, call it right now.
        return (bound_method()
                if isinstance(unbound_public_method, property) else
                bound_method)

</%def>

<%def name="decl(cls)">

<%
   type_name = cls.name().camel
   ptr_name = '{}_Ptr'.format(type_name)
   dec_ref = '_{}_dec_ref'.format(type_name)

   base_classes = ['ctypes.Structure',
                   '_BaseEnvElement'
                       if cls.is_entity_type else
                       '_BaseStruct']
%>

class ${type_name}(${', '.join(base_classes)}):
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
