## vim: filetype=makopython

<%def name="decl(cls)">

<%
   type_name = cls.name().camel
   struct_name = '{}_Struct'.format(pyapi.type_internal_name(cls))
   element_type = pyapi.type_internal_name(cls.element_type())
   ptr_name = pyapi.type_internal_name(cls)
   dec_ref = '_{}_dec_ref'.format(type_name)
%>

${py_doc(cls)}


class ${struct_name}(ctypes.Structure):
    _fields_ = [('n', ctypes.c_int),
                ('ref_count', ctypes.c_int),
                ('items', ${element_type} * 1)]


${ptr_name} = ctypes.POINTER(${struct_name})

${dec_ref} = _import_func(
   '${cls.c_dec_ref(capi)}',
   [${ptr_name}], None
)


class ${type_name}(object):
    """
    Wrapper class for arrays of ${cls.element_type().name()}.
    """

    def __init__(self, c_value):
        self._c_value = c_value
        self._length = c_value.contents.n

        items_addr = _field_address(c_value.contents, 'items')
        items = ${element_type}.from_address(items_addr)
        self._items = ctypes.pointer(items)

    def __repr__(self):
        return '<${type_name} object at {} {}>'.format(
            hex(id(self)),
            list(self)
        )

    def __del__(self):
        ${dec_ref}(self._c_value)
        self._c_value = None
        self._length = None
        self._items = None

    def __len__(self):
        return self._length

    def __getitem__(self, key):
        if not isinstance(key, int):
            raise TypeError('array indices must be integers, not {}'.format(
                type(key)
            ))
        elif not (0 <= key < self._length):
            raise IndexError()

        item = self._items[key]
        ## In the case of array of Structure instances, array[index] returns a
        ## reference to the record. Thus, in order to keep memory safety, we
        ## must copy the record itself so that the array can be deallocated
        ## while the user still has a reference to a record.
        <% elt_type = cls.element_type() %>
        % if is_struct_type(elt_type) and not is_ast_node(elt_type):
        return item.copy()
        % else:
        return ${pyapi.wrap_value('item', elt_type)}
        % endif

</%def>
