## vim: filetype=makopython

<%def name="base_decl()">

class _BaseArray:
    """
    Base class for Ada arrays bindings.
    """

    c_element_type: ClassVar[Any]
    """
    Ctype class for array elements.
    """

    items_refcounted = False
    """
    Whether items for this arrays are ref-counted.
    """

    __slots__ = ('c_value', 'length', 'items')

    def __init__(self, c_value):
        self.c_value = c_value

        self.length = c_value.contents.n

        items_addr = _field_address(c_value.contents, 'items')
        items = self.c_element_type.from_address(items_addr)
        self.items = ctypes.pointer(items)

    def __repr__(self):
        return '<{} {}>'.format(type(self).__name__, list(self))

    def clear(self):
        self.c_value = None
        self.length = None
        self.items = None

    def __del__(self):
        self.dec_ref(self.c_value)
        self.clear()

    @classmethod
    def wrap(cls, c_value, from_field_access):
        helper = cls(c_value)

        result = []
        for i in range(helper.length):
            # In ctypes, accessing an array element does not copy it, which
            # means the the array must live at least as long as the accessed
            # element. We cannot guarantee that, so we must copy the element so
            # that it is independent of the array it comes from.
            #
            # The try/except block tries to do a copy if "item" is indeed a
            # buffer to be copied, and will fail if it's a mere integer, which
            # does not need the buffer copy anyway, hence the "pass".
            item = helper.items[i]
            try:
                item = cls.c_element_type.from_buffer_copy(item)
            except TypeError:
                pass
            result.append(helper.wrap_item(item))

        # If this array value comes from a structure field, we must not call
        # its dec_ref primitive, as it is up to the structure's dec_ref
        # primitive to take care of it.
        if from_field_access:
            helper.clear()

        return result

    @classmethod
    def unwrap(cls, value, context=None):
        if not isinstance(value, list):
            _raise_type_error('list', value)

        # Create a holder for the result
        result = cls(cls.create(len(value)))

        # Unwrap all items at once, preserving their holder so that resources
        # are deallocated if there is an error at some point.
        items = [result.unwrap_item(item, context) for item in value]

        # Initialize the resulting array
        for i, (_, item) in enumerate(items):
            result.items[i] = item

        # At this point, we know that this is successful. We don't want
        # holders to dec-ref the content so that the return array takes over
        # the corresponding refcounting shares.
        if cls.items_refcounted:
            for holder, _ in items:
                holder.clear()

        return result

</%def>

<%def name="decl(cls)">

<%
    element_type = cls.element_type
    c_element_type = pyapi.c_type(element_type)
%>

class ${cls.py_converter}(_BaseArray):
    """
    Wrapper class for arrays of ${cls.element_type.name}.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseArray.__slots__
    items_refcounted = ${cls.element_type.is_refcounted}

    @staticmethod
    def wrap_item(item):
        return ${pyapi.wrap_value('item', element_type,
                                  from_field_access=True)}

    @staticmethod
    def unwrap_item(item, context=None):
        c_holder = ${pyapi.unwrap_value('item', element_type, 'context')}
        c_value = ${pyapi.extract_c_value('c_holder', element_type)}
        return (c_holder, c_value)

    ## If this is a string type, override wrapping to return native unicode
    ## instances.
    % if cls.is_string_type:
    @classmethod
    def wrap(cls, c_value, from_field_access):
        # Reinterpret this array of uint32_t values as the equivalent array of
        # characters, then decode it using the appropriate UTF-32 encoding.
        chars = ctypes.cast(ctypes.pointer(c_value.contents.items),
                            ctypes.POINTER(ctypes.c_char))
        return chars[:4 * c_value.contents.n].decode(_text.encoding)

    @classmethod
    def unwrap(cls, value, context=None):
        # If `value` is not a list, assume it's a string, and convert it to the
        # expected list.
        if not isinstance(value, list):
            value = list(_text.cast(value))

        return super(${cls.py_converter}, cls).unwrap(value, context)
    % endif

    c_element_type = ${c_element_type}

    class c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', ${c_element_type} * 1)]

    c_type = ctypes.POINTER(c_struct)

    create = staticmethod(_import_func(
        '${cls.c_create(capi)}', [ctypes.c_int], c_type))
    inc_ref = staticmethod(_import_func(
        '${cls.c_inc_ref(capi)}', [c_type], None))
    dec_ref = staticmethod(_import_func(
        '${cls.c_dec_ref(capi)}', [c_type], None))

</%def>
