## vim: filetype=makopython

<%def name="base_decl()">

class _BaseArray(object):
    """
    Base class for Ada arrays bindings.
    """

    _c_element_type = None
    """
    Ctype class for array elements.
    """

    __slots__ = ('c_value', 'length', 'items')

    def __init__(self, c_value):
        self.c_value = c_value

        self.length = c_value.contents.n

        items_addr = _field_address(c_value.contents, 'items')
        items = self._c_element_type.from_address(items_addr)
        self.items = ctypes.pointer(items)

    def __repr__(self):
        return '<{} {}>'.format(type(self).__name__, list(self))

    def __del__(self):
        self._dec_ref(self.c_value)
        self.c_value = None
        self.length = None
        self.items = None

    @classmethod
    def _wrap(cls, c_value):
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
                item = cls._c_element_type.from_buffer_copy(item)
            except TypeError:
                pass
            result.append(helper._wrap_item(item))

        return result

    @classmethod
    def _unwrap(cls, value):
        if not isinstance(value, list):
            _raise_type_error('list', value)

        result = cls(cls._create(len(value)))
        for i, item in enumerate(value):
            result.items[i] = result._unwrap_item(item)
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
    expose values that various methods return.
    """

    __slots__ = _BaseArray.__slots__

    @staticmethod
    def _wrap_item(item):
        return ${pyapi.wrap_value('item', element_type,
                                  from_field_access=True)}

    @staticmethod
    def _unwrap_item(item):
        return ${pyapi.unwrap_value('item', element_type)}

    ## If this is a string type, override wrapping to return native unicode
    ## instances.
    % if cls.is_string_type:
    @classmethod
    def _wrap(cls, c_value):
        # Reinterpret this array of uint32_t values as the equivalent array of
        # characters, then decode it using the appropriate UTF-32 encoding.
        chars = ctypes.cast(ctypes.pointer(c_value.contents.items),
                            ctypes.POINTER(ctypes.c_char))
        return chars[:4 * c_value.contents.n].decode(_text.encoding)
    % endif

    _c_element_type = ${c_element_type}

    class _c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', ${c_element_type} * 1)]

    _c_type = ctypes.POINTER(_c_struct)

    _create = staticmethod(_import_func('${cls.c_create(capi)}',
                                        [ctypes.c_int], _c_type))
    _inc_ref = staticmethod(_import_func('${cls.c_inc_ref(capi)}',
                            [_c_type], None))
    _dec_ref = staticmethod(_import_func('${cls.c_dec_ref(capi)}',
                            [_c_type], None))

</%def>
