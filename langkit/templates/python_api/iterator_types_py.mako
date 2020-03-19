## vim: filetype=makopython

<%def name="base_decl()">

class _BaseIterator(object):
    """
    Base class for Ada iterator bindings.
    """

    c_element_type = None
    """
    Ctype class for iterator elements.
    """

    __slots__ = ('c_value',)

    def __init__(self, c_value):
        self.c_value = c_value

    def __repr__(self):
        return '<{}>'.format(type(self).__name__)

    def clear(self):
        self.c_value = None

    def __del__(self):
        self.dec_ref(self.c_value)
        self.clear()

    @classmethod
    def wrap(cls, c_value):
        res = cls(c_value)
        # inc_ref?
        return res

</%def>

<%def name="decl(cls)">

<%
    element_type = cls.element_type
    c_element_type = pyapi.c_type(element_type)
%>

class ${cls.py_converter}(_BaseIterator):
    """
    Wrapper class for iterators of ${cls.element_type.name}.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseIterator.__slots__

    @staticmethod
    def wrap_item(item):
        return ${pyapi.wrap_value('item', element_type,
                                  from_field_access=False)}

    def __iter__(self):
        return self

    def __next__(self):
        x = self.c_element_type()
        if self.get_next(self.c_value, ctypes.pointer(x)):
            return self.wrap_item(x)
        raise StopIteration

    # For Python2 compatibility
    next = __next__


    c_element_type = ${c_element_type}
    c_element_type_ptr = ctypes.POINTER(c_element_type)

    c_type = ctypes.c_void_p

    get_next = staticmethod(_import_func(
        '${cls.c_next(capi)}',
        [c_type, c_element_type_ptr],
        ctypes.c_int))
    inc_ref = staticmethod(_import_func(
        '${cls.c_inc_ref(capi)}', [c_type], None))
    dec_ref = staticmethod(_import_func(
        '${cls.c_dec_ref(capi)}', [c_type], None))

</%def>
