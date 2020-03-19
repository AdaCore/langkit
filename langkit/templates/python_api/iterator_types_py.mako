## vim: filetype=makopython

<%def name="base_decl()">

class _BaseIterator(object):
    ${py_doc('langkit.iterator_type')}

    _c_element_type = None
    """
    Ctype class for iterator elements.
    """

    __slots__ = ('_c_value',)

    def __init__(self, c_value):
        self._c_value = c_value

    def __repr__(self):
        return '<{}>'.format(type(self).__name__)

    def _clear(self):
        self._c_value = None

    def __del__(self):
        self._dec_ref(self._c_value)
        self._clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(c_value)

    def __iter__(self):
        return self

    def __next__(self):
        ${py_doc('langkit.iterator_next')}
        x = self._c_element_type()
        if self._get_next(self._c_value, ctypes.byref(x)):
            return self._wrap_item(x)
        raise StopIteration

    # For Python2 compatibility
    next = __next__

</%def>

<%def name="decl(cls)">

<%
    element_type = cls.element_type
    c_element_type = pyapi.c_type(element_type)
%>

class ${cls.py_converter}(_BaseIterator):
    """
    Iterator over ${cls.element_type.name}.

    This class is not meant to be directly instantiated: it is only used for
    for the return values of properties returning iterators.
    """

    __slots__ = _BaseIterator.__slots__

    @staticmethod
    def _wrap_item(item):
        return ${pyapi.wrap_value('item', element_type,
                                  from_field_access=False)}

    _c_element_type = ${c_element_type}
    _c_element_type_ptr = ctypes.POINTER(_c_element_type)

    _c_type = ctypes.c_void_p

    _get_next = staticmethod(_import_func(
        '${cls.c_next(capi)}',
        [_c_type, _c_element_type_ptr],
        ctypes.c_int))
    _inc_ref = staticmethod(_import_func(
        '${cls.c_inc_ref(capi)}', [_c_type], None))
    _dec_ref = staticmethod(_import_func(
        '${cls.c_dec_ref(capi)}', [_c_type], None))

</%def>
