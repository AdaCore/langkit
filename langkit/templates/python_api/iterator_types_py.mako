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
        return cls(c_value) if c_value else None

    @classmethod
    def unwrap(cls, value):
        if value is None:
            return None
        elif not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)
        else:
            return value._c_value

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

class ${cls.api_name.camel}(_BaseIterator):
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

<%def name="mypy_decl(cls)">

class ${pyapi.type_public_name(cls)}(object):
    ${py_doc(cls, 4)}

    def __iter__(self) -> Iterator[${cls.element_type.mypy_type_hint}]: ...
    def __next__(self) -> ${cls.element_type.mypy_type_hint}: ...

    % for f in cls.get_fields():
    @property
    def ${f.name.lower}(self) -> ${f.type.mypy_type_hint}:
        ${py_doc(f, 8, or_pass=True)}
    % endfor

</%def>
