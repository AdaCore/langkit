## vim: filetype=makopython

<%def name="base_decl()">

_IteratedType = TypeVar("_IteratedType")

class _BaseIterator(Generic[_IteratedType]):
    ${py_doc('langkit.iterator_type')}

    _c_element_type: ClassVar[Any]
    """
    Ctype class for iterator elements.
    """

    __slots__ = ('_c_value',)

    def __init__(self, c_value: Any):
        self._c_value = c_value

    def __repr__(self) -> str:
        return '<{}>'.format(type(self).__name__)

    def _clear(self) -> None:
        self._c_value = None

    def __del__(self) -> None:
        self._dec_ref(self._c_value)
        self._clear()

    @classmethod
    def _wrap(cls, c_value: Any) -> Opt[_BaseIterator]:
        return cls(c_value) if c_value else None

    @classmethod
    def unwrap(cls, value: Opt[_BaseIterator]) -> Any:
        if value is None:
            return None
        elif not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)
        else:
            return value._c_value

    def __iter__(self) -> Iterator[_IteratedType]:
        return self

    def __next__(self) -> _IteratedType:
        ${py_doc('langkit.iterator_next', 6)}
        x = self._c_element_type()
        if self._get_next(self._c_value, ctypes.byref(x)):
            return self._wrap_item(x)
        raise StopIteration

    # For Python2 compatibility
    next = __next__

    # The following methods are just for type hints: subclasses override them

    @staticmethod
    def _get_next(c_value: Any, item_ptr: Any) -> Any:
        pass

    @staticmethod
    def _inc_ref(c_value: Any) -> None:
        pass

    @staticmethod
    def _dec_ref(c_value: Any) -> None:
        pass

    @staticmethod
    def _wrap_item(item: Any) -> _IteratedType:
        pass

</%def>

<%def name="decl(cls)">

<%
    element_type = cls.element_type
    c_element_type = pyapi.c_type(element_type)
%>

class ${cls.api_name.camel}(_BaseIterator[${element_type.mypy_type_hint}]):
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
