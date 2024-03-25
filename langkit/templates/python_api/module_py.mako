## vim: filetype=makopython

"""
Python binding of the ${ctx.lib_name.camel} API.

Please consider all exported entities whose names that start with an underscore
("_") as internal implementation details. They are not meant to be used
directly.
"""

<%namespace name="array_types"    file="array_types_py.mako" />
<%namespace name="iterator_types" file="iterator_types_py.mako" />
<%namespace name="astnode_types"  file="astnode_types_py.mako" />
<%namespace name="struct_types"   file="struct_types_py.mako" />
<%namespace name="exts"           file="/extensions.mako" />

from __future__ import annotations


<%
    root_astnode_name = pyapi.type_public_name(T.root_node)
    c_node = '{}._node_c_type'.format(root_astnode_name)
    c_entity = pyapi.c_type(root_entity)
    c_entity_info = pyapi.c_type(T.entity_info)
    c_metadata = pyapi.c_type(T.env_md)
%>


import argparse
import collections
import ctypes
import io
import json
import os
import re
import sys
import traceback
from typing import (
    Any, AnyStr, Callable, ClassVar, Dict, Generic, IO, Iterator, List,
    Optional as Opt, TYPE_CHECKING, Tuple, Type, TypeVar, Union
)
import weakref


# Protocol was added to "typing" in Python 3.8
if TYPE_CHECKING:
    from typing import Protocol
else:
    Protocol = object


#
# Low-level binding - First part
#

_so_ext = {
    'win32':  'dll',
    'darwin': 'dylib',
}.get(sys.platform, 'so')

# Loading the shared library here is quite involved as we want to support
# Python packages that embed all the required shared libraries: if we can
# find the shared library in the package directory, import it from there
# directly.

# Directory that contains this __init__.py module
_self_path = os.path.dirname(os.path.abspath(__file__))

# Base and full names for the shared library to load. Full name assumes the
# shared lib is in the package directory.
_c_lib_name = 'lib${c_api.shared_object_basename}.{}'.format(_so_ext)
_c_lib_path = os.path.join(_self_path, _c_lib_name)

# If we can find the shared lirbray in the package directory, load it from
# here, otherwise let the dynamic loader find it in the environment. On
# Windows, there is no RPATH trick, so we need to temporarily alter the PATH
# environment variable in order to import the whole closure of DLLs.
_old_env_path = None
if os.path.exists(_c_lib_path):
    if sys.platform == 'win32':
        _old_env_path = os.environ['PATH']
        os.environ['PATH'] = '{}{}{}'.format(_self_path, os.path.pathsep,
                                             os.environ['PATH'])
else:
    _c_lib_path = _c_lib_name

% if generate_auto_dll_dirs:
# If 'os.add_dll_directory' is available (i.e. on Windows) we need to add the
# DLL directories from the PATH environment variable manually.
_add_dll_directory = getattr(os, "add_dll_directory", None)
if _add_dll_directory:
    for path in os.environ.get('PATH', '').split(os.pathsep):
        try:
            os.add_dll_directory(os.path.realpath(path))
        except FileNotFoundError as _:
            pass # Do nothing on purpose
% endif

# Finally load the library
_c_lib = ctypes.cdll.LoadLibrary(_c_lib_path)

# Restore the PATH environment variable if we altered it
if _old_env_path is not None:
    os.environ['PATH'] = _old_env_path


def _import_func(name, argtypes, restype, exc_wrap=True):
    """
    Import "name" from the C library, set its arguments/return types and return
    the binding.

    :param str name: Name of the symbol for the function to import.
    :param list[ctypes._CData] argtypes: Types for function argruments.
    :param None|ctypes._CData restype: Function return type, or None if it
        does not return anything.
    :param bool exc_wrap: If True, wrap the returned function to check for
      exceptions.
    """
    func = getattr(_c_lib, name)
    func.argtypes = argtypes
    func.restype = restype

    def check_argcount(args, kwargs):
        argcount = len(args) + len(kwargs)
        if argcount != len(argtypes):
            raise TypeError(
                '{} takes {} positional arguments but {} was given'
                .format(name, len(argtypes), argcount))

    # Wrapper for "func" that raises a NativeException in case of internal
    # error.

    if exc_wrap:
        def wrapper(*args, **kwargs):
            check_argcount(args, kwargs)
            result = func(*args, **kwargs)
            exc = _get_last_exception()
            if exc:
                raise exc.contents._wrap()
            return result
    else:
        def wrapper(*args, **kwargs):
            check_argcount(args, kwargs)
            return func(*args, **kwargs)

    return wrapper


class _Exception(ctypes.Structure):
    _fields_ = [('kind', ctypes.c_int),
                ('information', ctypes.c_char_p)]

    def _wrap(self):
        # Turn information into native strings, i.e. decode bytes.  These
        # strings are only informative, so do not raise an error if decoding
        # fails: do best effort decoding instead to be as helpful as possible.
        info = self.information.decode(errors='replace')
        return _exception_kind_to_type[self.kind](info)


def _type_fullname(t: type) -> str:
    """
    Return the fully qualified name for the given `t` type.
    """
    name = t.__name__
    module = t.__module__
    return (name
            if module in (None, object.__class__.__module__) else
            '{}.{}'.format(module, name))


def _raise_type_error(expected_type_name: str, actual_value: Any) -> Any:
    raise TypeError('{} instance expected, got {} instead'.format(
        expected_type_name, _type_fullname(type(actual_value))
    ))


def _log_uncaught_error(context):
    """
    Log an uncaught exception on stderr.

    Useful to warn users about an exception that occurs in a Python function
    used as a C callback: we cannot let the exception propagate in this case.
    """
    print(f"Uncaught exception in {context}:", file=sys.stderr)
    traceback.print_exc()


_get_last_exception = _import_func(
   '${capi.get_name("get_last_exception")}',
   [], ctypes.POINTER(_Exception),
   exc_wrap=False
)


def _hashable_c_pointer(pointed_type=None):
    """
    Create a "pointer to `pointed_type` type and make it hashable.

    :param pointed_type: ctypes type class. If left to `None`, we return a
        subclass of `ctypes.c_void_p`.
    :rtype: ctypes.POINTER
    """

    if pointed_type is None:
        class _c_type(ctypes.c_void_p):
            @property
            def _pointer_value(self):
                return self.value or 0
    else:
        @property
        def _pointer_value(self):
            return ctypes.cast(self, ctypes.c_void_p).value or 0

        _c_type = ctypes.POINTER(pointed_type)
        _c_type._pointer_value = _pointer_value

    def __hash__(self):
        return self._pointer_value

    def __eq__(self, other):
        return self._pointer_value == other._pointer_value

    def __ne__(self, other):
        return not (self == other)

    _c_type.__hash__ = __hash__
    _c_type.__eq__ = __eq__
    _c_type.__ne__ = __ne__
    return _c_type


def _unwrap_filename(filename: Opt[AnyStr]) -> Opt[bytes]:
    """Turn filename into a suitable C value for filenames."""
    if filename is None:
        return None
    elif isinstance(filename, str):
        return filename.encode()
    elif not isinstance(filename, bytes):
        raise ValueError(f"invalid filename: {filename}")
    else:
        return filename


def _unwrap_charset(charset: Opt[AnyStr]) -> Opt[bytes]:
    """Turn charset into a suitable C value for charsets."""
    if charset is None:
        return None
    elif isinstance(charset, str):
        return charset.encode()
    elif not isinstance(charset, bytes):
        raise ValueError(f"invalid charset: {charset}")
    else:
        return charset


class _text(ctypes.Structure):
    """
    C value for unicode strings. This object is the owner of the underlying
    buffer, so the string will be deallocated when ``self`` is destroyed.

    ``_unwrap`` takes a string/unicode object and returns a ``_text`` instance,
    while ``_wrap`` retuns an unicode instance.
    """
    # The chars field really is a uint32_t* but considering it as a char* here
    # is more convenient for conversion in this binding layer. On the other
    # side, we have to be careful about converting the length when retrieving
    # the chars.
    _fields_ = [("chars", ctypes.POINTER(ctypes.c_char)),
                ("length", ctypes.c_size_t),
                ("is_allocated", ctypes.c_int),]

    encoding = 'utf-32le' if sys.byteorder == 'little' else 'utf-32be'

    # Instances can hold buffers that they own. In this case, the buffer must
    # be deallocated when the instance is destroyed. Thus instances will hold
    # a "text_buffer" attribute that will be automatically destroyed.
    text_buffer = None

    @classmethod
    def _create_buffer(cls, value: AnyStr) -> Tuple[Any, int]:
        """
        Turn `value` into the corresponding UTF-32 string buffer.

        Return both the string buffer and the number of codepoints it contains
        (not the number of bytes!).
        """
        string = cls.cast(value)
        buf = ctypes.create_string_buffer(string.encode(cls.encoding))
        return (buf, len(string))

    @classmethod
    def _decode_buffer(cls, buf: Any, length: int) -> str:
        """
        Decode the UTF-32 string in `buf`.

        :param buf: String buffer (of type `POINTER(c_char_p)`) to decode.
        :param length: Number of codepoints in `buf` (not the number of
            bytes!).
        """
        if length > 0:
            # `length` tells how much UTF-32 chars there are in `buf` but `buf`
            # is a char* so we have to fetch 4 times more bytes than bytes.
            return buf[:4 * length].decode(cls.encoding)
        else:
            return ""

    @classmethod
    def _unwrap(cls, value: AnyStr) -> _text:
        text_buffer, length = cls._create_buffer(value)
        text_buffer_ptr = ctypes.cast(
            ctypes.pointer(text_buffer),
            ctypes.POINTER(ctypes.c_char)
        )
        result = _text(text_buffer_ptr, length)
        result.text_buffer = text_buffer
        return result

    def _wrap(self) -> str:
        return self._decode_buffer(self.chars, self.length)

    @classmethod
    def cast(cls, value: AnyStr) -> str:
        """
        Try to cast ``value`` into an unicode object. Raise a TypeError, or
        raise a string decoding error when this is not possible.
        """
        if isinstance(value, bytes):
            return value.decode('ascii')
        elif not isinstance(value, str):
            _raise_type_error('text string', value)
        else:
            return value

    def __del__(self) -> None:
        _destroy_text(ctypes.byref(self))


class _symbol_type(ctypes.Structure):
    _fields_ = [('thin_symbol', ctypes.c_uint32),
                ('table', ctypes.c_void_p)]

    @classmethod
    def wrap(cls, c_value: Any) -> str:
        # First extract the text associated to this symbol in "text"
        text = _text()
        _symbol_text(ctypes.byref(c_value), ctypes.byref(text))

        # Then wrap this text
        return text._wrap()

    @classmethod
    def unwrap(cls, py_value: AnyStr, context: Any) -> _symbol_type:
        # First turn the given symbol into a low-level text object
        text = _text._unwrap(py_value)

        # Then convert it to a symbol
        result = cls()
        if not _context_symbol(context, ctypes.byref(text),
                               ctypes.byref(result)):
            raise InvalidSymbolError(py_value)
        return result


class _big_integer:

    class c_type(ctypes.c_void_p):
        pass

    def __init__(self, c_value: Any):
        self.c_value = c_value

    @classmethod
    def unwrap(cls, value: int) -> _big_integer:
        if not isinstance(value, int):
            _raise_type_error('int or long', value)

        text = _text._unwrap(str(value))
        c_value = cls.create(ctypes.byref(text))
        return cls(c_value)

    @classmethod
    def wrap(cls, c_value: Any) -> int:
        helper = cls(c_value)
        text = _text()
        cls.text(helper.c_value, ctypes.byref(text))
        return int(text._wrap())

    def clear(self) -> None:
        self.c_value = None

    def __del__(self) -> None:
        self.decref(self.c_value)
        self.clear()

    create = staticmethod(_import_func(
        '${capi.get_name("create_big_integer")}',
        [ctypes.POINTER(_text)], c_type
    ))
    text = staticmethod(_import_func(
        '${capi.get_name("big_integer_text")}',
        [c_type, ctypes.POINTER(_text)], None
    ))
    decref = staticmethod(_import_func(
        '${capi.get_name("big_integer_decref")}',
        [c_type], None
    ))


class _String:
    """
    Helper to wrap/unwrap string values for properties arguments/return types.
    """

    class c_struct(ctypes.Structure):
        _fields_ = [("length", ctypes.c_int),
                    ("ref_count", ctypes.c_int),

                    # See the "chars" field in the _text structure
                    ("content", ctypes.c_char * 1)]
    c_type = ctypes.POINTER(c_struct)

    __slots__ = ("c_value", )

    def __init__(self, c_value):
        self.c_value = c_value

    def __del__(self):
        self.dec_ref(self.c_value)
        self.c_value = None

    @classmethod
    def unwrap(cls, value: AnyStr) -> _String:
        # Convert "value" into the corresponding UTF-32 string buffer
        buf, length = _text._create_buffer(value)
        return cls(cls.create(buf, length))

    @classmethod
    def wrap(cls, value: Any) -> str:
        struct = value.contents

        # "struct.content" will get a one-byte copy of the actual string
        # because of the hack above to handle variable-length struct field. To
        # get the whole string, compute a pointer to this field fierst.
        content_addr = _field_address(struct, "content")
        content = ctypes.pointer(ctypes.c_char.from_address(content_addr))

        return _text._decode_buffer(content, struct.length)

    create = staticmethod(_import_func(
        '${capi.get_name("create_string")}',
        [ctypes.POINTER(ctypes.c_char), ctypes.c_int], c_type
    ))
    dec_ref = staticmethod(_import_func(
        '${capi.get_name("string_dec_ref")}',
        [c_type], None
    ))


if TYPE_CHECKING:
    _EnumType = TypeVar("_EnumType", bound=_Enum)


class _Enum:

    _name: ClassVar[str]
    """
    Name for this enumeration type.
    """

    _c_to_py: ClassVar[List[str]]
    """
    Mapping from C values to user-level Python values.
    """

    _py_to_c: ClassVar[Dict[str, int]]
    """
    Mapping from user-level Python values to C values.
    """

    @classmethod
    def _unwrap(cls, py_value: str) -> int:
        if not isinstance(py_value, str):
            _raise_type_error('str', py_value)
        try:
            return cls._py_to_c[py_value]
        except KeyError:
            raise ValueError('Invalid {}: {}'.format(cls._name, py_value))

    @classmethod
    def _wrap(cls: Type[_EnumType], c_value: Any) -> _EnumType:
        if isinstance(c_value, ctypes.c_int):
            c_value = c_value.value
        return cls._c_to_py[c_value]


% for enum_type in ctx.enum_types:
class ${enum_type.py_helper}(_Enum):
    ${py_doc(enum_type, 4)}

    % for v in enum_type.values:
    ${v.name.lower} = ${repr(v.name.lower)}
    % endfor

    _name = ${repr(enum_type.api_name.camel)}
    _c_to_py = [
        ${', '.join(v.name.lower for v in enum_type.values)}]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}
% endfor


default_grammar_rule = GrammarRule.${ctx.main_rule_api_name.lower}


_file_reader = _hashable_c_pointer()
_unit_provider = _hashable_c_pointer()
_event_handler = _hashable_c_pointer()


class _EventHandlerWrapper:
    """
    Wrapper for EventHandler instances, responsible to create the low-level
    event handler value and hold its callbacks.
    """

    __slots__ = (
        "event_handler",
        "c_value",
        "destroy_callback",
        "unit_requested_callback",
        "unit_parsed_callback",
    )

    def __init__(self, event_handler: EventHandler):
        self.event_handler = event_handler

        # Create the C callbacks (wrappers around the _EventHandlerWrapper
        # static method) and keep references to them in "self" so that they
        # survive at least as long as "self".
        self.destroy_callback = _event_handler_destroy_func(
            _EventHandlerWrapper.destroy_func
        )
        self.unit_requested_callback = _event_handler_unit_requested_func(
            _EventHandlerWrapper.unit_requested_func
        )
        self.unit_parsed_callback = _event_handler_unit_parsed_func(
            _EventHandlerWrapper.unit_parsed_func
        )

        # Create the C-level event handler, which keeps a reference to "self"
        # and uses _EventHandlerWrapper's static methods as callbacks.
        self.c_value = _create_event_handler(
            ctypes.py_object(self),
            self.destroy_callback,
            self.unit_requested_callback,
            self.unit_parsed_callback,
        )

    def __del__(self) -> None:
        _dec_ref_event_handler(self.c_value)
        self.c_value = None

    @classmethod
    def create(
        cls,
        event_handler: Opt[EventHandler]
    ) -> Tuple[Opt[_EventHandlerWrapper], Opt[object]]:
        """
        Helper to wrap an EventHandler instance. Return also the C value that
        is created for that event handler. For convenience, just return None
        for both if ``event_handler`` is None.
        """
        if event_handler is None:
            return None, None
        else:
            eh = cls(event_handler)
            return eh, eh.c_value

    @staticmethod
    def destroy_func(self: _EventHandlerWrapper) -> None:
        pass

    @staticmethod
    def unit_requested_func(self: _EventHandlerWrapper,
                            context: object,
                            name: _text,
                            from_unit: object,
                            found: ctypes.c_uint8,
                            is_not_found_error: ctypes.c_uint8) -> None:
        py_context = AnalysisContext._wrap(context)
        py_name = name.contents._wrap()
        py_from_unit = AnalysisUnit._wrap(from_unit)
        try:
            self.event_handler.unit_requested_callback(
                py_context,
                py_name,
                py_from_unit,
                bool(found),
                bool(is_not_found_error),
            )
        except BaseException as exc:
            _log_uncaught_error("EventHandler.unit_requested_callback")

    @staticmethod
    def unit_parsed_func(self: _EventHandlerWrapper,
                         context: object,
                         unit: object,
                         reparsed: ctypes.c_uint8) -> None:
        py_context = AnalysisContext._wrap(context)
        py_unit = AnalysisUnit._wrap(unit)
        try:
            self.event_handler.unit_parsed_callback(
                py_context,
                py_unit,
                bool(reparsed),
            )
        except BaseException as exc:
            _log_uncaught_error("EventHandler.unit_parsed_callback")


def _canonicalize_buffer(buffer: AnyStr,
                         charset: Opt[bytes]) -> Tuple[bytes, Opt[bytes]]:
    """Canonicalize source buffers to be bytes buffers."""
    if isinstance(buffer, str):
        if charset:
            raise TypeError('`charset` must be null when the buffer is'
                            ' Unicode')
        return (buffer.encode('utf-8'), b'utf-8')
    elif not isinstance(buffer, bytes):
        raise TypeError('`buffer` must be a string')
    else:
        return (buffer, charset)


#
# High-level binding
#


% for e in ctx.sorted_exception_types:
class ${e.name.camel}(Exception):
    ${py_doc(e.doc_entity, 4)}
    pass
% endfor

_exception_kind_to_type = [
% for e in ctx.sorted_exception_types:
    ${e.name.camel},
% endfor
]

${exts.include_extension(
   ctx.ext('python_api', 'exceptions')
)}


class EventHandler(Protocol):
    ${py_doc('langkit.event_handler_type', 4)}

    def unit_requested_callback(self,
                                context: AnalysisContext,
                                name: str,
                                from_unit: AnalysisUnit,
                                found: bool,
                                is_not_found_error: bool) -> None:
        ${py_doc('langkit.event_handler_unit_requested_callback', 8)}
        pass

    def unit_parsed_callback(self,
                             context: AnalysisContext,
                             unit: AnalysisUnit,
                             reparsed: bool) -> None:
        ${py_doc('langkit.event_handler_unit_parsed_callback', 8)}
        pass


class AnalysisContext:
    ${py_doc('langkit.analysis_context_type', 4)}

    __slots__ = ('_c_value', '_unit_provider', '_event_handler_wrapper',
                 '_serial_number', '_unit_cache', '__weakref__')

    _context_cache: weakref.WeakValueDictionary[Any, AnalysisContext] = (
        weakref.WeakValueDictionary()
    )
    """
    Cache for analysis context wrappers. Indexed by analysis context addresses,
    which are known to stay valid forever (and re-used).

    Unlike unit and node caches, this one should contain weak references so
    that analysis contexts (and their units/nodes) can be free'd when user code
    does not reference them anymore.
    """

    def __init__(self,
                 charset: Opt[str] = None,
                 file_reader: Opt[FileReader] = None,
                 unit_provider: Opt[UnitProvider] = None,
                 event_handler: Opt[EventHandler] = None,
                 with_trivia: bool = True,
                 tab_stop: int = ${ctx.default_tab_stop},
                 *,
                 _c_value: Any = None) -> None:
        ${py_doc('langkit.create_context', 8)}

        # Initialize this field in case we raise an exception during
        # construction, so that the destructor can run later on.
        self._c_value = None

        # Create the analysis context if requested, otherwise increase the
        # refcount of the existing context.
        if _c_value is None:
            _charset = _unwrap_charset(charset)
            if not isinstance(tab_stop, int) or tab_stop < 1:
                raise ValueError(
                    'Invalid tab_stop (positive integer expected)')
            c_file_reader = file_reader._c_value if file_reader else None
            c_unit_provider = unit_provider._c_value if unit_provider else None
            self._event_handler_wrapper, c_event_handler = (
                _EventHandlerWrapper.create(event_handler)
            )
            self._c_value = _allocate_analysis_context()
        else:
            self._c_value = _context_incref(_c_value)

        # Register the context in our cache so that wrapping the context in the
        # future always yields the same instance.
        assert self._c_value not in self._context_cache
        self._context_cache[self._c_value] = self

        # Initialize the serial number and the unit cache
        self._serial_number: Opt[int] = None
        self._unit_cache: Dict[str, AnalysisUnit] = {}
        """
        Cache for AnalysisUnit wrappers, indexed by analysis unit addresses,
        which are known to stay valid as long as the context is alive.
        """

        self._check_unit_cache()

        # Now that we have an AnalysisContext wrapper registered, if we just
        # created the analysis context, also initialize it.
        if _c_value is None:
            _initialize_analysis_context(
                self._c_value,
                _charset,
                c_file_reader,
                c_unit_provider,
                c_event_handler,
                with_trivia,
                tab_stop
            )

        # Keep a reference to the unit provider so that it is live at least as
        # long as the analysis context is live.
        self._unit_provider = unit_provider

    def __del__(self) -> None:
        if self._c_value:
            _context_decref(self._c_value)

    def __eq__(self, other: Any) -> bool:
        return self._c_value == other._c_value

    def __hash__(self) -> int:
        return hash(self._c_value)

    def get_from_file(self,
                      filename: AnyStr,
                      charset: Opt[str] = None,
                      reparse: bool = False,
                      rule: str = default_grammar_rule) -> AnalysisUnit:
        ${py_doc('langkit.get_unit_from_file', 8)}
        _filename = _unwrap_filename(filename)
        _charset = _unwrap_charset(charset)
        c_value = _get_analysis_unit_from_file(self._c_value, _filename,
                                               _charset, reparse,
                                               GrammarRule._unwrap(rule))
        return AnalysisUnit._wrap(c_value)

    def get_from_buffer(self,
                        filename: AnyStr,
                        buffer: AnyStr,
                        charset: Opt[str] = None,
                        reparse: bool = False,
                        rule: str = default_grammar_rule) -> AnalysisUnit:
        ${py_doc('langkit.get_unit_from_buffer', 8)}
        _filename = _unwrap_filename(filename)
        _charset = _unwrap_charset(charset)
        _buffer, _charset = _canonicalize_buffer(buffer, _charset)
        c_value = _get_analysis_unit_from_buffer(self._c_value, _filename,
                                                 _charset,
                                                 _buffer, len(_buffer),
                                                 GrammarRule._unwrap(rule))
        return AnalysisUnit._wrap(c_value)

% if ctx.default_unit_provider:
    def get_from_provider(
        self,
        name: AnyStr,
        kind: str,
        charset: Opt[str] = None,
        reparse: bool = False
    ) -> AnalysisUnit:
        ${py_doc('langkit.get_unit_from_provider', 8)}
        if isinstance(name, bytes):
            text_name = name.decode()
        else:
            text_name = name
        _charset = _unwrap_charset(charset)

        _name = _text._unwrap(text_name)
        _kind = ${pyapi.unwrap_value('kind', T.AnalysisUnitKind, None)}
        c_value = _get_analysis_unit_from_provider(
            self._c_value, ctypes.byref(_name), _kind, _charset, reparse
        )
        if c_value:
            return AnalysisUnit._wrap(c_value)
        else:
            raise InvalidUnitNameError('Invalid unit name: {} ({})'.format(
                repr(name), kind
            ))
% endif

    def discard_errors_in_populate_lexical_env(self,
                                               discard: bool) -> None:
        ${py_doc('langkit.context_discard_errors_in_populate_lexical_env', 8)}
        _discard_errors_in_populate_lexical_env(self._c_value, bool(discard))

    class _c_struct(ctypes.Structure):
        _fields_ = [('serial_number', ctypes.c_uint64)]
    _c_type = _hashable_c_pointer(_c_struct)

    @classmethod
    def _wrap(cls, c_value):
        try:
            return cls._context_cache[c_value]
        except KeyError:
            return cls(_c_value=c_value)

    def _check_unit_cache(self):
        """
        If this context has been re-used, invalidate its unit cache.
        """
        serial_number = self._c_value.contents.serial_number
        if self._serial_number != serial_number:
            self._unit_cache = {}
            self._serial_number = serial_number

    ${exts.include_extension(ctx.ext("python_api", "analysis_context_class"))}


class AnalysisUnit:
    ${py_doc('langkit.analysis_unit_type', 4)}

    __slots__ = ('_c_value', '_context_link', '_cache_version_number',
                 '_node_cache')

    class TokenIterator:
        ${py_doc('langkit.python.AnalysisUnit.TokenIterator', 8)}

        def __init__(self, first: Opt[Token]):
            self.first: Opt[Token] = first

        def __iter__(self) -> AnalysisUnit.TokenIterator:
            return self

        def __next__(self) -> Token:
            if not self.first:
                raise StopIteration()
            result = self.first
            self.first = self.first.next
            return result
        next = __next__

    def __init__(self, context: AnalysisContext, c_value: Any) -> None:
        """
        This constructor is an implementation detail, and is not meant to be
        used directly. Please use AnalysisContext.get_from_* methods to create
        analysis unit instances instead.
        """
        self._c_value = c_value

        # Keep a reference on the owning context so that we keep it alive at
        # least as long as this unit is alive.
        self._context_link = context

        # Store this wrapper in caches for later re-use
        assert c_value not in context._unit_cache
        context._unit_cache[c_value] = self

        self._cache_version_number: Opt[int] = None
        """
        Last version number we saw for this analysis unit wrapper. If it's
        different from `self._unit_version`, it means that the unit was
        reparsed: in this case we need to clear the node cache below (see the
        `_check_node_cache` method).
        """

        self._node_cache: Dict[Tuple[Any, Any, Any], ${root_astnode_name}] = {}
        """
        Cache for all node wrappers in this unit. Indexed by couples:
        (c_value, metadata, rebindings).
        """

        self._check_node_cache()

    def __eq__(self, other: Any) -> bool:
        return self._c_value == other._c_value

    def __hash__(self) -> int:
        return hash(self._c_value)

    @property
    def context(self) -> AnalysisContext:
        ${py_doc('langkit.unit_context', 8)}
        return self._context_link

    def reparse(self,
                buffer: Opt[AnyStr] = None,
                charset: Opt[str] = None) -> None:
        ${py_doc('langkit.unit_reparse_generic', 8)}
        _charset = _unwrap_charset(charset)
        if buffer is None:
            _unit_reparse_from_file(self._c_value, _charset)
        else:
            _buffer, _charset = _canonicalize_buffer(buffer, _charset)
            _unit_reparse_from_buffer(self._c_value, _charset, _buffer,
                                      len(_buffer))

    def populate_lexical_env(
        self,
        % if ctx.ple_unit_root:
            ple_root_index: int,
        % endif
    ) -> None:
        ${py_doc('langkit.unit_populate_lexical_env', 8)}
        if not _unit_populate_lexical_env(
            self._c_value,
            % if ctx.ple_unit_root:
                ple_root_index,
            % endif
        ):
            raise PropertyError()

    @property
    def root(self) -> ${root_astnode_name}:
        ${py_doc('langkit.unit_root', 8)}
        result = ${c_entity}()
        _unit_root(self._c_value, ctypes.byref(result))
        return ${root_astnode_name}._wrap(result)

    @property
    def first_token(self) -> Opt[Token]:
        ${py_doc('langkit.unit_first_token', 8)}
        result = Token._c_struct()
        _unit_first_token(self._c_value, ctypes.byref(result))
        return Token._wrap(result)

    @property
    def last_token(self) -> Opt[Token]:
        ${py_doc('langkit.unit_last_token', 8)}
        result = Token._c_struct()
        _unit_last_token(self._c_value, ctypes.byref(result))
        return Token._wrap(result)

    @property
    def text(self) -> str:
        ${py_doc('langkit.unit_text', 8)}
        if self.first_token:
            assert self.last_token
            return Token.text_range(self.first_token, self.last_token)
        else:
            return ""

    @property
    def token_count(self) -> int:
        ${py_doc('langkit.unit_token_count', 8)}
        return _unit_token_count(self._c_value)

    @property
    def trivia_count(self) -> int:
        ${py_doc('langkit.unit_trivia_count', 8)}
        return _unit_trivia_count(self._c_value)

    def lookup_token(self, sloc: Sloc) -> Opt[Token]:
        ${py_doc('langkit.unit_lookup_token', 8)}
        unit = AnalysisUnit._unwrap(self)
        _sloc = Sloc._c_type._unwrap(sloc)
        result = Token._c_struct()
        _unit_lookup_token(unit, ctypes.byref(_sloc), ctypes.byref(result))
        return Token._wrap(result)

    def _dump_lexical_env(self) -> None:
        ${py_doc('langkit.unit_dump_lexical_env', 8)}
        unit = AnalysisUnit._unwrap(self)
        _unit_dump_lexical_env(unit)

    def iter_tokens(self) -> AnalysisUnit.TokenIterator:
        ${py_doc('langkit.python.AnalysisUnit.iter_tokens', 8)}
        return self.TokenIterator(self.first_token)

    @property
    def filename(self) -> str:
        ${py_doc('langkit.unit_filename', 8)}
        filename = _unit_filename(self._c_value)
        return _unwrap_str(filename)

    @property
    def diagnostics(self) -> List[Diagnostic]:
        ${py_doc('langkit.python.AnalysisUnit.diagnostics', 8)}
        count = _unit_diagnostic_count(self._c_value)
        result = []
        diag = Diagnostic._c_type()
        for i in range(count):
            success = _unit_diagnostic(self._c_value, i, ctypes.byref(diag))
            assert success
            result.append(diag._wrap())
        return result

    def format_gnu_diagnostic(self, d: Diagnostic) -> str:
        ${py_doc('langkit.format_gnu_diagnostic', 8)}
        sloc = d.sloc_range.start
        prefix = f"{os.path.basename(self.filename)}:"
        if sloc:
            prefix += f"{sloc}:"
        return f"{prefix} {d.message}"

    def __repr__(self) -> str:
        return '<AnalysisUnit {}>'.format(repr(
            os.path.basename(self.filename)
        ))

    class _c_struct(ctypes.Structure):
        _fields_ = [('unit_version', ctypes.c_uint64)]
    _c_type = _hashable_c_pointer(_c_struct)

    @classmethod
    def _wrap(cls, c_value):
        if not c_value:
            return None

        # Invalidate the unit cache if needed, then look for an existing
        # wrapper for this unit.
        context = cls._context(c_value)
        context._check_unit_cache()

        try:
            return context._unit_cache[c_value]
        except KeyError:
            return cls(context, c_value)

    @classmethod
    def _unwrap(cls, value):
        if value is None:
            return value
        elif not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)
        else:
            return value._c_value

    @classmethod
    def _context(cls, c_value) -> AnalysisContext:
        ctx = _unit_context(c_value)
        return AnalysisContext._wrap(ctx)

    @property
    def _unit_version(self) -> int:
        return self._c_value.contents.unit_version

    def _check_node_cache(self) -> None:
        """
        If this unit has been reparsed, invalidate its node cache.
        """
        if self._cache_version_number != self._unit_version:
            self._node_cache = {}
            self._cache_version_number = self._unit_version


class Sloc:
    ${py_doc('langkit.sloc_type', 4)}

    def __init__(self, line: int, column: int):
        assert line >= 0 and column >= 0
        self.line = line
        self.column = column

    def __bool__(self) -> bool:
        return bool(self.line or self.column)

    def __lt__(self, other: Sloc) -> bool:
        # First compare line numbers...
        if self.line < other.line:
            return True
        elif self.line > other.line:
            return False

        # Past this point, we know that both are on the same line, so now
        # compare column numbers.
        else:
            return self.column < other.column

    def __eq__(self, other: Any) -> bool:
        return self.line == other.line and self.column == other.column

    def __hash__(self) -> int:
        return hash((self.line, self.column))

    def __str__(self) -> str:
        return '{}:{}'.format(self.line, self.column)

    def __repr__(self) -> str:
        return '<Sloc {} at {:#x}>'.format(self, id(self))

    class _c_type(ctypes.Structure):
        _fields_ = [("line", ctypes.c_uint32),
                    ("column", ctypes.c_uint16)]

        def _wrap(self) -> Sloc:
            return Sloc(self.line, self.column)

        @classmethod
        def _unwrap(cls, sloc: Sloc) -> Sloc._c_type:
            return cls(sloc.line, sloc.column)


class SlocRange:
    ${py_doc('langkit.sloc_range_type', 4)}

    def __init__(self, start: Sloc, end: Sloc):
        self.start = start
        self.end = end

    def __bool__(self) -> bool:
        return bool(self.start or self.end)

    def __lt__(self, other: SlocRange) -> bool:
        raise NotImplementedError('SlocRange comparison not supported')

    def __eq__(self, other: Any) -> bool:
        return self.start == other.start and self.end == other.end

    def __hash__(self) -> int:
        return hash((self.start, self.end))

    def __str__(self) -> str:
        return '{}-{}'.format(self.start, self.end)

    def __repr__(self) -> str:
        return "<SlocRange {}:{}-{}:{}>".format(
            self.start.line, self.start.column,
            self.end.line, self.end.column
        )

    class _c_type(ctypes.Structure):
        _fields_ = [("start", Sloc._c_type),
                    ("end", Sloc._c_type)]

        def _wrap(self) -> SlocRange:
            return SlocRange(self.start._wrap(), self.end._wrap())


class Diagnostic:
    ${py_doc('langkit.diagnostic_type', 4)}

    def __init__(self, sloc_range: SlocRange, message: str):
        self.sloc_range = sloc_range
        self.message = message

    @property
    def as_text(self) -> str:
        return (u'{}: {}'.format(self.sloc_range, self.message)
                if self.sloc_range else
                self.message)

    def __str__(self) -> str:
        return self.as_text

    def __repr__(self) -> str:
        return '<Diagnostic {}>'.format(self)


    class _c_type(ctypes.Structure):
        _fields_ = [('sloc_range', SlocRange._c_type),
                    ('message', _text)]

        def _wrap(self) -> Diagnostic:
            return Diagnostic(self.sloc_range._wrap(), self.message._wrap())


class _tdh_c_struct(ctypes.Structure):
    _fields_ = [('version', ctypes.c_uint64)]
_tdh_c_type = _hashable_c_pointer(_tdh_c_struct)


class Token:
    ${py_doc('langkit.token_reference_type', 4)}

    __slots__ = ("_c_value", "_context_version", "_tdh_version")

    class _c_struct(ctypes.Structure):
        _fields_ = [('context',      AnalysisContext._c_type),
                    ('token_data',   _tdh_c_type),
                    ('token_index',  ctypes.c_int),
                    ('trivia_index', ctypes.c_int)]
    _c_type = _hashable_c_pointer(_c_struct)

    def __init__(self, c_value: Any):
        """
        This constructor is an implementation detail and is not meant to be
        used directly.
        """
        self._c_value = c_value
        self._context_version = c_value.context.contents.serial_number
        self._tdh_version = c_value.token_data.contents.version

    @classmethod
    def _wrap(cls, c_value: Any) -> Opt[Token]:
        return cls(c_value) if c_value.token_data else None

    @classmethod
    def _unwrap(cls, value):
        cls._check_token(value)
        return value._c_value

    def _check_stale_reference(self) -> None:
        # First, check that the reference to the context is not stale
        if (
            self._c_value.context.contents.serial_number
            != self._context_version
        ):
            raise StaleReferenceError("owning context was deallocated")

        # The context is valid, so the token data handler is, too: check that
        # no reparsing occured.
        if self._c_value.token_data.contents.version != self._tdh_version:
            raise StaleReferenceError("owning unit was reparsed")

    @staticmethod
    def _check_token(value: Any) -> None:
        if not isinstance(value, Token):
            raise TypeError('invalid token: {}'.format(value))
        value._check_stale_reference()

    def _check_same_unit(self, other: Token) -> None:
        if self._c_value.token_data != other._c_value.token_data:
            raise ValueError('{} and {} come from different analysis units'
                             .format(self, other))

    @property
    def next(self) -> Opt[Token]:
        ${py_doc('langkit.token_next', 8)}
        self._check_stale_reference()
        result = self._c_struct()
        _token_next(ctypes.byref(self._c_value), ctypes.byref(result))
        return self._wrap(result)

    @property
    def previous(self) -> Opt[Token]:
        ${py_doc('langkit.token_previous', 8)}
        self._check_stale_reference()
        result = self._c_struct()
        _token_previous(ctypes.byref(self._c_value), ctypes.byref(result))
        return self._wrap(result)

    def range_until(self, other: Token) -> Iterator[Token]:
        ${py_doc('langkit.token_range_until', 8)}
        self._check_stale_reference()
        self._check_token(other)
        self._check_same_unit(other)

        # Keep the generator as a nested function so that the above checks are
        # executed when the generator is created, instead of only when its
        # first item is requested.
        #
        # Note that, because the execution of a generator stops and resumes,
        # the tokens may become stale after it resumes: check for stale
        # references at starting and resuming time.
        def generator() -> Iterator[Token]:
            self._check_stale_reference()
            if other < self:
                return

            yield self
            current = self
            while current < other:
                next = current.next
                assert next is not None
                yield next
                self._check_stale_reference()
                current = next
        return generator()

    def is_equivalent(self, other: Token) -> bool:
        ${py_doc('langkit.token_is_equivalent', 8)}
        self._check_stale_reference()
        self._check_token(other)
        return bool(_token_is_equivalent(
            ctypes.byref(self._c_value), ctypes.byref(other._c_value))
        )

    @property
    def kind(self) -> str:
        ${py_doc('langkit.token_kind', 8)}
        self._check_stale_reference()
        kind = _token_get_kind(self._c_value)
        name = _token_kind_name(kind)
        # The _token_kind_name wrapper is already supposed to handle exceptions
        # so this should always return a non-null value.
        assert name
        return _unwrap_str(name)

    @property
    def is_trivia(self) -> bool:
        ${py_doc('langkit.token_is_trivia', 8)}
        self._check_stale_reference()
        return self._c_value.trivia_index != 0

    @property
    def index(self) -> int:
        ${py_doc('langkit.token_index', 8)}
        self._check_stale_reference()
        return (self._c_value.token_index - 1
                if self._c_value.trivia_index == 0 else
                self._c_value.trivia_index - 1)

    @property
    def text(self) -> str:
        ${py_doc('langkit.token_text', 8)}
        return self.text_range(self, self)

    @classmethod
    def text_range(cls, first: Token, last: Token) -> str:
        ${py_doc('langkit.token_range_text', 8)}
        cls._check_token(first)
        cls._check_token(last)
        first._check_same_unit(last)
        result = _text()
        success = _token_range_text(
            ctypes.byref(first._c_value),
            ctypes.byref(last._c_value),
            ctypes.byref(result),
        )
        assert success
        return result._wrap() or u''

    @property
    def sloc_range(self) -> SlocRange:
        ${py_doc('langkit.token_sloc_range', 8)}
        self._check_stale_reference()
        result = SlocRange._c_type()
        _token_sloc_range(ctypes.byref(self._c_value), ctypes.byref(result))
        return result._wrap()

    def __eq__(self, other: Any) -> bool:
        ${py_doc('langkit.python.Token.__eq__', 8)}
        return (isinstance(other, Token)
                and self._identity_tuple == other._identity_tuple)

    def __hash__(self) -> int:
        return hash(self._identity_tuple)

    def __repr__(self) -> str:
        self._check_stale_reference()
        return '<Token {}{} at {}>'.format(
            self.kind,
            ' {}'.format(repr(self.text)) if self.text else '',
            self.sloc_range
        )

    def __lt__(self, other: Opt[Token]):
        ${py_doc('langkit.python.Token.__lt__', 8)}
        self._check_stale_reference()

        # None always comes first
        if other is None:
            return False

        self._check_token(other)
        self._check_same_unit(other)
        return self._identity_tuple < other._identity_tuple

    def __le__(self, other: Opt[Token]) -> bool:
        return self == other or self < other

    def __gt__(self, other: Opt[Token]) -> bool:
        return not (self <= other)

    def __ge__(self, other: Opt[Token]) -> bool:
        return not (self < other)

    def to_data(self) -> dict:
        ${py_doc('langkit.python.Token.to_data', 8)}
        return {"kind": "Token", "token_kind": self.kind, "text": self.text}

    @property
    def _identity_tuple(self) -> Tuple[Any, int, int]:
        """
        Return a tuple that return a tuple that contains "identity" information
        for this token. Think of it as a database primary key.

        This property is for internal use only.
        """
        return (
            self._c_value.token_data,
            self._c_value.token_index,
            self._c_value.trivia_index
        )


## TODO: if needed one day, also bind create_file_provider to allow Python
## users to implement their own file readers.
class FileReader:
    ${py_doc('langkit.file_reader_type', 4)}

    def __init__(self, c_value: Any):
        ${py_doc('langkit.python.FileReader.__init__', 8)}
        self._c_value = c_value

    def __del__(self) -> None:
        _dec_ref_file_reader(self._c_value)

${exts.include_extension(
   ctx.ext('python_api', 'file_readers', 'methods')
)}


class UnitProvider:
    ${py_doc('langkit.unit_provider_type', 4)}

    def __init__(self, c_value: Any):
        ${py_doc('langkit.python.UnitProvider.__init__', 8)}
        self._c_value = c_value

    def __del__(self) -> None:
        _dec_ref_unit_provider(self._c_value)

${exts.include_extension(
   ctx.ext('python_api', 'unit_providers', 'methods')
)}


class ${root_astnode_name}:
    ${py_doc(T.root_node, 4)}

    is_list_type = False
    __slots__ = ('_unprotected_c_value', '_node_c_value', '_metadata',
                 '_rebindings', '_unprotected_getitem_cache', '_unit',
                 '_unit_version', '_rebindings_version')

    _kind_name: str
    _field_names: Tuple[str, ...]

    ${astnode_types.subclass_decls(T.root_node)}

    def __init__(self, c_value: Any, node_c_value: Any, rebindings: Any):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly. For now, the creation of AST nodes can happen only as
        part of the parsing of an analysis unit.
        """

        self._unprotected_c_value = c_value

        # Access to these fields is unprotected from stale references, but it
        # is supposed to be used only in _id_tuple, which itself should not be
        # used outside of hashing/equality use cases.
        self._node_c_value = node_c_value
        self._rebindings = rebindings

        self._unprotected_getitem_cache: Dict[int,
                                              Opt[${root_astnode_name}]] = {}
        """
        Cache for the __getitem__ override.
        """

        # Information to check before accessing node data that it is still
        # valid.
        self._unit = self._fetch_unit(c_value)
        self._unit_version = self._unit._unit_version
        self._rebindings_version = (
            rebindings.contents.version if rebindings else None
        )

    def _check_stale_reference(self) -> None:
        # We have a reference to the owning unit, so there is no need to
        # check that the unit and the context are still valid. Just check that
        # the unit has not been reparsed.
        if self._unit._unit_version != self._unit_version:
            raise StaleReferenceError("unit was reparsed")

        # Also check that the rebindings are still valid
        if (
            self._rebindings
            and self._rebindings.contents.version != self._rebindings_version
        ):
            raise StaleReferenceError("related unit was reparsed")

    @property
    def _c_value(self) -> Any:
        self._check_stale_reference()
        return self._unprotected_c_value

    @property
    def _getitem_cache(self) -> Dict[int, Opt[${root_astnode_name}]]:
        self._check_stale_reference()
        return self._unprotected_getitem_cache

    def __eq__(self, other: Any) -> bool:
        return (
            isinstance(other, ${root_astnode_name})
            and bool(
                _node_is_equivalent(
                    ctypes.byref(self._unsafe_unwrap),
                    ctypes.byref(other._unsafe_unwrap)
                )
            )
        )

    def __ne__(self, other: Any) -> bool:
        return not (self == other)

    def __hash__(self) -> int:
        return _node_hash(ctypes.byref(self._unsafe_unwrap))

    @property
    def kind_name(self) -> str:
        ${py_doc('langkit.node_kind', 8)}
        return self._kind_name

    @property
    def is_token_node(self) -> bool:
        ${py_doc('langkit.node_is_token_node', 8)}
        node = self._unwrap(self)
        return bool(_node_is_token_node(ctypes.byref(node)))

    @property
    def is_synthetic(self) -> bool:
        ${py_doc('langkit.node_is_synthetic', 8)}
        node = self._unwrap(self)
        return bool(_node_is_synthetic(ctypes.byref(node)))

    @property
    def sloc_range(self) -> SlocRange:
        ${py_doc('langkit.node_sloc_range', 8)}
        node = self._unwrap(self)
        result = SlocRange._c_type()
        _node_sloc_range(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def text(self) -> str:
        ${py_doc('langkit.node_text', 8)}
        node = self._unwrap(self)
        result = _text()
        _node_text(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def image(self) -> str:
        ${py_doc('langkit.node_image', 8)}
        c_node = self._unwrap(self)
        c_result = _text()
        _node_image(ctypes.byref(c_node), ctypes.byref(c_result))
        return c_result._wrap()

    def lookup(self, sloc: Sloc) -> Opt[${root_astnode_name}]:
        ${py_doc('langkit.lookup_in_node', 8)}
        node = self._unwrap(self)
        c_sloc = Sloc._c_type._unwrap(sloc)
        result = ${c_entity}()
        _lookup_in_node(ctypes.byref(node), ctypes.byref(c_sloc),
                        ctypes.byref(result))
        return ${root_astnode_name}._wrap(result)

    def __bool__(self) -> bool:
        ${py_doc('langkit.python.root_node.__bool__', 8)}
        return True

    def __iter__(self) -> Iterator[Opt[${root_astnode_name}]]:
        ${py_doc('langkit.python.root_node.__iter__', 8)}
        for i in range(len(self)):
            yield self[i]

    def __len__(self) -> int:
        ${py_doc('langkit.python.root_node.__len__', 8)}
        node = self._unwrap(self)
        return _node_children_count(ctypes.byref(node))

    def __getitem__(self, key: int) -> Opt[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.__getitem__', 8)}
        if not isinstance(key, int):
            msg = ('${root_astnode_name} children are integer-indexed'
                   ' (got {})').format(type(key))
            raise TypeError(msg)

        if key < 0:
            key += len(self)

        if key in self._getitem_cache:
            return self._getitem_cache[key]

        node = self._unwrap(self)
        result_struct = ${c_entity}()
        success = _node_child(
            ctypes.byref(node), key, ctypes.byref(result_struct)
        )
        if not success:
            raise IndexError('child index out of range')
        else:
            result = ${root_astnode_name}._wrap(result_struct)
            self._getitem_cache[key] = result
            return result

    def iter_fields(self) -> Iterator[Tuple[str, Opt[${root_astnode_name}]]]:
        ${py_doc('langkit.python.root_node.iter_fields', 8)}
        if self.is_list_type:
            for i, value in enumerate(self):
                yield ('item_{}'.format(i), value)
        else:
            for field_name in self._field_names:
                yield (field_name, getattr(self, '{}'.format(field_name)))

    def dump_str(self) -> str:
        ${py_doc('langkit.python.root_node.dump_str', 8)}
        output = io.StringIO()
        self.dump(file=output)
        ret = output.getvalue()
        output.close()
        return ret

    def dump(self, indent: str = '', file: IO[str] = sys.stdout) -> None:
        ${py_doc('langkit.python.root_node.dump', 8)}

        def print_node(name, value):
            if isinstance(value, ${root_astnode_name}):
                print('{}{}:'.format(indent, name), file=file)
                value.dump(indent + '  ', file)
            else:
                print('{}{}: {}'.format(indent, name, value), file=file)

        node_repr = str(self)[1:-1]
        print('{}{}{}'.format(
            indent, node_repr,
            ': {}'.format(self.text) if self.is_token_node else ''
        ), file=file)
        indent = indent + '|'
        if self.is_list_type:
            for i, value in enumerate(self):
                print_node("item_{}".format(i), value)
        else:
            for name, value in self.iter_fields():
                print_node(name, value)

    def findall(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> List[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.findall', 8)}
        return list(self.finditer(ast_type_or_pred, **kwargs))

    def find(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> Opt[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.find', 8)}
        try:
            return next(self.finditer(ast_type_or_pred, **kwargs))
        except StopIteration:
            return None

    def finditer(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> Iterator[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.finditer', 8)}
        # Create a "pred" function to use as the node filter during the
        # traversal.
        if isinstance(ast_type_or_pred, type):
            sought_type = ast_type_or_pred
            pred = lambda node: isinstance(node, sought_type)
        elif isinstance(ast_type_or_pred, collections.abc.Sequence):
            sought_types = ast_type_or_pred
            pred = lambda node: isinstance(node, tuple(sought_types))
        else:
            pred = ast_type_or_pred

        def match(left, right):
            """
            :param left: Node child to match.
            :param right: Matcher, coming from ``kwargs``.
            """
            if left is None:
                return
            if hasattr(left, "match"):
                return left.match(right)
            else:
                return left == right

        def helper(node):
            for child in node:
                if child is not None:
                    if pred(child):
                        if not kwargs:
                            yield child
                        elif all([match(getattr(child, key, None), val)
                                  for key, val in kwargs.items()]):
                            yield child
                    for c in helper(child):
                        if c is not None:
                            yield c

        return helper(self)

    @property
    def parent_chain(self) -> List[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.parent_chain', 8)}
        def _parent_chain(node):
            yield node
            if node.parent is not None:
                for p in _parent_chain(node.parent):
                    yield p

        return list(_parent_chain(self))

    def __repr__(self) -> str:
        return self.image

    @property
    def tokens(self) -> Iterator[Token]:
        ${py_doc('langkit.python.root_node.tokens', 8)}
        start = self.token_start
        end = self.token_end

        # All nodes have non-null start/end tokens
        assert start is not None
        assert end is not None

        while not start == end:
            yield start
            next = start.next
            assert next is not None
            start = next
        yield end

    def to_data(self) -> Union[list, dict]:
        ${py_doc('langkit.python.root_node.to_data', 8)}
        if self.is_list_type:
            return [i.to_data() for i in self if i is not None]
        else:
            return {n: v.to_data()
                    for n, v in self.iter_fields()
                    if v is not None}

    def to_json(self) -> str:
        """
        Return a JSON representation of this node.
        """
        return json.dumps(self.to_data())

    def is_a(self, *types: Type[${root_astnode_name}]) -> bool:
        """
        Shortcut for isinstance(self, types).
        :rtype: bool
        """
        return isinstance(self, tuple(types))

    if TYPE_CHECKING:
        T = TypeVar('T', bound=${root_astnode_name})

    def cast(self, typ: Type[T]) -> T:
        """
        Fluent interface style method. Return ``self``, raise an error if self
        is not of type ``typ``.

        :type typ: () -> T
        :rtype: T
        """
        assert isinstance(self, typ)
        return self

    _node_c_type = _hashable_c_pointer()

    @classmethod
    def _wrap(cls, c_value):
        """
        Internal helper to wrap a low-level entity value into an instance of
        the the appropriate high-level Python wrapper subclass.
        """
        node_c_value = c_value.node
        if not node_c_value:
            return None

        rebindings = c_value.info.rebindings
        metadata = c_value.info.md

        # Look for an already existing wrapper for this node
        cache_key = (node_c_value, metadata, rebindings)
        unit = cls._fetch_unit(c_value)
        unit._check_node_cache()
        try:
            return unit._node_cache[cache_key]
        except KeyError:
            pass

        # Pick the right subclass to materialize this node in Python
        kind = _node_kind(ctypes.byref(c_value))
        result = _kind_to_astnode_cls[kind](c_value, node_c_value, rebindings)
        unit._node_cache[cache_key] = result
        return result

    @classmethod
    def _wrap_bare_node(cls, c_value: Any) -> Opt[${root_astnode_name}]:
        return cls._wrap(${c_entity}.from_bare_node(c_value))

    @classmethod
    def _unwrap(cls, py_value: Opt[${root_astnode_name}]) -> Any:
        """
        Internal helper to unwrap a high-level ASTNode instance into a
        low-level value. Raise a TypeError if the input value has unexpected
        type.
        """
        if py_value is None:
            return ${c_entity}._null_value
        elif not isinstance(py_value, ${root_astnode_name}):
            _raise_type_error(${repr(root_astnode_name)}, py_value)
        else:
            return py_value._c_value

    @property
    def _unsafe_unwrap(self) -> Any:
        """
        Unsafe version of _unwrap, meant for internal uses where we don't want
        to check whether the reference is stale or not.
        """
        return self._unprotected_c_value

    @property
    def _unwrap_einfo(self):
        return self._c_value.info

    @classmethod
    def _fetch_unit(cls, c_value: Any) -> AnalysisUnit:
        return ${pyapi.wrap_value('_node_unit(ctypes.byref(c_value))',
                                  T.AnalysisUnit)}

    def _eval_field(self, c_result: Any, c_accessor: Any, *c_args: Any) -> Any:
        """
        Internal helper to evaluate low-level field accessors/properties.

        This calls "c_accessor" on this node with the input arguments and puts
        the result in "c_result". This raises a PropertyError if the evaluation
        failed. Return "c_result" for convenience.
        """
        args = (self._unwrap(self), ) + c_args + (ctypes.byref(c_result), )
        if not c_accessor(*args):
            raise PropertyError()
        return c_result

    def _eval_astnode_field(self, c_accessor: Any) -> Any:
        """
        Internal helper. Wrapper around _eval_field for fields that return an
        AST node and that accept no explicit argument. This is useful as it's
        the most common case of field, so using this wrapper reduces generated
        code length.
        """
        return ${root_astnode_name}._wrap(
            self._eval_field(${c_entity}(), c_accessor)
        )


% for astnode in ctx.astnode_types:
    % if astnode != T.root_node:
${astnode_types.decl(astnode)}
    % endif
% endfor


class _EnvRebindingsType_c_type(ctypes.Structure):
    _fields_ = [("version", ctypes.c_uint64)]


_EnvRebindings_c_type = _hashable_c_pointer(_EnvRebindingsType_c_type)


${struct_types.base_decls()}

% for struct_type in ctx.struct_types:
    ## Emit a single C type for all entities, as they are all ABI compatible.
    ## We emit them as a special case as we just need the C structure layout:
    ## the node/entity wrapper will take care of the rest.
    % if struct_type.is_entity_type:
        % if struct_type is root_entity:
class ${c_entity}(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
        ${struct_types.ctype_fields(struct_type)}
    )
    _null_value: ClassVar[${c_entity}]

    @classmethod
    def from_bare_node(cls, node_c_value):
        return cls(node_c_value, ${c_entity_info}._null_value)
        % endif
    ## Likewise for entity info structures: they will never be wrapped
    % elif struct_type is T.entity_info:
class ${c_entity_info}(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
        ${struct_types.ctype_fields(struct_type)}
    )
    _null_value: ClassVar[${c_entity_info}]
    ## Likewise for metadata structures
    % elif struct_type is T.env_md:
class ${c_metadata}(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
        ${struct_types.ctype_fields(struct_type)}
    )
    _null_value: ClassVar[${c_metadata}]

    @property
    def as_tuple(self):
        return tuple(getattr(self, f) for f, _ in self._fields_)

    def __eq__(self, other):
        return (isinstance(other, type(self)) and
                self.as_tuple == other.as_tuple)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(self.as_tuple)
    ## Emit other (and regular) structures
    % elif struct_type.exposed:
${struct_types.decl(struct_type)}
    % endif
% endfor


${c_metadata}._null_value = ${pyapi.c_type(T.env_md)}(${', '.join(
    'None' if f.type.is_entity_type else 'False'
    for f in T.env_md.get_fields()
)})
${c_entity_info}._null_value = ${c_entity_info}(${c_metadata}._null_value,
                                                None)


#
# Low-level binding - Second part
#

# For performance, allocate a single C API entity for all uses of null
# entities.
${c_entity}._null_value = ${c_entity}()
${c_entity}._null_value.node = None

${array_types.base_decl()}
% for array_type in ctx.array_types:
    % if array_type.exposed and array_type.emit_c_type:
${array_types.decl(array_type)}
    % endif
% endfor

${iterator_types.base_decl()}
% for iterator_type in ctx.iterator_types:
    % if iterator_type.exposed and iterator_type.emit_c_type:
${iterator_types.decl(iterator_type)}
    % endif
% endfor


_free = _import_func(
    '${capi.get_name("free")}',
    [ctypes.c_void_p], None
)

_destroy_text = _import_func(
    '${capi.get_name("destroy_text")}', [ctypes.POINTER(_text)], None
)

_symbol_text = _import_func(
    '${capi.get_name("symbol_text")}',
    [ctypes.POINTER(_symbol_type), ctypes.POINTER(_text)], None
)

_get_versions = _import_func(
    '${capi.get_name("get_versions")}',
    [ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)], None
)

# Analysis primitives
_allocate_analysis_context = _import_func(
    '${capi.get_name("allocate_analysis_context")}',
    [],
    AnalysisContext._c_type,
)
_initialize_analysis_context = _import_func(
    '${capi.get_name("initialize_analysis_context")}',
    [AnalysisContext._c_type, # context
     ctypes.c_char_p,         # charset
     _file_reader,            # file_reader
     _unit_provider,          # unit_provider
     _event_handler,          # event_handler
     ctypes.c_int,            # with_trivia
     ctypes.c_int],           # tab_stop
    None,
)
_context_incref = _import_func(
    '${capi.get_name("context_incref")}',
    [AnalysisContext._c_type], AnalysisContext._c_type
)
_context_decref = _import_func(
    '${capi.get_name("context_decref")}',
    [AnalysisContext._c_type], None
)
_context_symbol = _import_func(
    '${capi.get_name("context_symbol")}',
    [AnalysisContext._c_type,
     ctypes.POINTER(_text),
     ctypes.POINTER(_symbol_type)], ctypes.c_int
)
_discard_errors_in_populate_lexical_env = _import_func(
   '${capi.get_name("context_discard_errors_in_populate_lexical_env")}',
   [AnalysisContext._c_type, ctypes.c_int], None
)
_get_analysis_unit_from_file = _import_func(
    '${capi.get_name("get_analysis_unit_from_file")}',
    [AnalysisContext._c_type,  # context
     ctypes.c_char_p,          # filename
     ctypes.c_char_p,          # charset
     ctypes.c_int,             # reparse
     ctypes.c_int],            # grammar rule
    AnalysisUnit._c_type
)
_get_analysis_unit_from_buffer = _import_func(
    '${capi.get_name("get_analysis_unit_from_buffer")}',
    [AnalysisContext._c_type,  # context
     ctypes.c_char_p,          # filename
     ctypes.c_char_p,          # charset
     ctypes.c_char_p,          # buffer
     ctypes.c_size_t,          # buffer_size
     ctypes.c_int],            # grammar rule
    AnalysisUnit._c_type
)
% if ctx.default_unit_provider:
_get_analysis_unit_from_provider = _import_func(
    '${capi.get_name("get_analysis_unit_from_provider")}',
    [AnalysisContext._c_type,  # context
     ctypes.POINTER(_text),    # name
     ctypes.c_int,             # kind
     ctypes.c_char_p,          # charset
     ctypes.c_int],            # reparse
    AnalysisUnit._c_type
)
% endif
_unit_root = _import_func(
    '${capi.get_name("unit_root")}',
    [AnalysisUnit._c_type, ctypes.POINTER(${c_entity})], None
)
_unit_first_token = _import_func(
    "${capi.get_name('unit_first_token')}",
    [AnalysisUnit._c_type, Token._c_type], None
)
_unit_last_token = _import_func(
    "${capi.get_name('unit_last_token')}",
    [AnalysisUnit._c_type, Token._c_type], None
)
_unit_token_count = _import_func(
    "${capi.get_name('unit_token_count')}",
    [AnalysisUnit._c_type], ctypes.c_int
)
_unit_trivia_count = _import_func(
    "${capi.get_name('unit_trivia_count')}",
    [AnalysisUnit._c_type], ctypes.c_int
)
_unit_lookup_token = _import_func(
    "${capi.get_name('unit_lookup_token')}",
    [AnalysisUnit._c_type,
     ctypes.POINTER(Sloc._c_type),
     Token._c_type],
    None
)
_unit_dump_lexical_env = _import_func(
    "${capi.get_name('unit_dump_lexical_env')}",
    [AnalysisUnit._c_type], None
)
_unit_filename = _import_func(
    "${capi.get_name('unit_filename')}",
    [AnalysisUnit._c_type], ctypes.POINTER(ctypes.c_char)
)
_unit_diagnostic_count = _import_func(
    '${capi.get_name("unit_diagnostic_count")}',
    [AnalysisUnit._c_type], ctypes.c_uint
)
_unit_diagnostic = _import_func(
    '${capi.get_name("unit_diagnostic")}',
    [AnalysisUnit._c_type, ctypes.c_uint, ctypes.POINTER(Diagnostic._c_type)],
    ctypes.c_int
)
_unit_context = _import_func(
    '${capi.get_name("unit_context")}',
    [AnalysisUnit._c_type], AnalysisContext._c_type
)
_unit_reparse_from_file = _import_func(
    '${capi.get_name("unit_reparse_from_file")}',
    [AnalysisUnit._c_type,    # unit
     ctypes.c_char_p],        # charset
    ctypes.c_int
)
_unit_reparse_from_buffer = _import_func(
    '${capi.get_name("unit_reparse_from_buffer")}',
    [AnalysisUnit._c_type, # unit
     ctypes.c_char_p,      # charset
     ctypes.c_char_p,      # buffer
     ctypes.c_size_t],     # buffer_size
    None
)
_unit_populate_lexical_env = _import_func(
    '${capi.get_name("unit_populate_lexical_env")}',
    [
        AnalysisUnit._c_type,
        % if ctx.ple_unit_root:
            ctypes.c_int,
        % endif
    ],
    ctypes.c_int
)

# General AST node primitives
_node_hash = _import_func(
    '${capi.get_name("node_hash")}',
    [ctypes.POINTER(${c_entity})], ctypes.c_uint32
)

_node_is_equivalent = _import_func(
    '${capi.get_name("node_is_equivalent")}',
    [ctypes.POINTER(${c_entity}),
     ctypes.POINTER(${c_entity})], ctypes.c_uint8
)

_node_kind = _import_func(
    '${capi.get_name("node_kind")}',
    [ctypes.POINTER(${c_entity})], ctypes.c_int
)
_node_unit = _import_func(
    '${capi.get_name("node_unit")}',
    [ctypes.POINTER(${c_entity})], AnalysisUnit._c_type
)
_node_is_token_node = _import_func(
    '${capi.get_name("node_is_token_node")}',
    [ctypes.POINTER(${c_entity})], ctypes.c_int
)
_node_is_synthetic = _import_func(
    '${capi.get_name("node_is_synthetic")}',
    [ctypes.POINTER(${c_entity})], ctypes.c_int
)
_node_image = _import_func(
    '${capi.get_name("node_image")}',
    [ctypes.POINTER(${c_entity}), ctypes.POINTER(_text)], None
)
_node_text = _import_func(
    '${capi.get_name("node_text")}',
    [ctypes.POINTER(${c_entity}), ctypes.POINTER(_text)], None
)
_node_sloc_range = _import_func(
    '${capi.get_name("node_sloc_range")}',
    [ctypes.POINTER(${c_entity}), ctypes.POINTER(SlocRange._c_type)], None
)
_lookup_in_node = _import_func(
    '${capi.get_name("lookup_in_node")}',
    [ctypes.POINTER(${c_entity}),
     ctypes.POINTER(Sloc._c_type),
     ctypes.POINTER(${c_entity})], None
)
_node_children_count = _import_func(
    '${capi.get_name("node_children_count")}',
    [ctypes.POINTER(${c_entity})], ctypes.c_uint
)
_node_child = _import_func(
    '${capi.get_name("node_child")}',
    [ctypes.POINTER(${c_entity}), ctypes.c_uint, ctypes.POINTER(${c_entity})],
    ctypes.c_int
)

% for astnode in ctx.astnode_types:
    % for field in astnode.fields_with_accessors():
_${field.accessor_basename.lower} = _import_func(
    '${capi.get_name(field.accessor_basename)}',
    [ctypes.POINTER(${c_entity}),
     % for arg in field.arguments:
        <%
            type_expr = pyapi.c_type(arg.public_type)
            if arg.public_type.is_ada_record:
                type_expr = 'ctypes.POINTER({})'.format(type_expr)
        %>
        ${type_expr},
     % endfor
     ctypes.POINTER(${pyapi.c_type(field.public_type)})],
    ctypes.c_int
)
    % endfor
% endfor

# File readers
_dec_ref_file_reader = _import_func(
    '${capi.get_name("dec_ref_file_reader")}',
    [_file_reader], None
)
${exts.include_extension(
   ctx.ext('python_api', 'file_readers', 'low_level_bindings')
)}

# Event handlers
_event_handler_destroy_func = ctypes.CFUNCTYPE(None, ctypes.py_object)
_event_handler_unit_requested_func = ctypes.CFUNCTYPE(
    None,
    ctypes.py_object,        # data
    AnalysisContext._c_type, # context
    ctypes.POINTER(_text),   # name
    AnalysisUnit._c_type,    # from
    ctypes.c_uint8,          # found
    ctypes.c_uint8,          # is_not_found_error
)
_event_handler_unit_parsed_func = ctypes.CFUNCTYPE(
    None,
    ctypes.py_object,        # data
    AnalysisContext._c_type, # context
    AnalysisUnit._c_type,    # unit
    ctypes.c_uint8,          # reparsed
)
_create_event_handler = _import_func(
    '${capi.get_name("create_event_handler")}',
    [
        ctypes.py_object,
        _event_handler_destroy_func,
        _event_handler_unit_requested_func,
        _event_handler_unit_parsed_func,
    ],
    _event_handler,
)
_dec_ref_event_handler = _import_func(
    '${capi.get_name("dec_ref_event_handler")}', [_event_handler], None
)

# Unit providers
_dec_ref_unit_provider = _import_func(
    '${capi.get_name("dec_ref_unit_provider")}',
    [_unit_provider], None
)
${exts.include_extension(
   ctx.ext('python_api', 'unit_providers', 'low_level_bindings')
)}

# Misc
_token_get_kind = _import_func(
    "${capi.get_name('token_get_kind')}", [Token._c_type], ctypes.c_int
)
_token_kind_name = _import_func(
    "${capi.get_name('token_kind_name')}",
    [ctypes.c_int], ctypes.POINTER(ctypes.c_char)
)
_token_sloc_range = _import_func(
    "${capi.get_name('token_sloc_range')}",
    [Token._c_type, ctypes.POINTER(SlocRange._c_type)], None
)
_token_next = _import_func(
    "${capi.get_name('token_next')}",
    [Token._c_type, Token._c_type], None
)
_token_is_equivalent = _import_func(
    "${capi.get_name('token_is_equivalent')}",
    [Token._c_type, Token._c_type], ctypes.c_int
)
_token_previous = _import_func(
    "${capi.get_name('token_previous')}",
    [Token._c_type, Token._c_type], None
)
_token_range_text = _import_func(
    "${capi.get_name('token_range_text')}",
    [Token._c_type, Token._c_type, ctypes.POINTER(_text)],
    ctypes.c_int
)


#
# Layering helpers
#

def _unwrap_str(c_char_p_value: Any) -> str:
    """
    Assuming c_char_p_value is a valid char*, convert it to a native Python
    string and free the C pointer.
    """
    result = ctypes.c_char_p(ctypes.addressof(c_char_p_value.contents)).value
    _free(c_char_p_value)
    return (result or b'').decode()


_kind_to_astnode_cls = {
    % for subclass in ctx.astnode_types:
        % if not subclass.abstract:
    ${ctx.node_kind_constants[subclass]}: ${pyapi.type_public_name(subclass)},
        % endif
    % endfor
}


def _field_address(struct: ctypes.Structure, field_name: str) -> int:
    """
    Get the address of a structure field from a structure value.

    For instance::

        class Foo(ctypes.Structure):
            _fields_ = [('i', ctypes.c_int)]

        f = Foo()
        i_addr =_field_address(f, 'i')
    """
    struct_type = type(struct)
    struct_addr = ctypes.addressof(struct)
    field = getattr(struct_type, field_name)
    field_type = None
    for field_desc in struct_type._fields_:
        f_name = field_desc[0]
        f_type = field_desc[1]
        if f_name == field_name:
            field_type = f_type
            break
    assert field_type is not None
    return struct_addr + field.offset

def _extract_versions() -> Tuple[str, str]:
    v_ptr = ctypes.c_char_p()
    bd_ptr = ctypes.c_char_p()
    _get_versions(ctypes.byref(v_ptr), ctypes.byref(bd_ptr))

    _version = v_ptr.value
    assert isinstance(_version, bytes)
    version = _version.decode()
    _free(v_ptr)

    _build_version = bd_ptr.value
    assert isinstance(_build_version, bytes)
    build_version = _build_version.decode()
    _free(bd_ptr)

    return version, build_version

version, build_date = _extract_versions()


#
# Language specific extensions #
#

${exts.include_extension(ctx.ext("python"))}

#
# App base class
#

class App:
    """
    Base class to regroup logic for an app. We use a class so that
    specific languages implementations can add specific arguments and
    processing by overriding specific methods:

    - `main`, which will be the main method of the app.

    - `add_arguments` to add arguments to the argparse.Parser instance

    - `create_unit_provider` to return a custom unit provider to be used by the
      AnalysisContext.

    - `description` to change the description of the app.

    Inside of `main`, the user can access app specific state:

    - `self.units` is a map of filenames to analysis units.
    - `self.ctx` is the analysis context.
    - `self.u` is the last parsed unit.

    The user can then run the app by calling `App.run()`.

    Here is a small example of an app subclassing `App`, that will simply print
    the tree of every unit passed as argument:

    .. code-block:: python

        from ${module_name} import App


        class ExampleApp(App):
            def main(self):
                for u in self.units.values():
                    print u.filename
                    print u.root.dump()

        ExampleApp.run()
    """

    parser: argparse.ArgumentParser
    args: argparse.Namespace
    u: AnalysisUnit
    units: Dict[str, AnalysisUnit]
    ctx: AnalysisContext

    @property
    def description(self) -> str:
        """
        Description for this app. Empty by default.
        """
        return ""

    def __init__(self, args: Opt[List[str]] = None):
        self.parser = argparse.ArgumentParser(description=self.description)
        self.parser.add_argument('files', nargs='*', help='Files')
        self.add_arguments()

        # Parse command line arguments
        self.args = self.parser.parse_args(args)

        self.ctx = AnalysisContext(
            charset='utf-8',
            unit_provider=self.create_unit_provider(),
            event_handler=self.create_event_handler(),
            with_trivia=True,
        )

        files = self.args.files
        if not files:
            files = self.default_get_files()

        # Parse files
        self.units = {}
        for file_name in files:
            self.u = self.ctx.get_from_file(file_name)
            if self.u.diagnostics:
                self.on_parsing_errors(self.u)
            self.units[file_name] = self.u

    def on_parsing_errors(self, unit: AnalysisUnit) -> None:
        """
        Callback invoked during App initialization, when a requested unit has a
        parsing error. By default, print the error on the standard output, but
        subclasses can override this behavior.
        """
        for d in unit.diagnostics:
            print(unit.format_gnu_diagnostic(d))

    def default_get_files(self) -> List[str]:
        """
        When no files are passed by the user on the command line, this method
        will be used. By default, it returns an empty list, but Langkit
        libraries can overload it to customize the behavior to adapt it to the
        specific language and toolchain.
        """
        return []

    def add_arguments(self) -> None:
        """
        Hook for subclasses to add arguments to self.parser. Default
        implementation does nothing.
        """
        pass

    def create_unit_provider(self) -> Opt[UnitProvider]:
        """
        Hook for subclasses to return a custom unit provider.
        Default implementation returns None.
        """
        return None

    def create_event_handler(self) -> Opt[EventHandler]:
        """
        Hook for subclasses to return a custom event handler. Default
        implementation returns None.
        """
        return None

    def main(self) -> None:
        """
        Default implementation for App.main: just iterates on every units and
        call ``process_unit`` on it.
        """
        for u in sorted(self.units.values(), key=lambda u: u.filename):
            self.process_unit(u)

    def process_unit(self, unit: AnalysisUnit) -> None:
        """
        Abstract method that processes one unit. Needs to be subclassed by
        implementors.
        """
        raise NotImplementedError()

    @classmethod
    def run(cls, args: Opt[List[str]]=None) -> None:
        """
        Instantiate and run this application.
        """
        cls(args).main()

    ${exts.include_extension(ctx.ext('python_api/app_exts'))}
