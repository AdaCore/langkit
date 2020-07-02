## vim: filetype=makopython

"""
Python binding of the ${ctx.lib_name.camel} API.

Please consider all exported entities whose names that start with an underscore
("_") as internal implementation details. They are not meant to be used
directly.
"""

<%namespace name="array_types"   file="array_types_py.mako" />
<%namespace name="astnode_types" file="astnode_types_py.mako" />
<%namespace name="struct_types"  file="struct_types_py.mako" />
<%namespace name="exts"          file="/extensions.mako" />

from __future__ import absolute_import, division, print_function


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
import json
import os
import sys
import weakref

import ${pyapi.module_name}._py2to3 as _py2to3


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
        # In Python3, turn information into native strings, i.e. decode bytes.
        # These strings are only informative, so do not raise an error if
        # decoding fails: do best effort decoding instead to be as helpful as
        # possible.
        info = self.information
        if not _py2to3.python2:
            info = info.decode(errors='replace')
        return _exception_kind_to_type[self.kind](info)


def _type_fullname(t):
    """
    Return the fully qualified name for the given `t` type.
    """
    name = t.__name__
    module = t.__module__
    return (name
            if module in (None, object.__class__.__module__) else
            '{}.{}'.format(module, name))


def _raise_type_error(expected_type_name, actual_value):
    raise TypeError('{} instance expected, got {} instead'.format(
        expected_type_name, _type_fullname(type(actual_value))
    ))


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
            return ctypes.addressof(self.contents)

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
    def _unwrap(cls, value):
        value = cls.cast(value)

        text = value.encode(cls.encoding)
        text_buffer = ctypes.create_string_buffer(text)
        text_buffer_ptr = ctypes.cast(
            ctypes.pointer(text_buffer),
            ctypes.POINTER(ctypes.c_char)
        )
        result = _text(text_buffer_ptr, len(value))
        result.text_buffer = text_buffer
        return result

    def _wrap(self):
        if self.length > 0:
            # self.length tells how much UTF-32 chars there are in self.chars
            # but self.chars is a char* so we have to fetch 4 times more bytes
            # than characters.
            return self.chars[:4 * self.length].decode(self.encoding)
        else:
            return u''

    @classmethod
    def cast(cls, value):
        """
        Try to cast ``value`` into an unicode object. Raise a TypeError, or
        raise a string decoding error when this is not possible.
        """
        if isinstance(value, _py2to3.bytes_type):
            return value.decode('ascii')
        elif not isinstance(value, _py2to3.text_type):
            _raise_type_error('text string', value)
        else:
            return value

    def __del__(self):
        _destroy_text(ctypes.byref(self))


class _symbol_type(ctypes.Structure):
    _fields_ = [('data', ctypes.c_void_p),
                ('bounds', ctypes.c_void_p)]

    @classmethod
    def wrap(cls, c_value):
        # First extract the text associated to this symbol in "text"
        text = _text()
        _symbol_text(ctypes.byref(c_value), ctypes.byref(text))

        # Then wrap this text
        return text._wrap()

    @classmethod
    def unwrap(cls, py_value, context):
        # First turn the given symbol into a low-level text object
        text = _text._unwrap(py_value)

        # Then convert it to a symbol
        result = cls()
        if not _context_symbol(context, ctypes.byref(text),
                               ctypes.byref(result)):
            raise InvalidSymbolError(py_value)
        return result


class _big_integer(object):

    class c_type(ctypes.c_void_p):
        pass

    def __init__(self, c_value):
        self.c_value = c_value

    @classmethod
    def unwrap(cls, value):
        if not _py2to3.is_int(value):
            _raise_type_error('int or long', value)

        text = _text._unwrap(str(value))
        c_value = cls.create(ctypes.byref(text))
        return cls(c_value)

    @classmethod
    def wrap(cls, c_value):
        helper = cls(c_value)
        text = _text()
        cls.text(helper.c_value, ctypes.byref(text))
        return int(text._wrap())

    def clear(self):
        self.c_value = None

    def __del__(self):
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


class _Enum(object):

    _name = None
    """
    Name for this enumeration type.
    :type: str
    """

    _c_to_py = None
    """
    Mapping from C values to user-level Python values.
    :type: list[str]
    """

    _py_to_c = None
    """
    Mapping from user-level Python values to C values.
    :type: dict[str, int]
    """

    @classmethod
    def _unwrap(cls, py_value):
        if not isinstance(py_value, str):
            _raise_type_error('str', py_value)
        try:
            return _py2to3.text_to_bytes(cls._py_to_c[py_value])
        except KeyError:
            raise ValueError('Invalid {}: {}'.format(cls._name, py_value))

    @classmethod
    def _wrap(cls, c_value):
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


_unit_provider = _hashable_c_pointer()


def _canonicalize_buffer(buffer, charset):
    """Canonicalize source buffers to be bytes buffers."""
    if isinstance(buffer, _py2to3.text_type):
        if charset:
            raise TypeError('`charset` must be null when the buffer is'
                            ' Unicode')
        buffer = buffer.encode('utf-8')
        charset = b'utf-8'
    elif not isinstance(buffer, _py2to3.bytes_type):
        raise TypeError('`buffer` must be a string')
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


class AnalysisContext(object):
    ${py_doc('langkit.analysis_context_type', 4)}

    __slots__ = ('_c_value', '_unit_provider', '_serial_number', '_unit_cache',
                 '__weakref__')

    _context_cache = weakref.WeakValueDictionary()
    """
    Cache for analysis context wrappers. Indexed by analysis context addresses,
    which are known to stay valid forever (and re-used).

    Unlike unit and node caches, this one should contain weak references so
    that analysis contexts (and their units/nodes) can be free'd when user code
    does not reference them anymore.

    :type: dict[AnalysisContext._c_type, AnalysisContext]
    """

    def __init__(self,
                 charset=None,
                 unit_provider=None,
                 with_trivia=True,
                 tab_stop=${ctx.default_tab_stop},
                 _c_value=None):
        ${py_doc('langkit.create_context', 8)}

        # Initialize this field in case we raise an exception during
        # construction, so that the destructor can run later on.
        self._c_value = None

        if _c_value is None:
            charset = _py2to3.text_to_bytes(charset)
            if not isinstance(tab_stop, int) or tab_stop < 1:
                raise ValueError(
                    'Invalid tab_stop (positive integer expected)')
            c_unit_provider = unit_provider._c_value if unit_provider else None
            self._c_value = _create_analysis_context(charset, c_unit_provider,
                                                     with_trivia, tab_stop)
        else:
            self._c_value = _context_incref(_c_value)
        assert self._c_value not in self._context_cache
        self._context_cache[self._c_value] = self

        # Keep a reference to the unit provider so that it is live at least as
        # long as the analysis context is live.
        self._unit_provider = unit_provider

        self._serial_number = None
        self._unit_cache = {}
        """
        Cache for AnalysisUnit wrappers, indexed by analysis unit addresses,
        which are known to stay valid as long as the context is alive.

        :type: dict[str, AnalysisUnit]
        """

        self._check_unit_cache()

    def __del__(self):
        if self._c_value:
            _context_decref(self._c_value)

    def __eq__(self, other):
        return self._c_value == other._c_value

    def __hash__(self):
        return hash(self._c_value)

    def get_from_file(self, filename, charset=None, reparse=False,
                      rule=default_grammar_rule):
        ${py_doc('langkit.get_unit_from_file', 8)}
        filename = _py2to3.text_to_bytes(filename)
        charset = _py2to3.text_to_bytes(charset or '')
        c_value = _get_analysis_unit_from_file(self._c_value, filename,
                                               charset, reparse,
                                               GrammarRule._unwrap(rule))
        return AnalysisUnit._wrap(c_value)

    def get_from_buffer(self, filename, buffer, charset=None, reparse=False,
                        rule=default_grammar_rule):
        ${py_doc('langkit.get_unit_from_buffer', 8)}
        filename = _py2to3.text_to_bytes(filename)
        charset = _py2to3.text_to_bytes(charset or '')
        buffer, charset = _canonicalize_buffer(buffer, charset)
        c_value = _get_analysis_unit_from_buffer(self._c_value, filename,
                                                 charset,
                                                 buffer, len(buffer),
                                                 GrammarRule._unwrap(rule))
        return AnalysisUnit._wrap(c_value)

    def get_from_provider(self, name, kind, charset=None, reparse=False):
        ${py_doc('langkit.get_unit_from_provider', 8)}
        name = _py2to3.bytes_to_text(name)
        charset = _py2to3.text_to_bytes(charset or '')

        _name = _text._unwrap(name)
        _kind = ${pyapi.unwrap_value('kind', T.AnalysisUnitKind, None)}
        c_value = _get_analysis_unit_from_provider(
            self._c_value, ctypes.byref(_name), _kind, charset, reparse
        )
        if c_value:
            return AnalysisUnit._wrap(c_value)
        else:
            raise InvalidUnitNameError('Invalid unit name: {} ({})'.format(
                repr(name), kind
            ))

    def discard_errors_in_populate_lexical_env(self, discard):
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


class AnalysisUnit(object):
    ${py_doc('langkit.analysis_unit_type', 4)}

    __slots__ = ('_c_value', '_context_link', '_cache_version_number',
                 '_node_cache')

    class TokenIterator(object):
        """Iterator over the tokens in an analysis unit."""
        def __init__(self, first):
            self.first = first

        def __iter__(self):
            return self

        def __next__(self):
            if not self.first:
                raise StopIteration()
            result = self.first
            self.first = self.first.next
            return result
        next = __next__

    def __init__(self, context, c_value):
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

        self._cache_version_number = None
        """
        Last version number we saw for this analysis unit wrapper. If it's
        different from `self._unit_version`, it means that the unit was
        reparsed: in this case we need to clear the node cache below (see the
        `_check_node_cache` method).

        :type: int
        """

        self._node_cache = {}
        """
        Cache for all node wrappers in this unit. Indexed by couples:
        (c_value, metadata, rebindings).

        :type: dict[T, ${root_astnode_name}]
        """

        self._check_node_cache()

    def __eq__(self, other):
        return self._c_value == other._c_value

    def __hash__(self):
        return hash(self._c_value)

    @property
    def context(self):
        ${py_doc('langkit.unit_context', 8)}
        return self._context_link

    def reparse(self, buffer=None, charset=None):
        ${py_doc('langkit.unit_reparse_generic', 8)}
        charset = _py2to3.text_to_bytes(charset or '')
        if buffer is None:
            _unit_reparse_from_file(self._c_value, charset)
        else:
            buffer, charset = _canonicalize_buffer(buffer, charset)
            _unit_reparse_from_buffer(self._c_value, charset, buffer,
                                      len(buffer))

    def populate_lexical_env(self):
        ${py_doc('langkit.unit_populate_lexical_env', 8)}
        if not _unit_populate_lexical_env(self._c_value):
            raise PropertyError()

    @property
    def root(self):
        ${py_doc('langkit.unit_root', 8, rtype=T.root_node)}
        result = ${c_entity}()
        _unit_root(self._c_value, ctypes.byref(result))
        return ${root_astnode_name}._wrap(result)

    @property
    def first_token(self):
        ${py_doc('langkit.unit_first_token', 8)}
        result = Token()
        _unit_first_token(self._c_value, ctypes.byref(result))
        return result._wrap()

    @property
    def last_token(self):
        ${py_doc('langkit.unit_last_token', 8)}
        result = Token()
        _unit_last_token(self._c_value, ctypes.byref(result))
        return result._wrap()

    @property
    def text(self):
        ${py_doc('langkit.unit_text', 8)}
        return Token.text_range(self.first_token, self.last_token)

    @property
    def token_count(self):
        ${py_doc('langkit.unit_token_count', 8)}
        return _unit_token_count(self._c_value)

    @property
    def trivia_count(self):
        ${py_doc('langkit.unit_trivia_count', 8)}
        return _unit_trivia_count(self._c_value)

    def lookup_token(self, sloc):
        ${py_doc('langkit.unit_lookup_token', 8)}
        unit = AnalysisUnit._unwrap(self)
        _sloc = Sloc._c_type._unwrap(sloc)
        tok = Token()
        _unit_lookup_token(unit, ctypes.byref(_sloc), ctypes.byref(tok))
        return tok._wrap()

    def _dump_lexical_env(self):
        ${py_doc('langkit.unit_dump_lexical_env', 8)}
        unit = AnalysisUnit._unwrap(self)
        _unit_dump_lexical_env(unit)

    def iter_tokens(self):
        """
        Return an iterator that yields all the tokens in this unit.
        """
        return self.TokenIterator(self.first_token)

    @property
    def filename(self):
        ${py_doc('langkit.unit_filename', 8)}
        filename = _unit_filename(self._c_value)
        return _unwrap_str(filename)

    @property
    def diagnostics(self):
        """Diagnostics for this unit."""
        count = _unit_diagnostic_count(self._c_value)
        result = []
        diag = Diagnostic._c_type()
        for i in range(count):
            assert _unit_diagnostic(self._c_value, i, ctypes.byref(diag))
            result.append(diag._wrap())
        return result

    def __repr__(self):
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
    def _context(cls, c_value):
        ctx = _unit_context(c_value)
        return AnalysisContext._wrap(ctx)

    @property
    def _unit_version(self):
        return self._c_value.contents.unit_version

    def _check_node_cache(self):
        """
        If this unit has been reparsed, invalidate its node cache.
        """
        if self._cache_version_number != self._unit_version:
            self._node_cache = {}
            self._cache_version_number = self._unit_version


class Sloc(object):
    ${py_doc('langkit.sloc_type', 4)}

    def __init__(self, line, column):
        assert line >= 0 and column >= 0
        self.line = line
        self.column = column

    def __bool__(self):
        return bool(self.line or self.column)
    __nonzero__ = __bool__

    def __lt__(self, other):
        # First compare line numbers...
        if self.line < other.line:
            return True
        elif self.line > other.line:
            return False

        # Past this point, we know that both are on the same line, so now
        # compare column numbers.
        else:
            return self.column < other.column

    def __eq__(self, other):
        return self.line == other.line and self.column == other.column

    def __hash__(self):
        return hash((self.line, self.column))

    def __str__(self):
        return '{}:{}'.format(self.line, self.column)

    def __repr__(self):
        return '<Sloc {} at {:#x}>'.format(self, id(self))

    class _c_type(ctypes.Structure):
        _fields_ = [("line", ctypes.c_uint32),
                    ("column", ctypes.c_uint16)]

        def _wrap(self):
            return Sloc(self.line, self.column)

        @classmethod
        def _unwrap(cls, sloc):
            return cls(sloc.line, sloc.column)


class SlocRange(object):
    ${py_doc('langkit.sloc_range_type', 4)}

    def __init__(self, start, end):
        self.start = start
        self.end = end

    def __bool__(self):
        return bool(self.start or self.end)
    __nonzero__ = __bool__

    def __lt__(self, other):
        raise NotImplementedError('SlocRange comparison not supported')

    def __eq__(self, other):
        return self.start == other.start and self.end == other.end

    def __hash__(self):
        return hash((self.start, self.end))

    def __str__(self):
        return '{}-{}'.format(self.start, self.end)

    def __repr__(self):
        return "<SlocRange {}:{}-{}:{}>".format(
            self.start.line, self.start.column,
            self.end.line, self.end.column
        )


    class _c_type(ctypes.Structure):
        _fields_ = [("start", Sloc._c_type),
                    ("end", Sloc._c_type)]

        def _wrap(self):
            return SlocRange(self.start._wrap(), self.end._wrap())


class Diagnostic(object):
    ${py_doc('langkit.diagnostic_type', 4)}

    def __init__(self, sloc_range, message):
        self.sloc_range = sloc_range
        self.message = message

    @property
    def as_text(self):
        return (u'{}: {}'.format(self.sloc_range, self.message)
                if self.sloc_range else
                self.message)

    def __str__(self):
        result = self.as_text
        if _py2to3.python2:
            result = result.encode('ascii', errors='replace')
        return result

    def __repr__(self):
        return '<Diagnostic {}>'.format(self)


    class _c_type(ctypes.Structure):
        _fields_ = [('sloc_range', SlocRange._c_type),
                    ('message', _text)]

        def _wrap(self):
            return Diagnostic(self.sloc_range._wrap(), self.message._wrap())


class Token(ctypes.Structure):
    ${py_doc('langkit.token_reference_type', 4)}

    _tdh_c_type = _hashable_c_pointer()

    _fields_ = [('_token_data',   _tdh_c_type),
                ('_token_index',  ctypes.c_int),
                ('_trivia_index', ctypes.c_int),
                ('_kind',         ctypes.c_int),
                ('_text',         _text),
                ('_sloc_range',   SlocRange._c_type)]

    def _wrap(self):
        return self if self._token_data else None

    @staticmethod
    def _check_token(value):
        if not isinstance(value, Token):
            raise TypeError('invalid token: {}'.format(value))

    def _check_same_unit(self, other):
        if self._token_data != other._token_data:
            raise ValueError('{} and {} come from different analysis units'
                             .format(self, other))

    @property
    def next(self):
        ${py_doc('langkit.token_next', 8)}
        t = Token()
        _token_next(ctypes.byref(self), ctypes.byref(t))
        return t._wrap()

    @property
    def previous(self):
        ${py_doc('langkit.token_previous', 8)}
        t = Token()
        _token_previous(ctypes.byref(self), ctypes.byref(t))
        return t._wrap()

    def range_until(self, other):
        ${py_doc('langkit.token_range_until', 8)}
        self._check_token(other)
        self._check_same_unit(other)

        # Keep the generator as a nested function so that the above checks are
        # executed when the generator is created, instead of only when its
        # first item is requested.
        def generator():
            if other < self:
                return

            yield self
            current = self
            while current < other:
                current = current.next
                yield current
        return generator()

    def is_equivalent(self, other):
        ${py_doc('langkit.token_is_equivalent', 8)}
        self._check_token(other)
        return bool(_token_is_equivalent(
            ctypes.byref(self), ctypes.byref(other))
        )

    @property
    def kind(self):
        ${py_doc('langkit.token_kind', 8)}
        name = _token_kind_name(self._kind)
        # The _token_kind_name wrapper is already supposed to handle exceptions
        # so this should always return a non-null value.
        assert name
        return _unwrap_str(name)

    @property
    def is_trivia(self):
        ${py_doc('langkit.token_is_trivia', 8)}
        return self._trivia_index != 0

    @property
    def index(self):
        ${py_doc('langkit.token_index', 8)}
        return (self._token_index - 1
                if self._trivia_index == 0 else
                self._trivia_index - 1)

    @property
    def text(self):
        ${py_doc('langkit.token_text', 8)}
        return self._text._wrap()

    @classmethod
    def text_range(cls, first, last):
        ${py_doc('langkit.token_range_text', 8)}
        cls._check_token(first)
        cls._check_token(last)
        first._check_same_unit(last)
        result = _text()
        assert _token_range_text(ctypes.byref(first), ctypes.byref(last),
                                 ctypes.byref(result))
        return result._wrap() or u''

    @property
    def sloc_range(self):
        ${py_doc('langkit.token_sloc_range', 8)}
        return self._sloc_range._wrap()

    def __eq__(self, other):
        """
        Return whether the two tokens refer to the same token in the same unit.

        Note that this does not actually compares the token data.
        """
        return (isinstance(other, Token)
                and self._identity_tuple == other._identity_tuple)

    def __hash__(self):
        return hash(self._identity_tuple)

    def __repr__(self):
        return '<Token {}{} at {}>'.format(
            self.kind,
            ' {}'.format(_py2to3.text_repr(self.text)) if self.text else '',
            self.sloc_range
        )

    def __lt__(self, other):
        """
        Consider that None comes before all tokens. Then, sort by unit, token
        index, and trivia index.
        """
        # None always comes first
        if other is None:
            return False

        self._check_token(other)
        self._check_same_unit(other)
        return self._identity_tuple < other._identity_tuple

    def __le__(self, other):
        return self == other or self < other

    def __gt__(self, other):
        return not (self <= other)

    def __ge__(self, other):
        return not (self < other)

    def to_data(self):
        """
        Return a dict representation of this Token.
        """
        return {"kind": "Token", "token_kind": self.kind, "text": self.text}

    @property
    def _identity_tuple(self):
        """
        Return a tuple that return a tuple that contains "identity" information
        for this token. Think of it as a database primary key.

        This property is for internal use only.
        """
        return (self._token_data, self._token_index, self._trivia_index)


## TODO: if this is needed some day, also bind create_unit_provider to allow
## Python users to create their own unit providers.
class UnitProvider(object):
    ${py_doc('langkit.unit_provider_type', 4)}

    def __init__(self, c_value):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly.
        """
        self._c_value = c_value

    def __del__(self):
        _dec_ref_unit_provider(self._c_value)

${exts.include_extension(
   ctx.ext('python_api', 'unit_providers', 'methods')
)}


class ${root_astnode_name}(object):
    ${py_doc(T.root_node, 4)}

    is_list_type = False
    __slots__ = ('_unprotected_c_value', '_node_c_value', '_metadata',
                 '_rebindings', '_unprotected_getitem_cache', '_unit',
                 '_unit_version')

    ${astnode_types.subclass_decls(T.root_node)}

    def __init__(self, c_value, node_c_value, metadata, rebindings):
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
        self._metadata = metadata

        self._unprotected_getitem_cache = {}
        """
        Cache for the __getitem__ override.

        :type: dict[int, ${root_astnode_name}]
        """

        # Information to check before accessing node data that it is still
        # valid.
        self._unit = self._fetch_unit(c_value)
        self._unit_version = self._unit._unit_version

    def _check_stale_reference(self):
        # We have a reference to the owning unit, so there is no need to
        # check that the unit and the context are still valid. Just check that
        # the unit has not been reparsed.
        if self._unit._unit_version != self._unit_version:
            raise StaleReferenceError()

    @property
    def _c_value(self):
        self._check_stale_reference()
        return self._unprotected_c_value

    @property
    def _getitem_cache(self):
        self._check_stale_reference()
        return self._unprotected_getitem_cache

    @property
    def _id_tuple(self):
        return (self._node_c_value, self._rebindings)

    def __eq__(self, other):
        return (isinstance(other, ${root_astnode_name}) and
                self._id_tuple == other._id_tuple)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(self._id_tuple)

    @property
    def kind_name(self):
        ${py_doc('langkit.node_kind', 8)}
        return self._kind_name

    @property
    def is_token_node(self):
        ${py_doc('langkit.node_is_token_node', 8)}
        node = self._unwrap(self)
        return bool(_node_is_token_node(ctypes.byref(node)))

    @property
    def is_synthetic(self):
        ${py_doc('langkit.node_is_synthetic', 8)}
        node = self._unwrap(self)
        return bool(_node_is_synthetic(ctypes.byref(node)))

    @property
    def sloc_range(self):
        ${py_doc('langkit.node_sloc_range', 8)}
        node = self._unwrap(self)
        result = SlocRange._c_type()
        _node_sloc_range(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def text(self):
        ${py_doc('langkit.node_text', 8)}
        node = self._unwrap(self)
        result = _text()
        _node_text(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def image(self):
        ${py_doc('langkit.node_image', 8)}
        c_node = self._unwrap(self)
        c_result = _text()
        _node_image(ctypes.byref(c_node), ctypes.byref(c_result))
        return c_result._wrap()

    def lookup(self, sloc):
        ${py_doc('langkit.lookup_in_node', 8)}
        node = self._unwrap(self)
        c_sloc = Sloc._c_type._unwrap(sloc)
        result = ${c_entity}()
        _lookup_in_node(ctypes.byref(node), ctypes.byref(c_sloc),
                        ctypes.byref(result))
        return ${root_astnode_name}._wrap(result)

    def __bool__(self):
        """
        Return always True so that checking a node against None can be done as
        simply as::

            if node:
                ...
        """
        return True
    __nonzero__ = __bool__

    def __iter__(self):
        """
        Return an iterator on the children of this node.
        """
        for i in range(len(self)):
            yield self[i]

    def __len__(self):
        """Return the number of ${root_astnode_name} children this node has."""
        node = self._unwrap(self)
        return _node_children_count(ctypes.byref(node))

    def __getitem__(self, key):
        """
        Return the Nth ${root_astnode_name} child this node has.

        This handles negative indexes the same way Python lists do. Raise an
        IndexError if "key" is out of range.
        """
        if not isinstance(key, int):
            msg = ('${root_astnode_name} children are integer-indexed'
                   ' (got {})').format(type(key))
            raise TypeError(msg)

        if key < 0:
            key += len(self)

        if key in self._getitem_cache:
            return self._getitem_cache[key]

        node = self._unwrap(self)
        result = ${c_entity}()
        success = _node_child(ctypes.byref(node), key, ctypes.byref(result))
        if not success:
            raise IndexError('child index out of range')
        else:
            result = ${root_astnode_name}._wrap(result)
            self._getitem_cache[key] = result
            return result

    def iter_fields(self):
        """
        Iterate through all the fields this node contains.

        Return an iterator that yields (name, value) couples for all abstract
        fields in this node. If "self" is a list, field names will be
        "item_{n}" with "n" being the index.
        """
        if self.is_list_type:
            for i, value in enumerate(self):
                yield ('item_{}'.format(i), value)
        else:
            for field_name in self._field_names:
                yield (field_name, getattr(self, '{}'.format(field_name)))

    def dump_str(self):
        """
        Dump the sub-tree to a string in a human-readable format.
        """
        output = _py2to3.StringIO()
        self.dump(file=output)
        ret = output.getvalue()
        output.close()
        return ret

    def dump(self, indent='', file=sys.stdout):
        """
        Dump the sub-tree in a human-readable format on the given file.

        :param str indent: Prefix printed on each line during the dump.
        :param file file: File in which the dump must occur.
        """

        def print_node(name, value):
            if isinstance(value, ${root_astnode_name}):
                print('{}{}:'.format(indent, name), file=file)
                value.dump(indent + '  ', file)
            else:
                print('{}{}: {}'.format(indent, name, value), file=file)

        erepr = self.entity_repr[1:-1]
        print('{}{}{}'.format(
            indent, erepr,
            ': {}'.format(self.text) if self.is_token_node else ''
        ), file=file)
        indent = indent + '|'
        if self.is_list_type:
            for i, value in enumerate(self):
                print_node("item_{}".format(i), value)
        else:
            for name, value in self.iter_fields():
                # Remove the f_ prefix to have the same behavior as the Ada
                # dumper.
                print_node(name[2:], value)

    def findall(self, ast_type_or_pred, **kwargs):
        """
        Helper for finditer that will return all results as a list. See
        finditer's documentation for more details.
        """
        return list(self.finditer(ast_type_or_pred, **kwargs))

    def find(self, ast_type_or_pred, **kwargs):
        """
        Helper for finditer that will return only the first result. See
        finditer's documentation for more details.
        """
        try:
            return next(self.finditer(ast_type_or_pred, **kwargs))
        except StopIteration:
            return None

    @property
    def parent_chain(self):
        """
        Return the parent chain of self. Self will be the first element,
        followed by the first parent, then this parent's parent, etc.
        """
        def _parent_chain(node):
            yield node
            if node.parent is not None:
                for p in _parent_chain(node.parent):
                    yield p

        return list(_parent_chain(self))

    def finditer(self, ast_type_or_pred, **kwargs):
        """
        Find every node corresponding to the passed predicates.

        :param ast_type_or_pred: If supplied with a subclass of
            ${root_astnode_name}, will constrain the resulting collection to
            only the instances of this type or any subclass. If supplied with a
            predicate, it will apply the predicate on every node and keep only
            the ones for which it returns True. If supplied with a list of
            subclasses of ${root_astnode_name}, it will match all instances of
            any of them.
        :type ast_type_or_pred:
            type|((${root_astnode_name}) -> bool)|list[type]

        :param kwargs: Allows the user to filter on attributes of the node. For
            every key value association, if the node has an attribute of name
            key that has the specified value, then the child is kept.
        :type kwargs: dict[str, Any]
        """
        # Create a "pred" function to use as the node filter during the
        # traversal.
        if isinstance(ast_type_or_pred, type):
            sought_type = ast_type_or_pred
            pred = lambda node: isinstance(node, sought_type)
        elif isinstance(ast_type_or_pred, collections.Sequence):
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

    def __repr__(self):
        return self.image

    @property
    def entity_repr(self):
        c_value = self._unwrap(self)
        c_result = _text()
        _entity_image(ctypes.byref(c_value), ctypes.byref(c_result))
        return c_result._wrap()

    @property
    def tokens(self):
        """
        Return an iterator on the range of tokens that self encompasses.
        """
        start = self.token_start
        end = self.token_end
        while not start == end:
            yield start
            start = start.next
        yield end

    def to_data(self):
        """
        Return a nested python data-structure, constituted only of standard
        data types (dicts, lists, strings, ints, etc), and representing the
        portion of the AST corresponding to this node.
        """
        if self.is_list_type:
            return [i.to_data() for i in self if i is not None]
        else:
            return {n: v.to_data()
                    for n, v in self.iter_fields()
                    if v is not None}

    def to_json(self):
        """
        Return a JSON representation of this node.
        """
        return json.dumps(self.to_data())

    def is_a(self, *types):
        """
        Shortcut for isinstance(self, types).
        :rtype: bool
        """
        return isinstance(self, tuple(types))

    def cast(self, typ):
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
        result = _kind_to_astnode_cls[kind](c_value, node_c_value, metadata,
                                            rebindings)
        unit._node_cache[cache_key] = result
        return result

    @classmethod
    def _wrap_bare_node(cls, c_value):
        return cls._wrap(${c_entity}.from_bare_node(c_value))

    @classmethod
    def _unwrap(cls, py_value):
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
    def _unwrap_einfo(self):
        return self._c_value.info

    @classmethod
    def _fetch_unit(cls, c_value):
        return ${pyapi.wrap_value('_node_unit(ctypes.byref(c_value))',
                                  T.AnalysisUnit)}

    def _eval_field(self, c_result, c_accessor, *c_args):
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

    def _eval_astnode_field(self, c_accessor):
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


_EnvRebindings_c_type = _hashable_c_pointer()


${struct_types.base_decls()}

% for struct_type in ctx.struct_types:
    ## Emit a single C type for all entities, as they are all ABI compatible.
    ## We emit them as a special case as we just need the C structure layout:
    ## the node/entity wrapper will take care of the rest.
    % if struct_type.is_entity_type:
        % if struct_type is root_entity:
class ${c_entity}(ctypes.Structure):
    _fields_ = ${struct_types.ctype_fields(struct_type)}

    @classmethod
    def from_bare_node(cls, node_c_value):
        return cls(node_c_value, ${c_entity_info}._null_value)
        % endif
    ## Likewise for entity info structures: they will never be wrapped
    % elif struct_type is T.entity_info:
class ${c_entity_info}(ctypes.Structure):
    _fields_ = ${struct_types.ctype_fields(struct_type)}
    ## Likewise for metadata structures
    % elif struct_type is T.env_md:
class ${c_metadata}(ctypes.Structure):
    _fields_ = ${struct_types.ctype_fields(struct_type)}

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
${array_types.decl(T.root_node.array)}
${array_types.decl(T.entity.array)}
% for array_type in ctx.array_types:
    % if array_type.exposed and array_type.emit_c_type:
${array_types.decl(array_type)}
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

# Analysis primitives
_create_analysis_context = _import_func(
    '${capi.get_name("create_analysis_context")}',
    [ctypes.c_char_p, # charset
     _unit_provider,  # unit_provider
     ctypes.c_int,    # with_trivia
     ctypes.c_int],   # tab_stop
    AnalysisContext._c_type
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
    [AnalysisUnit._c_type, ctypes.POINTER(Token)], None
)
_unit_last_token = _import_func(
    "${capi.get_name('unit_last_token')}",
    [AnalysisUnit._c_type, ctypes.POINTER(Token)], None
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
     ctypes.POINTER(Token)],
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
    [AnalysisUnit._c_type], ctypes.c_int
)

# General AST node primitives
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

# Unit providers
_dec_ref_unit_provider = _import_func(
    '${capi.get_name("dec_ref_unit_provider")}',
    [_unit_provider], None
)
${exts.include_extension(
   ctx.ext('python_api', 'unit_providers', 'low_level_bindings')
)}

# Misc
_token_kind_name = _import_func(
   "${capi.get_name('token_kind_name')}",
   [ctypes.c_int], ctypes.POINTER(ctypes.c_char)
)
_token_next = _import_func(
    "${capi.get_name('token_next')}",
    [ctypes.POINTER(Token), ctypes.POINTER(Token)], None
)
_token_is_equivalent = _import_func(
    "${capi.get_name('token_is_equivalent')}",
    [ctypes.POINTER(Token), ctypes.POINTER(Token)], ctypes.c_int
)
_token_previous = _import_func(
    "${capi.get_name('token_previous')}",
    [ctypes.POINTER(Token), ctypes.POINTER(Token)], None
)
_token_range_text = _import_func(
    "${capi.get_name('token_range_text')}",
    [ctypes.POINTER(Token), ctypes.POINTER(Token), ctypes.POINTER(_text)],
    ctypes.c_int
)
_entity_image = _import_func(
    "${capi.get_name('entity_image')}",
    [ctypes.POINTER(${c_entity}), ctypes.POINTER(_text)], None
)


#
# Layering helpers
#

def _unwrap_str(c_char_p_value):
    """
    Assuming c_char_p_value is a valid char*, convert it to a native Python
    string and free the C pointer.
    """
    result = ctypes.c_char_p(ctypes.addressof(c_char_p_value.contents)).value
    _free(c_char_p_value)
    return _py2to3.bytes_to_text(result)


_kind_to_astnode_cls = {
    % for subclass in ctx.astnode_types:
        % if not subclass.abstract:
    ${ctx.node_kind_constants[subclass]}: ${pyapi.type_public_name(subclass)},
        % endif
    % endfor
}


def _field_address(struct, field_name):
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
    for f_name, f_type in struct_type._fields_:
        if f_name == field_name:
            field_type = f_type
            break
    assert field_type is not None
    return struct_addr + field.offset


#
# Language specific extensions #
#

${exts.include_extension(ctx.ext("python"))}

#
# App base class
#

class App(object):
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

    @property
    def description(self):
        """
        Description for this app. Empty by default.
        """
        return ""

    def __init__(self):
        self.parser = argparse.ArgumentParser(description=self.description)
        self.parser.add_argument('files', nargs='+', help='Files')
        self.add_arguments()
        self.args = self.parser.parse_args()
        self.ctx = AnalysisContext(
            'utf-8', with_trivia=True,
            unit_provider=self.create_unit_provider()
        )

    def add_arguments(self):
        """
        Hook for subclasses to add arguments to self.parser. Default
        implementation does nothing.
        """
        pass

    def create_unit_provider(self):
        """
        Hook for subclasses to return a custom unit provider.
        Default implementation returns None.
        """
        return None

    def process_files(self):
        """
        Load units for all source files on the command-line in `self.units`.
        Put the last one in `self.u`.
        """
        self.units = {}
        for file_name in self.args.files:
            self.u = self.ctx.get_from_file(file_name)
            self.units[file_name] = self.u

    @classmethod
    def run(cls):
        """
        Instantiate and run this application.
        """
        instance = cls()
        instance.process_files()
        instance.main()

    ${exts.include_extension(ctx.ext('python_api/app_exts'))}
