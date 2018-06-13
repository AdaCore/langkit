## vim: filetype=makopython

"""
Python binding of the ${ctx.lib_name.camel} API.

Please consider all exported entities whose names that start with an underscore
("_") as internal implementation details. They are not meant to be used
directly.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

<%namespace name="array_types"   file="array_types_py.mako" />
<%namespace name="astnode_types" file="astnode_types_py.mako" />
<%namespace name="struct_types"  file="struct_types_py.mako" />
<%namespace name="exts"          file="/extensions.mako" />


<%
    root_astnode_name = T.root_node.kwless_raw_name.camel
    c_node = '{}._node_c_type'.format(root_astnode_name)
    c_entity = '{}._c_type'.format(root_entity.name.camel)
%>


import collections
import ctypes
import json
import os
import sys


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

    :param bool exc_wrap: If True, wrap the returned function to check for
      exceptions.
    """
    func = getattr(_c_lib, name)
    func.argtypes = argtypes
    func.restype = restype

    # Wrapper for "func" that raises a NativeException in case of internal
    # error.

    def wrapper(*args, **kwargs):
        result = func(*args, **kwargs)
        exc = _get_last_exception()
        if exc and exc.contents.is_fatal:
            raise exc.contents._wrap()
        return result

    return wrapper if exc_wrap else func


class _Exception(ctypes.Structure):
    _fields_ = [("is_fatal", ctypes.c_int),
                ("information", ctypes.c_char_p)]

    def _wrap(self):
        return NativeException(self.information)


def _raise_type_error(expected_type_name, actual_value):
    raise TypeError('{} instance expected, got {} instead'.format(
        expected_type_name, type(actual_value)
    ))


_get_last_exception = _import_func(
   '${capi.get_name("get_last_exception")}',
   [], ctypes.POINTER(_Exception),
   exc_wrap=False
)


class _hashable_void_p(ctypes.c_void_p):
    """
    Base class for pointers, can be used in hashed maps.
    """

    def __hash__(self):
        return self.value or 0

    def __eq__(self, other):
        return self.value == other.value

    def __ne__(self, other):
        return not (self == other)


class _text(ctypes.Structure):
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
        if isinstance(value, str):
            value = value.decode('ascii')
        elif not isinstance(value, unicode):
            _raise_type_error('string or unicode', value)

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
            return None

    def __del__(self):
        _destroy_text(ctypes.byref(self))


class _big_integer(_text):

    @classmethod
    def _unwrap(cls, value):
        if not isinstance(value, (int, long)):
            _raise_type_error('int or long', value)

        return super(_big_integer, cls)._unwrap(str(value))

    def _wrap(self):
        return int(super(_big_integer, self)._wrap())


% if ctx.default_unit_provider:
${py_doc('langkit.unit_kind_type')}
_str_to_unit_kind = {
    'specification': 0,
    'body': 1,
}
_unit_kind_to_str = {c_val: py_val
                     for py_val, c_val in _str_to_unit_kind.items()}


def _unwrap_unit_kind(kind):
    """
    Given a string representing an analysis unit kind in the Python API, return
    the corresponding C API value.
    """
    return _unwrap_enum(kind, 'analysis unit kind', _str_to_unit_kind)


class _unit_provider(_hashable_void_p):
    pass
% endif


#
# High-level binding
#


class NativeException(Exception):
    """
    Exception raised when the underlying C API reports an error that occurred
    in the library.

    This kind of exception is raised for internal errors: they should never
    happen in normal situations and if they are raised at some point, it means
    the state is potentially corrupted.

    Nevertheless, the library does its best not to crash the program,
    materializing internal errors as Python exceptions.
    """
    pass

class InvalidUnitNameError(Exception):
    ${py_doc('langkit.invalid_unit_name_error', 4)}
    pass

class PropertyError(Exception):
    ${py_doc('langkit.property_error', 4)}
    pass

${exts.include_extension(
   ctx.ext('python_api', 'exceptions')
)}


class AnalysisContext(object):
    ${py_doc('langkit.analysis_context_type', 4)}

    __slots__ = ('_c_value', '_unit_provider')

    def __init__(self,
                 charset=None,
                 with_trivia=True,
% if ctx.default_unit_provider:
                 unit_provider=None,
% endif
                 _c_value=None):
        ${py_doc('langkit.create_context', 8)}
% if ctx.default_unit_provider:
        c_unit_provider = unit_provider._c_value if unit_provider else None
% endif
        self._c_value = (
            _create_analysis_context(
                charset, with_trivia,
% if ctx.default_unit_provider:
                c_unit_provider,
% endif
            )
            if _c_value is None else
            _context_incref(_c_value)
        )

% if ctx.default_unit_provider:
        # Keep a reference to the unit provider so that it is live at least as
        # long as the analysis context is live.
        self._unit_provider = unit_provider
% endif

    def __del__(self):
        _context_decref(self._c_value)

    def __eq__(self, other):
        return self._c_value == other._c_value

    def __hash__(self):
        return hash(self._c_value)

    def get_from_file(self, filename, charset=None, reparse=False):
        ${py_doc('langkit.get_unit_from_file', 8)}
        c_value = _get_analysis_unit_from_file(self._c_value, filename,
                                               charset or '', reparse)
        return AnalysisUnit(c_value)

    def get_from_buffer(self, filename, buffer, charset=None, reparse=False):
        ${py_doc('langkit.get_unit_from_buffer', 8)}
        c_value = _get_analysis_unit_from_buffer(self._c_value, filename,
                                                 charset or '',
                                                 buffer, len(buffer))
        return AnalysisUnit(c_value)

% if ctx.default_unit_provider:
    def get_from_provider(self, name, kind, charset=None, reparse=False):
        ${py_doc('langkit.get_unit_from_provider', 8)}
        _name = _text._unwrap(name)
        _kind = _unwrap_unit_kind(kind)
        c_value = _get_analysis_unit_from_provider(
            self._c_value, ctypes.byref(_name), _kind, charset or '', reparse
        )
        if c_value:
            return AnalysisUnit(c_value)
        else:
            raise InvalidUnitNameError('Invalid unit name: {} ({})'.format(
                repr(name), kind
            ))
% endif

    def remove(self, filename):
        ${py_doc('langkit.remove_unit', 8)}
        if not _remove_analysis_unit(self._c_value, filename):
            raise KeyError('No such unit: {}'.format(filename))

    def discard_errors_in_populate_lexical_env(self, discard):
        ${py_doc('langkit.context_discard_errors_in_populate_lexical_env', 8)}
        _discard_errors_in_populate_lexical_env(self._c_value, bool(discard))

    class _c_type(_hashable_void_p):
        pass


class AnalysisUnit(object):
    ${py_doc('langkit.analysis_unit_type', 4)}

    __slots__ = ('_c_value', )

    class DiagnosticsList(object):
        """List of analysis unit diagnostics."""
        def __init__(self, unit):
            self.unit = unit

        def __repr__(self):
            return 'DiagnosticsList({})'.format(repr(list(self)))

        def __len__(self):
            return _unit_diagnostic_count(self.unit._c_value)

        def __getitem__(self, key):
            if not isinstance(key, int):
                msg = 'list indices must be integers, not {}'.format(
                    type(key))
                raise TypeError(msg)

            diag = Diagnostic._c_type()
            success = _unit_diagnostic(self.unit._c_value, key,
                                       ctypes.byref(diag))
            if not success:
                raise IndexError('diagnostic index out of range')
            else:
                result = diag._wrap()
                return result

    class TokenIterator(object):
        """Iterator over the tokens in an analysis unit."""
        def __init__(self, first):
            self.first = first

        def __iter__(self):
            return self

        def next(self):
            if not self.first:
                raise StopIteration()
            result = self.first
            self.first = self.first.next
            return result

    def __init__(self, c_value):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly. Please use AnalysisContext.get_from_* methods to create
        analysis unit instances instead.
        """
        self._c_value = c_value
        _unit_incref(self._c_value)

    def __del__(self):
        _unit_decref(self._c_value)
        super(AnalysisUnit, self).__init__()

    def __eq__(self, other):
        return self._c_value == other._c_value

    def __hash__(self):
        return hash(self._c_value)

    @property
    def context(self):
        ${py_doc('langkit.unit_context', 8)}
        ctx = _unit_context(self._c_value)
        return AnalysisContext(_c_value=ctx)

    def reparse(self, buffer=None, charset=None):
        ${py_doc('langkit.unit_reparse_generic', 8)}
        if buffer is None:
            _unit_reparse_from_file(self._c_value, charset or '')
        else:
            _unit_reparse_from_buffer(self._c_value, charset or '',
                                      buffer, len(buffer))

    def populate_lexical_env(self):
        ${py_doc('langkit.unit_populate_lexical_env', 8)}
        if not _unit_populate_lexical_env(self._c_value):
            exc = _get_last_exception()
            if exc:
                raise PropertyError(*exc.contents._wrap().args)
            else:
                raise PropertyError()

    @property
    def root(self):
        ${py_doc('langkit.unit_root', 8)}
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
        return self.DiagnosticsList(self)

    def __repr__(self):
        return '<AnalysisUnit {}>'.format(repr(
            os.path.basename(self.filename)
        ))

    class _c_type(_hashable_void_p):
        pass

    @classmethod
    def _wrap(cls, c_value):
        return cls(c_value) if c_value else None

    @classmethod
    def _unwrap(cls, value):
        if value is None:
            return 0
        elif not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)
        else:
            return value._c_value


class Sloc(object):
    ${py_doc('langkit.sloc_type', 4)}

    def __init__(self, line, column):
        assert line >= 0 and column >= 0
        self.line = line
        self.column = column

    def __nonzero__(self):
        return bool(self.line or self.column)

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

    def __nonzero__(self):
        return bool(self.start or self.end)

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

    def __str__(self):
        return ('{}: {}'.format(self.sloc_range, self.message)
                if self.sloc_range else
                self.message)

    def __repr__(self):
        return '<Diagnostic {} at {:#x}>'.format(repr(str(self)), id(self))


    class _c_type(ctypes.Structure):
        _fields_ = [("sloc_range", SlocRange._c_type),
                    ("message", _text)]

        def _wrap(self):
            return Diagnostic(self.sloc_range._wrap(), self.message._wrap())


class Token(ctypes.Structure):
    ${py_doc('langkit.token_type', 4)}

    _fields_ = [('_token_data',   _hashable_void_p),
                ('_token_index',  ctypes.c_int),
                ('_trivia_index', ctypes.c_int),
                ('_kind',         ctypes.c_int),
                ('_text',         _text),
                ('_sloc_range',   SlocRange._c_type)]

    def _wrap(self):
        return self if self._token_data else None

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
        if other is not None and self._token_data != other._token_data:
            raise ValueError('{} and {} come from different analysis'
                             ' units'.format(self, other))

        if other < self:
            return

        yield self
        current = self
        while current < other:
            current = current.next
            yield current

    def is_equivalent(self, other):
        ${py_doc('langkit.token_is_equivalent', 8)}
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
        return self._text._wrap()

    @classmethod
    def text_range(cls, first, last):
        ${py_doc('langkit.token_range_text', 8)}
        result = _text()
        if not _token_range_text(
            ctypes.byref(first), ctypes.byref(last),
            ctypes.byref(result)
        ):
            raise ValueError(
               "{} and {} don't belong to the same analysis unit".format(
                  first, last
               )
            )
        return result._wrap() or u''

    @property
    def sloc_range(self):
        return self._sloc_range._wrap()

    def __eq__(self, other):
        """
        Return whether the two tokens refer to the same token in the same unit.

        Note that this does not actually compares the token data.
        """
        return (other is not None
                and self._identity_tuple == other._identity_tuple)

    def __hash__(self):
        return hash(self._identity_tuple)

    def __repr__(self):
        return '<Token {}{} at {}>'.format(
            self.kind,
            ' {}'.format(repr(self.text)) if self.text else '',
            self.sloc_range
        )

    def __lt__(self, other):
        """
        Consider that None comes before all tokens. Then, sort by unit, token
        index, and trivia index.
        """
        if other is None:
            return False
        if self._token_data != other._token_data:
            raise ValueError('{} and {} come from different units'.format(
                self, other
            ))
        return (other is not None
                and self._identity_tuple < other._identity_tuple)

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


% if ctx.default_unit_provider:

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
        _destroy_unit_provider(self._c_value)

${exts.include_extension(
   ctx.ext('python_api', 'unit_providers', 'methods')
)}

% endif


class ${root_astnode_name}(object):
    ${py_doc(T.root_node, 4)}

    is_list_type = False
    __slots__ = ('_node_ext', '_metadata', '_rebindings')

    ${astnode_types.subclass_decls(T.root_node)}

    def __init__(self, c_value, node_c_value, rebindings):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly. For now, the creation of AST nodes can happen only as
        part of the parsing of an analysis unit.
        """
        self._c_value = c_value
        self._node_c_value = node_c_value

        # For performance, don't unwrap metadata until it's necessary: see the
        # "metadata" property.
        self._metadata = None

        self._rebindings = rebindings

        self._getitem_cache = {}
        """
        Cache for the __getitem__ override.

        :type: dict[int, ${root_astnode_name}]
        """

        self._field_cache = {}
        """
        Cache for fields and argument-less properties.

        :type: dict[int, T]
        """

    @property
    def metadata(self):
        if self._metadata is None:
            self._metadata = ${pyapi.wrap_value('self._c_value.info.md',
                                                T.env_md)}
        return self._metadata

    def __del__(self):
        super(${root_astnode_name}, self).__init__()

    @property
    def _id_tuple(self):
        return (self._node_c_value, self.metadata, self._rebindings)

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
    def sloc_range(self):
        ${py_doc('langkit.node_sloc_range', 8)}
        node = self._unwrap(self)
        result = SlocRange._c_type()
        _node_sloc_range(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def text(self):
        """
        Return the source buffer slice corresponding to the text that spans
        between the first and the last tokens of `self`.
        """
        return ('' if self.is_ghost else
                Token.text_range(self.token_start, self.token_end))

    @property
    def short_image(self):
        ${py_doc('langkit.node_short_image', 8)}
        node = self._unwrap(self)
        text = _node_short_image(ctypes.byref(node))
        return text._wrap()

    def lookup(self, sloc):
        ${py_doc('langkit.lookup_in_node', 8)}
        node = self._unwrap(self)
        c_sloc = Sloc._c_type._unwrap(sloc)
        result = ${c_entity}()
        _lookup_in_node(ctypes.byref(node), ctypes.byref(c_sloc),
                        ctypes.byref(result))
        return ${root_astnode_name}._wrap(result)

    def __nonzero__(self):
        """
        Return always True so that checking a node against None can be done as
        simply as::

            if node:
                ...
        """
        return True

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
                if field_name.startswith('f_'):
                    yield (field_name, getattr(self, '{}'.format(field_name)))

    def dump_str(self):
        """
        Dump the sub-tree to a string in a human-readable format.
        """
        import StringIO
        output = StringIO.StringIO()
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
                # dumper. Also filter out non-exposed types to keep the same
                # output with debug builds.
                if getattr(value, '_exposed', True):
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
        except Exception:
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
        return self.short_image

    @property
    def entity_repr(self):
        c_value = self._unwrap(self)
        return _entity_image(ctypes.byref(c_value))._wrap()

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

    class _node_c_type(_hashable_void_p):
        pass

    @classmethod
    def _wrap(cls, c_value):
        """
        Internal helper to wrap a low-level entity value into an instance of
        the the appropriate high-level Python wrapper subclass.
        """
        node_c_value = c_value.el
        if not node_c_value:
            return None

        rebindings = ${(pyapi.wrap_value('c_value.info.rebindings',
                                         T.EnvRebindingsType))}

        # Pick the right subclass to materialize this node in Python
        kind = _node_kind(ctypes.byref(c_value))
        return _kind_to_astnode_cls[kind](c_value, node_c_value, rebindings)

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

    def _eval_field(self, c_result, c_accessor, *c_args):
        """
        Internal helper to evaluate low-level field accessors/properties.

        This calls "c_accessor" on this node with the input arguments and puts
        the result in "c_result". This raises a PropertyError if the evaluation
        failed. Return "c_result" for convenience.
        """
        args = (self._unwrap(self), ) + c_args + (ctypes.byref(c_result), )
        if not c_accessor(*args):
            exc = _get_last_exception()
            if exc:
                raise PropertyError(*exc.contents._wrap().args)
            else:
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


class _Extension(object):
    """
    Internal holder for a handle to a C API resource.

    We use the extension mechanism to keep a single _Extension instance per
    underlying C API resource.
    """

    __slots__ = ('_c_handle', )

    @classmethod
    def lookup(cls, c_handle):
        raise NotImplementedError()

    @classmethod
    def get_or_create(cls, c_handle):
        if not c_handle:
            return None

        # If there is already an _Extension instance for the `c_handle`
        # resource, return it.
        c_pyobj_p = ctypes.cast(cls.lookup(c_handle),
                                ctypes.POINTER(ctypes.py_object))
        if c_pyobj_p.contents:
            return c_pyobj_p.contents.value

        # Otherwise, create a dedicated one...
        ext = cls(c_handle)

        # .. and store it in our extension.
        c_pyobj_p[0] = ctypes.py_object(ext)

        # We want to increment its ref count so that the wrapper will be
        # alive as long as the extension references it.
        ctypes.pythonapi.Py_IncRef(ctypes.py_object(ext))
        return ext

    def __init__(self, c_handle):
        self._c_handle = c_handle

    @property
    def c_handle(self):
        return self._c_handle

    @classmethod
    def unwrap(cls, value):
        if value is None:
            return None
        assert isinstance(value, cls)
        return value.c_handle


## TODO: use the _Extension mechanism for env rebindings

class EnvRebindings(object):
    ${py_doc('langkit.env_rebindings_type', 4)}

    def __init__(self, c_value):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly.
        """
        self._c_value = c_value

    @property
    def _address(self):
        if self._c_value:
            # Create a dummy int* value so that we can get the pointed address
            ptr = ctypes.cast(self._c_value, ctypes.POINTER(ctypes.c_int))
            return ctypes.addressof(ptr.contents)
        else:
            return 0

    def __repr__(self):
        return '<EnvRebindings at {}>'.format(hex(self._address))

    def __eq__(self, other):
        return (isinstance(other, EnvRebindings) and
                self._address == other._address)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(self._address)

    class _c_type(_hashable_void_p):
        pass

    @classmethod
    def _unwrap(cls, value):
        if value is None:
            return None
        elif not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)
        else:
            return value._c_value

    @classmethod
    def _wrap(cls, c_value):
        return cls(c_value) if c_value else None


% for astnode in ctx.astnode_types:
    % if astnode != T.root_node:
${astnode_types.decl(astnode)}
    % endif
% endfor


def _unwrap_enum(py_value, type_name, translator):
    """
    Internal helper to unwrap a high-level enumeration value (i.e. a string)
    into a low-level value (i.e. an integer). Raise a TypeError if the input
    value has an unexpected type and a ValueError if the string does not
    represent a valid enumerator.

    :param str py_value: The high-level enumeration value.
    :param str type_name: Name for the enumeration type.
    :param dict[str, int] translator: A mapping that provides the low-level
        values for all high-level ones.
    """
    if not isinstance(py_value, basestring):
        raise TypeError('str expected but got {} instead'.format(
            type(py_value)
        ))
    try:
        return translator[py_value]
    except KeyError:
        raise ValueError('Invalid {}: {}'.format(type_name, py_value))


${struct_types.base_decls()}

% for struct_type in ctx.struct_types:
    % if struct_type._exposed and struct_type.emit_c_type:
${struct_types.decl(struct_type)}
    % endif
% endfor

#
# Low-level binding - Second part
#

# For performance, allocate a single C API entity for all uses of null
# entities.
${c_entity}._null_value = ${c_entity}()
${c_entity}._null_value.el = None

${array_types.base_decl()}
${array_types.decl(T.root_node.array)}
${array_types.decl(T.entity.array)}
% for array_type in ctx.sorted_types(ctx.array_types):
    % if array_type._exposed and array_type.emit_c_type:
${array_types.decl(array_type)}
    % endif
% endfor


_initialize = _import_func(
    '${capi.lib_name}_initialize',
    [], None, exc_wrap=False
)
_initialize()

_free = _import_func(
    '${capi.get_name("free")}',
    [ctypes.c_void_p], None
)

_destroy_text = _import_func(
    '${capi.get_name("destroy_text")}', [ctypes.POINTER(_text)], None
)

# Analysis primitives
_create_analysis_context = _import_func(
    '${capi.get_name("create_analysis_context")}',
    [
        ctypes.c_char_p,
% if ctx.default_unit_provider:
        _unit_provider,
% endif
    ], AnalysisContext._c_type
)
_context_incref = _import_func(
    '${capi.get_name("context_incref")}',
    [AnalysisContext._c_type], AnalysisContext._c_type
)
_context_decref = _import_func(
    '${capi.get_name("context_decref")}',
    [AnalysisContext._c_type], None
)
_discard_errors_in_populate_lexical_env = _import_func(
   '${capi.get_name("context_discard_errors_in_populate_lexical_env")}',
   [AnalysisContext._c_type, ctypes.c_int], None
)
_destroy_analysis_context = _import_func(
    '${capi.get_name("destroy_analysis_context")}',
    [AnalysisContext._c_type, ], None
)
_get_analysis_unit_from_file = _import_func(
    '${capi.get_name("get_analysis_unit_from_file")}',
    [AnalysisContext._c_type,  # context
     ctypes.c_char_p,          # filename
     ctypes.c_char_p,          # charset
     ctypes.c_int],            # reparse
    AnalysisUnit._c_type
)
_get_analysis_unit_from_buffer = _import_func(
    '${capi.get_name("get_analysis_unit_from_buffer")}',
    [AnalysisContext._c_type,  # context
     ctypes.c_char_p,          # filename
     ctypes.c_char_p,          # charset
     ctypes.c_char_p,          # buffer
     ctypes.c_size_t],         # buffer_size
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
_remove_analysis_unit = _import_func(
    '${capi.get_name("remove_analysis_unit")}',
    [AnalysisContext._c_type, ctypes.c_char_p], ctypes.c_int
)
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
_unit_incref = _import_func(
    '${capi.get_name("unit_incref")}',
    [AnalysisUnit._c_type], AnalysisUnit._c_type
)
_unit_decref = _import_func(
    '${capi.get_name("unit_decref")}',
    [AnalysisUnit._c_type], None
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
_kind_name = _import_func(
    '${capi.get_name("kind_name")}',
    [ctypes.c_int], _text
)
_node_is_token_node = _import_func(
    '${capi.get_name("node_is_token_node")}',
    [ctypes.POINTER(${c_entity})], ctypes.c_int
)
_node_short_image = _import_func(
    '${capi.get_name("node_short_image")}',
    [ctypes.POINTER(${c_entity})], _text
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
            type_expr = pyapi.type_internal_name(arg.type)
            if arg.type.is_ada_record:
                type_expr = 'ctypes.POINTER({})'.format(type_expr)
        %>
        ${type_expr},
     % endfor
     ctypes.POINTER(${(c_entity
                       if field.type.is_ast_node else
                       pyapi.type_internal_name(field.type))})],
    ctypes.c_int
)
    % endfor
% endfor


# Extensions handling
_register_extension = _import_func(
    '${capi.get_name("register_extension")}',
    [ctypes.c_char_p], ctypes.c_uint
)

% if ctx.default_unit_provider:
# Unit providers
_destroy_unit_provider = _import_func(
    '${capi.get_name("destroy_unit_provider")}',
    [_unit_provider], None
)
${exts.include_extension(
   ctx.ext('python_api', 'unit_providers', 'low_level_bindings')
)}
% endif

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
% if T.entity._exposed:
_entity_image = _import_func(
    "${capi.get_name('entity_image')}",
    [ctypes.POINTER(Entity._c_type)], _text
)
% endif


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
    return result


_kind_to_astnode_cls = {
    % for subclass in ctx.astnode_types:
        % if not subclass.abstract:
    ${ctx.node_kind_constants[subclass]}: ${subclass.kwless_raw_name.camel},
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
