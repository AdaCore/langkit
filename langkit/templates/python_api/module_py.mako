## vim: filetype=makopython

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

<%namespace name="array_types"   file="array_types_py.mako" />
<%namespace name="astnode_types" file="astnode_types_py.mako" />
<%namespace name="enum_types"    file="enum_types_py.mako" />
<%namespace name="struct_types"  file="struct_types_py.mako" />
<%namespace name="exts"          file="/extensions.mako" />


<%
    root_astnode_name = T.root_node.name().camel
    c_node = '{}._c_type'.format(root_astnode_name)
    c_node_enum = '{}._c_enum_type'.format(root_astnode_name)
%>


import collections
import ctypes
import json
import os
import sys


#
# Basic types for the low-level binding
#


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
    def unwrap(cls, value):
        if isinstance(value, str):
            value = value.decode('ascii')
        elif not isinstance(value, unicode):
            raise TypeError('String or unicode object expected but got {}'
                            ' instead'.format(type(value)))

        text = value.encode(cls.encoding)
        text_buffer = ctypes.create_string_buffer(text)
        text_buffer_ptr = ctypes.cast(
            ctypes.pointer(text_buffer),
            ctypes.POINTER(ctypes.c_char)
        )
        result = _text(text_buffer_ptr, len(value))
        result.text_buffer = text_buffer
        return result

    def wrap(self):
        if self.length > 0:
            # self.length tells how much UTF-32 chars there are in self.chars
            # but self.chars is a char* so we have to fetch 4 times more bytes
            # than characters.
            return self.chars[:4 * self.length].decode(self.encoding)
        else:
            return None

    def __del__(self):
        _destroy_text(ctypes.byref(self))


class _Exception(ctypes.Structure):
    _fields_ = [("is_fatal", ctypes.c_int),
                ("information", ctypes.c_char_p)]

    def wrap(self):
        return NativeException(self.information)


% if ctx.default_unit_file_provider:
${py_doc('langkit.unit_kind_type')}
str_to_unit_kind = {
    'specification': 0,
    'body': 1,
}
unit_kind_to_str = {c_val: py_val
                    for py_val, c_val in str_to_unit_kind.items()}


def _unwrap_unit_kind(kind):
    """
    Given a string representing an analysis unit kind in the Python API, return
    the corresponding C API value.
    """
    return _unwrap_enum(kind, 'analysis unit kind', str_to_unit_kind)


class _unit_file_provider(ctypes.c_void_p):
    pass
% endif


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


#
# High-level binding
#


class AnalysisContext(object):
    ${py_doc('langkit.analysis_context_type', 4)}

    __slots__ = ('_c_value', '_unit_file_provider')

    def __init__(self,
                 charset=None,
% if ctx.default_unit_file_provider:
                 unit_file_provider=None,
% endif
                 _c_value=None):
        ${py_doc('langkit.create_context', 8)}
% if ctx.default_unit_file_provider:
        c_ufp = unit_file_provider._c_value if unit_file_provider else None
% endif
        self._c_value = (
            _create_analysis_context(
                charset,
% if ctx.default_unit_file_provider:
                c_ufp,
% endif
            )
            if _c_value is None else
            _context_incref(_c_value)
        )

% if ctx.default_unit_file_provider:
        # Keep a reference to the unit file provider so that it is live at
        # least as long as the analysis context is live.
        self._unit_file_provider = unit_file_provider
% endif

    def __del__(self):
        _context_decref(self._c_value)

    def __eq__(self, other):
        return self._c_value == other._c_value

    def __hash__(self):
        return hash(self._c_value)

    def get_from_file(self, filename, charset=None, reparse=False,
                      with_trivia=False):
        ${py_doc('langkit.get_unit_from_file', 8)}
        c_value = _get_analysis_unit_from_file(self._c_value, filename,
                                               charset or '', reparse,
                                               with_trivia)
        return AnalysisUnit(c_value)

    def get_from_buffer(self, filename, buffer, charset=None, reparse=False,
                        with_trivia=False):
        ${py_doc('langkit.get_unit_from_buffer', 8)}
        c_value = _get_analysis_unit_from_buffer(self._c_value, filename,
                                                 charset or '',
                                                 buffer, len(buffer),
                                                 with_trivia)
        return AnalysisUnit(c_value)

% if ctx.default_unit_file_provider:
    def get_from_provider(self, name, kind, charset=None, reparse=False,
                          with_trivia=False):
        ${py_doc('langkit.get_unit_from_provider', 8)}
        _name = _text.unwrap(name)
        _kind = _unwrap_unit_kind(kind)
        c_value = _get_analysis_unit_from_provider(self._c_value, _name, _kind,
                                                   charset or '', reparse,
                                                   with_trivia)
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

    class _c_type(ctypes.c_void_p):
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
                result = diag.wrap()
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
                raise PropertyError(*exc.contents.wrap().args)
            else:
                raise PropertyError()

    @property
    def root(self):
        ${py_doc('langkit.unit_root', 8)}
        return ${root_astnode_name}.wrap(_unit_root(self._c_value))

    @property
    def first_token(self):
        ${py_doc('langkit.unit_first_token', 8)}
        result = Token()
        _unit_first_token(self._c_value, ctypes.byref(result))
        return result.wrap()

    @property
    def last_token(self):
        ${py_doc('langkit.unit_last_token', 8)}
        result = Token()
        _unit_last_token(self._c_value, ctypes.byref(result))
        return result.wrap()

    @property
    def token_count(self):
        ${py_doc('langkit.unit_token_count', 8)}
        return _unit_token_count(self._c_value)

    @property
    def trivia_count(self):
        ${py_doc('langkit.unit_trivia_count', 8)}
        return _unit_trivia_count(self._c_value)

    def iter_tokens(self):
        """
        Return an iterator that yields all the tokens in this unit.
        """
        return self.TokenIterator(self.first_token)

    @property
    def filename(self):
        ${py_doc('langkit.unit_filename', 8)}
        filename = _unit_filename(self._c_value)
        return unwrap_str(filename)

    @property
    def diagnostics(self):
        """Diagnostics for this unit."""
        return self.DiagnosticsList(self)

    class _c_type(ctypes.c_void_p):
        pass

    @classmethod
    def wrap(cls, c_value):
        return cls(c_value) if c_value else None


% if ctx.library_fields_all_public:
class LexicalEnv(object):
    ${py_doc('langkit.lexical_env_type', 4)}

    def __init__(self, c_value):
        self._c_value = c_value

    @property
    def parent(self):
        return LexicalEnv.wrap(_lexical_env_parent(self._c_value))

    @property
    def node(self):
        return ${root_astnode_name}.wrap(_lexical_env_node(self._c_value))

    def get(self, name):
        ${py_doc('langkit.lexical_env_get', 8)}
        result = _lexical_env_get(self._c_value, _text.unwrap(name))
        return ${pyapi.wrap_value('result',
                                  T.root_node.env_el().array_type())}

    def __del__(self):
        _lexical_env_dec_ref(self._c_value)
        self._c_value = None

    class _c_type(ctypes.c_void_p):
        pass

    @classmethod
    def unwrap(cls, value):
        if value is None:
            raise ValueError('None is not an allowed LexicalEnv value')
        return value._c_value

    @classmethod
    def wrap(cls, c_value):
        return cls(c_value) if c_value else None


class BasePointerBinding(object):
    """
    Base class for dummy type bindings around mere pointers.
    """

    def __init__(self, c_value):
        self._c_value = c_value

    class _c_type(ctypes.c_void_p):
        pass

    @classmethod
    def unwrap(cls, value):
        return 0 if value is None else value._c_value

    @classmethod
    def wrap(cls, c_value):
        return cls(c_value) if c_value else None


class LogicVar(BasePointerBinding):
    ${py_doc('langkit.logic_var_type', 4)}
    pass


class Equation(BasePointerBinding):
    ${py_doc('langkit.equation_type', 4)}
    pass
% endif


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

        def wrap(self):
            return Sloc(self.line, self.column)

        @classmethod
        def unwrap(cls, sloc):
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

        def wrap(self):
            return SlocRange(self.start.wrap(), self.end.wrap())


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

        def wrap(self):
            return Diagnostic(self.sloc_range.wrap(), self.message.wrap())


class Token(ctypes.Structure):
    ${py_doc('langkit.token_type', 4)}

    _fields_ = [('_token_data',   ctypes.c_void_p),
                ('_token_index',  ctypes.c_int),
                ('_trivia_index', ctypes.c_int),
                ('_kind',         ctypes.c_int),
                ('_text',         _text),
                ('_sloc_range',   SlocRange._c_type)]

    def wrap(self):
        return self if self._token_data else None

    @property
    def next(self):
        ${py_doc('langkit.token_next', 8)}
        t = Token()
        _token_next(ctypes.byref(self), ctypes.byref(t))
        return t.wrap()

    def is_equivalent(self, other):
        ${py_doc('langkit.token_is_equivalent', 8)}
        return bool(_token_is_equivalent(
            ctypes.byref(self), ctypes.byref(other))
        )

    @property
    def previous(self):
        ${py_doc('langkit.token_previous', 8)}
        t = Token()
        _token_previous(ctypes.byref(self), ctypes.byref(t))
        return t.wrap()

    @property
    def kind(self):
        ${py_doc('langkit.token_kind', 8)}
        name = _token_kind_name(self._kind)
        # The _token_kind_name wrapper is already supposed to handle exceptions
        # so this should always return a non-null value.
        assert name
        return unwrap_str(name)

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
        return self._text.wrap()

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
        return result.wrap() or u''

    @property
    def sloc_range(self):
        return self._sloc_range.wrap()

    def __eq__(self, other):
        """
        Return whether the two tokens refer to the same token in the same unit.

        Note that this does not actually compares the token data.
        """
        return (other is not None
                and self._token_data == other._token_data
                and self._token_index == other._token_index
                and self._trivia_index == other._trivia_index)

    def __repr__(self):
        return '<Token {}{}>'.format(
            self.kind,
            ' {}'.format(repr(self.text)) if self.text else ''
        )

    def to_data(self):
        """
        Return a dict representation of this Token.
        """
        return {"kind": "Token", "token_kind": self.kind, "text": self.text}


% if ctx.default_unit_file_provider:

## TODO: if this is needed some day, also bind create_unit_file_provider to
## allow Python users to create their own unit file providers.
class UnitFileProvider(object):
    ${py_doc('langkit.unit_file_provider_type', 4)}

    def __init__(self, c_value):
        self._c_value = c_value

    def __del__(self):
        _destroy_unit_file_provider(self._c_value)

${exts.include_extension(
   ctx.ext('python_api', 'unit_file_providers', 'methods')
)}

% endif

class ${root_astnode_name}(object):
    ${py_doc(T.root_node, 4)}

    is_list_type = False

    ${astnode_types.subclass_decls(T.root_node)}

    def __init__(self, c_value):
        self._c_value = c_value

    def __del__(self):
        super(${root_astnode_name}, self).__init__()

    @property
    def unit(self):
        ${py_doc('langkit.node_unit', 8)}
        return AnalysisUnit(_node_unit(self._c_value))

    @property
    def kind_name(self):
        ${py_doc('langkit.node_kind', 8)}
        return self._kind_name

    @property
    def is_ghost(self):
        ${py_doc('langkit.node_is_ghost', 8)}
        return bool(_node_is_ghost(self._c_value))

    @property
    def sloc_range(self):
        ${py_doc('langkit.node_sloc_range', 8)}
        result = SlocRange._c_type()
        _node_sloc_range(self._c_value, ctypes.byref(result))
        return result.wrap()

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
        text = _node_short_image(self._c_value)
        return text.wrap()

    def lookup(self, sloc):
        ${py_doc('langkit.lookup_in_node', 8)}
        c_sloc = Sloc._c_type.unwrap(sloc)
        c_node =_lookup_in_node(self._c_value,
                                ctypes.byref(c_sloc))
        return ${root_astnode_name}.wrap(c_node)

    def __len__(self):
        """Return the number of ${root_astnode_name} children this node has."""
        return _node_child_count(self._c_value)

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

        result = self._c_type()
        success = _node_child(self._c_value, key, ctypes.byref(result))
        if not success:
            raise IndexError('child index out of range')
        else:
            return ${root_astnode_name}.wrap(result)

    def iter_fields(self, with_fields=True, with_properties=True):
        """Iterate through all the fields this node contains

        Return an iterator that yields (name, value) couples for all abstract
        fields in this node. If "with_fields", this includes parsing fields. If
        "with_properties", this also includes properties.
        """
        for field_name in self._field_names:
            if ((field_name.startswith('f_') and with_fields) or
                    (field_name.startswith('p_') and with_properties)):
                yield (field_name, getattr(self, '{}'.format(field_name)))

    def dump(self, indent='', file=sys.stdout):
        """Dump the sub-tree in a human-readable format on the given file.

        :param str indent: Prefix printed on each line during the dump.
        :param file file: File in which the dump must occur.
        """

        def print_node(name, value):
            if isinstance(value, ${root_astnode_name}):
                print('{}{}:'.format(indent, name), file=file)
                value.dump(indent + '  ', file)
            elif isinstance(value, Token):
                print('{}{}: Token({})'.format(indent, name, repr(value.text)),
                      file=file)
            else:
                print('{}{}: {}'.format(indent, name, value), file=file)

        print('{}<{}>'.format(indent, self.kind_name), file=file)
        indent = indent + '|'
        if self.is_list_type:
            for i, value in enumerate(self):
                print_node("item {}".format(i), value)
        else:
            for name, value in self.iter_fields(with_properties=False):
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
        except Exception:
            return None

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
        if isinstance(ast_type_or_pred, type):
            sought_type = ast_type_or_pred
            pred = lambda node: isinstance(node, sought_type)
        elif isinstance(ast_type_or_pred, collections.Sequence):
            sought_types = ast_type_or_pred
            pred = lambda node: isinstance(node, tuple(sought_types))
        else:
            pred = ast_type_or_pred

        def match(left, right):
            if left is None:
                return
            if hasattr(left, "match"):
                return left.match(right)
            else:
                return left == right

        for child in self:
            if child is not None:
                if pred(child):
                    if not kwargs:
                        yield child
                    elif all([match(getattr(child, key, None), val)
                              for key, val in kwargs.items()]):
                        yield child
                for c in child.finditer(pred, **kwargs):
                    if c is not None:
                        yield c

    def __repr__(self):
        return self.short_image

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
                    for n, v in self.iter_fields(with_properties=False)
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

    class _c_type(ctypes.c_void_p):
        pass
    _c_enum_type = ctypes.c_uint

    @classmethod
    def wrap(cls, c_value):
        """
        Internal helper to wrap a low-level ASTnode value into an instance of
        the the appropriate high-level ASTNode subclass.
        """
        if not c_value:
            return None

        # First, look if we already built a wrapper for this node so that we
        # only have one wrapper per node.
        c_pyobj_p = _node_extension(c_value, _node_extension_id,
                                    _node_ext_dtor_c)
        c_pyobj_p = ctypes.cast(
            c_pyobj_p,
            ctypes.POINTER(ctypes.py_object)
        )
        if c_pyobj_p.contents:
            return c_pyobj_p.contents.value
        else:
            # Create a new wrapper for this node...
            kind = _node_kind(c_value)
            py_obj = _kind_to_astnode_cls[kind](c_value)

            # .. and store it in our extension.
            c_pyobj_p[0] = ctypes.py_object(py_obj)

            # We want to increment its ref count so that the wrapper will be
            # alive as long as the extension references it.
            ctypes.pythonapi.Py_IncRef(ctypes.py_object(py_obj))

            return py_obj

    @classmethod
    def unwrap(cls, py_value):
        """
        Internal helper to unwrap a high-level ASTNode instance into a
        low-level value. Raise a TypeError if the input value has unexpected
        type.
        """
        if py_value is None:
            return None
        if not isinstance(py_value, ${root_astnode_name}):
            raise TypeError(
                '${root_astnode_name} expected but got {} instead'.format(
                    type(py_value)
                )
            )
        return py_value._c_value


class EnvRebindings(object):
    ${py_doc('langkit.env_rebindings_type', 4)}

    def __init__(self, c_value):
        self._c_value = c_value

    class _c_type(ctypes.c_void_p):
        pass

    @classmethod
    def unwrap(cls, value):
        return 0 if value is None else value._c_value

    @classmethod
    def wrap(cls, c_value):
        return cls(c_value) if c_value else None


% for astnode in ctx.astnode_types:
    % if astnode != T.root_node:
${astnode_types.decl(astnode)}
    % endif
% endfor

UNINITIALIZED = 'uninitialized'

% for enum_type in ctx.sorted_types(ctx.enum_types):
${enum_types.decl(enum_type)}
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


% for struct_type in ctx.struct_types:
${struct_types.decl(struct_type)}
% endfor

#
# Rest of the low-level binding
#

so_ext = {
    'win32':  'dll',
    'darwin': 'dylib',
}.get(sys.platform, 'so')
_c_lib = ctypes.cdll.LoadLibrary(
    "lib${c_api.shared_object_basename}.{}".format(so_ext)
)


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
            raise exc.contents.wrap()
        return result

    return wrapper if exc_wrap else func


% for struct_type in ctx.struct_types:
${struct_types.low_level_decl(struct_type)}
% endfor


% for array_type in ctx.sorted_types(ctx.array_types):
${array_types.decl(array_type)}
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
% if ctx.default_unit_file_provider:
        _unit_file_provider,
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
% if ctx.default_unit_file_provider:
_get_analysis_unit_from_provider = _import_func(
    '${capi.get_name("get_analysis_unit_from_provider")}',
    [AnalysisContext._c_type,  # context
     _text,                    # name
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
    [AnalysisUnit._c_type], ${c_node}
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
_node_unit = _import_func(
    '${capi.get_name("node_unit")}',
    [${c_node}], AnalysisUnit._c_type
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
    [${c_node}], ${c_node_enum}
)
_kind_name = _import_func(
    '${capi.get_name("kind_name")}',
    [${c_node_enum}], _text
)
_node_is_ghost = _import_func(
    '${capi.get_name("node_is_ghost")}',
    [${c_node}], ctypes.c_int
)
_node_short_image = _import_func(
    '${capi.get_name("node_short_image")}',
    [${c_node}], _text
)
_node_sloc_range = _import_func(
    '${capi.get_name("node_sloc_range")}',
    [${c_node}, ctypes.POINTER(SlocRange._c_type)], None
)
_lookup_in_node = _import_func(
    '${capi.get_name("lookup_in_node")}',
    [${c_node}, ctypes.POINTER(Sloc._c_type)], ${c_node}
)
_node_child_count = _import_func(
    '${capi.get_name("node_child_count")}',
    [${c_node}], ctypes.c_uint
)
_node_child = _import_func(
    '${capi.get_name("node_child")}',
    [${c_node}, ctypes.c_uint, ctypes.POINTER(${c_node})], ctypes.c_int
)

% if ctx.library_fields_all_public:
# Lexical environment primitives
_lexical_env_parent = _import_func(
    '${capi.get_name("lexical_env_parent")}',
    [LexicalEnv._c_type], LexicalEnv._c_type
)
_lexical_env_node = _import_func(
    '${capi.get_name("lexical_env_node")}',
    [LexicalEnv._c_type], ${c_node}
)
_lexical_env_get = _import_func(
    '${capi.get_name("lexical_env_get")}',
    [LexicalEnv._c_type, _text],
    ${pyapi.type_internal_name(T.root_node.env_el().array_type())}
)
_lexical_env_dec_ref = _import_func(
   '${capi.get_name("lexical_env_dec_ref")}',
   [LexicalEnv._c_type], None
)
% endif

% for astnode in ctx.astnode_types:
    % for field in astnode.fields_with_accessors():
_${field.accessor_basename.lower} = _import_func(
    '${capi.get_name(field.accessor_basename)}',
    [${c_node},
     % for arg in field.explicit_arguments:
        ${pyapi.type_internal_name(arg.type)},
     % endfor
     ctypes.POINTER(${pyapi.type_internal_name(field.type)})],
    ctypes.c_int
)
    % endfor
% endfor


# Extensions handling
_register_extension = _import_func(
    '${capi.get_name("register_extension")}',
    [ctypes.c_char_p], ctypes.c_uint
)
_node_extension_destructor = ctypes.CFUNCTYPE(
    ctypes.c_void_p,
    ${c_node}, ctypes.c_void_p
)
_node_extension = _import_func(
    '${capi.get_name("node_extension")}',
    [${c_node}, ctypes.c_uint, _node_extension_destructor],
    ctypes.POINTER(ctypes.c_void_p)
)

% if ctx.default_unit_file_provider:
# Unit file providers
_destroy_unit_file_provider = _import_func(
    '${capi.get_name("destroy_unit_file_provider")}',
    [_unit_file_provider], None
)
${exts.include_extension(
   ctx.ext('python_api', 'unit_file_providers', 'low_level_bindings')
)}
% endif

# Misc
_get_last_exception = _import_func(
   '${capi.get_name("get_last_exception")}',
   [], ctypes.POINTER(_Exception),
   exc_wrap=False
)
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


#
# Layering helpers
#

def unwrap_str(c_char_p_value):
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
    ${ctx.node_kind_constants[subclass]}: ${subclass.name()},
        % endif
    % endfor
}

# We use the extension mechanism to keep a single wrapper ${root_astnode_name}
# instance per underlying AST node. This way, users can store attributes in
# wrappers and expect to find these attributes back when getting the same node
# later.

# TODO: this mechanism currently introduces reference loops between the
# ${root_astnode_name} and its wrapper. When a Python wraper is created for
# some ${root_astnode_name}, both will never be deallocated (i.e. we have
# memory leaks).  This absolutely needs to be fixed for real world use but in
# the meantime, let's keep this implementation for prototyping.

_node_extension_id = _register_extension("python_api_astnode_wrapper")
def _node_ext_dtor_py(c_node, c_pyobj):
    """
    Callback for extension upon ${root_astnode_name} destruction: free the
    reference for the Python wrapper.
    """
    # At this point, c_pyobj is a System.Address in Ada that have been decoded
    # by ctypes.c_void_p as a "long" Python object. We used to try to convert
    # it into a ctypes.py_object with:
    #   ctypes.py_object(c_pyobj)
    # but this was wrong: the result was a reference to the long object itself,
    # not to the object whose address was stored in the long. And this led to
    # random memory issues with the call to Py_DecRef... Actual casting is the
    # way to go.
    c_pyobj = ctypes.cast(c_pyobj, ctypes.py_object)
    ctypes.pythonapi.Py_DecRef(c_pyobj)


_node_ext_dtor_c = _node_extension_destructor(_node_ext_dtor_py)


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

################################
# Language specific extensions #
################################

${exts.include_extension(ctx.ext("python"))}
