## vim: filetype=makopython

<%namespace name="array_types"   file="array_types_py.mako" />
<%namespace name="astnode_types" file="astnode_types_py.mako" />
<%namespace name="enum_types"    file="enum_types_py.mako" />
<%namespace name="struct_types"  file="struct_types_py.mako" />
<%namespace name="exts"          file="/extensions.mako" />


<% root_astnode_name = _self.root_grammar_class.name().camel %>


import collections
import ctypes
import os
import sys


#
# Basic types for the low-level binding
#


class _analysis_context(ctypes.c_void_p):
    pass
class _analysis_unit(ctypes.c_void_p):
    pass
class _node(ctypes.c_void_p):
    pass
_enum_node_kind = ctypes.c_uint
class _lexical_env(ctypes.c_void_p):
    pass

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
        _destroy_text_type(ctypes.byref(self))

class _Sloc(ctypes.Structure):
    _fields_ = [("line", ctypes.c_uint32),
                ("column", ctypes.c_uint16)]

    def wrap(self):
        return Sloc(self.line, self.column)


class _SlocRange(ctypes.Structure):
    _fields_ = [("start", _Sloc),
                ("end", _Sloc)]

    def wrap(self):
        return SlocRange(self.start.wrap(), self.end.wrap())


class _Diagnostic(ctypes.Structure):
    _fields_ = [("sloc_range", _SlocRange),
                ("message", _text)]

    def wrap(self):
        return Diagnostic(self.sloc_range.wrap(),
                          self.message.wrap())


class _Exception(ctypes.Structure):
    _fields_ = [("is_fatal", ctypes.c_int),
                ("information", ctypes.c_char_p)]

    def wrap(self):
        return NativeException(self.information)


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

class PropertyError(Exception):
    ${py_doc('langkit.property_error', 4)}
    pass


#
# High-level binding
#


class AnalysisContext(object):
    ${py_doc('langkit.analysis_context_type', 4)}

    def __init__(self, charset=None):
        ${py_doc('langkit.create_context', 8)}
        self._c_value = _create_analysis_context(charset)

    def __del__(self):
        _destroy_analysis_context(self._c_value)
        super(AnalysisContext, self).__init__()

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

    def remove(self, filename):
        ${py_doc('langkit.remove_unit', 8)}
        if not _remove_analysis_unit(self._c_value, filename):
            raise KeyError('No such unit: {}'.format(filename))


class AnalysisUnit(object):
    ${py_doc('langkit.analysis_unit_type', 4)}

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

            diag = _Diagnostic()
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

    def reparse(self, buffer=None, charset=None):
        ${py_doc('langkit.unit_reparse_generic', 8)}
        if buffer is None:
            _unit_reparse_from_file(self._c_value, charset or '')
        else:
            _unit_reparse_from_buffer(self._c_value, charset or '',
                                      buffer, len(buffer))

    def populate_lexical_env(self):
        ${py_doc('langkit.unit_populate_lexical_env', 8)}
        _unit_populate_lexical_env(self._c_value)

    @property
    def root(self):
        ${py_doc('langkit.unit_root', 8)}
        return _wrap_astnode(_unit_root(self._c_value))

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

    def iter_tokens(self):
        """
        Return an iterator that yields all the tokens in this unit.
        """
        return self.TokenIterator(self.first_token)

    @property
    def diagnostics(self):
        """Diagnostics for this unit."""
        return self.DiagnosticsList(self)


class LexicalEnv(object):
    ${py_doc('langkit.lexical_env_type', 4)}

    def __init__(self, c_value):
        self._c_value = c_value

    def unwrap(self):
        return self._c_value

    @classmethod
    def wrap(self, c_value):
        return LexicalEnv(c_value) if c_value else None

    @property
    def parent(self):
        return LexicalEnv.wrap(_lexical_env_parent(self._c_value))

    @property
    def node(self):
        return _wrap_astnode(_lexical_env_node(self._c_value))

% if env_element_type:
    def get(self, name):
        ${py_doc('langkit.lexical_env_get', 8)}
        result = _lexical_env_get(self._c_value, _text.unwrap(name))
        return ${pyapi.wrap_value('result', _self.env_element.array_type())}
% endif

    def __del__(self):
        _lexical_env_dec_ref(self._c_value)
        self._c_value = None


class Token(ctypes.Structure):
    ${py_doc('langkit.token_type', 4)}

    _fields_ = [('_token_data',   ctypes.c_void_p),
                ('_token_index',  ctypes.c_int),
                ('_trivia_index', ctypes.c_int),
                ('_kind',         ctypes.c_int),
                ('_text',         _text),
                ('_sloc_range',   _SlocRange)]

    def wrap(self):
        return self if self._token_data else None

    @property
    def next(self):
        ${py_doc('langkit.token_next', 8)}
        t = Token()
        _token_next(ctypes.byref(self), ctypes.byref(t))
        return t.wrap()

    @property
    def previous(self):
        ${py_doc('langkit.token_previous', 8)}
        t = Token()
        _token_previous(ctypes.byref(self), ctypes.byref(t))
        return t.wrap()

    @property
    def kind(self):
        name = _token_kind_name(self._kind)
        # The _token_kind_name wrapper is already supposed to handle exceptions
        # so this should always return a non-null value.
        assert name
        result = ctypes.c_char_p(ctypes.addressof(name.contents)).value
        _free(name)
        return result

    @property
    def text(self):
        return self._text.wrap()

    @property
    def sloc_range(self):
        return self._sloc_range.wrap()

    def __eq__(self, other):
        """
        Return whether the two tokens refer to the same token in the same unit.

        Note that this does not actually compares the token data.
        """
        return (self._token_data == other._token_data
                and self._token_index == other._token_index
                and self._trivia_index == other._trivia_index)

    def __repr__(self):
        return '<Token {}{}>'.format(
            self.kind,
            ' {}'.format(repr(self.text)) if self.text else ''
        )


class Sloc(object):
    # TODO: document this class and its methods

    def __init__(self, line, column):
        self.line = line
        self.column = column

    def __nonzero__(self):
        return bool(self.line or self.column)

    def __str__(self):
        return '{}:{}'.format(self.line, self.column)

    def __repr__(self):
        return '<Sloc {} at {:#x}>'.format(self, id(self))


class SlocRange(object):
    # TODO: document this class and its methods

    def __init__(self, start, end):
        self.start = start
        self.end = end

    def __nonzero__(self):
        return bool(self.start or self.end)

    def __str__(self):
        return '{}-{}'.format(self.start, self.end)

    def __repr__(self):
        return "<SlocRange {}:{}-{}:{}>".format(
            self.start.line, self.start.column,
            self.end.line, self.end.column
        )


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


class ${root_astnode_name}(object):
    ${py_doc(ctx.root_grammar_class, 4)}

    ${astnode_types.subclass_decls(ctx.root_grammar_class)}

    def __init__(self, c_value):
        self._c_value = c_value

    def __del__(self):
        super(${root_astnode_name}, self).__init__()

    @property
    def kind_name(self):
        ${py_doc('langkit.node_kind', 8)}
        return self._kind_name

    @property
    def sloc_range(self):
        ${py_doc('langkit.node_sloc_range', 8)}
        result = _SlocRange()
        _node_sloc_range(self._c_value, ctypes.byref(result))
        return result.wrap()

    def lookup(self, sloc):
        ${py_doc('langkit.lookup_in_node', 8)}
        c_sloc = _unwrap_sloc(sloc)
        c_node =_lookup_in_node(self._c_value,
                                ctypes.byref(c_sloc))
        return _wrap_astnode(c_node)

    def __len__(self):
        """Return the number of ${root_astnode_name} children this node has."""
        return _node_child_count(self._c_value)

    def __getitem__(self, key):
        """Return the Nth ${root_astnode_name} child this node has.

        Raise an IndexError if "key" is out of range.
        """
        if not isinstance(key, int):
            msg = ('${root_astnode_name} children are integer-indexed'
                   ' (got {})').format(type(key))
            raise TypeError(msg)

        result = _node()
        success = _node_child(self._c_value, key, ctypes.byref(result))
        if not success:
            raise IndexError('child index out of range')
        else:
            return _wrap_astnode(result)

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
                print >> file, '{}{}:'.format(indent, name)
                value.dump(indent + '  ', file)
            elif isinstance(value, Token):
                print >> file, '{}{}: Token({})'.format(indent, name,
                                                        repr(value.text))
            else:
                print >> file, '{}{}: {}'.format(indent, name, value)

        print >> file, '{}<{}>'.format(indent, self.kind_name)
        indent = indent + '|'
        if isinstance(self, ASTList):
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
        return "<{} {}>".format(self.__class__.__name__, self.sloc_range)


class ASTList(${root_astnode_name}):
    # TODO: document this class
    _kind_name = 'list'


% for astnode in _self.astnode_types:
    % if astnode != _self.root_grammar_class:
${astnode_types.decl(astnode)}
    % endif
% endfor

UNINITIALIZED = 'uninitialized'

% for enum_type in _self.sorted_types(_self.enum_types):
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
    if not isinstance(py_value, str):
        raise TypeError('str expected but got {} instead'.format(
            type(py_value)
        ))
    try:
        return translator[py_value]
    except KeyError:
        raise ValueError('Invalid {}: {}'.format(type_name, py_value))


% for struct_type in _self.struct_types:
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


% for array_type in _self.sorted_types(_self.array_types):
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

_destroy_text_type = _import_func(
    '${capi.get_name("destroy_text_type")}', [ctypes.POINTER(_text)], None
)

# Analysis primitives
_create_analysis_context = _import_func(
    '${capi.get_name("create_analysis_context")}',
    [ctypes.c_char_p], _analysis_context
)
_destroy_analysis_context = _import_func(
    '${capi.get_name("destroy_analysis_context")}',
    [_analysis_context, ], None
)
_get_analysis_unit_from_file = _import_func(
    '${capi.get_name("get_analysis_unit_from_file")}',
    [_analysis_context,  # context
     ctypes.c_char_p,    # filename
     ctypes.c_char_p,    # charset
     ctypes.c_int],      # reparse
    _analysis_unit
)
_get_analysis_unit_from_buffer = _import_func(
    '${capi.get_name("get_analysis_unit_from_buffer")}',
    [_analysis_context,  # context
     ctypes.c_char_p,    # filename
     ctypes.c_char_p,    # charset
     ctypes.c_char_p,    # buffer
     ctypes.c_size_t],   # buffer_size
    _analysis_unit
)
_remove_analysis_unit = _import_func(
    '${capi.get_name("remove_analysis_unit")}',
    [_analysis_context, ctypes.c_char_p], ctypes.c_int
)
_unit_root = _import_func(
    '${capi.get_name("unit_root")}',
    [_analysis_unit], _node
)
_unit_first_token = _import_func(
    "${capi.get_name('unit_first_token')}",
    [_analysis_unit, ctypes.POINTER(Token)], None
)
_unit_last_token = _import_func(
    "${capi.get_name('unit_last_token')}",
    [_analysis_unit, ctypes.POINTER(Token)], None
)
_unit_diagnostic_count = _import_func(
    '${capi.get_name("unit_diagnostic_count")}',
    [_analysis_unit], ctypes.c_uint
)
_unit_diagnostic = _import_func(
    '${capi.get_name("unit_diagnostic")}',
    [_analysis_unit, ctypes.c_uint, ctypes.POINTER(_Diagnostic)], ctypes.c_int
)
_unit_incref = _import_func(
    '${capi.get_name("unit_incref")}',
    [_analysis_unit], _analysis_unit
)
_unit_decref = _import_func(
    '${capi.get_name("unit_decref")}',
    [_analysis_unit], None
)
_unit_reparse_from_file = _import_func(
    '${capi.get_name("unit_reparse_from_file")}',
    [_analysis_unit,    # context
     ctypes.c_char_p],  # charset
    ctypes.c_int
)
_unit_reparse_from_buffer = _import_func(
    '${capi.get_name("unit_reparse_from_buffer")}',
    [_analysis_unit,    # context
     ctypes.c_char_p,   # charset
     ctypes.c_char_p,   # buffer
     ctypes.c_size_t],  # buffer_size
    None
)
_unit_populate_lexical_env = _import_func(
    '${capi.get_name("unit_populate_lexical_env")}',
    [_analysis_unit],
    None
)

# General AST node primitives
_node_kind = _import_func(
    '${capi.get_name("node_kind")}',
    [_node], _enum_node_kind
)
_kind_name = _import_func(
    '${capi.get_name("kind_name")}',
    [_enum_node_kind], _text
)
_node_sloc_range = _import_func(
    '${capi.get_name("node_sloc_range")}',
    [_node, ctypes.POINTER(_SlocRange)], None
)
_lookup_in_node = _import_func(
    '${capi.get_name("lookup_in_node")}',
    [_node, ctypes.POINTER(_Sloc)], _node
)
_node_child_count = _import_func(
    '${capi.get_name("node_child_count")}',
    [_node], ctypes.c_uint
)
_node_child = _import_func(
    '${capi.get_name("node_child")}',
    [_node, ctypes.c_uint, ctypes.POINTER(_node)], ctypes.c_int
)

# Lexical environment primitives
_lexical_env_parent = _import_func(
    '${capi.get_name("lexical_env_parent")}',
    [_lexical_env], _lexical_env
)
_lexical_env_node = _import_func(
    '${capi.get_name("lexical_env_node")}',
    [_lexical_env], _node
)
% if env_element_type:
_lexical_env_get = _import_func(
    '${capi.get_name("lexical_env_get")}',
    [_lexical_env, _text],
    ${pyapi.type_internal_name(_self.env_element.array_type())}
)
% endif
_lexical_env_dec_ref = _import_func(
   '${capi.get_name("lexical_env_dec_ref")}',
   [_lexical_env], None
)

% for astnode in _self.astnode_types:
    % for field in astnode.fields_with_accessors():
_${field.accessor_basename.lower} = _import_func(
    '${capi.get_name(field.accessor_basename)}',
    [_node,
     % for _, t, _ in field.explicit_arguments:
        ${pyapi.type_internal_name(t)},
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
    _node, ctypes.c_void_p
)
_node_extension = _import_func(
    '${capi.get_name("node_extension")}',
    [_node, ctypes.c_uint, _node_extension_destructor],
    ctypes.POINTER(ctypes.c_void_p)
)

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
_token_previous = _import_func(
    "${capi.get_name('token_previous')}",
    [ctypes.POINTER(Token), ctypes.POINTER(Token)], None
)


#
# Layering helpers
#


_kind_to_astnode_cls = {
    1: ASTList,
    % for subclass in _self.astnode_types:
        % if not subclass.abstract:
    ${_self.node_kind_constants[subclass]}: ${subclass.name()},
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


def _wrap_astnode(c_value):
    """
    Internal helper to wrap a low-level ASTnode value into an instance of the
    the appropriate high-level ASTNode subclass.
    """
    if not c_value:
        return None

    # First, look if we already built a wrapper for this node so that we only
    # have one wrapper per node.
    c_pyobj_p = _node_extension(c_value, _node_extension_id, _node_ext_dtor_c)
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

        # We want to increment its ref count so that the wrapper will be alive
        # as long as the extension references it.
        ctypes.pythonapi.Py_IncRef(ctypes.py_object(py_obj))

        return py_obj


def _unwrap_astnode(py_value):
    """
    Internal helper to unwrap a high-level ASTNode instance into a low-level
    value. Raise a TypeError if the input value has unexpected type.
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
