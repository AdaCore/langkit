
"""
Python binding of the Liblktlang API.

Please consider all exported entities whose names that start with an underscore
("_") as internal implementation details. They are not meant to be used
directly.
"""







from __future__ import annotations





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
_c_lib_name = 'liblktlang.{}'.format(_so_ext)
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

# If 'os.add_dll_directory' is available (i.e. on Windows) we need to add the
# DLL directories from the PATH environment variable manually.
_add_dll_directory = getattr(os, "add_dll_directory", None)
if _add_dll_directory:
    for path in os.environ.get('PATH', '').split(os.pathsep):
        try:
            os.add_dll_directory(os.path.realpath(path))
        except FileNotFoundError as _:
            pass # Do nothing on purpose

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
    _fields_ = [
        ("kind", ctypes.c_int),
        ("information", ctypes.c_char_p),
        ("stack_trace", ctypes.c_void_p),
    ]

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
   'lkt_get_last_exception',
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
        'lkt_create_big_integer',
        [ctypes.POINTER(_text)], c_type
    ))
    text = staticmethod(_import_func(
        'lkt_big_integer_text',
        [c_type, ctypes.POINTER(_text)], None
    ))
    decref = staticmethod(_import_func(
        'lkt_big_integer_decref',
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
        'lkt_create_string',
        [ctypes.POINTER(ctypes.c_char), ctypes.c_int], c_type
    ))
    dec_ref = staticmethod(_import_func(
        'lkt_string_dec_ref',
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


class AnalysisUnitKind(_Enum):
    """
    Specify a kind of analysis unit. Specification units provide an interface
    to the outer world while body units provide an implementation for the
    corresponding interface.
    """

    unit_specification = 'unit_specification'
    unit_body = 'unit_body'

    _name = 'AnalysisUnitKind'
    _c_to_py = [
        unit_specification, unit_body]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}
class CompletionItemKind(_Enum):
    """
    Type of completion item. Refer to the official LSP specification.
    """

    text_kind = 'text_kind'
    method_kind = 'method_kind'
    function_kind = 'function_kind'
    constructor_kind = 'constructor_kind'
    field_kind = 'field_kind'
    variable_kind = 'variable_kind'
    class_kind = 'class_kind'
    interface_kind = 'interface_kind'
    module_kind = 'module_kind'
    property_kind = 'property_kind'
    unit_kind = 'unit_kind'
    value_kind = 'value_kind'
    enum_kind = 'enum_kind'
    keyword_kind = 'keyword_kind'
    snippet_kind = 'snippet_kind'
    color_kind = 'color_kind'
    file_kind = 'file_kind'
    reference_kind = 'reference_kind'
    folder_kind = 'folder_kind'
    enum_member_kind = 'enum_member_kind'
    constant_kind = 'constant_kind'
    struct_kind = 'struct_kind'
    event_kind = 'event_kind'
    operator_kind = 'operator_kind'
    type_parameter_kind = 'type_parameter_kind'

    _name = 'CompletionItemKind'
    _c_to_py = [
        text_kind, method_kind, function_kind, constructor_kind, field_kind, variable_kind, class_kind, interface_kind, module_kind, property_kind, unit_kind, value_kind, enum_kind, keyword_kind, snippet_kind, color_kind, file_kind, reference_kind, folder_kind, enum_member_kind, constant_kind, struct_kind, event_kind, operator_kind, type_parameter_kind]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}
class DesignatedEnvKind(_Enum):
    """
    Discriminant for DesignatedEnv structures.
    """

    none = 'none'
    current_env = 'current_env'
    named_env = 'named_env'
    direct_env = 'direct_env'

    _name = 'DesignatedEnvKind'
    _c_to_py = [
        none, current_env, named_env, direct_env]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}
class GrammarRule(_Enum):
    """
    Gramar rule to use for parsing.
    """

    main_rule_rule = 'main_rule_rule'
    id_rule = 'id_rule'
    ref_id_rule = 'ref_id_rule'
    type_ref_id_rule = 'type_ref_id_rule'
    def_id_rule = 'def_id_rule'
    doc_rule = 'doc_rule'
    import_stmt_rule = 'import_stmt_rule'
    imports_rule = 'imports_rule'
    lexer_decl_rule = 'lexer_decl_rule'
    grammar_decl_rule = 'grammar_decl_rule'
    grammar_rule_rule = 'grammar_rule_rule'
    lexer_case_rule_rule = 'lexer_case_rule_rule'
    lexer_case_alt_rule = 'lexer_case_alt_rule'
    lexer_case_send_rule = 'lexer_case_send_rule'
    grammar_primary_rule = 'grammar_primary_rule'
    grammar_expr_rule = 'grammar_expr_rule'
    grammar_pick_rule = 'grammar_pick_rule'
    grammar_implicit_pick_rule = 'grammar_implicit_pick_rule'
    grammar_opt_rule = 'grammar_opt_rule'
    grammar_opt_error_rule = 'grammar_opt_error_rule'
    grammar_cut_rule = 'grammar_cut_rule'
    grammar_stopcut_rule = 'grammar_stopcut_rule'
    grammar_or_expr_rule = 'grammar_or_expr_rule'
    grammar_discard_expr_rule = 'grammar_discard_expr_rule'
    token_literal_rule = 'token_literal_rule'
    token_no_case_literal_rule = 'token_no_case_literal_rule'
    token_pattern_rule = 'token_pattern_rule'
    token_pattern_literal_rule = 'token_pattern_literal_rule'
    parse_node_expr_rule = 'parse_node_expr_rule'
    grammar_rule_ref_rule = 'grammar_rule_ref_rule'
    grammar_list_expr_rule = 'grammar_list_expr_rule'
    grammar_list_sep_rule = 'grammar_list_sep_rule'
    grammar_skip_rule = 'grammar_skip_rule'
    grammar_null_rule = 'grammar_null_rule'
    grammar_token_rule = 'grammar_token_rule'
    type_decl_rule = 'type_decl_rule'
    generic_decl_rule = 'generic_decl_rule'
    generic_param_type_rule = 'generic_param_type_rule'
    enum_lit_decl_rule = 'enum_lit_decl_rule'
    fun_decl_rule = 'fun_decl_rule'
    lambda_param_decl_rule = 'lambda_param_decl_rule'
    fun_param_decl_rule = 'fun_param_decl_rule'
    fun_param_list_rule = 'fun_param_list_rule'
    lambda_param_list_rule = 'lambda_param_list_rule'
    field_decl_rule = 'field_decl_rule'
    lexer_family_decl_rule = 'lexer_family_decl_rule'
    bare_decl_rule = 'bare_decl_rule'
    decl_rule = 'decl_rule'
    type_member_ref_rule = 'type_member_ref_rule'
    type_expr_rule = 'type_expr_rule'
    type_ref_rule = 'type_ref_rule'
    type_list_rule = 'type_list_rule'
    decls_rule = 'decls_rule'
    decl_block_rule = 'decl_block_rule'
    val_decl_rule = 'val_decl_rule'
    dynvar_decl_rule = 'dynvar_decl_rule'
    var_bind_rule = 'var_bind_rule'
    env_spec_action_rule = 'env_spec_action_rule'
    env_spec_decl_rule = 'env_spec_decl_rule'
    block_rule = 'block_rule'
    pattern_rule = 'pattern_rule'
    fil_pattern_rule = 'fil_pattern_rule'
    value_pattern_rule = 'value_pattern_rule'
    regex_pattern_rule = 'regex_pattern_rule'
    bool_pattern_rule = 'bool_pattern_rule'
    ellipsis_pattern_rule = 'ellipsis_pattern_rule'
    integer_pattern_rule = 'integer_pattern_rule'
    list_pattern_rule = 'list_pattern_rule'
    tuple_pattern_rule = 'tuple_pattern_rule'
    pattern_arg_rule = 'pattern_arg_rule'
    selector_call_rule = 'selector_call_rule'
    expr_rule = 'expr_rule'
    rel_rule = 'rel_rule'
    eq_rule = 'eq_rule'
    arith_1_rule = 'arith_1_rule'
    arith_2_rule = 'arith_2_rule'
    arith_3_rule = 'arith_3_rule'
    isa_or_primary_rule = 'isa_or_primary_rule'
    logic_propagate_call_rule = 'logic_propagate_call_rule'
    primary_rule = 'primary_rule'
    match_expr_rule = 'match_expr_rule'
    num_lit_rule = 'num_lit_rule'
    big_num_lit_rule = 'big_num_lit_rule'
    string_lit_rule = 'string_lit_rule'
    block_string_lit_rule = 'block_string_lit_rule'
    char_lit_rule = 'char_lit_rule'
    if_expr_rule = 'if_expr_rule'
    raise_expr_rule = 'raise_expr_rule'
    try_expr_rule = 'try_expr_rule'
    array_literal_rule = 'array_literal_rule'
    callable_ref_rule = 'callable_ref_rule'
    null_cond_qual_rule = 'null_cond_qual_rule'
    basic_expr_rule = 'basic_expr_rule'
    term_rule = 'term_rule'
    basic_name_rule = 'basic_name_rule'
    lambda_expr_rule = 'lambda_expr_rule'
    null_lit_rule = 'null_lit_rule'
    argument_rule = 'argument_rule'
    args_rule = 'args_rule'
    decl_annotation_args_rule = 'decl_annotation_args_rule'
    decl_annotation_rule = 'decl_annotation_rule'

    _name = 'GrammarRule'
    _c_to_py = [
        main_rule_rule, id_rule, ref_id_rule, type_ref_id_rule, def_id_rule, doc_rule, import_stmt_rule, imports_rule, lexer_decl_rule, grammar_decl_rule, grammar_rule_rule, lexer_case_rule_rule, lexer_case_alt_rule, lexer_case_send_rule, grammar_primary_rule, grammar_expr_rule, grammar_pick_rule, grammar_implicit_pick_rule, grammar_opt_rule, grammar_opt_error_rule, grammar_cut_rule, grammar_stopcut_rule, grammar_or_expr_rule, grammar_discard_expr_rule, token_literal_rule, token_no_case_literal_rule, token_pattern_rule, token_pattern_literal_rule, parse_node_expr_rule, grammar_rule_ref_rule, grammar_list_expr_rule, grammar_list_sep_rule, grammar_skip_rule, grammar_null_rule, grammar_token_rule, type_decl_rule, generic_decl_rule, generic_param_type_rule, enum_lit_decl_rule, fun_decl_rule, lambda_param_decl_rule, fun_param_decl_rule, fun_param_list_rule, lambda_param_list_rule, field_decl_rule, lexer_family_decl_rule, bare_decl_rule, decl_rule, type_member_ref_rule, type_expr_rule, type_ref_rule, type_list_rule, decls_rule, decl_block_rule, val_decl_rule, dynvar_decl_rule, var_bind_rule, env_spec_action_rule, env_spec_decl_rule, block_rule, pattern_rule, fil_pattern_rule, value_pattern_rule, regex_pattern_rule, bool_pattern_rule, ellipsis_pattern_rule, integer_pattern_rule, list_pattern_rule, tuple_pattern_rule, pattern_arg_rule, selector_call_rule, expr_rule, rel_rule, eq_rule, arith_1_rule, arith_2_rule, arith_3_rule, isa_or_primary_rule, logic_propagate_call_rule, primary_rule, match_expr_rule, num_lit_rule, big_num_lit_rule, string_lit_rule, block_string_lit_rule, char_lit_rule, if_expr_rule, raise_expr_rule, try_expr_rule, array_literal_rule, callable_ref_rule, null_cond_qual_rule, basic_expr_rule, term_rule, basic_name_rule, lambda_expr_rule, null_lit_rule, argument_rule, args_rule, decl_annotation_args_rule, decl_annotation_rule]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}
class LookupKind(_Enum):
    """

    """

    recursive = 'recursive'
    flat = 'flat'
    minimal = 'minimal'

    _name = 'LookupKind'
    _c_to_py = [
        recursive, flat, minimal]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}


default_grammar_rule = GrammarRule.main_rule_rule


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


class FileReadError(Exception):
    """
    Subprograms may raise this when they cannot open a source file. Note that
    this does *not* concern analysis unit getters, which create diagnostic
    vectors for such errors.
    """
    pass
class BadTypeError(Exception):
    """
    Raised when introspection functions (``Liblktlang.Introspection``) are
    provided mismatching types/values.
    """
    pass
class OutOfBoundsError(Exception):
    """
    Raised when introspection functions (``Liblktlang.Introspection``) are
    passed an out of bounds index.
    """
    pass
class InvalidInput(Exception):
    """
    Raised by lexing functions (``Liblktlang.Lexer``) when the input contains
    an invalid byte sequence.
    """
    pass
class InvalidSymbolError(Exception):
    """
    Exception raise when an invalid symbol is passed to a subprogram.
    """
    pass
class InvalidUnitNameError(Exception):
    """
    Raised when an invalid unit name is provided.
    """
    pass
class NativeException(Exception):
    """
    Exception raised in language bindings when the underlying C API reports an
    unexpected error that occurred in the library.

    This kind of exception is raised for internal errors: they should never
    happen in normal situations and if they are raised at some point, it means
    the library state is potentially corrupted.

    Nevertheless, the library does its best not to crash the program,
    materializing internal errors using this kind of exception.
    """
    pass
class PreconditionFailure(Exception):
    """
    Exception raised when an API is called while its preconditions are not
    satisfied.
    """
    pass
class PropertyError(Exception):
    """
    Exception that is raised when an error occurs while evaluating any AST node
    method whose name starts with ``p_``. This is the only exceptions that such
    functions can raise.
    """
    pass
class TemplateArgsError(Exception):
    """
    Exception raised when the provided arguments for a template don't match
    what the template expects.
    """
    pass
class TemplateFormatError(Exception):
    """
    Exception raised when a template has an invalid syntax, such as badly
    formatted placeholders.
    """
    pass
class TemplateInstantiationError(Exception):
    """
    Exception raised when the instantiation of a template cannot be parsed.
    """
    pass
class StaleReferenceError(Exception):
    """
    Exception raised while trying to access data that was deallocated. This
    happens when one tries to use a node whose unit has been reparsed, for
    instance.
    """
    pass
class SyntaxError(Exception):
    """
    Subprograms may raise this when they try to parse invalid syntax. Note that
    this does *not* concern analysis unit getters, which create diagnostic
    vectors for such errors.
    """
    pass
class UnknownCharset(Exception):
    """
    Raised by lexing functions (``Liblktlang.Lexer``) when the input charset is
    not supported.
    """
    pass
class MalformedTreeError(Exception):
    """
    Raised when unparsing functions working on rewritten trees
    (``Liblktlang.Rewriting``) are called on malformed trees.
    """
    pass

_exception_kind_to_type = [
    FileReadError,
    BadTypeError,
    OutOfBoundsError,
    InvalidInput,
    InvalidSymbolError,
    InvalidUnitNameError,
    NativeException,
    PreconditionFailure,
    PropertyError,
    TemplateArgsError,
    TemplateFormatError,
    TemplateInstantiationError,
    StaleReferenceError,
    SyntaxError,
    UnknownCharset,
    MalformedTreeError,
]





class EventHandler(Protocol):
    """
    Interface to handle events sent by the analysis context.
    """

    def unit_requested_callback(self,
                                context: AnalysisContext,
                                name: str,
                                from_unit: AnalysisUnit,
                                found: bool,
                                is_not_found_error: bool) -> None:
        """
        Callback that will be called when a unit is requested from the context
        ``Context``.

        ``Name`` is the name of the requested unit.

        ``From`` is the unit from which the unit was requested.

        ``Found`` indicates whether the requested unit was found or not.

        ``Is_Not_Found_Error`` indicates whether the fact that the unit was not
        found is an error or not.

        .. warning:: The interface of this callback is probably subject to
           change, so should be treated as experimental.
        """
        pass

    def unit_parsed_callback(self,
                             context: AnalysisContext,
                             unit: AnalysisUnit,
                             reparsed: bool) -> None:
        """
        Callback that will be called when any unit is parsed from the context
        ``Context``.

        ``Unit`` is the resulting unit.

        ``Reparsed`` indicates whether the unit was reparsed, or whether it was
        the first parse.
        """
        pass


class AnalysisContext:
    """
    This type represents a context for all source analysis. This is the first
    type you need to create to use Liblktlang. It will contain the results of
    all analysis, and is the main holder for all the data.

    You can create several analysis contexts if you need to, which enables you,
    for example to:

    * analyze several different projects at the same time;

    * analyze different parts of the same projects in parallel.

    In the current design, contexts always keep all of their analysis units
    allocated. If you need to get this memory released, the only option at your
    disposal is to destroy your analysis context instance.
    """

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
                 tab_stop: int = 8,
                 *,
                 _c_value: Any = None) -> None:
        """
        Create a new analysis context.

        ``Charset`` will be used as a default charset to decode input sources
        in analysis units. Please see ``GNATCOLL.Iconv`` for several supported
        charsets. Be careful: passing an unsupported charset is not guaranteed
        to raise an error here. If no charset is provided, ``"utf-8"`` is the
        default.

        .. TODO: Passing an unsupported charset here is not guaranteed to raise
           an error right here, but this would be really helpful for users.

        When ``With_Trivia`` is true, the parsed analysis units will contain
        trivias.

        If provided, ``File_Reader`` will be used to fetch the contents of
        source files instead of the default, which is to just read it from the
        filesystem and decode it using the regular charset rules. Note that if
        provided, all parsing APIs that provide a buffer are forbidden, and any
        use of the rewriting API with the returned context is rejected.

        If provided, ``Unit_Provider`` will be used to query the file name that
        corresponds to a unit reference during semantic analysis. If it is
        ``None``, the default one is used instead.

        If provided, ``Event_Handler`` will be notified when various events
        happen.

        ``Tab_Stop`` is a positive number to describe the effect of tabulation
        characters on the column number in source files.
        """

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
        """
        Create a new analysis unit for ``Filename`` or return the existing one
        if any. If ``Reparse`` is true and the analysis unit already exists,
        reparse it from ``Filename``.

        ``Rule`` controls which grammar rule is used to parse the unit.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.
        """
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
        """
        Create a new analysis unit for ``Filename`` or return the existing one
        if any. Whether the analysis unit already exists or not, (re)parse it
        from the source code in ``Buffer``.

        ``Rule`` controls which grammar rule is used to parse the unit.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.
        """
        _filename = _unwrap_filename(filename)
        _charset = _unwrap_charset(charset)
        _buffer, _charset = _canonicalize_buffer(buffer, _charset)
        c_value = _get_analysis_unit_from_buffer(self._c_value, _filename,
                                                 _charset,
                                                 _buffer, len(_buffer),
                                                 GrammarRule._unwrap(rule))
        return AnalysisUnit._wrap(c_value)

    def get_from_provider(
        self,
        name: AnyStr,
        kind: str,
        charset: Opt[str] = None,
        reparse: bool = False
    ) -> AnalysisUnit:
        """
        Create a new analysis unit for ``Name``/``Kind`` or return the existing
        one if any. If ``Reparse`` is true and the analysis unit already
        exists, reparse it from the on-disk source file.

        The ``Name`` and ``Kind`` arguments are forwarded directly to query the
        context's unit provider and get the filename for the returned unit.
        ``Name`` must be a string, while ``Kind`` must be an
        ``AnalysisUnitKind`` enumeration value. See the documentation of the
        relevant unit provider for their exact semantics.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

        If the unit name cannot be tuned into a file name, raise an
        ``InvalidUnitNameError`` exception. If any other failure occurs, such
        as file opening, decoding, lexing or parsing failure, return an
        analysis unit anyway: errors are described as diagnostics of the
        returned analysis unit.
        """
        if isinstance(name, bytes):
            text_name = name.decode()
        else:
            text_name = name
        _charset = _unwrap_charset(charset)

        _name = _text._unwrap(text_name)
        _kind = AnalysisUnitKind._unwrap(kind)
        c_value = _get_analysis_unit_from_provider(
            self._c_value, ctypes.byref(_name), _kind, _charset, reparse
        )
        if c_value:
            return AnalysisUnit._wrap(c_value)
        else:
            raise InvalidUnitNameError('Invalid unit name: {} ({})'.format(
                repr(name), kind
            ))

    def discard_errors_in_populate_lexical_env(self,
                                               discard: bool) -> None:
        """
        Debug helper. Set whether ``Property_Error`` exceptions raised in
        ``Populate_Lexical_Env`` should be discarded. They are by default.
        """
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

    



class AnalysisUnit:
    """
    This type represents the analysis of a single file.
    """

    __slots__ = ('_c_value', '_context_link', '_cache_version_number',
                 '_node_cache')

    class TokenIterator:
        """
        Iterator over the tokens in an analysis unit.
        """

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

        self._node_cache: Dict[Tuple[Any, Any, Any], LktNode] = {}
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
        """
        Return the context that owns this unit.
        """
        return self._context_link

    def reparse(self,
                buffer: Opt[AnyStr] = None,
                charset: Opt[str] = None) -> None:
        """
        Reparse an analysis unit from a buffer, if provided, or from the
        original file otherwise. If ``Charset`` is empty or ``None``, use the
        last charset successfuly used for this unit, otherwise use it to decode
        the content of the source file.

        If any failure occurs, such as decoding, lexing or parsing failure,
        diagnostic are emitted to explain what happened.
        """
        _charset = _unwrap_charset(charset)
        if buffer is None:
            _unit_reparse_from_file(self._c_value, _charset)
        else:
            _buffer, _charset = _canonicalize_buffer(buffer, _charset)
            _unit_reparse_from_buffer(self._c_value, _charset, _buffer,
                                      len(_buffer))

    def populate_lexical_env(
        self,
    ) -> None:
        """
        Create lexical environments for this analysis unit, according to the
        specifications given in the language spec.

        If not done before, it will be automatically called during semantic
        analysis. Calling it before enables one to control where the latency
        occurs.

        Depending on whether errors are discarded (see
        ``Discard_Errors_In_Populate_Lexical_Env``), raise a ``Property_Error``
        on failure.
        """
        if not _unit_populate_lexical_env(
            self._c_value,
        ):
            raise PropertyError()

    @property
    def root(self) -> LktNode:
        """
        Return the root node for this unit, or ``None`` if there is none.
        """
        result = _Entity_c_type()
        _unit_root(self._c_value, ctypes.byref(result))
        return LktNode._wrap(result)

    @property
    def first_token(self) -> Opt[Token]:
        """
        Return a reference to the first token scanned in this unit.
        """
        result = Token._c_struct()
        _unit_first_token(self._c_value, ctypes.byref(result))
        return Token._wrap(result)

    @property
    def last_token(self) -> Opt[Token]:
        """
        Return a reference to the last token scanned in this unit.
        """
        result = Token._c_struct()
        _unit_last_token(self._c_value, ctypes.byref(result))
        return Token._wrap(result)

    @property
    def text(self) -> str:
        """
        Return the source buffer associated to this unit.
        """
        if self.first_token:
            assert self.last_token
            return Token.text_range(self.first_token, self.last_token)
        else:
            return ""

    @property
    def token_count(self) -> int:
        """
        Return the number of tokens in this unit.
        """
        return _unit_token_count(self._c_value)

    @property
    def trivia_count(self) -> int:
        """
        Return the number of trivias in this unit. This is 0 for units that
        were parsed with trivia analysis disabled.
        """
        return _unit_trivia_count(self._c_value)

    def lookup_token(self, sloc: Sloc) -> Opt[Token]:
        """
        Look for a token in this unit that contains the given source location.
        If this falls before the first token, return the first token. If this
        falls between two tokens, return the token that appears before. If this
        falls after the last token, return the last token. If there is no token
        in this unit, return no token.
        """
        unit = AnalysisUnit._unwrap(self)
        _sloc = Sloc._c_type._unwrap(sloc)
        result = Token._c_struct()
        _unit_lookup_token(unit, ctypes.byref(_sloc), ctypes.byref(result))
        return Token._wrap(result)

    def _dump_lexical_env(self) -> None:
        """
        Debug helper: output the lexical envs for the given analysis unit.
        """
        unit = AnalysisUnit._unwrap(self)
        _unit_dump_lexical_env(unit)

    def iter_tokens(self) -> AnalysisUnit.TokenIterator:
        """
        Iterator over the tokens in an analysis unit.
        """
        return self.TokenIterator(self.first_token)

    @property
    def filename(self) -> str:
        """
        Return the filename this unit is associated to.
        """
        filename = _unit_filename(self._c_value)
        return _unwrap_str(filename)

    @property
    def diagnostics(self) -> List[Diagnostic]:
        """
        Diagnostics for this unit.
        """
        count = _unit_diagnostic_count(self._c_value)
        result = []
        diag = Diagnostic._c_type()
        for i in range(count):
            success = _unit_diagnostic(self._c_value, i, ctypes.byref(diag))
            assert success
            result.append(diag._wrap())
        return result

    def format_gnu_diagnostic(self, d: Diagnostic) -> str:
        """
        Format a diagnostic in a GNU fashion. See
        <https://www.gnu.org/prep/standards/html_node/Errors.html>.
        """
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
    """
    Location in a source file. Line and column numbers are one-based.
    """

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
    """
    Location of a span of text in a source file.
    """

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
    """
    Diagnostic for an analysis unit: cannot open the source file, parsing
    error, ...
    """

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
    """
    Reference to a token in an analysis unit.
    """

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
        """
        Return a reference to the next token in the corresponding analysis
        unit.
        """
        self._check_stale_reference()
        result = self._c_struct()
        _token_next(ctypes.byref(self._c_value), ctypes.byref(result))
        return self._wrap(result)

    @property
    def previous(self) -> Opt[Token]:
        """
        Return a reference to the previous token in the corresponding analysis
        unit.
        """
        self._check_stale_reference()
        result = self._c_struct()
        _token_previous(ctypes.byref(self._c_value), ctypes.byref(result))
        return self._wrap(result)

    def range_until(self, other: Token) -> Iterator[Token]:
        """
        Return an iterator on the list of tokens that spans between ``self``
        and ``other`` (included). This returns an empty list if the first token
        appears after the other one in the source code. Raise a ``ValueError``
        if both tokens come from different analysis units.
        """
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
        """
        Return whether ``L`` and ``R`` are structurally equivalent tokens. This
        means that their position in the stream won't be taken into account,
        only the kind and text of the token.
        """
        self._check_stale_reference()
        self._check_token(other)
        return bool(_token_is_equivalent(
            ctypes.byref(self._c_value), ctypes.byref(other._c_value))
        )

    @property
    def kind(self) -> str:
        """
        Kind for this token.
        """
        self._check_stale_reference()
        kind = _token_get_kind(self._c_value)
        name = _token_kind_name(kind)
        # The _token_kind_name wrapper is already supposed to handle exceptions
        # so this should always return a non-null value.
        assert name
        return _unwrap_str(name)

    @property
    def is_trivia(self) -> bool:
        """
        Return whether this token is a trivia. If it's not, it's a regular
        token.
        """
        self._check_stale_reference()
        return self._c_value.trivia_index != 0

    @property
    def index(self) -> int:
        """
        Zero-based index for this token/trivia. Tokens and trivias get their
        own index space.
        """
        self._check_stale_reference()
        return (self._c_value.token_index - 1
                if self._c_value.trivia_index == 0 else
                self._c_value.trivia_index - 1)

    @property
    def text(self) -> str:
        """
        Return the text of the given token.
        """
        return self.text_range(self, self)

    @classmethod
    def text_range(cls, first: Token, last: Token) -> str:
        """
        Compute the source buffer slice corresponding to the text that spans
        between the ``First`` and ``Last`` tokens (both included). This yields
        an empty slice if ``Last`` actually appears before ``First``.

        This raises a ``ValueError`` if ``First`` and ``Last`` don't belong to
        the same analysis unit.
        """
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
        """
        Return the source location range of the given token.
        """
        self._check_stale_reference()
        result = SlocRange._c_type()
        _token_sloc_range(ctypes.byref(self._c_value), ctypes.byref(result))
        return result._wrap()

    def __eq__(self, other: Any) -> bool:
        """
        Return whether the two tokens refer to the same token in the same unit.

        Note that this does not actually compares the token data.
        """
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
        """
        Consider that None comes before all tokens. Then, sort by unit, token
        index, and trivia index.
        """
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
        """
        Return a dict representation of this Token.
        """
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


class FileReader:
    """
    Interface to override how source files are fetched and decoded.
    """

    def __init__(self, c_value: Any):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly.
        """
        self._c_value = c_value

    def __del__(self) -> None:
        _dec_ref_file_reader(self._c_value)





class UnitProvider:
    """
    Interface to fetch analysis units from a name and a unit kind.

    The unit provider mechanism provides an abstraction which assumes that to
    any couple (unit name, unit kind) we can associate at most one source file.
    This means that several couples can be associated to the same source file,
    but on the other hand, only one one source file can be associated to a
    couple.

    This is used to make the semantic analysis able to switch from one analysis
    units to another.

    See the documentation of each unit provider for the exact semantics of the
    unit name/kind information.
    """

    def __init__(self, c_value: Any):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly.
        """
        self._c_value = c_value

    def __del__(self) -> None:
        _dec_ref_unit_provider(self._c_value)


      
    @classmethod
    def from_directories(cls, directories: list[str]) -> UnitProvider:
        """
        Return a unit provider that will look for units in the given list of
        directories. Note that the current directory is implicitly looked at
        first.
        """
        # Create a NULL-terminated array of strings
        c_strings = [
            ctypes.c_char_p(
                _coerce_bytes("directories", d, "a list of bytes strings")
            )
            for d in directories
        ]
        c_array_type = ctypes.c_char_p * (len(directories) + 1)
        c_array = c_array_type()
        for i, c_str in enumerate(c_strings):
            c_array[i] = c_str
        c_array[-1] = None

        c_array_ptr = ctypes.pointer(c_array)
        directories_arg = ctypes.cast(
            c_array_ptr, ctypes.POINTER(ctypes.c_char_p)
        )

        c_value = _create_default_provider(directories_arg)
        return cls(c_value)

    @classmethod
    def from_lkt_path(cls) -> UnitProvider:
        """
        Return a unit provider created from the ``LKT_PATH`` environment
        variable.
        """
        return cls.from_directories(
            os.environ.get("LKT_PATH", "").split(os.path.pathsep)
        )




class LktNode:
    """
    Root node class for lkt AST nodes.

    Derived nodes: :py:class:`Argument`, :py:class:`BaseLexerCaseRuleAlt`,
    :py:class:`BaseMatchBranch`, :py:class:`BlockExprClause`,
    :py:class:`BlockStringLine`, :py:class:`ClassQualifier`,
    :py:class:`DeclAnnotationArgs`, :py:class:`DeclAnnotation`,
    :py:class:`Decl`, :py:class:`DynEnvWrapper`, :py:class:`ElsifBranch`,
    :py:class:`EnumClassCase`, :py:class:`ExcludesNull`, :py:class:`Expr`,
    :py:class:`FullDecl`, :py:class:`GrammarListSep`, :py:class:`Import`,
    :py:class:`LangkitRoot`, :py:class:`LexerCaseRuleSend`,
    :py:class:`LexerCaseRule`, :py:class:`ListKind`,
    :py:class:`LktNodeBaseList`, :py:class:`NullCondQualifier`, :py:class:`Op`,
    :py:class:`PatternDetail`, :py:class:`Pattern`, :py:class:`SelectorCall`,
    :py:class:`TypeRef`, :py:class:`VarBind`
    """

    is_list_type = False
    __slots__ = ('_unprotected_c_value', '_node_c_value', '_metadata',
                 '_rebindings', '_unprotected_getitem_cache', '_unit',
                 '_unit_version', '_rebindings_version')

    _kind_name: str
    _field_names: Tuple[str, ...]

    
    

    
    @property
    def parent(
        self
    ) -> LktNode:
        """
        Return the syntactic parent for this node. Return null for the root
        node.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_parent)
        result = LktNode._wrap(c_result)


        return result
    
    def parents(
        self, with_self: bool = True
    ) -> List[LktNode]:
        """
        Return an array that contains the lexical parents, this node included
        iff ``with_self`` is True. Nearer parents are first in the list.
        """
        

        

        unwrapped_with_self = bool(with_self)

        
        c_result = self._eval_field(_LktNodeArrayConverter.c_type(), _lkt_node_parents, unwrapped_with_self)
        result = _LktNodeArrayConverter.wrap(c_result, False)


        return result
    
    @property
    def children(
        self
    ) -> List[LktNode]:
        """
        Return an array that contains the direct lexical children.

        .. warning:: This constructs a whole array every-time you call it, and
           as such is less efficient than calling the ``Child`` built-in.
        """
        

        


        
        c_result = self._eval_field(_LktNodeArrayConverter.c_type(), _lkt_node_children)
        result = _LktNodeArrayConverter.wrap(c_result, False)


        return result
    
    @property
    def token_start(
        self
    ) -> Opt[Token]:
        """
        Return the first token used to parse this node.
        """
        

        


        
        c_result = self._eval_field(Token._c_struct(), _lkt_node_token_start)
        result = Token._wrap(c_result)


        return result
    
    @property
    def token_end(
        self
    ) -> Opt[Token]:
        """
        Return the last token used to parse this node.
        """
        

        


        
        c_result = self._eval_field(Token._c_struct(), _lkt_node_token_end)
        result = Token._wrap(c_result)


        return result
    
    @property
    def child_index(
        self
    ) -> int:
        """
        Return the 0-based index for Node in its parent's children.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_int(), _lkt_node_child_index)
        result = c_result.value


        return result
    
    @property
    def previous_sibling(
        self
    ) -> LktNode:
        """
        Return the node's previous sibling, or null if there is no such
        sibling.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_previous_sibling)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def next_sibling(
        self
    ) -> LktNode:
        """
        Return the node's next sibling, or null if there is no such sibling.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_next_sibling)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def unit(
        self
    ) -> AnalysisUnit:
        """
        Return the analysis unit owning this node.
        """
        

        


        
        c_result = self._eval_field(AnalysisUnit._c_type(), _lkt_node_unit)
        result = AnalysisUnit._wrap(c_result)


        return result
    
    @property
    def is_ghost(
        self
    ) -> bool:
        """
        Return whether the node is a ghost.

        Unlike regular nodes, ghost nodes cover no token in the input source:
        they are logically located instead between two tokens. Both the
        ``token_start`` and the ``token_end`` of all ghost nodes is the token
        right after this logical position.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _lkt_node_is_ghost)
        result = bool(c_result.value)


        return result
    
    @property
    def full_sloc_image(
        self
    ) -> str:
        """
        Return a string containing the filename + the sloc in GNU conformant
        format. Useful to create diagnostics from a node.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _lkt_node_full_sloc_image)
        result = _String.wrap(c_result)


        return result
    
    def completion_item_kind_to_int(
        self, kind: str
    ) -> int:
        """
        Convert a CompletionItemKind enum to its corresponding integer value.
        """
        

        

        unwrapped_kind = CompletionItemKind._unwrap(kind)

        
        c_result = self._eval_field(ctypes.c_int(), _lkt_node_completion_item_kind_to_int, unwrapped_kind)
        result = c_result.value


        return result
    
    def p_set_solver_debug_mode(
        self, enable: bool
    ) -> bool:
        """
        Enable or disable the solver traces for debugging purposes.
        """
        

        

        unwrapped_enable = bool(enable)

        
        c_result = self._eval_field(ctypes.c_uint8(), _lkt_node_p_set_solver_debug_mode, unwrapped_enable)
        result = bool(c_result.value)


        return result
    
    @property
    def p_basic_trait_gen(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the ``BasicTrait`` builtin generic trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_basic_trait_gen)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_basic_trait(
        self
    ) -> TraitDecl:
        """
        Unit method. Return the ``BasicTrait`` builtin trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_basic_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_node_gen_trait(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the ``Node`` builtin generic trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_node_gen_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_node_trait(
        self
    ) -> TraitDecl:
        """
        Unit method. Return the ``Node`` builtin trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_node_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_indexable_gen_trait(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the ``Node`` builtin generic trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_indexable_gen_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_indexable_trait(
        self
    ) -> TraitDecl:
        """
        Unit method. Return the ``Node`` builtin trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_indexable_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_token_node_trait(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the ``TokenNode`` builtin trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_token_node_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_error_node_trait(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the ``ErrorNode`` builtin trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_error_node_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_char_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the character builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_char_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_int_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the integer builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_int_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_bool_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the boolean builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_bool_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_bigint_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the big integer builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_bigint_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_string_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the string builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_string_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_symbol_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the string builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_symbol_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_property_error_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the property error builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_property_error_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_regexp_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the regexp builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_regexp_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_entity_gen_type(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the logicvar builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_entity_gen_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_entity_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the logicvar builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_entity_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_logicvar_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the logicvar builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_logicvar_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_equation_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the logicvar builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_equation_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_array_gen_type(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the array builtin generic type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_array_gen_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_array_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the array builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_array_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_astlist_gen_type(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the ASTList builtin generic type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_astlist_gen_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_astlist_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the ASTList builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_astlist_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_node_builder_gen_type(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the NodeBuilder builtin generic type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_node_builder_gen_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_node_builder_type(
        self
    ) -> NamedTypeDecl:
        """
        Unit method. Return the NodeBuilder builtin type.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_node_builder_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_iterator_gen_trait(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the Iterator builtin generic trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_iterator_gen_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_iterator_trait(
        self
    ) -> TraitDecl:
        """
        Unit method. Return the Iterator builtin trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_iterator_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_analysis_unit_gen_trait(
        self
    ) -> GenericDecl:
        """
        Unit method. Return the ``AnalysisUnit`` builtin generic trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_analysis_unit_gen_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_analysis_unit_trait(
        self
    ) -> TraitDecl:
        """
        Unit method. Return the ``AnalysisUnit`` builtin trait.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _lkt_node_p_analysis_unit_trait)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_topmost_invalid_decl(
        self
    ) -> LktNode:
        """
        Return the topmost (from ``Self`` to the root node) FullDecl annotated
        with ``@invalid``, null otherwise.
        """
        

        

        result = self._eval_astnode_field(_lkt_node_p_topmost_invalid_decl)



        return result
    
    @property
    def p_nameres_diagnostics(
        self
    ) -> List[SolverDiagnostic]:
        """
        If name resolution on this lkt compilation unit fails, this returns all
        the diagnostics that were produced while resolving it.
        """
        

        


        
        c_result = self._eval_field(_SolverDiagnosticArrayConverter.c_type(), _lkt_node_p_nameres_diagnostics)
        result = _SolverDiagnosticArrayConverter.wrap(c_result, False)


        return result
    
    @property
    def p_solve_enclosing_context(
        self
    ) -> SolverResult:
        """
        Finds the nearest parent that is an xref_entry_point and solve its
        equation.
        """
        

        


        
        c_result = self._eval_field(SolverResult._c_type(), _lkt_node_p_solve_enclosing_context)
        result = SolverResult._wrap(c_result)


        return result
    
    @property
    def p_xref_entry_point(
        self
    ) -> bool:
        """
        Designates entities that are entry point for the xref solving
        infrastructure. If this returns true, then nameres_diagnostics can be
        called on it.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _lkt_node_p_xref_entry_point)
        result = bool(c_result.value)


        return result
    
    @property
    def p_complete(
        self
    ) -> List[CompleteItem]:
        """
        Return an array of completion item for language server clients
        """
        

        


        
        c_result = self._eval_field(_CompleteItemArrayConverter.c_type(), _lkt_node_p_complete)
        result = _CompleteItemArrayConverter.wrap(c_result, False)


        return result

    _field_names = () + (
    )




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
                                              Opt[LktNode]] = {}
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
    def _getitem_cache(self) -> Dict[int, Opt[LktNode]]:
        self._check_stale_reference()
        return self._unprotected_getitem_cache

    def __eq__(self, other: Any) -> bool:
        return (
            isinstance(other, LktNode)
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
        """
        Return the kind of this node.
        """
        return self._kind_name

    @property
    def is_token_node(self) -> bool:
        """
        Return whether this node is a node that contains only a single token.
        """
        node = self._unwrap(self)
        return bool(_node_is_token_node(ctypes.byref(node)))

    @property
    def is_synthetic(self) -> bool:
        """
        Return whether this node is synthetic.
        """
        node = self._unwrap(self)
        return bool(_node_is_synthetic(ctypes.byref(node)))

    @property
    def sloc_range(self) -> SlocRange:
        """
        Return the spanning source location range for this node.

        Note that this returns the sloc of the parent for synthetic nodes.
        """
        node = self._unwrap(self)
        result = SlocRange._c_type()
        _node_sloc_range(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def text(self) -> str:
        """
        Return the source buffer slice corresponding to the text that spans
        between the first and the last tokens of this node.

        Note that this returns the empty string for synthetic nodes.
        """
        node = self._unwrap(self)
        result = _text()
        _node_text(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def image(self) -> str:
        """
        Return a representation of this node as a string.
        """
        c_node = self._unwrap(self)
        c_result = _text()
        _node_image(ctypes.byref(c_node), ctypes.byref(c_result))
        return c_result._wrap()

    def lookup(self, sloc: Sloc) -> Opt[LktNode]:
        """
        Return the bottom-most node from in ``Node`` and its children which
        contains ``Sloc``, or ``None`` if there is none.
        """
        node = self._unwrap(self)
        c_sloc = Sloc._c_type._unwrap(sloc)
        result = _Entity_c_type()
        _lookup_in_node(ctypes.byref(node), ctypes.byref(c_sloc),
                        ctypes.byref(result))
        return LktNode._wrap(result)

    def __bool__(self) -> bool:
        """
        Return always True so that checking a node against None can be done as
        simply as:

        .. code::

           if node:
               ...
        """
        return True

    def __iter__(self) -> Iterator[Opt[LktNode]]:
        """
        Return an iterator on the children of this node.
        """
        for i in range(len(self)):
            yield self[i]

    def __len__(self) -> int:
        """
        Return the number of LktNode children this node has.
        """
        node = self._unwrap(self)
        return _node_children_count(ctypes.byref(node))

    def __getitem__(self, key: int) -> Opt[LktNode]:
        """
        Return the Nth LktNode child this node has.

        This handles negative indexes the same way Python lists do. Raise an
        IndexError if "key" is out of range.
        """
        if not isinstance(key, int):
            msg = ('LktNode children are integer-indexed'
                   ' (got {})').format(type(key))
            raise TypeError(msg)

        if key < 0:
            key += len(self)

        if key in self._getitem_cache:
            return self._getitem_cache[key]

        node = self._unwrap(self)
        result_struct = _Entity_c_type()
        success = _node_child(
            ctypes.byref(node), key, ctypes.byref(result_struct)
        )
        if not success:
            raise IndexError('child index out of range')
        else:
            result = LktNode._wrap(result_struct)
            self._getitem_cache[key] = result
            return result

    def iter_fields(self) -> Iterator[Tuple[str, Opt[LktNode]]]:
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

    def dump_str(self) -> str:
        """
        Dump the sub-tree to a string in a human-readable format.
        """
        output = io.StringIO()
        self.dump(file=output)
        ret = output.getvalue()
        output.close()
        return ret

    def dump(self, indent: str = '', file: IO[str] = sys.stdout) -> None:
        """
        Dump the sub-tree in a human-readable format on the given file.

        :param str indent: Prefix printed on each line during the dump.

        :param file file: File in which the dump must occur.
        """

        def print_node(name, value):
            if isinstance(value, LktNode):
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
        ast_type_or_pred: Union[Type[LktNode],
                                Callable[[LktNode], bool]],
        **kwargs: Any
    ) -> List[LktNode]:
        """
        Helper for finditer that will return all results as a list. See
        finditer's documentation for more details.
        """
        return list(self.finditer(ast_type_or_pred, **kwargs))

    def find(
        self,
        ast_type_or_pred: Union[Type[LktNode],
                                Callable[[LktNode], bool]],
        **kwargs: Any
    ) -> Opt[LktNode]:
        """
        Helper for finditer that will return only the first result. See
        finditer's documentation for more details.
        """
        try:
            return next(self.finditer(ast_type_or_pred, **kwargs))
        except StopIteration:
            return None

    def finditer(
        self,
        ast_type_or_pred: Union[Type[LktNode],
                                Callable[[LktNode], bool]],
        **kwargs: Any
    ) -> Iterator[LktNode]:
        """
        Find every node corresponding to the passed predicates.

        :param ast_type_or_pred: If supplied with a subclass of LktNode, will
           constrain the resulting collection to only the instances of this
           type or any subclass. If supplied with a predicate, it will apply
           the predicate on every node and keep only the ones for which it
           returns True. If supplied with a list of subclasses of LktNode, it
           will match all instances of any of them.

        :param kwargs: Allows the user to filter on attributes of the node. For
           every key value association, if the node has an attribute of name
           key that has the specified value, then the child is kept.
        """
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
    def parent_chain(self) -> List[LktNode]:
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

    def __repr__(self) -> str:
        return self.image

    @property
    def tokens(self) -> Iterator[Token]:
        """
        Return an iterator on the range of tokens that self encompasses.
        """
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

    def to_json(self) -> str:
        """
        Return a JSON representation of this node.
        """
        return json.dumps(self.to_data())

    def is_a(self, *types: Type[LktNode]) -> bool:
        """
        Shortcut for isinstance(self, types). :rtype: bool
        """
        return isinstance(self, tuple(types))

    if TYPE_CHECKING:
        T = TypeVar('T', bound=LktNode)

    def cast(self, typ: Type[T]) -> T:
        """
        Fluent interface style method. Return ``self``, raise an error if self
        is not of type ``typ``.
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
    def _wrap_bare_node(cls, c_value: Any) -> Opt[LktNode]:
        return cls._wrap(_Entity_c_type.from_bare_node(c_value))

    @classmethod
    def _unwrap(cls, py_value: Opt[LktNode]) -> Any:
        """
        Internal helper to unwrap a high-level ASTNode instance into a
        low-level value. Raise a TypeError if the input value has unexpected
        type.
        """
        if py_value is None:
            return _Entity_c_type._null_value
        elif not isinstance(py_value, cls):
            _raise_type_error(cls.__name__, py_value)
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
        return AnalysisUnit._wrap(_node_unit(ctypes.byref(c_value)))

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
        return LktNode._wrap(
            self._eval_field(_Entity_c_type(), c_accessor)
        )




class Argument(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Argument for function calls or for annotations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> RefId:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_argument_f_name)



        return result
    
    @property
    def f_value(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_argument_f_value)



        return result

    _field_names = LktNode._field_names + (
        "f_name",
        "f_value",
    )

    _kind_name = 'Argument'






class BaseLexerCaseRuleAlt(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Base class for the different kind of alternatives allowed in a case rule.

    Derived nodes: :py:class:`ErrorLexerCaseRuleAlt`,
    :py:class:`LexerCaseRuleCondAlt`, :py:class:`LexerCaseRuleDefaultAlt`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNode._field_names + (
    )







class ErrorLexerCaseRuleAlt(BaseLexerCaseRuleAlt):
    """
    Subclass of :py:class:`BaseLexerCaseRuleAlt`.

    Placeholder node for syntax errors in case rules.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseLexerCaseRuleAlt._field_names + (
    )

    _kind_name = 'ErrorLexerCaseRuleAlt'






class LexerCaseRuleCondAlt(BaseLexerCaseRuleAlt):
    """
    Subclass of :py:class:`BaseLexerCaseRuleAlt`.

    Alternative of a case rule which sends the token only if the kind of the
    previous token is among a given set.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_cond_exprs(
        self
    ) -> RefIdList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_case_rule_cond_alt_f_cond_exprs)



        return result
    
    @property
    def f_send(
        self
    ) -> LexerCaseRuleSend:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_case_rule_cond_alt_f_send)



        return result

    _field_names = BaseLexerCaseRuleAlt._field_names + (
        "f_cond_exprs",
        "f_send",
    )

    _kind_name = 'LexerCaseRuleCondAlt'






class LexerCaseRuleDefaultAlt(BaseLexerCaseRuleAlt):
    """
    Subclass of :py:class:`BaseLexerCaseRuleAlt`.

    Default alternative of a case rule which sends the token if all the
    previous alternatives failed.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_send(
        self
    ) -> LexerCaseRuleSend:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_case_rule_default_alt_f_send)



        return result

    _field_names = BaseLexerCaseRuleAlt._field_names + (
        "f_send",
    )

    _kind_name = 'LexerCaseRuleDefaultAlt'






class BaseMatchBranch(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Abstract base class for match branches, exists to accommodate the existence
    of two different syntaxes.

    Derived nodes: :py:class:`MatchBranch`, :py:class:`PatternMatchBranch`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_base_match_branch_f_expr)



        return result
    
    @property
    def p_match_part(
        self
    ) -> LktNode:
        """
        Return the "match" part of the branch, either a pattern branch or a
        legacy match branch with variable declaration.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _base_match_branch_p_match_part)
        result = LktNode._wrap(c_result)


        return result

    _field_names = LktNode._field_names + (
    )







class MatchBranch(BaseMatchBranch):
    """
    Subclass of :py:class:`BaseMatchBranch`.

    Branch inside a match expression. Classic limited Lkt syntax based on
    ``case <name> : <type> => <expr>``, for the moment, the only supported
    syntax in Lkt.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_decl(
        self
    ) -> MatchValDecl:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_match_branch_f_decl)



        return result

    _field_names = BaseMatchBranch._field_names + (
        "f_decl",
        "f_expr",
    )

    _kind_name = 'MatchBranch'






class PatternMatchBranch(BaseMatchBranch):
    """
    Subclass of :py:class:`BaseMatchBranch`.

    Branch inside a match expression. LKQL pattern based syntax ``case
    <pattern> => <expr>``.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`OrPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_pattern_match_branch_f_pattern)



        return result

    _field_names = BaseMatchBranch._field_names + (
        "f_pattern",
        "f_expr",
    )

    _kind_name = 'PatternMatchBranch'






class BlockExprClause(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Clause (value declaration or dynamic variable binding) in a block
    expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_clause(
        self
    ) -> LktNode:
        """
        This field can contain one of the following nodes:
        :py:class:`FullDecl`, :py:class:`VarBind`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_block_expr_clause_f_clause)



        return result

    _field_names = LktNode._field_names + (
        "f_clause",
    )

    _kind_name = 'BlockExprClause'






class BlockStringLine(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    A single line in a block string literal.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNode._field_names + (
    )

    _kind_name = 'BlockStringLine'






class ClassQualifier(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Whether this generic parameter type must be a class.

    Derived nodes: :py:class:`ClassQualifierAbsent`,
    :py:class:`ClassQualifierPresent`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_as_bool(
        self
    ) -> bool:
        """
        Return whether this node is present
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _class_qualifier_p_as_bool)
        result = bool(c_result.value)


        return result

    _field_names = LktNode._field_names + (
    )







class ClassQualifierAbsent(ClassQualifier):
    """
    Subclass of :py:class:`ClassQualifier`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ClassQualifier._field_names + (
    )

    _kind_name = 'ClassQualifierAbsent'






class ClassQualifierPresent(ClassQualifier):
    """
    Subclass of :py:class:`ClassQualifier`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ClassQualifier._field_names + (
    )

    _kind_name = 'ClassQualifierPresent'






class Decl(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Base class for declarations. Encompasses regular declarations as well as
    special declarations such as grammars, grammar rules, etc.

    Derived nodes: :py:class:`BaseGrammarRuleDecl`, :py:class:`BaseValDecl`,
    :py:class:`EnvSpecDecl`, :py:class:`ErrorDecl`, :py:class:`GenericDecl`,
    :py:class:`GrammarDecl`, :py:class:`LexerDecl`,
    :py:class:`LexerFamilyDecl`, :py:class:`SynthFunDecl`,
    :py:class:`SynthParamDecl`, :py:class:`TypeDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_syn_name(
        self
    ) -> DefId:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_decl_f_syn_name)



        return result
    
    @property
    def p_custom_image(
        self
    ) -> str:
        """
        Return the image string using entity information.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _decl_p_custom_image)
        result = _String.wrap(c_result)


        return result
    
    @property
    def p_decl_type_name(
        self
    ) -> str:
        """
        Return the name of the declaration type, as it should be seen by
        users/shown in diagnostics.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _decl_p_decl_type_name)
        result = _String.wrap(c_result)


        return result
    
    @property
    def p_def_ids(
        self
    ) -> List[DefId]:
        """
        Return all the defining names that this declaration defines.
        """
        

        


        
        c_result = self._eval_field(_LktNodeArrayConverter.c_type(), _decl_p_def_ids)
        result = _LktNodeArrayConverter.wrap(c_result, False)


        return result
    
    @property
    def p_as_bare_decl(
        self
    ) -> Decl:
        """
        Get this declaration without rebindings information.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _decl_p_as_bare_decl)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_get_type(
        self
    ) -> TypeDecl:
        """
        Return the type of the Decl.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _decl_p_get_type)
        result = LktNode._wrap(c_result)


        return result
    
    def p_get_cast_type(
        self, cast_to: TypeDecl
    ) -> TypeDecl:
        """
        If we are casting an entity (Self) to something that is not an entity,
        make it an entity.
        """
        

        

        unwrapped_cast_to = TypeDecl._unwrap(cast_to)

        
        c_result = self._eval_field(_Entity_c_type(), _decl_p_get_cast_type, unwrapped_cast_to)
        result = LktNode._wrap(c_result)


        return result
    
    def p_get_keep_type(
        self, keep_type: TypeDecl
    ) -> TypeDecl:
        """
        Return the type of Entity when we only keep elements of type keep_type.
        If we are casting an entity (Self) to something that is not an entity,
        make it an entity.
        """
        

        

        unwrapped_keep_type = TypeDecl._unwrap(keep_type)

        
        c_result = self._eval_field(_Entity_c_type(), _decl_p_get_keep_type, unwrapped_keep_type)
        result = LktNode._wrap(c_result)


        return result
    
    def p_get_suffix_type(
        self, prefix_type: TypeDecl
    ) -> TypeDecl:
        """
        If we are accessing a ParseField of an entity, then that field's type
        also needs to be an entity.
        """
        

        

        unwrapped_prefix_type = TypeDecl._unwrap(prefix_type)

        
        c_result = self._eval_field(_Entity_c_type(), _decl_p_get_suffix_type, unwrapped_prefix_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_is_generic(
        self
    ) -> bool:
        """
        Returns whether the Decl is generic.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _decl_p_is_generic)
        result = bool(c_result.value)


        return result
    
    @property
    def p_return_type_is_instantiated(
        self
    ) -> bool:
        """
        Return True if the return type of this function is instantiated.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _decl_p_return_type_is_instantiated)
        result = bool(c_result.value)


        return result
    
    @property
    def p_is_instantiated(
        self
    ) -> bool:
        """
        Return True if Self is an instantiated declaration, meaning that it
        does not use any of its declared generic types.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _decl_p_is_instantiated)
        result = bool(c_result.value)


        return result
    
    @property
    def p_name(
        self
    ) -> str:
        """
        Return the symbol corresponding to the name of this declaration.
        """
        

        


        
        c_result = self._eval_field(_symbol_type(), _decl_p_name)
        result = _symbol_type.wrap(c_result)


        return result
    
    @property
    def p_full_name(
        self
    ) -> str:
        """
        Return the full name of this decl, as it should be seen by users/shown
        in diagnostics.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _decl_p_full_name)
        result = _String.wrap(c_result)


        return result

    _field_names = LktNode._field_names + (
    )







class BaseGrammarRuleDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Base class for grammar rules inside of grammars/lexers.

    Derived nodes: :py:class:`GrammarRuleDecl`, :py:class:`SyntheticLexerDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_base_grammar_rule_decl_f_expr)



        return result

    _field_names = Decl._field_names + (
    )







class GrammarRuleDecl(BaseGrammarRuleDecl):
    """
    Subclass of :py:class:`BaseGrammarRuleDecl`.

    Declaration of a grammar rule inside of a grammar.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseGrammarRuleDecl._field_names + (
        "f_syn_name",
        "f_expr",
    )

    _kind_name = 'GrammarRuleDecl'






class SyntheticLexerDecl(BaseGrammarRuleDecl):
    """
    Subclass of :py:class:`BaseGrammarRuleDecl`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseGrammarRuleDecl._field_names + (
    )

    _kind_name = 'SyntheticLexerDecl'






class BaseValDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Abstract class for named values declarations, such as parameters, local
    value bindings, fields, etc.

    Derived nodes: :py:class:`NodeDecl`, :py:class:`SelfDecl`,
    :py:class:`UserValDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Decl._field_names + (
    )







class NodeDecl(BaseValDecl):
    """
    Subclass of :py:class:`BaseValDecl`.

    Synthetic declaration for the implicit "node" variable available in
    properties.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseValDecl._field_names + (
    )

    _kind_name = 'NodeDecl'






class SelfDecl(BaseValDecl):
    """
    Subclass of :py:class:`BaseValDecl`.

    Synthetic declaration for the implicit "self" variable available in
    properties.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseValDecl._field_names + (
    )

    _kind_name = 'SelfDecl'






class UserValDecl(BaseValDecl):
    """
    Subclass of :py:class:`BaseValDecl`.

    Class for user declared val declarations (not synthetic).

    Derived nodes: :py:class:`BindingValDecl`, :py:class:`EnumLitDecl`,
    :py:class:`ExplicitlyTypedDecl`, :py:class:`FunDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseValDecl._field_names + (
    )







class BindingValDecl(UserValDecl):
    """
    Subclass of :py:class:`UserValDecl`.

    Variable declaration in pattern

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = UserValDecl._field_names + (
        "f_syn_name",
    )

    _kind_name = 'BindingValDecl'






class EnumLitDecl(UserValDecl):
    """
    Subclass of :py:class:`UserValDecl`.

    Enum literal declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = UserValDecl._field_names + (
        "f_syn_name",
    )

    _kind_name = 'EnumLitDecl'






class ExplicitlyTypedDecl(UserValDecl):
    """
    Subclass of :py:class:`UserValDecl`.

    Subset of user declared value declarations for values that have a type that
    can be syntactically annotated by the user.

    Derived nodes: :py:class:`ComponentDecl`, :py:class:`DynVarDecl`,
    :py:class:`MatchValDecl`, :py:class:`ValDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_decl_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_explicitly_typed_decl_f_decl_type)



        return result

    _field_names = UserValDecl._field_names + (
    )







class ComponentDecl(ExplicitlyTypedDecl):
    """
    Subclass of :py:class:`ExplicitlyTypedDecl`.

    Subset of explicitly typed declarations for value declarations that:

    1. Have an optional default value.

    2. Are part of a bigger declaration that can be referred to via a call
       expression (either a type or a function).

    Derived nodes: :py:class:`FieldDecl`, :py:class:`FunParamDecl`,
    :py:class:`LambdaParamDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_default_val(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_component_decl_f_default_val)



        return result

    _field_names = ExplicitlyTypedDecl._field_names + (
    )







class FieldDecl(ComponentDecl):
    """
    Subclass of :py:class:`ComponentDecl`.

    Field declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_trait_ref(
        self
    ) -> DotExpr:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_field_decl_f_trait_ref)



        return result

    _field_names = ComponentDecl._field_names + (
        "f_syn_name",
        "f_decl_type",
        "f_trait_ref",
        "f_default_val",
    )

    _kind_name = 'FieldDecl'






class FunParamDecl(ComponentDecl):
    """
    Subclass of :py:class:`ComponentDecl`.

    Function parameter declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_decl_annotations(
        self
    ) -> DeclAnnotationList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_fun_param_decl_f_decl_annotations)



        return result

    _field_names = ComponentDecl._field_names + (
        "f_decl_annotations",
        "f_syn_name",
        "f_decl_type",
        "f_default_val",
    )

    _kind_name = 'FunParamDecl'






class LambdaParamDecl(ComponentDecl):
    """
    Subclass of :py:class:`ComponentDecl`.

    Function parameter declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ComponentDecl._field_names + (
        "f_syn_name",
        "f_decl_type",
        "f_default_val",
    )

    _kind_name = 'LambdaParamDecl'






class DynVarDecl(ExplicitlyTypedDecl):
    """
    Subclass of :py:class:`ExplicitlyTypedDecl`.

    Dynamic variable declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ExplicitlyTypedDecl._field_names + (
        "f_syn_name",
        "f_decl_type",
    )

    _kind_name = 'DynVarDecl'






class MatchValDecl(ExplicitlyTypedDecl):
    """
    Subclass of :py:class:`ExplicitlyTypedDecl`.

    Value declaration in a match branch.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ExplicitlyTypedDecl._field_names + (
        "f_syn_name",
        "f_decl_type",
    )

    _kind_name = 'MatchValDecl'






class ValDecl(ExplicitlyTypedDecl):
    """
    Subclass of :py:class:`ExplicitlyTypedDecl`.

    Value declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_val_decl_f_expr)



        return result

    _field_names = ExplicitlyTypedDecl._field_names + (
        "f_syn_name",
        "f_decl_type",
        "f_expr",
    )

    _kind_name = 'ValDecl'






class FunDecl(UserValDecl):
    """
    Subclass of :py:class:`UserValDecl`.

    Function declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_params(
        self
    ) -> FunParamDeclList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_fun_decl_f_params)



        return result
    
    @property
    def f_return_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_fun_decl_f_return_type)



        return result
    
    @property
    def f_trait_ref(
        self
    ) -> DotExpr:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_fun_decl_f_trait_ref)



        return result
    
    @property
    def f_body(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_fun_decl_f_body)



        return result
    
    @property
    def p_is_dynamic_combiner(
        self
    ) -> bool:
        """
        When this property is used as a a combinder inside an NPropagate
        equation, return whether it expects a dynamic number of arguments.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _fun_decl_p_is_dynamic_combiner)
        result = bool(c_result.value)


        return result
    
    def p_find_all_overrides(
        self, units: List[AnalysisUnit]
    ) -> List[FunDecl]:
        """
        Return the list of all RefId that refer to this DefId.
        """
        

        

        unwrapped_units = _AnalysisUnitArrayConverter.unwrap(units)

        
        c_result = self._eval_field(_LktNodeArrayConverter.c_type(), _fun_decl_p_find_all_overrides, unwrapped_units.c_value)
        result = _LktNodeArrayConverter.wrap(c_result, False)


        return result

    _field_names = UserValDecl._field_names + (
        "f_syn_name",
        "f_params",
        "f_return_type",
        "f_trait_ref",
        "f_body",
    )

    _kind_name = 'FunDecl'






class EnvSpecDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Env spec declaration.

    Each node type can have one or no env spec. Env specs contains only a list
    of env actions.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_actions(
        self
    ) -> CallExprList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_env_spec_decl_f_actions)



        return result

    _field_names = Decl._field_names + (
        "f_syn_name",
        "f_actions",
    )

    _kind_name = 'EnvSpecDecl'






class ErrorDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Placeholder node for syntax errors in lists of declarations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Decl._field_names + (
    )

    _kind_name = 'ErrorDecl'






class GenericDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Generic entity declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_generic_param_decls(
        self
    ) -> GenericParamDeclList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_generic_decl_f_generic_param_decls)



        return result
    
    @property
    def f_decl(
        self
    ) -> Decl:
        """
        This field can contain one of the following nodes:
        :py:class:`DynVarDecl`, :py:class:`EnvSpecDecl`, :py:class:`ErrorDecl`,
        :py:class:`FieldDecl`, :py:class:`FunDecl`, :py:class:`GenericDecl`,
        :py:class:`GrammarDecl`, :py:class:`GrammarRuleDecl`,
        :py:class:`LexerDecl`, :py:class:`LexerFamilyDecl`,
        :py:class:`NamedTypeDecl`, :py:class:`ValDecl`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_generic_decl_f_decl)



        return result

    _field_names = Decl._field_names + (
        "f_generic_param_decls",
        "f_decl",
    )

    _kind_name = 'GenericDecl'






class GrammarDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Declaration of a language's grammar.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_rules(
        self
    ) -> FullDeclList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_decl_f_rules)



        return result

    _field_names = Decl._field_names + (
        "f_syn_name",
        "f_rules",
    )

    _kind_name = 'GrammarDecl'






class LexerDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Declaration of a language's lexer.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_rules(
        self
    ) -> LktNodeList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`FullDecl`, :py:class:`LexerCaseRule`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_decl_f_rules)



        return result

    _field_names = Decl._field_names + (
        "f_syn_name",
        "f_rules",
    )

    _kind_name = 'LexerDecl'






class LexerFamilyDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Declaration of a token family.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_rules(
        self
    ) -> FullDeclList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_family_decl_f_rules)



        return result

    _field_names = Decl._field_names + (
        "f_syn_name",
        "f_rules",
    )

    _kind_name = 'LexerFamilyDecl'






class SynthFunDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Logic function declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Decl._field_names + (
    )

    _kind_name = 'SynthFunDecl'






class SynthParamDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Logic function parameter declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Decl._field_names + (
    )

    _kind_name = 'SynthParamDecl'






class TypeDecl(Decl):
    """
    Subclass of :py:class:`Decl`.

    Abstract base class for type declarations.

    Derived nodes: :py:class:`AnyTypeDecl`, :py:class:`EnumClassAltDecl`,
    :py:class:`FunctionType`, :py:class:`GenericParamTypeDecl`,
    :py:class:`NamedTypeDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_traits(
        self
    ) -> TypeRefList:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_type_decl_f_traits)



        return result
    
    @property
    def f_syn_base_type(
        self
    ) -> TypeRef:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_type_decl_f_syn_base_type)



        return result
    
    @property
    def p_def_id(
        self
    ) -> DefId:
        """
        Return the defining name of this type declaration
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _type_decl_p_def_id)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_base_type(
        self
    ) -> TypeRef:
        """
        Return the base type for this node, if any.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _type_decl_p_base_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_base_type_if_entity(
        self
    ) -> TypeDecl:
        """
        Return the base type for this node, if any.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _type_decl_p_base_type_if_entity)
        result = LktNode._wrap(c_result)


        return result

    _field_names = Decl._field_names + (
    )







class AnyTypeDecl(TypeDecl):
    """
    Subclass of :py:class:`TypeDecl`.

    Internal type to represent a type that can be matched with anything.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = TypeDecl._field_names + (
        "f_traits",
    )

    _kind_name = 'AnyTypeDecl'






class EnumClassAltDecl(TypeDecl):
    """
    Subclass of :py:class:`TypeDecl`.

    Alternative for an enum class decl.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = TypeDecl._field_names + (
        "f_syn_name",
        "f_traits",
    )

    _kind_name = 'EnumClassAltDecl'






class FunctionType(TypeDecl):
    """
    Subclass of :py:class:`TypeDecl`.

    Function type.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = TypeDecl._field_names + (
        "f_traits",
    )

    _kind_name = 'FunctionType'






class GenericParamTypeDecl(TypeDecl):
    """
    Subclass of :py:class:`TypeDecl`.

    Declaration of a parameter type in a generic declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_has_class(
        self
    ) -> ClassQualifier:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_generic_param_type_decl_f_has_class)



        return result

    _field_names = TypeDecl._field_names + (
        "f_has_class",
        "f_syn_name",
        "f_traits",
    )

    _kind_name = 'GenericParamTypeDecl'






class NamedTypeDecl(TypeDecl):
    """
    Subclass of :py:class:`TypeDecl`.

    Explicit named type declaration.

    Derived nodes: :py:class:`BasicClassDecl`, :py:class:`EnumTypeDecl`,
    :py:class:`StructDecl`, :py:class:`TraitDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_decls(
        self
    ) -> DeclBlock:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_named_type_decl_f_decls)



        return result

    _field_names = TypeDecl._field_names + (
    )







class BasicClassDecl(NamedTypeDecl):
    """
    Subclass of :py:class:`NamedTypeDecl`.

    Common ancestor for declarations of regular classes and enum classes.

    Derived nodes: :py:class:`ClassDecl`, :py:class:`EnumClassDecl`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = NamedTypeDecl._field_names + (
        "f_syn_name",
        "f_syn_base_type",
        "f_traits",
    )







class ClassDecl(BasicClassDecl):
    """
    Subclass of :py:class:`BasicClassDecl`.

    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BasicClassDecl._field_names + (
        "f_decls",
    )

    _kind_name = 'ClassDecl'






class EnumClassDecl(BasicClassDecl):
    """
    Subclass of :py:class:`BasicClassDecl`.

    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_branches(
        self
    ) -> EnumClassCaseList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_enum_class_decl_f_branches)



        return result

    _field_names = BasicClassDecl._field_names + (
        "f_branches",
        "f_decls",
    )

    _kind_name = 'EnumClassDecl'






class EnumTypeDecl(NamedTypeDecl):
    """
    Subclass of :py:class:`NamedTypeDecl`.

    Enum type declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_literals(
        self
    ) -> EnumLitDeclList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_enum_type_decl_f_literals)



        return result

    _field_names = NamedTypeDecl._field_names + (
        "f_syn_name",
        "f_traits",
        "f_literals",
        "f_decls",
    )

    _kind_name = 'EnumTypeDecl'






class StructDecl(NamedTypeDecl):
    """
    Subclass of :py:class:`NamedTypeDecl`.

    Declaration for a LK struct.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = NamedTypeDecl._field_names + (
        "f_syn_name",
        "f_traits",
        "f_decls",
    )

    _kind_name = 'StructDecl'






class TraitDecl(NamedTypeDecl):
    """
    Subclass of :py:class:`NamedTypeDecl`.

    Trait declaration. For the moment, a Trait can just be used to group
    behavior for built-in types. It's not usable as a type-bound since we don't
    have generics, and you cannot implement one either.

    The reason they're added is to lay down the basics of what we want the Lkt
    type system to be.

    TODO: Traits are *not* types. They're treated as such in the grammar for
    convenience for now, but it's probably not a good idea. Migrate away from
    this.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = NamedTypeDecl._field_names + (
        "f_syn_name",
        "f_traits",
        "f_decls",
    )

    _kind_name = 'TraitDecl'






class DeclAnnotation(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Compile time annotation attached to a declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> Id:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_decl_annotation_f_name)



        return result
    
    @property
    def f_args(
        self
    ) -> DeclAnnotationArgs:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_decl_annotation_f_args)



        return result

    _field_names = LktNode._field_names + (
        "f_name",
        "f_args",
    )

    _kind_name = 'DeclAnnotation'






class DeclAnnotationArgs(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    List of arguments for an annotation with a call syntax. This intermediate
    node is necessary in order to determine after parsing whether there is no
    argument list, or if the list is empty.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_args(
        self
    ) -> ArgumentList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_decl_annotation_args_f_args)



        return result

    _field_names = LktNode._field_names + (
        "f_args",
    )

    _kind_name = 'DeclAnnotationArgs'






class DynEnvWrapper(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Synthetic node to instantiate a DynamicEnvironment for generics.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNode._field_names + (
    )

    _kind_name = 'DynEnvWrapper'






class ElsifBranch(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Elsif branch of an if expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_cond_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_elsif_branch_f_cond_expr)



        return result
    
    @property
    def f_then_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_elsif_branch_f_then_expr)



        return result

    _field_names = LktNode._field_names + (
        "f_cond_expr",
        "f_then_expr",
    )

    _kind_name = 'ElsifBranch'






class EnumClassCase(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Case branch for an enum class declaration.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_decls(
        self
    ) -> EnumClassAltDeclList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_enum_class_case_f_decls)



        return result

    _field_names = LktNode._field_names + (
        "f_decls",
    )

    _kind_name = 'EnumClassCase'






class ExcludesNull(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Whether the containing cast expression will raise on null cast result or
    not.

    Derived nodes: :py:class:`ExcludesNullAbsent`,
    :py:class:`ExcludesNullPresent`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_as_bool(
        self
    ) -> bool:
        """
        Return whether this node is present
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _excludes_null_p_as_bool)
        result = bool(c_result.value)


        return result

    _field_names = LktNode._field_names + (
    )







class ExcludesNullAbsent(ExcludesNull):
    """
    Subclass of :py:class:`ExcludesNull`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ExcludesNull._field_names + (
    )

    _kind_name = 'ExcludesNullAbsent'






class ExcludesNullPresent(ExcludesNull):
    """
    Subclass of :py:class:`ExcludesNull`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ExcludesNull._field_names + (
    )

    _kind_name = 'ExcludesNullPresent'






class Expr(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Base class for expressions. Encompasses regular expressions as well as
    special expressions (grammar expressions, etc).

    Derived nodes: :py:class:`AnyOf`, :py:class:`ArrayLiteral`,
    :py:class:`BaseCallExpr`, :py:class:`BinOp`, :py:class:`BlockExpr`,
    :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
    :py:class:`GenericInstantiation`, :py:class:`GrammarExpr`, :py:class:`Id`,
    :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
    :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
    :py:class:`LogicExpr`, :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
    :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
    :py:class:`RaiseExpr`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`,
    :py:class:`UnOp`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_get_type(
        self
    ) -> TypeDecl:
        """
        Return the type of this expression.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _expr_p_get_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_get_generic_type(
        self
    ) -> TypeDecl:
        """
        Return the expected type of this expression.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _expr_p_get_generic_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_get_expected_type(
        self
    ) -> TypeDecl:
        """
        Return the expected type of this expression.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _expr_p_get_expected_type)
        result = LktNode._wrap(c_result)


        return result
    
    @property
    def p_referenced_decl(
        self
    ) -> Decl:
        """
        Return the declaration referenced by this expression, if applicable,
        else null.

        The property is memoized in order to avoid use the value inside logic
        variables on every redundent call, causing faulty behavior when used
        with rebindings. TODO: Do like LAL to avoid memoization for more
        safety.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _expr_p_referenced_decl)
        result = LktNode._wrap(c_result)


        return result

    _field_names = LktNode._field_names + (
    )







class AnyOf(Expr):
    """
    Subclass of :py:class:`Expr`.

    "Any of" expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RaiseExpr`,
        :py:class:`RefId`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_any_of_f_expr)



        return result
    
    @property
    def f_values(
        self
    ) -> AnyOfList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`ArrayLiteral`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`KeepExpr`, :py:class:`LambdaExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RaiseExpr`,
        :py:class:`RefId`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_any_of_f_values)



        return result

    _field_names = Expr._field_names + (
        "f_expr",
        "f_values",
    )

    _kind_name = 'AnyOf'






class ArrayLiteral(Expr):
    """
    Subclass of :py:class:`Expr`.

    Literal for an array value.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_exprs(
        self
    ) -> ExprList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`AnyOf`, :py:class:`ArrayLiteral`, :py:class:`BinOp`,
        :py:class:`BlockExpr`, :py:class:`CallExpr`, :py:class:`CastExpr`,
        :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`, :py:class:`Isa`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicAssign`, :py:class:`LogicExpr`,
        :py:class:`LogicPredicate`, :py:class:`LogicPropagate`,
        :py:class:`LogicUnify`, :py:class:`MatchExpr`, :py:class:`NotExpr`,
        :py:class:`ParenExpr`, :py:class:`RaiseExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`, :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_array_literal_f_exprs)



        return result
    
    @property
    def f_element_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_array_literal_f_element_type)



        return result

    _field_names = Expr._field_names + (
        "f_exprs",
        "f_element_type",
    )

    _kind_name = 'ArrayLiteral'






class BaseCallExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Base class for expressions that are syntactically call-like.

    Derived nodes: :py:class:`CallExpr`, :py:class:`LogicCallExpr`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_base_call_expr_f_name)



        return result
    
    @property
    def f_args(
        self
    ) -> ArgumentList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_base_call_expr_f_args)



        return result

    _field_names = Expr._field_names + (
        "f_name",
        "f_args",
    )







class CallExpr(BaseCallExpr):
    """
    Subclass of :py:class:`BaseCallExpr`.

    Call expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseCallExpr._field_names + (
    )

    _kind_name = 'CallExpr'






class LogicCallExpr(BaseCallExpr):
    """
    Subclass of :py:class:`BaseCallExpr`.

    Base class for logic call expresions, of the form:

    .. code::

       name%(args)

    Derived nodes: :py:class:`LogicPredicate`, :py:class:`LogicPropagateCall`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseCallExpr._field_names + (
    )







class LogicPredicate(LogicCallExpr):
    """
    Subclass of :py:class:`LogicCallExpr`.

    Class for "predicate" equations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LogicCallExpr._field_names + (
    )

    _kind_name = 'LogicPredicate'






class LogicPropagateCall(LogicCallExpr):
    """
    Subclass of :py:class:`LogicCallExpr`.

    Class for the call inside "propagate" equations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LogicCallExpr._field_names + (
    )

    _kind_name = 'LogicPropagateCall'






class BinOp(Expr):
    """
    Subclass of :py:class:`Expr`.

    Binary operator expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_left(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_bin_op_f_left)



        return result
    
    @property
    def f_op(
        self
    ) -> Op:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_bin_op_f_op)



        return result
    
    @property
    def f_right(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_bin_op_f_right)



        return result

    _field_names = Expr._field_names + (
        "f_left",
        "f_op",
        "f_right",
    )

    _kind_name = 'BinOp'






class BlockExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Block expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_clauses(
        self
    ) -> LktNodeList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`AnyOf`, :py:class:`ArrayLiteral`, :py:class:`BinOp`,
        :py:class:`BlockExprClause`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorDecl`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`, :py:class:`Isa`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicAssign`, :py:class:`LogicExpr`,
        :py:class:`LogicPredicate`, :py:class:`LogicPropagate`,
        :py:class:`LogicUnify`, :py:class:`MatchExpr`, :py:class:`NotExpr`,
        :py:class:`ParenExpr`, :py:class:`RaiseExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`, :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_block_expr_f_clauses)



        return result

    _field_names = Expr._field_names + (
        "f_clauses",
    )

    _kind_name = 'BlockExpr'






class CastExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Cast expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_cast_expr_f_expr)



        return result
    
    @property
    def f_null_cond(
        self
    ) -> NullCondQualifier:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_cast_expr_f_null_cond)



        return result
    
    @property
    def f_excludes_null(
        self
    ) -> ExcludesNull:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_cast_expr_f_excludes_null)



        return result
    
    @property
    def f_dest_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_cast_expr_f_dest_type)



        return result

    _field_names = Expr._field_names + (
        "f_expr",
        "f_null_cond",
        "f_excludes_null",
        "f_dest_type",
    )

    _kind_name = 'CastExpr'






class DotExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Dotted expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_prefix(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_dot_expr_f_prefix)



        return result
    
    @property
    def f_null_cond(
        self
    ) -> NullCondQualifier:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_dot_expr_f_null_cond)



        return result
    
    @property
    def f_suffix(
        self
    ) -> RefId:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_dot_expr_f_suffix)



        return result

    _field_names = Expr._field_names + (
        "f_prefix",
        "f_null_cond",
        "f_suffix",
    )

    _kind_name = 'DotExpr'






class ErrorOnNull(Expr):
    """
    Subclass of :py:class:`Expr`.

    Expression that throws an error if LHS is null.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_error_on_null_f_expr)



        return result

    _field_names = Expr._field_names + (
        "f_expr",
    )

    _kind_name = 'ErrorOnNull'






class GenericInstantiation(Expr):
    """
    Subclass of :py:class:`Expr`.

    Generic instantiation.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_generic_instantiation_f_name)



        return result
    
    @property
    def f_args(
        self
    ) -> TypeRefList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_generic_instantiation_f_args)



        return result

    _field_names = Expr._field_names + (
        "f_name",
        "f_args",
    )

    _kind_name = 'GenericInstantiation'






class GrammarExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Base class for expressions related to grammars.

    Derived nodes: :py:class:`ErrorGrammarExpr`, :py:class:`GrammarCut`,
    :py:class:`GrammarDiscard`, :py:class:`GrammarDontSkip`,
    :py:class:`GrammarList`, :py:class:`GrammarNull`,
    :py:class:`GrammarOptErrorGroup`, :py:class:`GrammarOptError`,
    :py:class:`GrammarOptGroup`, :py:class:`GrammarOpt`,
    :py:class:`GrammarOrExpr`, :py:class:`GrammarPick`,
    :py:class:`GrammarPredicate`, :py:class:`GrammarRuleRef`,
    :py:class:`GrammarSkip`, :py:class:`GrammarStopCut`,
    :py:class:`ParseNodeExpr`, :py:class:`TokenLit`,
    :py:class:`TokenNoCaseLit`, :py:class:`TokenPatternConcat`,
    :py:class:`TokenPatternLit`, :py:class:`TokenRef`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Expr._field_names + (
    )







class ErrorGrammarExpr(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Placeholder node for syntax errors in grammar expressions.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GrammarExpr._field_names + (
    )

    _kind_name = 'ErrorGrammarExpr'






class GrammarCut(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a cut.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GrammarExpr._field_names + (
    )

    _kind_name = 'GrammarCut'






class GrammarDiscard(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression to discard the match.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_discard_f_expr)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_expr",
    )

    _kind_name = 'GrammarDiscard'






class GrammarDontSkip(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression (error recovery) to ensure that any nested skip parser
    calls won't skip certain parse results.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_dont_skip_f_expr)



        return result
    
    @property
    def f_dont_skip(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_dont_skip_f_dont_skip)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_expr",
        "f_dont_skip",
    )

    _kind_name = 'GrammarDontSkip'






class GrammarList(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression to parse lists of results. Results can be separated by a
    separator. List can be empty ('*') or not ('+').

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_list_type(
        self
    ) -> TypeRef:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_list_f_list_type)



        return result
    
    @property
    def f_kind(
        self
    ) -> ListKind:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_list_f_kind)



        return result
    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_list_f_expr)



        return result
    
    @property
    def f_sep(
        self
    ) -> GrammarListSep:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_grammar_list_f_sep)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_list_type",
        "f_kind",
        "f_expr",
        "f_sep",
    )

    _kind_name = 'GrammarList'






class GrammarNull(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression to parse a null node.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_grammar_null_f_name)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_name",
    )

    _kind_name = 'GrammarNull'






class GrammarOpt(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for an optional parsing result.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_opt_f_expr)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_expr",
    )

    _kind_name = 'GrammarOpt'






class GrammarOptError(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for an optional parsing result. Missing result creates
    an error, but parsing continues.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_opt_error_f_expr)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_expr",
    )

    _kind_name = 'GrammarOptError'






class GrammarOptErrorGroup(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a group of optional parsing results. Failure to
    parse an optional result creates an error, but parsing continues.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExprList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_opt_error_group_f_expr)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_expr",
    )

    _kind_name = 'GrammarOptErrorGroup'






class GrammarOptGroup(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a group of optional parsing results.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExprList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_opt_group_f_expr)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_expr",
    )

    _kind_name = 'GrammarOptGroup'






class GrammarOrExpr(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar ``Or`` expression (disjunctive choice between several grammar
    options).

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_sub_exprs(
        self
    ) -> GrammarExprListList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_or_expr_f_sub_exprs)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_sub_exprs",
    )

    _kind_name = 'GrammarOrExpr'






class GrammarPick(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression to pick the significant parse out of a list of parses
    (will automatically discard token results).

    Derived nodes: :py:class:`GrammarImplicitPick`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_exprs(
        self
    ) -> GrammarExprList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_pick_f_exprs)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_exprs",
    )

    _kind_name = 'GrammarPick'






class GrammarImplicitPick(GrammarPick):
    """
    Subclass of :py:class:`GrammarPick`.

    Implicit pick operation.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GrammarPick._field_names + (
    )

    _kind_name = 'GrammarImplicitPick'






class GrammarPredicate(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a predicate: Only parse something if the predicate
    (that is a reference to a node property) returns True.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_predicate_f_expr)



        return result
    
    @property
    def f_prop_ref(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`DotExpr`,
        :py:class:`RefId`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_predicate_f_prop_ref)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_expr",
        "f_prop_ref",
    )

    _kind_name = 'GrammarPredicate'






class GrammarRuleRef(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a reference to another grammar rule.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_node_name(
        self
    ) -> RefId:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_rule_ref_f_node_name)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_node_name",
    )

    _kind_name = 'GrammarRuleRef'






class GrammarSkip(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression (error recovery) to skip a parsing result.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_skip_f_name)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_name",
    )

    _kind_name = 'GrammarSkip'






class GrammarStopCut(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a StopCut.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_stop_cut_f_expr)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_expr",
    )

    _kind_name = 'GrammarStopCut'






class ParseNodeExpr(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Expression for the parsing of a Node.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_node_name(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_parse_node_expr_f_node_name)



        return result
    
    @property
    def f_sub_exprs(
        self
    ) -> GrammarExprList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_parse_node_expr_f_sub_exprs)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_node_name",
        "f_sub_exprs",
    )

    _kind_name = 'ParseNodeExpr'






class TokenLit(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a token literal.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_denoted_value(
        self
    ) -> DecodedStringValue:
        """
        Return the content of the given token literal node.
        """
        

        


        
        c_result = self._eval_field(DecodedStringValue._c_type(), _token_lit_p_denoted_value)
        result = DecodedStringValue._wrap(c_result)


        return result

    _field_names = GrammarExpr._field_names + (
    )

    _kind_name = 'TokenLit'






class TokenNoCaseLit(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a case insensitive token literal.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_lit(
        self
    ) -> TokenLit:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_token_no_case_lit_f_lit)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_lit",
    )

    _kind_name = 'TokenNoCaseLit'






class TokenPatternConcat(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for the concatenation of two patterns.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_left(
        self
    ) -> GrammarExpr:
        """
        This field can contain one of the following nodes:
        :py:class:`TokenPatternConcat`, :py:class:`TokenPatternLit`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_token_pattern_concat_f_left)



        return result
    
    @property
    def f_right(
        self
    ) -> TokenPatternLit:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_token_pattern_concat_f_right)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_left",
        "f_right",
    )

    _kind_name = 'TokenPatternConcat'






class TokenPatternLit(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a pattern literal.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_denoted_value(
        self
    ) -> DecodedStringValue:
        """
        Return the content of the given token pattern literal node.
        """
        

        


        
        c_result = self._eval_field(DecodedStringValue._c_type(), _token_pattern_lit_p_denoted_value)
        result = DecodedStringValue._wrap(c_result)


        return result

    _field_names = GrammarExpr._field_names + (
    )

    _kind_name = 'TokenPatternLit'






class TokenRef(GrammarExpr):
    """
    Subclass of :py:class:`GrammarExpr`.

    Grammar expression for a token reference.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_token_name(
        self
    ) -> RefId:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_token_ref_f_token_name)



        return result
    
    @property
    def f_expr(
        self
    ) -> TokenLit:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_token_ref_f_expr)



        return result

    _field_names = GrammarExpr._field_names + (
        "f_token_name",
        "f_expr",
    )

    _kind_name = 'TokenRef'






class Id(Expr):
    """
    Subclass of :py:class:`Expr`.

    Identifier.

    Derived nodes: :py:class:`DefId`, :py:class:`ModuleRefId`,
    :py:class:`RefId`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_custom_image(
        self
    ) -> str:
        """
        Returns the image of this RefId using entity information.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _id_p_custom_image)
        result = _String.wrap(c_result)


        return result

    _field_names = Expr._field_names + (
    )

    _kind_name = 'Id'






class DefId(Id):
    """
    Subclass of :py:class:`Id`.

    Defining identifier.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_name(
        self
    ) -> str:
        """
        Return the name defined by this DefId.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _def_id_p_name)
        result = _String.wrap(c_result)


        return result
    
    def p_get_implementatinons(
        self, units: List[AnalysisUnit]
    ) -> List[DefId]:
        """
        Return the implementations of this name.
        """
        

        

        unwrapped_units = _AnalysisUnitArrayConverter.unwrap(units)

        
        c_result = self._eval_field(_LktNodeArrayConverter.c_type(), _def_id_p_get_implementatinons, unwrapped_units.c_value)
        result = _LktNodeArrayConverter.wrap(c_result, False)


        return result
    
    @property
    def p_decl_detail(
        self
    ) -> str:
        """
        Return the details to display in the language server client when it
        requests for completion or hovering information.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _def_id_p_decl_detail)
        result = _String.wrap(c_result)


        return result
    
    @property
    def p_completion_item_kind(
        self
    ) -> int:
        """
        Return the kind of completion item for this DefId.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_int(), _def_id_p_completion_item_kind)
        result = c_result.value


        return result
    
    @property
    def p_doc(
        self
    ) -> str:
        """
        Return the documentation associated to this DefId.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _def_id_p_doc)
        result = _String.wrap(c_result)


        return result
    
    def p_find_all_references(
        self, units: List[AnalysisUnit]
    ) -> List[RefResult]:
        """
        Return the list of all RefId that refer to this DefId.
        """
        

        

        unwrapped_units = _AnalysisUnitArrayConverter.unwrap(units)

        
        c_result = self._eval_field(_RefResultArrayConverter.c_type(), _def_id_p_find_all_references, unwrapped_units.c_value)
        result = _RefResultArrayConverter.wrap(c_result, False)


        return result

    _field_names = Id._field_names + (
    )

    _kind_name = 'DefId'






class ModuleRefId(Id):
    """
    Subclass of :py:class:`Id`.

    Id referencing a langkit module.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Id._field_names + (
    )

    _kind_name = 'ModuleRefId'






class RefId(Id):
    """
    Subclass of :py:class:`Id`.

    Reference identifier.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_referenced_defining_name(
        self
    ) -> DefId:
        """
        Return the referenced defining name.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _ref_id_p_referenced_defining_name)
        result = LktNode._wrap(c_result)


        return result

    _field_names = Id._field_names + (
    )

    _kind_name = 'RefId'






class IfExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    If expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_cond_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_if_expr_f_cond_expr)



        return result
    
    @property
    def f_then_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_if_expr_f_then_expr)



        return result
    
    @property
    def f_alternatives(
        self
    ) -> ElsifBranchList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_if_expr_f_alternatives)



        return result
    
    @property
    def f_else_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_if_expr_f_else_expr)



        return result

    _field_names = Expr._field_names + (
        "f_cond_expr",
        "f_then_expr",
        "f_alternatives",
        "f_else_expr",
    )

    _kind_name = 'IfExpr'






class Isa(Expr):
    """
    Subclass of :py:class:`Expr`.

    Isa expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RaiseExpr`,
        :py:class:`RefId`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_isa_f_expr)



        return result
    
    @property
    def f_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`OrPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_isa_f_pattern)



        return result

    _field_names = Expr._field_names + (
        "f_expr",
        "f_pattern",
    )

    _kind_name = 'Isa'






class KeepExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Keep expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_keep_expr_f_expr)



        return result
    
    @property
    def f_null_cond(
        self
    ) -> NullCondQualifier:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_keep_expr_f_null_cond)



        return result
    
    @property
    def f_keep_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_keep_expr_f_keep_type)



        return result

    _field_names = Expr._field_names + (
        "f_expr",
        "f_null_cond",
        "f_keep_type",
    )

    _kind_name = 'KeepExpr'






class LambdaExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Lambda expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_params(
        self
    ) -> LambdaParamDeclList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lambda_expr_f_params)



        return result
    
    @property
    def f_return_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_lambda_expr_f_return_type)



        return result
    
    @property
    def f_body(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lambda_expr_f_body)



        return result

    _field_names = Expr._field_names + (
        "f_params",
        "f_return_type",
        "f_body",
    )

    _kind_name = 'LambdaExpr'






class Lit(Expr):
    """
    Subclass of :py:class:`Expr`.

    Base class for literals.

    Derived nodes: :py:class:`BigNumLit`, :py:class:`CharLit`,
    :py:class:`NullLit`, :py:class:`NumLit`, :py:class:`StringLit`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Expr._field_names + (
    )







class BigNumLit(Lit):
    """
    Subclass of :py:class:`Lit`.

    Big number literal expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Lit._field_names + (
    )

    _kind_name = 'BigNumLit'






class CharLit(Lit):
    """
    Subclass of :py:class:`Lit`.

    Character literal expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_denoted_value(
        self
    ) -> DecodedCharValue:
        """
        Return the content of the given character literal node.
        """
        

        


        
        c_result = self._eval_field(DecodedCharValue._c_type(), _char_lit_p_denoted_value)
        result = DecodedCharValue._wrap(c_result)


        return result

    _field_names = Lit._field_names + (
    )

    _kind_name = 'CharLit'






class NullLit(Lit):
    """
    Subclass of :py:class:`Lit`.

    Null literal expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_dest_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_null_lit_f_dest_type)



        return result

    _field_names = Lit._field_names + (
        "f_dest_type",
    )

    _kind_name = 'NullLit'






class NumLit(Lit):
    """
    Subclass of :py:class:`Lit`.

    Number literal expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Lit._field_names + (
    )

    _kind_name = 'NumLit'






class StringLit(Lit):
    """
    Subclass of :py:class:`Lit`.

    Base node type for string literals.

    Derived nodes: :py:class:`BlockStringLit`, :py:class:`SingleLineStringLit`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_denoted_value(
        self
    ) -> DecodedStringValue:
        """
        Return the content of the given string literal node.
        """
        

        


        
        c_result = self._eval_field(DecodedStringValue._c_type(), _string_lit_p_denoted_value)
        result = DecodedStringValue._wrap(c_result)


        return result
    
    @property
    def p_is_prefixed_string(
        self
    ) -> bool:
        """
        Return whether this string is prefixed or not.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _string_lit_p_is_prefixed_string)
        result = bool(c_result.value)


        return result
    
    @property
    def p_prefix(
        self
    ) -> str:
        """
        Return the prefix of this string, or the null character if there is no
        prefix.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint32(), _string_lit_p_prefix)
        result = chr(c_result.value)


        return result
    
    @property
    def p_is_regexp_literal(
        self
    ) -> bool:
        """
        Return whether this string literal is actually a regexp literal, by
        checking that this string is prefixed by 'p'.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _string_lit_p_is_regexp_literal)
        result = bool(c_result.value)


        return result

    _field_names = Lit._field_names + (
    )







class BlockStringLit(StringLit):
    """
    Subclass of :py:class:`StringLit`.

    String literal expression, made of multiple line strings.

    The denoted string value is the concatenation of all line string items.
    Each line string item must be either:

    * The empty string designator (``|"``), to denote an empty line (``\n``).

    * ``|" <content>``, to designate a non-empty line. The space before
      ``<content>`` is mandatory, and is not included in the denoted string
      value. ``<content>`` can be anything that appear in a regular string
      literal: escape sequences are interpreted the same way.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_lines(
        self
    ) -> BlockStringLineList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_block_string_lit_f_lines)



        return result

    _field_names = StringLit._field_names + (
        "f_lines",
    )

    _kind_name = 'BlockStringLit'






class SingleLineStringLit(StringLit):
    """
    Subclass of :py:class:`StringLit`.

    Single line string literal expression.

    Note that in order to reduce the size of the node type hierarchy, we define
    only one node (StringLit) for all our string literals (only regular strings
    and pattern string literals at the moment). This will also make it easy to
    add new string prefixes in the future.

    Derived nodes: :py:class:`PatternSingleLineStringLit`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = StringLit._field_names + (
    )

    _kind_name = 'SingleLineStringLit'






class PatternSingleLineStringLit(SingleLineStringLit):
    """
    Subclass of :py:class:`SingleLineStringLit`.

    Pattern single line string literal expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = SingleLineStringLit._field_names + (
    )

    _kind_name = 'PatternSingleLineStringLit'






class LogicAssign(Expr):
    """
    Subclass of :py:class:`Expr`.

    Class for "assign to logic var" equations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_dest_var(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`, :py:class:`Isa`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicAssign`, :py:class:`LogicExpr`,
        :py:class:`LogicPredicate`, :py:class:`LogicPropagate`,
        :py:class:`LogicUnify`, :py:class:`MatchExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_logic_assign_f_dest_var)



        return result
    
    @property
    def f_value(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RaiseExpr`,
        :py:class:`RefId`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_logic_assign_f_value)



        return result

    _field_names = Expr._field_names + (
        "f_dest_var",
        "f_value",
    )

    _kind_name = 'LogicAssign'






class LogicExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Class for logic expressions (any ``basic_expr`` starting with %).

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`CallExpr`, :py:class:`RefId`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_logic_expr_f_expr)



        return result

    _field_names = Expr._field_names + (
        "f_expr",
    )

    _kind_name = 'LogicExpr'






class LogicPropagate(Expr):
    """
    Subclass of :py:class:`Expr`.

    Class for "propagate" equations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_dest_var(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`, :py:class:`Isa`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicAssign`, :py:class:`LogicExpr`,
        :py:class:`LogicPredicate`, :py:class:`LogicPropagate`,
        :py:class:`LogicUnify`, :py:class:`MatchExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_logic_propagate_f_dest_var)



        return result
    
    @property
    def f_call(
        self
    ) -> LogicPropagateCall:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_logic_propagate_f_call)



        return result

    _field_names = Expr._field_names + (
        "f_dest_var",
        "f_call",
    )

    _kind_name = 'LogicPropagate'






class LogicUnify(Expr):
    """
    Subclass of :py:class:`Expr`.

    Class for "unify" equations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_lhs(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`, :py:class:`Isa`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicAssign`, :py:class:`LogicExpr`,
        :py:class:`LogicPredicate`, :py:class:`LogicPropagate`,
        :py:class:`LogicUnify`, :py:class:`MatchExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_logic_unify_f_lhs)



        return result
    
    @property
    def f_rhs(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RaiseExpr`,
        :py:class:`RefId`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_logic_unify_f_rhs)



        return result

    _field_names = Expr._field_names + (
        "f_lhs",
        "f_rhs",
    )

    _kind_name = 'LogicUnify'






class MatchExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Binary operator expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_match_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_match_expr_f_match_expr)



        return result
    
    @property
    def f_branches(
        self
    ) -> BaseMatchBranchList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_match_expr_f_branches)



        return result

    _field_names = Expr._field_names + (
        "f_match_expr",
        "f_branches",
    )

    _kind_name = 'MatchExpr'






class NotExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Boolean negation expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RaiseExpr`,
        :py:class:`RefId`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`,
        :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_not_expr_f_expr)



        return result

    _field_names = Expr._field_names + (
        "f_expr",
    )

    _kind_name = 'NotExpr'






class ParenExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Parenthesized expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_paren_expr_f_expr)



        return result

    _field_names = Expr._field_names + (
        "f_expr",
    )

    _kind_name = 'ParenExpr'






class RaiseExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Raise expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_dest_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_raise_expr_f_dest_type)



        return result
    
    @property
    def f_except_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_raise_expr_f_except_expr)



        return result

    _field_names = Expr._field_names + (
        "f_dest_type",
        "f_except_expr",
    )

    _kind_name = 'RaiseExpr'






class SubscriptExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Array subscript expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_prefix(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_subscript_expr_f_prefix)



        return result
    
    @property
    def f_null_cond(
        self
    ) -> NullCondQualifier:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_subscript_expr_f_null_cond)



        return result
    
    @property
    def f_index(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_subscript_expr_f_index)



        return result

    _field_names = Expr._field_names + (
        "f_prefix",
        "f_null_cond",
        "f_index",
    )

    _kind_name = 'SubscriptExpr'






class TryExpr(Expr):
    """
    Subclass of :py:class:`Expr`.

    Try expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_try_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_try_expr_f_try_expr)



        return result
    
    @property
    def f_or_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_try_expr_f_or_expr)



        return result

    _field_names = Expr._field_names + (
        "f_try_expr",
        "f_or_expr",
    )

    _kind_name = 'TryExpr'






class UnOp(Expr):
    """
    Subclass of :py:class:`Expr`.

    Unary operator expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_op(
        self
    ) -> Op:
        """
        This field can contain one of the following nodes: :py:class:`OpMinus`,
        :py:class:`OpPlus`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_un_op_f_op)



        return result
    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`IfExpr`, :py:class:`Isa`,
        :py:class:`KeepExpr`, :py:class:`LambdaExpr`, :py:class:`Lit`,
        :py:class:`LogicAssign`, :py:class:`LogicExpr`,
        :py:class:`LogicPredicate`, :py:class:`LogicPropagate`,
        :py:class:`LogicUnify`, :py:class:`MatchExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_un_op_f_expr)



        return result

    _field_names = Expr._field_names + (
        "f_op",
        "f_expr",
    )

    _kind_name = 'UnOp'






class FullDecl(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Container for an lkt declaration. Contains the decl node plus the
    documentation and annotations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_doc(
        self
    ) -> StringLit:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_full_decl_f_doc)



        return result
    
    @property
    def f_decl_annotations(
        self
    ) -> DeclAnnotationList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_full_decl_f_decl_annotations)



        return result
    
    @property
    def f_decl(
        self
    ) -> Decl:
        """
        This field can contain one of the following nodes:
        :py:class:`DynVarDecl`, :py:class:`EnvSpecDecl`, :py:class:`ErrorDecl`,
        :py:class:`FieldDecl`, :py:class:`FunDecl`, :py:class:`GenericDecl`,
        :py:class:`GenericParamTypeDecl`, :py:class:`GrammarDecl`,
        :py:class:`GrammarRuleDecl`, :py:class:`LexerDecl`,
        :py:class:`LexerFamilyDecl`, :py:class:`NamedTypeDecl`,
        :py:class:`ValDecl`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_full_decl_f_decl)



        return result
    
    def p_has_annotation(
        self, name: str
    ) -> bool:
        """
        Return whether this node has an annotation with name ``name``.
        """
        

        

        _context = self.unit.context._c_value
        unwrapped_name = _symbol_type.unwrap(name, _context)

        
        c_result = self._eval_field(ctypes.c_uint8(), _full_decl_p_has_annotation, unwrapped_name)
        result = bool(c_result.value)


        return result

    _field_names = LktNode._field_names + (
        "f_doc",
        "f_decl_annotations",
        "f_decl",
    )

    _kind_name = 'FullDecl'






class GrammarListSep(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Specification for the separator of a list parser.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_token(
        self
    ) -> GrammarExpr:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_grammar_list_sep_f_token)



        return result
    
    @property
    def f_extra(
        self
    ) -> Id:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_grammar_list_sep_f_extra)



        return result

    _field_names = LktNode._field_names + (
        "f_token",
        "f_extra",
    )

    _kind_name = 'GrammarListSep'






class Import(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Statement to import another source file.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> ModuleRefId:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_import_f_name)



        return result
    
    @property
    def p_referenced_unit(
        self
    ) -> AnalysisUnit:
        """
        Return the unit that this import statements designates. Load it if
        needed.
        """
        

        


        
        c_result = self._eval_field(AnalysisUnit._c_type(), _import_p_referenced_unit)
        result = AnalysisUnit._wrap(c_result)


        return result

    _field_names = LktNode._field_names + (
        "f_name",
    )

    _kind_name = 'Import'






class LangkitRoot(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    For the moment, root node of a lkt compilation unit.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_imports(
        self
    ) -> ImportList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_langkit_root_f_imports)



        return result
    
    @property
    def f_decls(
        self
    ) -> FullDeclList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_langkit_root_f_decls)



        return result
    
    @property
    def p_fetch_prelude(
        self
    ) -> AnalysisUnit:
        """
        External property that will fetch the prelude unit, containing
        predefined types and values.
        """
        

        


        
        c_result = self._eval_field(AnalysisUnit._c_type(), _langkit_root_p_fetch_prelude)
        result = AnalysisUnit._wrap(c_result)


        return result

    _field_names = LktNode._field_names + (
        "f_imports",
        "f_decls",
    )

    _kind_name = 'LangkitRoot'






class LexerCaseRule(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Lexer construct to introduce a conditional lexing action.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_expr(
        self
    ) -> GrammarExpr:
        """
        This field can contain one of the following nodes:
        :py:class:`ErrorGrammarExpr`, :py:class:`GrammarCut`,
        :py:class:`GrammarDiscard`, :py:class:`GrammarList`,
        :py:class:`GrammarNull`, :py:class:`GrammarOptErrorGroup`,
        :py:class:`GrammarOptError`, :py:class:`GrammarOptGroup`,
        :py:class:`GrammarOpt`, :py:class:`GrammarOrExpr`,
        :py:class:`GrammarPick`, :py:class:`GrammarRuleRef`,
        :py:class:`GrammarSkip`, :py:class:`GrammarStopCut`,
        :py:class:`ParseNodeExpr`, :py:class:`TokenLit`,
        :py:class:`TokenNoCaseLit`, :py:class:`TokenPatternConcat`,
        :py:class:`TokenPatternLit`, :py:class:`TokenRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_case_rule_f_expr)



        return result
    
    @property
    def f_alts(
        self
    ) -> BaseLexerCaseRuleAltList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_case_rule_f_alts)



        return result

    _field_names = LktNode._field_names + (
        "f_expr",
        "f_alts",
    )

    _kind_name = 'LexerCaseRule'






class LexerCaseRuleSend(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Lexer construction used by case alternatives to represent the token to send
    if that alternative was chosen.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_sent(
        self
    ) -> RefId:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_case_rule_send_f_sent)



        return result
    
    @property
    def f_match_size(
        self
    ) -> NumLit:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_lexer_case_rule_send_f_match_size)



        return result

    _field_names = LktNode._field_names + (
        "f_sent",
        "f_match_size",
    )

    _kind_name = 'LexerCaseRuleSend'






class ListKind(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Kind for list parser expressions.

    Derived nodes: :py:class:`ListKindOne`, :py:class:`ListKindZero`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNode._field_names + (
    )







class ListKindOne(ListKind):
    """
    Subclass of :py:class:`ListKind`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ListKind._field_names + (
    )

    _kind_name = 'ListKindOne'






class ListKindZero(ListKind):
    """
    Subclass of :py:class:`ListKind`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ListKind._field_names + (
    )

    _kind_name = 'ListKindZero'






class LktNodeBaseList(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Derived nodes: :py:class:`ArgumentList`,
    :py:class:`BaseLexerCaseRuleAltList`, :py:class:`BaseMatchBranchList`,
    :py:class:`BlockStringLineList`, :py:class:`CallExprList`,
    :py:class:`DeclAnnotationList`, :py:class:`ElsifBranchList`,
    :py:class:`EnumClassAltDeclList`, :py:class:`EnumClassCaseList`,
    :py:class:`EnumLitDeclList`, :py:class:`ExprList`,
    :py:class:`FullDeclList`, :py:class:`FunParamDeclList`,
    :py:class:`GrammarExprListList`, :py:class:`GrammarExprList`,
    :py:class:`ImportList`, :py:class:`LambdaParamDeclList`,
    :py:class:`LktNodeList`, :py:class:`PatternDetailList`,
    :py:class:`PatternList`, :py:class:`RefIdList`, :py:class:`TypeRefList`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNode._field_names + (
    )







class ArgumentList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of Argument.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'ArgumentList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[Argument]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> Argument:
        return super().__getitem__(index)  # type: ignore





class BaseLexerCaseRuleAltList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of BaseLexerCaseRuleAlt.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'BaseLexerCaseRuleAltList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[BaseLexerCaseRuleAlt]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> BaseLexerCaseRuleAlt:
        return super().__getitem__(index)  # type: ignore





class BaseMatchBranchList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of BaseMatchBranch.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'BaseMatchBranchList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[BaseMatchBranch]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> BaseMatchBranch:
        return super().__getitem__(index)  # type: ignore





class BlockStringLineList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of BlockStringLine.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'BlockStringLineList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[BlockStringLine]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> BlockStringLine:
        return super().__getitem__(index)  # type: ignore





class CallExprList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of CallExpr.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'CallExprList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[CallExpr]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> CallExpr:
        return super().__getitem__(index)  # type: ignore





class DeclAnnotationList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of DeclAnnotation.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'DeclAnnotationList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[DeclAnnotation]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> DeclAnnotation:
        return super().__getitem__(index)  # type: ignore





class ElsifBranchList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of ElsifBranch.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'ElsifBranchList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[ElsifBranch]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> ElsifBranch:
        return super().__getitem__(index)  # type: ignore





class EnumClassAltDeclList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of EnumClassAltDecl.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'EnumClassAltDeclList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[EnumClassAltDecl]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> EnumClassAltDecl:
        return super().__getitem__(index)  # type: ignore





class EnumClassCaseList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of EnumClassCase.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'EnumClassCaseList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[EnumClassCase]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> EnumClassCase:
        return super().__getitem__(index)  # type: ignore





class EnumLitDeclList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of EnumLitDecl.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'EnumLitDeclList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[EnumLitDecl]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> EnumLitDecl:
        return super().__getitem__(index)  # type: ignore





class ExprList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of Expr.

    This list node can contain one of the following nodes: :py:class:`AnyOf`,
    :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
    :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
    :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
    :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
    :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
    :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
    :py:class:`LogicPropagate`, :py:class:`LogicUnify`, :py:class:`MatchExpr`,
    :py:class:`NotExpr`, :py:class:`ParenExpr`, :py:class:`RaiseExpr`,
    :py:class:`RefId`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`,
    :py:class:`UnOp`

    Derived nodes: :py:class:`AnyOfList`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'ExprList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[Expr]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> Expr:
        return super().__getitem__(index)  # type: ignore





class AnyOfList(ExprList):
    """
    Subclass of :py:class:`ExprList`.

    Pipe-separated list of expressions.

    This is used to represent the "values" operand of an ``AnyOf`` expression.

    This list node can contain one of the following nodes:
    :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
    :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
    :py:class:`GenericInstantiation`, :py:class:`IfExpr`, :py:class:`KeepExpr`,
    :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicExpr`,
    :py:class:`LogicPredicate`, :py:class:`MatchExpr`, :py:class:`ParenExpr`,
    :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
    :py:class:`TryExpr`

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ExprList._field_names + (
    )

    _kind_name = 'AnyOfList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[Expr]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> Expr:
        return super().__getitem__(index)  # type: ignore





class FullDeclList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of FullDecl.

    Derived nodes: :py:class:`DeclBlock`, :py:class:`GenericParamDeclList`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'FullDeclList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[FullDecl]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> FullDecl:
        return super().__getitem__(index)  # type: ignore





class DeclBlock(FullDeclList):
    """
    Subclass of :py:class:`FullDeclList`.

    List of declarations that also introduces a containing lexical scope.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = FullDeclList._field_names + (
    )

    _kind_name = 'DeclBlock'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[FullDecl]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> FullDecl:
        return super().__getitem__(index)  # type: ignore





class GenericParamDeclList(FullDeclList):
    """
    Subclass of :py:class:`FullDeclList`.

    Comma-separated list of generic parameter types.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = FullDeclList._field_names + (
    )

    _kind_name = 'GenericParamDeclList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[FullDecl]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> FullDecl:
        return super().__getitem__(index)  # type: ignore





class FunParamDeclList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of FunParamDecl.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'FunParamDeclList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[FunParamDecl]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> FunParamDecl:
        return super().__getitem__(index)  # type: ignore





class GrammarExprList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of GrammarExpr.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'GrammarExprList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[GrammarExpr]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> GrammarExpr:
        return super().__getitem__(index)  # type: ignore





class GrammarExprListList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of ASTList[GrammarExpr].

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'GrammarExprListList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[GrammarExprList]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> GrammarExprList:
        return super().__getitem__(index)  # type: ignore





class ImportList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of Import.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'ImportList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[Import]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> Import:
        return super().__getitem__(index)  # type: ignore





class LambdaParamDeclList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of LambdaParamDecl.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'LambdaParamDeclList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[LambdaParamDecl]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> LambdaParamDecl:
        return super().__getitem__(index)  # type: ignore





class LktNodeList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of LktNode.

    This list node can contain one of the following nodes: :py:class:`AnyOf`,
    :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExprClause`,
    :py:class:`BlockExpr`, :py:class:`CallExpr`, :py:class:`CastExpr`,
    :py:class:`DotExpr`, :py:class:`ErrorDecl`, :py:class:`ErrorOnNull`,
    :py:class:`FullDecl`, :py:class:`GenericInstantiation`, :py:class:`IfExpr`,
    :py:class:`Isa`, :py:class:`KeepExpr`, :py:class:`LambdaExpr`,
    :py:class:`LexerCaseRule`, :py:class:`Lit`, :py:class:`LogicAssign`,
    :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
    :py:class:`LogicPropagate`, :py:class:`LogicUnify`, :py:class:`MatchExpr`,
    :py:class:`NotExpr`, :py:class:`ParenExpr`, :py:class:`RaiseExpr`,
    :py:class:`RefId`, :py:class:`SubscriptExpr`, :py:class:`TryExpr`,
    :py:class:`UnOp`

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'LktNodeList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[LktNode]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> LktNode:
        return super().__getitem__(index)  # type: ignore





class PatternDetailList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of PatternDetail.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'PatternDetailList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[PatternDetail]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> PatternDetail:
        return super().__getitem__(index)  # type: ignore





class PatternList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of Pattern.

    This list node can contain one of the following nodes:
    :py:class:`BindingPattern`, :py:class:`BoolPattern`,
    :py:class:`EllipsisPattern`, :py:class:`ExtendedPattern`,
    :py:class:`IntegerPattern`, :py:class:`ListPattern`,
    :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`ParenPattern`,
    :py:class:`RegexPattern`, :py:class:`TuplePattern`, :py:class:`TypePattern`

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'PatternList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[Pattern]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> Pattern:
        return super().__getitem__(index)  # type: ignore





class RefIdList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of RefId.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'RefIdList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[RefId]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> RefId:
        return super().__getitem__(index)  # type: ignore





class TypeRefList(LktNodeBaseList):
    """
    Subclass of :py:class:`LktNodeBaseList`.

    List of TypeRef.

    This list node can contain one of the following nodes:
    :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
    :py:class:`SimpleTypeRef`

    Derived nodes: :py:class:`SyntheticTypeRefList`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNodeBaseList._field_names + (
    )

    _kind_name = 'TypeRefList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[TypeRef]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> TypeRef:
        return super().__getitem__(index)  # type: ignore





class SyntheticTypeRefList(TypeRefList):
    """
    Subclass of :py:class:`TypeRefList`.

    Synthetic list of type references, used to create synthetic type
    declarations.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = TypeRefList._field_names + (
    )

    _kind_name = 'SyntheticTypeRefList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[TypeRef]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> TypeRef:
        return super().__getitem__(index)  # type: ignore





class NullCondQualifier(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Whether the "?" operation qualifier (to denote the null-conditional
    behavior) is present.

    Derived nodes: :py:class:`NullCondQualifierAbsent`,
    :py:class:`NullCondQualifierPresent`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_as_bool(
        self
    ) -> bool:
        """
        Return whether this node is present
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _null_cond_qualifier_p_as_bool)
        result = bool(c_result.value)


        return result

    _field_names = LktNode._field_names + (
    )







class NullCondQualifierAbsent(NullCondQualifier):
    """
    Subclass of :py:class:`NullCondQualifier`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = NullCondQualifier._field_names + (
    )

    _kind_name = 'NullCondQualifierAbsent'






class NullCondQualifierPresent(NullCondQualifier):
    """
    Subclass of :py:class:`NullCondQualifier`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = NullCondQualifier._field_names + (
    )

    _kind_name = 'NullCondQualifierPresent'






class Op(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Operator in a binary operator expression.

    Derived nodes: :py:class:`OpAmp`, :py:class:`OpAnd`, :py:class:`OpDiv`,
    :py:class:`OpEq`, :py:class:`OpGt`, :py:class:`OpGte`,
    :py:class:`OpLogicAnd`, :py:class:`OpLogicOr`, :py:class:`OpLt`,
    :py:class:`OpLte`, :py:class:`OpMinus`, :py:class:`OpMult`,
    :py:class:`OpNe`, :py:class:`OpOrInt`, :py:class:`OpOr`, :py:class:`OpPlus`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNode._field_names + (
    )







class OpAmp(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpAmp'






class OpAnd(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpAnd'






class OpDiv(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpDiv'






class OpEq(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpEq'






class OpGt(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpGt'






class OpGte(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpGte'






class OpLogicAnd(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpLogicAnd'






class OpLogicOr(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpLogicOr'






class OpLt(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpLt'






class OpLte(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpLte'






class OpMinus(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpMinus'






class OpMult(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpMult'






class OpNe(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpNe'






class OpOr(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpOr'






class OpOrInt(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpOrInt'






class OpPlus(Op):
    """
    Subclass of :py:class:`Op`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Op._field_names + (
    )

    _kind_name = 'OpPlus'






class Pattern(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Root node class for patterns.

    This is a mostly LKQL specific node for the moment, as are every node
    derived from it.

    The only patterns that are currently used and implemented in Lkt's IsA are
    ``OrPattern`` and ``TypePattern``.

    Derived nodes: :py:class:`AnyTypePattern`, :py:class:`BindingPattern`,
    :py:class:`BoolPattern`, :py:class:`EllipsisPattern`,
    :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
    :py:class:`IntegerPattern`, :py:class:`ListPattern`,
    :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`OrPattern`,
    :py:class:`ParenPattern`, :py:class:`RegexPattern`,
    :py:class:`TuplePattern`, :py:class:`TypePattern`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNode._field_names + (
    )







class AnyTypePattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern that allows to match any type. Only usable as an ExtendedPattern's
    left side pattern:

    .. code::

       *(f_field: BasicDecl(...))

    For the general case of matching any value, the idiom is to use a binding
    pattern with no right hand side:

    .. code::

       _ => true

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Pattern._field_names + (
    )

    _kind_name = 'AnyTypePattern'






class BindingPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern comprising a binding name and a value pattern.

    For instance:

    .. code::

       o@ObjectDecl

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_decl(
        self
    ) -> BindingValDecl:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_binding_pattern_f_decl)



        return result
    
    @property
    def f_sub_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`IntegerPattern`,
        :py:class:`ListPattern`, :py:class:`NotPattern`,
        :py:class:`NullPattern`, :py:class:`ParenPattern`,
        :py:class:`RegexPattern`, :py:class:`TuplePattern`,
        :py:class:`TypePattern`

        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_binding_pattern_f_sub_pattern)



        return result

    _field_names = Pattern._field_names + (
        "f_decl",
        "f_sub_pattern",
    )

    _kind_name = 'BindingPattern'






class BoolPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern to match on booleans.

    Derived nodes: :py:class:`BoolPatternFalse`, :py:class:`BoolPatternTrue`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Pattern._field_names + (
    )







class BoolPatternFalse(BoolPattern):
    """
    Subclass of :py:class:`BoolPattern`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BoolPattern._field_names + (
    )

    _kind_name = 'BoolPatternFalse'






class BoolPatternTrue(BoolPattern):
    """
    Subclass of :py:class:`BoolPattern`.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BoolPattern._field_names + (
    )

    _kind_name = 'BoolPatternTrue'






class EllipsisPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern to match any remaining number of elements in a list pattern.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_binding(
        self
    ) -> Id:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_ellipsis_pattern_f_binding)



        return result

    _field_names = Pattern._field_names + (
        "f_binding",
    )

    _kind_name = 'EllipsisPattern'






class ExtendedPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern that takes a base pattern, and adds details to match on the shape
    of what is being matched. The syntactic form is:

    .. code::

       <sub_pattern>(<detail>, <detail>, ...)

    For instance:

    .. code::

       ObjectDecl(any children: AspectAssoc)

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_sub_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`AnyTypePattern`, :py:class:`ParenPattern`,
        :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_extended_pattern_f_sub_pattern)



        return result
    
    @property
    def f_details(
        self
    ) -> PatternDetailList:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_extended_pattern_f_details)



        return result

    _field_names = Pattern._field_names + (
        "f_sub_pattern",
        "f_details",
    )

    _kind_name = 'ExtendedPattern'






class FilteredPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern with a filtering predicate, of the form: ``<pattern> when
    <predicate>``

    For instance:

    .. code::

       o@ObjectDecl when o.children.length == 3

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_sub_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`IntegerPattern`,
        :py:class:`ListPattern`, :py:class:`NotPattern`,
        :py:class:`NullPattern`, :py:class:`ParenPattern`,
        :py:class:`RegexPattern`, :py:class:`TuplePattern`,
        :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_filtered_pattern_f_sub_pattern)



        return result
    
    @property
    def f_predicate(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_filtered_pattern_f_predicate)



        return result

    _field_names = Pattern._field_names + (
        "f_sub_pattern",
        "f_predicate",
    )

    _kind_name = 'FilteredPattern'






class IntegerPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern to match on integers.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Pattern._field_names + (
    )

    _kind_name = 'IntegerPattern'






class ListPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern to match on lists.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_sub_patterns(
        self
    ) -> PatternList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`EllipsisPattern`, :py:class:`ExtendedPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_list_pattern_f_sub_patterns)



        return result

    _field_names = Pattern._field_names + (
        "f_sub_patterns",
    )

    _kind_name = 'ListPattern'






class NotPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern that matches if its inner pattern doesn't match.

    For instance:

    .. code::

       val non_objects = select not ObjectDecl

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_sub_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`IntegerPattern`,
        :py:class:`ListPattern`, :py:class:`NotPattern`,
        :py:class:`NullPattern`, :py:class:`ParenPattern`,
        :py:class:`RegexPattern`, :py:class:`TuplePattern`,
        :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_not_pattern_f_sub_pattern)



        return result

    _field_names = Pattern._field_names + (
        "f_sub_pattern",
    )

    _kind_name = 'NotPattern'






class NullPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Null pattern. Will only match the null value.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Pattern._field_names + (
    )

    _kind_name = 'NullPattern'






class OrPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern that matches if any of its subpatterns matches.

    For instance:

    .. code::

       val value_decls = select ObjectDecl | ParamSpec

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_left_sub_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_or_pattern_f_left_sub_pattern)



        return result
    
    @property
    def f_right_sub_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`OrPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_or_pattern_f_right_sub_pattern)



        return result

    _field_names = Pattern._field_names + (
        "f_left_sub_pattern",
        "f_right_sub_pattern",
    )

    _kind_name = 'OrPattern'






class ParenPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    A syntactically parenthesized pattern. Has no effect, only used to
    disambiguate syntax.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_sub_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`OrPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_paren_pattern_f_sub_pattern)



        return result

    _field_names = Pattern._field_names + (
        "f_sub_pattern",
    )

    _kind_name = 'ParenPattern'






class RegexPattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern that considers the value as text and matches it against the given
    regular expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Pattern._field_names + (
    )

    _kind_name = 'RegexPattern'






class TuplePattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern to match on tuples.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_sub_patterns(
        self
    ) -> PatternList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`IntegerPattern`,
        :py:class:`ListPattern`, :py:class:`NotPattern`,
        :py:class:`NullPattern`, :py:class:`ParenPattern`,
        :py:class:`RegexPattern`, :py:class:`TuplePattern`,
        :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_tuple_pattern_f_sub_patterns)



        return result

    _field_names = Pattern._field_names + (
        "f_sub_patterns",
    )

    _kind_name = 'TuplePattern'






class TypePattern(Pattern):
    """
    Subclass of :py:class:`Pattern`.

    Pattern matching on a specific type.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_type_name(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_type_pattern_f_type_name)



        return result

    _field_names = Pattern._field_names + (
        "f_type_name",
    )

    _kind_name = 'TypePattern'






class PatternDetail(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Base class for a detail in an ExtendedPattern.

    Derived nodes: :py:class:`FieldPatternDetail`,
    :py:class:`PropertyPatternDetail`, :py:class:`SelectorPatternDetail`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LktNode._field_names + (
    )







class FieldPatternDetail(PatternDetail):
    """
    Subclass of :py:class:`PatternDetail`.

    Pattern detail denoting an access to a field.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_id(
        self
    ) -> Id:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_field_pattern_detail_f_id)



        return result
    
    @property
    def f_expected_value(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`OrPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_field_pattern_detail_f_expected_value)



        return result

    _field_names = PatternDetail._field_names + (
        "f_id",
        "f_expected_value",
    )

    _kind_name = 'FieldPatternDetail'






class PropertyPatternDetail(PatternDetail):
    """
    Subclass of :py:class:`PatternDetail`.

    Pattern detail denoting an access to a property in a node pattern.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_call(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_property_pattern_detail_f_call)



        return result
    
    @property
    def f_expected_value(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`OrPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_property_pattern_detail_f_expected_value)



        return result

    _field_names = PatternDetail._field_names + (
        "f_call",
        "f_expected_value",
    )

    _kind_name = 'PropertyPatternDetail'






class SelectorPatternDetail(PatternDetail):
    """
    Subclass of :py:class:`PatternDetail`.

    Pattern detail denoting the use of a selector in a node pattern

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_call(
        self
    ) -> SelectorCall:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_selector_pattern_detail_f_call)



        return result
    
    @property
    def f_sub_pattern(
        self
    ) -> Pattern:
        """
        This field can contain one of the following nodes:
        :py:class:`BindingPattern`, :py:class:`BoolPattern`,
        :py:class:`ExtendedPattern`, :py:class:`FilteredPattern`,
        :py:class:`IntegerPattern`, :py:class:`ListPattern`,
        :py:class:`NotPattern`, :py:class:`NullPattern`, :py:class:`OrPattern`,
        :py:class:`ParenPattern`, :py:class:`RegexPattern`,
        :py:class:`TuplePattern`, :py:class:`TypePattern`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_selector_pattern_detail_f_sub_pattern)



        return result

    _field_names = PatternDetail._field_names + (
        "f_call",
        "f_sub_pattern",
    )

    _kind_name = 'SelectorPatternDetail'






class SelectorCall(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Root node for selector patterns

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_quantifier(
        self
    ) -> Id:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_selector_call_f_quantifier)



        return result
    
    @property
    def f_binding(
        self
    ) -> Id:
        """
        This field may be null even when there are no parsing errors.
        """
        

        

        result = self._eval_astnode_field(_selector_call_f_binding)



        return result
    
    @property
    def f_selector_call(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`ArrayLiteral`, :py:class:`BlockExpr`, :py:class:`CallExpr`,
        :py:class:`CastExpr`, :py:class:`DotExpr`, :py:class:`ErrorOnNull`,
        :py:class:`GenericInstantiation`, :py:class:`KeepExpr`,
        :py:class:`Lit`, :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`MatchExpr`, :py:class:`ParenExpr`, :py:class:`RefId`,
        :py:class:`SubscriptExpr`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_selector_call_f_selector_call)



        return result

    _field_names = LktNode._field_names + (
        "f_quantifier",
        "f_binding",
        "f_selector_call",
    )

    _kind_name = 'SelectorCall'






class TypeRef(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Base class for a reference to a type.

    Derived nodes: :py:class:`DefaultListTypeRef`, :py:class:`FunctionTypeRef`,
    :py:class:`GenericTypeRef`, :py:class:`SimpleTypeRef`
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_referenced_decl(
        self
    ) -> TypeDecl:
        """
        Returns the referenced type declaration.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _type_ref_p_referenced_decl)
        result = LktNode._wrap(c_result)


        return result

    _field_names = LktNode._field_names + (
    )







class DefaultListTypeRef(TypeRef):
    """
    Subclass of :py:class:`TypeRef`.

    "list" type reference in parsers.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = TypeRef._field_names + (
    )

    _kind_name = 'DefaultListTypeRef'






class FunctionTypeRef(TypeRef):
    """
    Subclass of :py:class:`TypeRef`.

    Reference to a function type.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_param_types(
        self
    ) -> TypeRefList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_function_type_ref_f_param_types)



        return result
    
    @property
    def f_return_type(
        self
    ) -> TypeRef:
        """
        This field can contain one of the following nodes:
        :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_function_type_ref_f_return_type)



        return result

    _field_names = TypeRef._field_names + (
        "f_param_types",
        "f_return_type",
    )

    _kind_name = 'FunctionTypeRef'






class GenericTypeRef(TypeRef):
    """
    Subclass of :py:class:`TypeRef`.

    Reference to a generic type.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_type_name(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`DotExpr`,
        :py:class:`RefId`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_generic_type_ref_f_type_name)



        return result
    
    @property
    def f_args(
        self
    ) -> TypeRefList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`FunctionTypeRef`, :py:class:`GenericTypeRef`,
        :py:class:`SimpleTypeRef`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_generic_type_ref_f_args)



        return result

    _field_names = TypeRef._field_names + (
        "f_type_name",
        "f_args",
    )

    _kind_name = 'GenericTypeRef'






class SimpleTypeRef(TypeRef):
    """
    Subclass of :py:class:`TypeRef`.

    Simple reference to a type.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_type_name(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`DotExpr`,
        :py:class:`RefId`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_simple_type_ref_f_type_name)



        return result

    _field_names = TypeRef._field_names + (
        "f_type_name",
    )

    _kind_name = 'SimpleTypeRef'






class VarBind(LktNode):
    """
    Subclass of :py:class:`LktNode`.

    Dynamic var bind expression.

    This node type has no derivation.
    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> RefId:
        """
        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_var_bind_f_name)



        return result
    
    @property
    def f_expr(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes: :py:class:`AnyOf`,
        :py:class:`ArrayLiteral`, :py:class:`BinOp`, :py:class:`BlockExpr`,
        :py:class:`CallExpr`, :py:class:`CastExpr`, :py:class:`DotExpr`,
        :py:class:`ErrorOnNull`, :py:class:`GenericInstantiation`,
        :py:class:`IfExpr`, :py:class:`Isa`, :py:class:`KeepExpr`,
        :py:class:`LambdaExpr`, :py:class:`Lit`, :py:class:`LogicAssign`,
        :py:class:`LogicExpr`, :py:class:`LogicPredicate`,
        :py:class:`LogicPropagate`, :py:class:`LogicUnify`,
        :py:class:`MatchExpr`, :py:class:`NotExpr`, :py:class:`ParenExpr`,
        :py:class:`RaiseExpr`, :py:class:`RefId`, :py:class:`SubscriptExpr`,
        :py:class:`TryExpr`, :py:class:`UnOp`

        When there are no parsing errors, this field is never null.
        """
        

        

        result = self._eval_astnode_field(_var_bind_f_expr)



        return result

    _field_names = LktNode._field_names + (
        "f_name",
        "f_expr",
    )

    _kind_name = 'VarBind'






class _EnvRebindingsType_c_type(ctypes.Structure):
    _fields_ = [("version", ctypes.c_uint64)]


_EnvRebindings_c_type = _hashable_c_pointer(_EnvRebindingsType_c_type)




class _BaseStruct:
    """
    Mixin for Ada struct wrappers.
    """

    # Subclasses will override this to a subclass of ctypes.Structure
    _c_type: ClassVar[ctypes.Structure]

    def __getitem__(self, key: int) -> Any:
        if not isinstance(key, int):
            raise TypeError(
               'Tuples items are indexed by integers, not {}'.format(type(key))
            )

        fields = self._c_type._fields_
        if 0 <= key < len(fields):
            field_name, _ = fields[key]
            return getattr(self, field_name)
        else:
            raise IndexError('There is no {}th field'.format(key))

    def __repr__(self) -> str:
        field_names = [
            name
            for name, _ in self._c_type._fields_
            if hasattr(self, name)
        ]
        if field_names:
            fields_suffix = (
                " "
                + " ".join(
                    "{}={}".format(name, getattr(self, name))
                    for name in field_names
                )
            )
        else:
            fields_suffix = ""
        return "<{}{}>".format(type(self).__name__, fields_suffix)

    @property
    def as_tuple(self) -> tuple:
        return tuple(getattr(self, f) for f, _ in self._c_type._fields_)

    def __eq__(self, other: Any) -> bool:
        return (isinstance(other, type(self)) and
                self.as_tuple == other.as_tuple)

    def __ne__(self, other: Any) -> bool:
        return not (self == other)

    def __hash__(self) -> int:
        return hash(self.as_tuple)




class _Metadata_c_type(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
         [
        ('dummy', ctypes.c_byte),
] 
    )
    _null_value: ClassVar[_Metadata_c_type]

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
class _EntityInfo_c_type(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
         [
        ('md',
            _Metadata_c_type
         ),
        ('rebindings',
            _EnvRebindings_c_type
         ),
        ('from_rebound',
            ctypes.c_uint8
         ),
] 
    )
    _null_value: ClassVar[_EntityInfo_c_type]
class _Entity_c_type(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
         [
        ('node',
            LktNode._node_c_type
         ),
        ('info',
            _EntityInfo_c_type
         ),
] 
    )
    _null_value: ClassVar[_Entity_c_type]

    @classmethod
    def from_bare_node(cls, node_c_value):
        return cls(node_c_value, _EntityInfo_c_type._null_value)




class CompleteItem(_BaseStruct):
    """
    Completion item for language servers
    """

    

    __slots__ = ('_declaration', )

    def __init__(
        self,
        declaration: Decl,
    ):
        self._declaration = declaration


    @property
    def declaration(self) -> Decl:
        """

        """
        return self._declaration

    class _c_type(ctypes.Structure):
        _fields_ =  [
        ('declaration',
            _Entity_c_type
         ),
] 

    class _Holder:
        def __init__(self, c_value):
            self.c_value = c_value

        def clear(self):
            self.c_value = None

        def __del__(self):
            self.clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(
            LktNode._wrap(c_value.declaration),
        )

    @classmethod
    def _unwrap(cls, value, context=None):
        if not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)

        
        declaration = Decl._unwrap(value.declaration)

        result = cls._Holder(cls._c_type(
            
            declaration=declaration,
        ))


        return result







class DecodedCharValue(_BaseStruct):
    """
    Result for ``CharLit.p_denoted_value``.

    If that property is successful, set ``has_error`` to false and ``value`` to
    the decoded character value. Otherwise, set ``has_error`` to true and
    ``error_sloc`` and ``error_message`` to give information about the decoding
    failure.
    """

    

    __slots__ = ('_value', '_has_error', '_error_sloc', '_error_message')

    def __init__(
        self,
        value: str,
        has_error: bool,
        error_sloc: Sloc,
        error_message: str,
    ):
        self._value = value
        self._has_error = has_error
        self._error_sloc = error_sloc
        self._error_message = error_message


    @property
    def value(self) -> str:
        """

        """
        return self._value

    @property
    def has_error(self) -> bool:
        """

        """
        return self._has_error

    @property
    def error_sloc(self) -> Sloc:
        """

        """
        return self._error_sloc

    @property
    def error_message(self) -> str:
        """

        """
        return self._error_message

    class _c_type(ctypes.Structure):
        _fields_ =  [
        ('value',
            ctypes.c_uint32
         ),
        ('has_error',
            ctypes.c_uint8
         ),
        ('error_sloc',
            Sloc._c_type
         ),
        ('error_message',
            _String.c_type
         ),
] 

    class _Holder:
        def __init__(self, c_value):
            self.c_value = c_value

        def clear(self):
            self.c_value = None

        def __del__(self):
            if self.c_value is not None:
                DecodedCharValue._dec_ref(self.c_value)
            self.clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(
            chr(c_value.value),
            bool(c_value.has_error),
            c_value.error_sloc._wrap(),
            _String.wrap(c_value.error_message),
        )

    @classmethod
    def _unwrap(cls, value, context=None):
        if not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)

        
        value = ord(value.value)
        
        has_error = bool(value.has_error)
        
        error_sloc = Sloc._c_type._unwrap(value.error_sloc)
        
        error_message = _String.unwrap(value.error_message)

        result = cls._Holder(cls._c_type(
            
            value=value,
            
            has_error=has_error,
            
            error_sloc=error_sloc,
            
            error_message=error_message.c_value,
        ))

        cls._inc_ref(result.c_value)

        return result

    _c_ptr_type = ctypes.POINTER(_c_type)
    _inc_ref = staticmethod(_import_func('lkt_internal_decoded_char_value_inc_ref',
                            [_c_ptr_type], None))
    _dec_ref = staticmethod(_import_func('lkt_internal_decoded_char_value_dec_ref',
                            [_c_ptr_type], None))






class DecodedStringValue(_BaseStruct):
    """
    Result for ``StringLit.p_denoted_value``.

    If that property is successful, set ``has_error`` to false and ``value`` to
    the decoded string value. Otherwise, set ``has_error`` to true and
    ``error_sloc`` and ``error_message`` to give information about the decoding
    failure.
    """

    

    __slots__ = ('_value', '_has_error', '_error_sloc', '_error_message')

    def __init__(
        self,
        value: str,
        has_error: bool,
        error_sloc: Sloc,
        error_message: str,
    ):
        self._value = value
        self._has_error = has_error
        self._error_sloc = error_sloc
        self._error_message = error_message


    @property
    def value(self) -> str:
        """

        """
        return self._value

    @property
    def has_error(self) -> bool:
        """

        """
        return self._has_error

    @property
    def error_sloc(self) -> Sloc:
        """

        """
        return self._error_sloc

    @property
    def error_message(self) -> str:
        """

        """
        return self._error_message

    class _c_type(ctypes.Structure):
        _fields_ =  [
        ('value',
            _String.c_type
         ),
        ('has_error',
            ctypes.c_uint8
         ),
        ('error_sloc',
            Sloc._c_type
         ),
        ('error_message',
            _String.c_type
         ),
] 

    class _Holder:
        def __init__(self, c_value):
            self.c_value = c_value

        def clear(self):
            self.c_value = None

        def __del__(self):
            if self.c_value is not None:
                DecodedStringValue._dec_ref(self.c_value)
            self.clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(
            _String.wrap(c_value.value),
            bool(c_value.has_error),
            c_value.error_sloc._wrap(),
            _String.wrap(c_value.error_message),
        )

    @classmethod
    def _unwrap(cls, value, context=None):
        if not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)

        
        value = _String.unwrap(value.value)
        
        has_error = bool(value.has_error)
        
        error_sloc = Sloc._c_type._unwrap(value.error_sloc)
        
        error_message = _String.unwrap(value.error_message)

        result = cls._Holder(cls._c_type(
            
            value=value.c_value,
            
            has_error=has_error,
            
            error_sloc=error_sloc,
            
            error_message=error_message.c_value,
        ))

        cls._inc_ref(result.c_value)

        return result

    _c_ptr_type = ctypes.POINTER(_c_type)
    _inc_ref = staticmethod(_import_func('lkt_internal_decoded_string_value_inc_ref',
                            [_c_ptr_type], None))
    _dec_ref = staticmethod(_import_func('lkt_internal_decoded_string_value_dec_ref',
                            [_c_ptr_type], None))






class LogicContext(_BaseStruct):
    """
    Describes an interpretation of a reference. Can be attached to logic atoms
    (e.g. Binds) to indicate under which interpretation this particular atom
    was produced, which can in turn be used to produce informative diagnostics
    for resolution failures.
    """

    

    __slots__ = ('_ref_node', '_decl_node')

    def __init__(
        self,
        ref_node: LktNode,
        decl_node: LktNode,
    ):
        self._ref_node = ref_node
        self._decl_node = decl_node


    @property
    def ref_node(self) -> LktNode:
        """

        """
        return self._ref_node

    @property
    def decl_node(self) -> LktNode:
        """

        """
        return self._decl_node

    class _c_type(ctypes.Structure):
        _fields_ =  [
        ('ref_node',
            _Entity_c_type
         ),
        ('decl_node',
            _Entity_c_type
         ),
] 

    class _Holder:
        def __init__(self, c_value):
            self.c_value = c_value

        def clear(self):
            self.c_value = None

        def __del__(self):
            self.clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(
            LktNode._wrap(c_value.ref_node),
            LktNode._wrap(c_value.decl_node),
        )

    @classmethod
    def _unwrap(cls, value, context=None):
        if not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)

        
        ref_node = LktNode._unwrap(value.ref_node)
        
        decl_node = LktNode._unwrap(value.decl_node)

        result = cls._Holder(cls._c_type(
            
            ref_node=ref_node,
            
            decl_node=decl_node,
        ))


        return result







class RefResult(_BaseStruct):
    """
    Reference result struct
    """

    

    __slots__ = ('_ref', )

    def __init__(
        self,
        ref: RefId,
    ):
        self._ref = ref


    @property
    def ref(self) -> RefId:
        """

        """
        return self._ref

    class _c_type(ctypes.Structure):
        _fields_ =  [
        ('ref',
            _Entity_c_type
         ),
] 

    class _Holder:
        def __init__(self, c_value):
            self.c_value = c_value

        def clear(self):
            self.c_value = None

        def __del__(self):
            self.clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(
            LktNode._wrap(c_value.ref),
        )

    @classmethod
    def _unwrap(cls, value, context=None):
        if not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)

        
        ref = RefId._unwrap(value.ref)

        result = cls._Holder(cls._c_type(
            
            ref=ref,
        ))


        return result







class SolverDiagnostic(_BaseStruct):
    """
    A raw diagnostic produced by a solver resolution failure. This contains as
    much information as possible to allow formatters down the chain to
    filter/choose which diagnostics to show among a set of diagnostics produced
    for a single equation.

    * ``Message_Template`` is a string explaining the error, which may contain
      holes represented by the ``{}`` characters. Literal opening braces are
      encoded as ``{{``.

    * ``Args`` is an array of nodes, which are to be plugged in the holes of
      the template in the same order (i.e. the first argument goes into the
      first hole of the template, etc.).

    * ``Location`` is a node which indicates the location of the error.

    * ``Contexts`` is the array of contexts that were deemed relevant for this
      error.

    * ``Round`` is the solver round during which this diagnostic was emitted.
    """

    

    __slots__ = ('_message_template', '_args', '_location', '_contexts', '_round')

    def __init__(
        self,
        message_template: str,
        args: List[LktNode],
        location: LktNode,
        contexts: List[LogicContext],
        round: int,
    ):
        self._message_template = message_template
        self._args = args
        self._location = location
        self._contexts = contexts
        self._round = round


    @property
    def message_template(self) -> str:
        """

        """
        return self._message_template

    @property
    def args(self) -> List[LktNode]:
        """

        """
        return self._args

    @property
    def location(self) -> LktNode:
        """

        """
        return self._location

    @property
    def contexts(self) -> List[LogicContext]:
        """

        """
        return self._contexts

    @property
    def round(self) -> int:
        """

        """
        return self._round

    class _c_type(ctypes.Structure):
        _fields_ =  [
        ('message_template',
            _String.c_type
         ),
        ('args',
             ctypes.c_void_p
         ),
        ('location',
            LktNode._node_c_type
         ),
        ('contexts',
             ctypes.c_void_p
         ),
        ('round',
            ctypes.c_int
         ),
] 

    class _Holder:
        def __init__(self, c_value):
            self.c_value = c_value

        def clear(self):
            self.c_value = None

        def __del__(self):
            if self.c_value is not None:
                SolverDiagnostic._dec_ref(self.c_value)
            self.clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(
            _String.wrap(c_value.message_template),
            _LktNodeArrayConverter.wrap(ctypes.cast(c_value.args, _LktNodeArrayConverter.c_type), True),
            LktNode._wrap_bare_node(c_value.location),
            _LogicContextArrayConverter.wrap(ctypes.cast(c_value.contexts, _LogicContextArrayConverter.c_type), True),
            c_value.round,
        )

    @classmethod
    def _unwrap(cls, value, context=None):
        if not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)

        
        message_template = _String.unwrap(value.message_template)
        
        args = _LktNodeArrayConverter.unwrap(value.args)
        
        location = value.location._node_c_value
        
        contexts = _LogicContextArrayConverter.unwrap(value.contexts)
        
        round = int(value.round)

        result = cls._Holder(cls._c_type(
            
            message_template=message_template.c_value,
            
            args=ctypes.cast(args.c_value, ctypes.c_void_p),
            
            location=location,
            
            contexts=ctypes.cast(contexts.c_value, ctypes.c_void_p),
            
            round=round,
        ))

        cls._inc_ref(result.c_value)

        return result

    _c_ptr_type = ctypes.POINTER(_c_type)
    _inc_ref = staticmethod(_import_func('lkt_internal_solver_diagnostic_inc_ref',
                            [_c_ptr_type], None))
    _dec_ref = staticmethod(_import_func('lkt_internal_solver_diagnostic_dec_ref',
                            [_c_ptr_type], None))






class SolverResult(_BaseStruct):
    """
    A pair returned by the ``Solve_With_Diagnostic`` primitive, consisting of:

    * A ``Success`` field indicating whether resolution was successful or not.

    * A ``Diagnostics`` field containing an array of diagnostics which may be
      non-empty if ``Success`` is ``False``.
    """

    

    __slots__ = ('_success', '_diagnostics')

    def __init__(
        self,
        success: bool,
        diagnostics: List[SolverDiagnostic],
    ):
        self._success = success
        self._diagnostics = diagnostics


    @property
    def success(self) -> bool:
        """

        """
        return self._success

    @property
    def diagnostics(self) -> List[SolverDiagnostic]:
        """

        """
        return self._diagnostics

    class _c_type(ctypes.Structure):
        _fields_ =  [
        ('success',
            ctypes.c_uint8
         ),
        ('diagnostics',
             ctypes.c_void_p
         ),
] 

    class _Holder:
        def __init__(self, c_value):
            self.c_value = c_value

        def clear(self):
            self.c_value = None

        def __del__(self):
            if self.c_value is not None:
                SolverResult._dec_ref(self.c_value)
            self.clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(
            bool(c_value.success),
            _SolverDiagnosticArrayConverter.wrap(ctypes.cast(c_value.diagnostics, _SolverDiagnosticArrayConverter.c_type), True),
        )

    @classmethod
    def _unwrap(cls, value, context=None):
        if not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)

        
        success = bool(value.success)
        
        diagnostics = _SolverDiagnosticArrayConverter.unwrap(value.diagnostics)

        result = cls._Holder(cls._c_type(
            
            success=success,
            
            diagnostics=ctypes.cast(diagnostics.c_value, ctypes.c_void_p),
        ))

        cls._inc_ref(result.c_value)

        return result

    _c_ptr_type = ctypes.POINTER(_c_type)
    _inc_ref = staticmethod(_import_func('lkt_internal_solver_result_inc_ref',
                            [_c_ptr_type], None))
    _dec_ref = staticmethod(_import_func('lkt_internal_solver_result_dec_ref',
                            [_c_ptr_type], None))




_Metadata_c_type._null_value = _Metadata_c_type()
_EntityInfo_c_type._null_value = _EntityInfo_c_type(_Metadata_c_type._null_value,
                                                None)


#
# Low-level binding - Second part
#

# For performance, allocate a single C API entity for all uses of null
# entities.
_Entity_c_type._null_value = _Entity_c_type()
_Entity_c_type._null_value.node = None



class _BaseArray:
    """
    Base class for Ada arrays bindings.
    """

    c_element_type: ClassVar[Any]
    """
    Ctype class for array elements.
    """

    items_refcounted = False
    """
    Whether items for this arrays are ref-counted.
    """

    __slots__ = ('c_value', 'length', 'items')

    def __init__(self, c_value):
        self.c_value = c_value

        self.length = c_value.contents.n

        items_addr = _field_address(c_value.contents, 'items')
        items = self.c_element_type.from_address(items_addr)
        self.items = ctypes.pointer(items)

    def __repr__(self):
        return '<{} {}>'.format(type(self).__name__, list(self))

    def clear(self):
        self.c_value = None
        self.length = None
        self.items = None

    def __del__(self):
        self.dec_ref(self.c_value)
        self.clear()

    @classmethod
    def wrap(cls, c_value, from_field_access):
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
                item = cls.c_element_type.from_buffer_copy(item)
            except TypeError:
                pass
            result.append(helper.wrap_item(item))

        # If this array value comes from a structure field, we must not call
        # its dec_ref primitive, as it is up to the structure's dec_ref
        # primitive to take care of it.
        if from_field_access:
            helper.clear()

        return result

    @classmethod
    def unwrap(cls, value, context=None):
        if not isinstance(value, list):
            _raise_type_error('list', value)

        # Create a holder for the result
        result = cls(cls.create(len(value)))

        # Unwrap all items at once, preserving their holder so that resources
        # are deallocated if there is an error at some point.
        items = [result.unwrap_item(item, context) for item in value]

        # Initialize the resulting array
        for i, (_, item) in enumerate(items):
            result.items[i] = item

        # At this point, we know that this is successful. We don't want
        # holders to dec-ref the content so that the return array takes over
        # the corresponding refcounting shares.
        if cls.items_refcounted:
            for holder, _ in items:
                holder.clear()

        return result






class _CompleteItemArrayConverter(_BaseArray):
    """
    Wrapper class for arrays of InternalCompleteItem.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseArray.__slots__
    items_refcounted = False

    @staticmethod
    def wrap_item(item):
        return CompleteItem._wrap(item)

    @staticmethod
    def unwrap_item(item, context=None):
        c_holder = CompleteItem._unwrap(item)
        c_value = c_holder.c_value
        return (c_holder, c_value)

    c_element_type = CompleteItem._c_type

    class c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', CompleteItem._c_type * 1)]

    c_type = ctypes.POINTER(c_struct)

    create = staticmethod(_import_func(
        'lkt_internal_complete_item_array_create', [ctypes.c_int], c_type))
    inc_ref = staticmethod(_import_func(
        'lkt_internal_complete_item_array_inc_ref', [c_type], None))
    dec_ref = staticmethod(_import_func(
        'lkt_internal_complete_item_array_dec_ref', [c_type], None))






class _LktNodeArrayConverter(_BaseArray):
    """
    Wrapper class for arrays of InternalEntity.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseArray.__slots__
    items_refcounted = False

    @staticmethod
    def wrap_item(item):
        return LktNode._wrap(item)

    @staticmethod
    def unwrap_item(item, context=None):
        c_holder = LktNode._unwrap(item)
        c_value = c_holder
        return (c_holder, c_value)

    c_element_type = _Entity_c_type

    class c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', _Entity_c_type * 1)]

    c_type = ctypes.POINTER(c_struct)

    create = staticmethod(_import_func(
        'lkt_node_array_create', [ctypes.c_int], c_type))
    inc_ref = staticmethod(_import_func(
        'lkt_node_array_inc_ref', [c_type], None))
    dec_ref = staticmethod(_import_func(
        'lkt_node_array_dec_ref', [c_type], None))






class _LogicContextArrayConverter(_BaseArray):
    """
    Wrapper class for arrays of InternalLogicContext.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseArray.__slots__
    items_refcounted = False

    @staticmethod
    def wrap_item(item):
        return LogicContext._wrap(item)

    @staticmethod
    def unwrap_item(item, context=None):
        c_holder = LogicContext._unwrap(item)
        c_value = c_holder.c_value
        return (c_holder, c_value)

    c_element_type = LogicContext._c_type

    class c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', LogicContext._c_type * 1)]

    c_type = ctypes.POINTER(c_struct)

    create = staticmethod(_import_func(
        'lkt_internal_logic_context_array_create', [ctypes.c_int], c_type))
    inc_ref = staticmethod(_import_func(
        'lkt_internal_logic_context_array_inc_ref', [c_type], None))
    dec_ref = staticmethod(_import_func(
        'lkt_internal_logic_context_array_dec_ref', [c_type], None))






class _RefResultArrayConverter(_BaseArray):
    """
    Wrapper class for arrays of InternalRefResult.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseArray.__slots__
    items_refcounted = False

    @staticmethod
    def wrap_item(item):
        return RefResult._wrap(item)

    @staticmethod
    def unwrap_item(item, context=None):
        c_holder = RefResult._unwrap(item)
        c_value = c_holder.c_value
        return (c_holder, c_value)

    c_element_type = RefResult._c_type

    class c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', RefResult._c_type * 1)]

    c_type = ctypes.POINTER(c_struct)

    create = staticmethod(_import_func(
        'lkt_internal_ref_result_array_create', [ctypes.c_int], c_type))
    inc_ref = staticmethod(_import_func(
        'lkt_internal_ref_result_array_inc_ref', [c_type], None))
    dec_ref = staticmethod(_import_func(
        'lkt_internal_ref_result_array_dec_ref', [c_type], None))






class _SolverDiagnosticArrayConverter(_BaseArray):
    """
    Wrapper class for arrays of InternalSolverDiagnostic.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseArray.__slots__
    items_refcounted = True

    @staticmethod
    def wrap_item(item):
        return SolverDiagnostic._wrap(item)

    @staticmethod
    def unwrap_item(item, context=None):
        c_holder = SolverDiagnostic._unwrap(item)
        c_value = c_holder.c_value
        return (c_holder, c_value)

    c_element_type = SolverDiagnostic._c_type

    class c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', SolverDiagnostic._c_type * 1)]

    c_type = ctypes.POINTER(c_struct)

    create = staticmethod(_import_func(
        'lkt_internal_solver_diagnostic_array_create', [ctypes.c_int], c_type))
    inc_ref = staticmethod(_import_func(
        'lkt_internal_solver_diagnostic_array_inc_ref', [c_type], None))
    dec_ref = staticmethod(_import_func(
        'lkt_internal_solver_diagnostic_array_dec_ref', [c_type], None))






class _AnalysisUnitArrayConverter(_BaseArray):
    """
    Wrapper class for arrays of InternalUnit.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseArray.__slots__
    items_refcounted = False

    @staticmethod
    def wrap_item(item):
        return AnalysisUnit._wrap(item)

    @staticmethod
    def unwrap_item(item, context=None):
        c_holder = AnalysisUnit._unwrap(item)
        c_value = c_holder
        return (c_holder, c_value)

    c_element_type = AnalysisUnit._c_type

    class c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', AnalysisUnit._c_type * 1)]

    c_type = ctypes.POINTER(c_struct)

    create = staticmethod(_import_func(
        'lkt_analysis_unit_array_create', [ctypes.c_int], c_type))
    inc_ref = staticmethod(_import_func(
        'lkt_analysis_unit_array_inc_ref', [c_type], None))
    dec_ref = staticmethod(_import_func(
        'lkt_analysis_unit_array_dec_ref', [c_type], None))





_IteratedType = TypeVar("_IteratedType")

class _BaseIterator(Generic[_IteratedType]):
    """
Base class for Ada iterator bindings.

An iterator provides a mean to retrieve values one-at-a-time.

Currently, each iterator is bound to the analysis context used to create it.
Iterators are invalidated as soon as any unit of that analysis is reparsed. Due
to the nature of iterators (lazy computations), this invalidation is necessary
to avoid use of inconsistent state, such as an iterator trying to use analysis
context data that is stale.
"""

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
        """
      Return the next value from the iterator. Raises ``StopIteration`` if
      there is no more element to retrieve.

      This raises a ``Stale_Reference_Error`` exception if the iterator is
      invalidated.
      """
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




_free = _import_func(
    'lkt_free',
    [ctypes.c_void_p], None
)

_destroy_text = _import_func(
    'lkt_destroy_text', [ctypes.POINTER(_text)], None
)

_symbol_text = _import_func(
    'lkt_symbol_text',
    [ctypes.POINTER(_symbol_type), ctypes.POINTER(_text)], None
)

_get_versions = _import_func(
    'lkt_get_versions',
    [ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)], None
)

# Analysis primitives
_allocate_analysis_context = _import_func(
    'lkt_allocate_analysis_context',
    [],
    AnalysisContext._c_type,
)
_initialize_analysis_context = _import_func(
    'lkt_initialize_analysis_context',
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
    'lkt_context_incref',
    [AnalysisContext._c_type], AnalysisContext._c_type
)
_context_decref = _import_func(
    'lkt_context_decref',
    [AnalysisContext._c_type], None
)
_context_symbol = _import_func(
    'lkt_context_symbol',
    [AnalysisContext._c_type,
     ctypes.POINTER(_text),
     ctypes.POINTER(_symbol_type)], ctypes.c_int
)
_discard_errors_in_populate_lexical_env = _import_func(
   'lkt_context_discard_errors_in_populate_lexical_env',
   [AnalysisContext._c_type, ctypes.c_int], None
)
_get_analysis_unit_from_file = _import_func(
    'lkt_get_analysis_unit_from_file',
    [AnalysisContext._c_type,  # context
     ctypes.c_char_p,          # filename
     ctypes.c_char_p,          # charset
     ctypes.c_int,             # reparse
     ctypes.c_int],            # grammar rule
    AnalysisUnit._c_type
)
_get_analysis_unit_from_buffer = _import_func(
    'lkt_get_analysis_unit_from_buffer',
    [AnalysisContext._c_type,  # context
     ctypes.c_char_p,          # filename
     ctypes.c_char_p,          # charset
     ctypes.c_char_p,          # buffer
     ctypes.c_size_t,          # buffer_size
     ctypes.c_int],            # grammar rule
    AnalysisUnit._c_type
)
_get_analysis_unit_from_provider = _import_func(
    'lkt_get_analysis_unit_from_provider',
    [AnalysisContext._c_type,  # context
     ctypes.POINTER(_text),    # name
     ctypes.c_int,             # kind
     ctypes.c_char_p,          # charset
     ctypes.c_int],            # reparse
    AnalysisUnit._c_type
)
_unit_root = _import_func(
    'lkt_unit_root',
    [AnalysisUnit._c_type, ctypes.POINTER(_Entity_c_type)], None
)
_unit_first_token = _import_func(
    "lkt_unit_first_token",
    [AnalysisUnit._c_type, Token._c_type], None
)
_unit_last_token = _import_func(
    "lkt_unit_last_token",
    [AnalysisUnit._c_type, Token._c_type], None
)
_unit_token_count = _import_func(
    "lkt_unit_token_count",
    [AnalysisUnit._c_type], ctypes.c_int
)
_unit_trivia_count = _import_func(
    "lkt_unit_trivia_count",
    [AnalysisUnit._c_type], ctypes.c_int
)
_unit_lookup_token = _import_func(
    "lkt_unit_lookup_token",
    [AnalysisUnit._c_type,
     ctypes.POINTER(Sloc._c_type),
     Token._c_type],
    None
)
_unit_dump_lexical_env = _import_func(
    "lkt_unit_dump_lexical_env",
    [AnalysisUnit._c_type], None
)
_unit_filename = _import_func(
    "lkt_unit_filename",
    [AnalysisUnit._c_type], ctypes.POINTER(ctypes.c_char)
)
_unit_diagnostic_count = _import_func(
    'lkt_unit_diagnostic_count',
    [AnalysisUnit._c_type], ctypes.c_uint
)
_unit_diagnostic = _import_func(
    'lkt_unit_diagnostic',
    [AnalysisUnit._c_type, ctypes.c_uint, ctypes.POINTER(Diagnostic._c_type)],
    ctypes.c_int
)
_unit_context = _import_func(
    'lkt_unit_context',
    [AnalysisUnit._c_type], AnalysisContext._c_type
)
_unit_reparse_from_file = _import_func(
    'lkt_unit_reparse_from_file',
    [AnalysisUnit._c_type,    # unit
     ctypes.c_char_p],        # charset
    ctypes.c_int
)
_unit_reparse_from_buffer = _import_func(
    'lkt_unit_reparse_from_buffer',
    [AnalysisUnit._c_type, # unit
     ctypes.c_char_p,      # charset
     ctypes.c_char_p,      # buffer
     ctypes.c_size_t],     # buffer_size
    None
)
_unit_populate_lexical_env = _import_func(
    'lkt_unit_populate_lexical_env',
    [
        AnalysisUnit._c_type,
    ],
    ctypes.c_int
)

# General AST node primitives
_node_hash = _import_func(
    'lkt_node_hash',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_uint32
)

_node_is_equivalent = _import_func(
    'lkt_node_is_equivalent',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)], ctypes.c_uint8
)

_node_kind = _import_func(
    'lkt_node_kind',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_int
)
_node_unit = _import_func(
    'lkt_node_unit',
    [ctypes.POINTER(_Entity_c_type)], AnalysisUnit._c_type
)
_node_is_token_node = _import_func(
    'lkt_node_is_token_node',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_int
)
_node_is_synthetic = _import_func(
    'lkt_node_is_synthetic',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_int
)
_node_image = _import_func(
    'lkt_node_image',
    [ctypes.POINTER(_Entity_c_type), ctypes.POINTER(_text)], None
)
_node_text = _import_func(
    'lkt_node_text',
    [ctypes.POINTER(_Entity_c_type), ctypes.POINTER(_text)], None
)
_node_sloc_range = _import_func(
    'lkt_node_sloc_range',
    [ctypes.POINTER(_Entity_c_type), ctypes.POINTER(SlocRange._c_type)], None
)
_lookup_in_node = _import_func(
    'lkt_lookup_in_node',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(Sloc._c_type),
     ctypes.POINTER(_Entity_c_type)], None
)
_node_children_count = _import_func(
    'lkt_node_children_count',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_uint
)
_node_child = _import_func(
    'lkt_node_child',
    [ctypes.POINTER(_Entity_c_type), ctypes.c_uint, ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)

_lkt_node_parent = _import_func(
    'lkt_lkt_node_parent',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_parents = _import_func(
    'lkt_lkt_node_parents',
    [ctypes.POINTER(_Entity_c_type),
        
        ctypes.c_uint8,
     ctypes.POINTER(_LktNodeArrayConverter.c_type)],
    ctypes.c_int
)
_lkt_node_children = _import_func(
    'lkt_lkt_node_children',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_LktNodeArrayConverter.c_type)],
    ctypes.c_int
)
_lkt_node_token_start = _import_func(
    'lkt_lkt_node_token_start',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(Token._c_struct)],
    ctypes.c_int
)
_lkt_node_token_end = _import_func(
    'lkt_lkt_node_token_end',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(Token._c_struct)],
    ctypes.c_int
)
_lkt_node_child_index = _import_func(
    'lkt_lkt_node_child_index',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_int)],
    ctypes.c_int
)
_lkt_node_previous_sibling = _import_func(
    'lkt_lkt_node_previous_sibling',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_next_sibling = _import_func(
    'lkt_lkt_node_next_sibling',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_unit = _import_func(
    'lkt_lkt_node_unit',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(AnalysisUnit._c_type)],
    ctypes.c_int
)
_lkt_node_is_ghost = _import_func(
    'lkt_lkt_node_is_ghost',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_lkt_node_full_sloc_image = _import_func(
    'lkt_lkt_node_full_sloc_image',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_lkt_node_completion_item_kind_to_int = _import_func(
    'lkt_lkt_node_completion_item_kind_to_int',
    [ctypes.POINTER(_Entity_c_type),
        
        ctypes.c_int,
     ctypes.POINTER(ctypes.c_int)],
    ctypes.c_int
)
_lkt_node_p_set_solver_debug_mode = _import_func(
    'lkt_lkt_node_p_set_solver_debug_mode',
    [ctypes.POINTER(_Entity_c_type),
        
        ctypes.c_uint8,
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_lkt_node_p_basic_trait_gen = _import_func(
    'lkt_lkt_node_p_basic_trait_gen',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_basic_trait = _import_func(
    'lkt_lkt_node_p_basic_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_node_gen_trait = _import_func(
    'lkt_lkt_node_p_node_gen_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_node_trait = _import_func(
    'lkt_lkt_node_p_node_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_indexable_gen_trait = _import_func(
    'lkt_lkt_node_p_indexable_gen_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_indexable_trait = _import_func(
    'lkt_lkt_node_p_indexable_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_token_node_trait = _import_func(
    'lkt_lkt_node_p_token_node_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_error_node_trait = _import_func(
    'lkt_lkt_node_p_error_node_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_char_type = _import_func(
    'lkt_lkt_node_p_char_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_int_type = _import_func(
    'lkt_lkt_node_p_int_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_bool_type = _import_func(
    'lkt_lkt_node_p_bool_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_bigint_type = _import_func(
    'lkt_lkt_node_p_bigint_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_string_type = _import_func(
    'lkt_lkt_node_p_string_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_symbol_type = _import_func(
    'lkt_lkt_node_p_symbol_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_property_error_type = _import_func(
    'lkt_lkt_node_p_property_error_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_regexp_type = _import_func(
    'lkt_lkt_node_p_regexp_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_entity_gen_type = _import_func(
    'lkt_lkt_node_p_entity_gen_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_entity_type = _import_func(
    'lkt_lkt_node_p_entity_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_logicvar_type = _import_func(
    'lkt_lkt_node_p_logicvar_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_equation_type = _import_func(
    'lkt_lkt_node_p_equation_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_array_gen_type = _import_func(
    'lkt_lkt_node_p_array_gen_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_array_type = _import_func(
    'lkt_lkt_node_p_array_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_astlist_gen_type = _import_func(
    'lkt_lkt_node_p_astlist_gen_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_astlist_type = _import_func(
    'lkt_lkt_node_p_astlist_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_node_builder_gen_type = _import_func(
    'lkt_lkt_node_p_node_builder_gen_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_node_builder_type = _import_func(
    'lkt_lkt_node_p_node_builder_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_iterator_gen_trait = _import_func(
    'lkt_lkt_node_p_iterator_gen_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_iterator_trait = _import_func(
    'lkt_lkt_node_p_iterator_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_analysis_unit_gen_trait = _import_func(
    'lkt_lkt_node_p_analysis_unit_gen_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_analysis_unit_trait = _import_func(
    'lkt_lkt_node_p_analysis_unit_trait',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_topmost_invalid_decl = _import_func(
    'lkt_lkt_node_p_topmost_invalid_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lkt_node_p_nameres_diagnostics = _import_func(
    'lkt_lkt_node_p_nameres_diagnostics',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_SolverDiagnosticArrayConverter.c_type)],
    ctypes.c_int
)
_lkt_node_p_solve_enclosing_context = _import_func(
    'lkt_lkt_node_p_solve_enclosing_context',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(SolverResult._c_type)],
    ctypes.c_int
)
_lkt_node_p_xref_entry_point = _import_func(
    'lkt_lkt_node_p_xref_entry_point',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_lkt_node_p_complete = _import_func(
    'lkt_lkt_node_p_complete',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_CompleteItemArrayConverter.c_type)],
    ctypes.c_int
)
_argument_f_name = _import_func(
    'lkt_argument_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_argument_f_value = _import_func(
    'lkt_argument_f_value',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lexer_case_rule_cond_alt_f_cond_exprs = _import_func(
    'lkt_lexer_case_rule_cond_alt_f_cond_exprs',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lexer_case_rule_cond_alt_f_send = _import_func(
    'lkt_lexer_case_rule_cond_alt_f_send',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lexer_case_rule_default_alt_f_send = _import_func(
    'lkt_lexer_case_rule_default_alt_f_send',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_base_match_branch_f_expr = _import_func(
    'lkt_base_match_branch_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_base_match_branch_p_match_part = _import_func(
    'lkt_base_match_branch_p_match_part',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_match_branch_f_decl = _import_func(
    'lkt_match_branch_f_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_pattern_match_branch_f_pattern = _import_func(
    'lkt_pattern_match_branch_f_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_block_expr_clause_f_clause = _import_func(
    'lkt_block_expr_clause_f_clause',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_class_qualifier_p_as_bool = _import_func(
    'lkt_class_qualifier_p_as_bool',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_decl_f_syn_name = _import_func(
    'lkt_decl_f_syn_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_p_custom_image = _import_func(
    'lkt_decl_p_custom_image',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_decl_p_decl_type_name = _import_func(
    'lkt_decl_p_decl_type_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_decl_p_def_ids = _import_func(
    'lkt_decl_p_def_ids',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_LktNodeArrayConverter.c_type)],
    ctypes.c_int
)
_decl_p_as_bare_decl = _import_func(
    'lkt_decl_p_as_bare_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_p_get_type = _import_func(
    'lkt_decl_p_get_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_p_get_cast_type = _import_func(
    'lkt_decl_p_get_cast_type',
    [ctypes.POINTER(_Entity_c_type),
        
        ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_p_get_keep_type = _import_func(
    'lkt_decl_p_get_keep_type',
    [ctypes.POINTER(_Entity_c_type),
        
        ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_p_get_suffix_type = _import_func(
    'lkt_decl_p_get_suffix_type',
    [ctypes.POINTER(_Entity_c_type),
        
        ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_p_is_generic = _import_func(
    'lkt_decl_p_is_generic',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_decl_p_return_type_is_instantiated = _import_func(
    'lkt_decl_p_return_type_is_instantiated',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_decl_p_is_instantiated = _import_func(
    'lkt_decl_p_is_instantiated',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_decl_p_name = _import_func(
    'lkt_decl_p_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_symbol_type)],
    ctypes.c_int
)
_decl_p_full_name = _import_func(
    'lkt_decl_p_full_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_base_grammar_rule_decl_f_expr = _import_func(
    'lkt_base_grammar_rule_decl_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_explicitly_typed_decl_f_decl_type = _import_func(
    'lkt_explicitly_typed_decl_f_decl_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_component_decl_f_default_val = _import_func(
    'lkt_component_decl_f_default_val',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_field_decl_f_trait_ref = _import_func(
    'lkt_field_decl_f_trait_ref',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_fun_param_decl_f_decl_annotations = _import_func(
    'lkt_fun_param_decl_f_decl_annotations',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_val_decl_f_expr = _import_func(
    'lkt_val_decl_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_fun_decl_f_params = _import_func(
    'lkt_fun_decl_f_params',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_fun_decl_f_return_type = _import_func(
    'lkt_fun_decl_f_return_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_fun_decl_f_trait_ref = _import_func(
    'lkt_fun_decl_f_trait_ref',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_fun_decl_f_body = _import_func(
    'lkt_fun_decl_f_body',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_fun_decl_p_is_dynamic_combiner = _import_func(
    'lkt_fun_decl_p_is_dynamic_combiner',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_fun_decl_p_find_all_overrides = _import_func(
    'lkt_fun_decl_p_find_all_overrides',
    [ctypes.POINTER(_Entity_c_type),
        
        _AnalysisUnitArrayConverter.c_type,
     ctypes.POINTER(_LktNodeArrayConverter.c_type)],
    ctypes.c_int
)
_env_spec_decl_f_actions = _import_func(
    'lkt_env_spec_decl_f_actions',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_generic_decl_f_generic_param_decls = _import_func(
    'lkt_generic_decl_f_generic_param_decls',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_generic_decl_f_decl = _import_func(
    'lkt_generic_decl_f_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_decl_f_rules = _import_func(
    'lkt_grammar_decl_f_rules',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lexer_decl_f_rules = _import_func(
    'lkt_lexer_decl_f_rules',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lexer_family_decl_f_rules = _import_func(
    'lkt_lexer_family_decl_f_rules',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_type_decl_f_traits = _import_func(
    'lkt_type_decl_f_traits',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_type_decl_f_syn_base_type = _import_func(
    'lkt_type_decl_f_syn_base_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_type_decl_p_def_id = _import_func(
    'lkt_type_decl_p_def_id',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_type_decl_p_base_type = _import_func(
    'lkt_type_decl_p_base_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_type_decl_p_base_type_if_entity = _import_func(
    'lkt_type_decl_p_base_type_if_entity',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_generic_param_type_decl_f_has_class = _import_func(
    'lkt_generic_param_type_decl_f_has_class',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_named_type_decl_f_decls = _import_func(
    'lkt_named_type_decl_f_decls',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_enum_class_decl_f_branches = _import_func(
    'lkt_enum_class_decl_f_branches',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_enum_type_decl_f_literals = _import_func(
    'lkt_enum_type_decl_f_literals',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_annotation_f_name = _import_func(
    'lkt_decl_annotation_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_annotation_f_args = _import_func(
    'lkt_decl_annotation_f_args',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_decl_annotation_args_f_args = _import_func(
    'lkt_decl_annotation_args_f_args',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_elsif_branch_f_cond_expr = _import_func(
    'lkt_elsif_branch_f_cond_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_elsif_branch_f_then_expr = _import_func(
    'lkt_elsif_branch_f_then_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_enum_class_case_f_decls = _import_func(
    'lkt_enum_class_case_f_decls',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_excludes_null_p_as_bool = _import_func(
    'lkt_excludes_null_p_as_bool',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_expr_p_get_type = _import_func(
    'lkt_expr_p_get_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_expr_p_get_generic_type = _import_func(
    'lkt_expr_p_get_generic_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_expr_p_get_expected_type = _import_func(
    'lkt_expr_p_get_expected_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_expr_p_referenced_decl = _import_func(
    'lkt_expr_p_referenced_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_any_of_f_expr = _import_func(
    'lkt_any_of_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_any_of_f_values = _import_func(
    'lkt_any_of_f_values',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_array_literal_f_exprs = _import_func(
    'lkt_array_literal_f_exprs',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_array_literal_f_element_type = _import_func(
    'lkt_array_literal_f_element_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_base_call_expr_f_name = _import_func(
    'lkt_base_call_expr_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_base_call_expr_f_args = _import_func(
    'lkt_base_call_expr_f_args',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_bin_op_f_left = _import_func(
    'lkt_bin_op_f_left',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_bin_op_f_op = _import_func(
    'lkt_bin_op_f_op',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_bin_op_f_right = _import_func(
    'lkt_bin_op_f_right',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_block_expr_f_clauses = _import_func(
    'lkt_block_expr_f_clauses',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_cast_expr_f_expr = _import_func(
    'lkt_cast_expr_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_cast_expr_f_null_cond = _import_func(
    'lkt_cast_expr_f_null_cond',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_cast_expr_f_excludes_null = _import_func(
    'lkt_cast_expr_f_excludes_null',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_cast_expr_f_dest_type = _import_func(
    'lkt_cast_expr_f_dest_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_dot_expr_f_prefix = _import_func(
    'lkt_dot_expr_f_prefix',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_dot_expr_f_null_cond = _import_func(
    'lkt_dot_expr_f_null_cond',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_dot_expr_f_suffix = _import_func(
    'lkt_dot_expr_f_suffix',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_error_on_null_f_expr = _import_func(
    'lkt_error_on_null_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_generic_instantiation_f_name = _import_func(
    'lkt_generic_instantiation_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_generic_instantiation_f_args = _import_func(
    'lkt_generic_instantiation_f_args',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_discard_f_expr = _import_func(
    'lkt_grammar_discard_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_dont_skip_f_expr = _import_func(
    'lkt_grammar_dont_skip_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_dont_skip_f_dont_skip = _import_func(
    'lkt_grammar_dont_skip_f_dont_skip',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_list_f_list_type = _import_func(
    'lkt_grammar_list_f_list_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_list_f_kind = _import_func(
    'lkt_grammar_list_f_kind',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_list_f_expr = _import_func(
    'lkt_grammar_list_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_list_f_sep = _import_func(
    'lkt_grammar_list_f_sep',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_null_f_name = _import_func(
    'lkt_grammar_null_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_opt_f_expr = _import_func(
    'lkt_grammar_opt_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_opt_error_f_expr = _import_func(
    'lkt_grammar_opt_error_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_opt_error_group_f_expr = _import_func(
    'lkt_grammar_opt_error_group_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_opt_group_f_expr = _import_func(
    'lkt_grammar_opt_group_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_or_expr_f_sub_exprs = _import_func(
    'lkt_grammar_or_expr_f_sub_exprs',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_pick_f_exprs = _import_func(
    'lkt_grammar_pick_f_exprs',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_predicate_f_expr = _import_func(
    'lkt_grammar_predicate_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_predicate_f_prop_ref = _import_func(
    'lkt_grammar_predicate_f_prop_ref',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_rule_ref_f_node_name = _import_func(
    'lkt_grammar_rule_ref_f_node_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_skip_f_name = _import_func(
    'lkt_grammar_skip_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_stop_cut_f_expr = _import_func(
    'lkt_grammar_stop_cut_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_parse_node_expr_f_node_name = _import_func(
    'lkt_parse_node_expr_f_node_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_parse_node_expr_f_sub_exprs = _import_func(
    'lkt_parse_node_expr_f_sub_exprs',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_token_lit_p_denoted_value = _import_func(
    'lkt_token_lit_p_denoted_value',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(DecodedStringValue._c_type)],
    ctypes.c_int
)
_token_no_case_lit_f_lit = _import_func(
    'lkt_token_no_case_lit_f_lit',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_token_pattern_concat_f_left = _import_func(
    'lkt_token_pattern_concat_f_left',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_token_pattern_concat_f_right = _import_func(
    'lkt_token_pattern_concat_f_right',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_token_pattern_lit_p_denoted_value = _import_func(
    'lkt_token_pattern_lit_p_denoted_value',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(DecodedStringValue._c_type)],
    ctypes.c_int
)
_token_ref_f_token_name = _import_func(
    'lkt_token_ref_f_token_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_token_ref_f_expr = _import_func(
    'lkt_token_ref_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_id_p_custom_image = _import_func(
    'lkt_id_p_custom_image',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_def_id_p_name = _import_func(
    'lkt_def_id_p_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_def_id_p_get_implementatinons = _import_func(
    'lkt_def_id_p_get_implementatinons',
    [ctypes.POINTER(_Entity_c_type),
        
        _AnalysisUnitArrayConverter.c_type,
     ctypes.POINTER(_LktNodeArrayConverter.c_type)],
    ctypes.c_int
)
_def_id_p_decl_detail = _import_func(
    'lkt_def_id_p_decl_detail',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_def_id_p_completion_item_kind = _import_func(
    'lkt_def_id_p_completion_item_kind',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_int)],
    ctypes.c_int
)
_def_id_p_doc = _import_func(
    'lkt_def_id_p_doc',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_def_id_p_find_all_references = _import_func(
    'lkt_def_id_p_find_all_references',
    [ctypes.POINTER(_Entity_c_type),
        
        _AnalysisUnitArrayConverter.c_type,
     ctypes.POINTER(_RefResultArrayConverter.c_type)],
    ctypes.c_int
)
_ref_id_p_referenced_defining_name = _import_func(
    'lkt_ref_id_p_referenced_defining_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_if_expr_f_cond_expr = _import_func(
    'lkt_if_expr_f_cond_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_if_expr_f_then_expr = _import_func(
    'lkt_if_expr_f_then_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_if_expr_f_alternatives = _import_func(
    'lkt_if_expr_f_alternatives',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_if_expr_f_else_expr = _import_func(
    'lkt_if_expr_f_else_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_isa_f_expr = _import_func(
    'lkt_isa_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_isa_f_pattern = _import_func(
    'lkt_isa_f_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_keep_expr_f_expr = _import_func(
    'lkt_keep_expr_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_keep_expr_f_null_cond = _import_func(
    'lkt_keep_expr_f_null_cond',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_keep_expr_f_keep_type = _import_func(
    'lkt_keep_expr_f_keep_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lambda_expr_f_params = _import_func(
    'lkt_lambda_expr_f_params',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lambda_expr_f_return_type = _import_func(
    'lkt_lambda_expr_f_return_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lambda_expr_f_body = _import_func(
    'lkt_lambda_expr_f_body',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_char_lit_p_denoted_value = _import_func(
    'lkt_char_lit_p_denoted_value',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(DecodedCharValue._c_type)],
    ctypes.c_int
)
_null_lit_f_dest_type = _import_func(
    'lkt_null_lit_f_dest_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_string_lit_p_denoted_value = _import_func(
    'lkt_string_lit_p_denoted_value',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(DecodedStringValue._c_type)],
    ctypes.c_int
)
_string_lit_p_is_prefixed_string = _import_func(
    'lkt_string_lit_p_is_prefixed_string',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_string_lit_p_prefix = _import_func(
    'lkt_string_lit_p_prefix',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint32)],
    ctypes.c_int
)
_string_lit_p_is_regexp_literal = _import_func(
    'lkt_string_lit_p_is_regexp_literal',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_block_string_lit_f_lines = _import_func(
    'lkt_block_string_lit_f_lines',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_logic_assign_f_dest_var = _import_func(
    'lkt_logic_assign_f_dest_var',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_logic_assign_f_value = _import_func(
    'lkt_logic_assign_f_value',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_logic_expr_f_expr = _import_func(
    'lkt_logic_expr_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_logic_propagate_f_dest_var = _import_func(
    'lkt_logic_propagate_f_dest_var',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_logic_propagate_f_call = _import_func(
    'lkt_logic_propagate_f_call',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_logic_unify_f_lhs = _import_func(
    'lkt_logic_unify_f_lhs',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_logic_unify_f_rhs = _import_func(
    'lkt_logic_unify_f_rhs',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_match_expr_f_match_expr = _import_func(
    'lkt_match_expr_f_match_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_match_expr_f_branches = _import_func(
    'lkt_match_expr_f_branches',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_not_expr_f_expr = _import_func(
    'lkt_not_expr_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_paren_expr_f_expr = _import_func(
    'lkt_paren_expr_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_raise_expr_f_dest_type = _import_func(
    'lkt_raise_expr_f_dest_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_raise_expr_f_except_expr = _import_func(
    'lkt_raise_expr_f_except_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_subscript_expr_f_prefix = _import_func(
    'lkt_subscript_expr_f_prefix',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_subscript_expr_f_null_cond = _import_func(
    'lkt_subscript_expr_f_null_cond',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_subscript_expr_f_index = _import_func(
    'lkt_subscript_expr_f_index',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_try_expr_f_try_expr = _import_func(
    'lkt_try_expr_f_try_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_try_expr_f_or_expr = _import_func(
    'lkt_try_expr_f_or_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_un_op_f_op = _import_func(
    'lkt_un_op_f_op',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_un_op_f_expr = _import_func(
    'lkt_un_op_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_full_decl_f_doc = _import_func(
    'lkt_full_decl_f_doc',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_full_decl_f_decl_annotations = _import_func(
    'lkt_full_decl_f_decl_annotations',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_full_decl_f_decl = _import_func(
    'lkt_full_decl_f_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_full_decl_p_has_annotation = _import_func(
    'lkt_full_decl_p_has_annotation',
    [ctypes.POINTER(_Entity_c_type),
        
        ctypes.POINTER(_symbol_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_grammar_list_sep_f_token = _import_func(
    'lkt_grammar_list_sep_f_token',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_grammar_list_sep_f_extra = _import_func(
    'lkt_grammar_list_sep_f_extra',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_import_f_name = _import_func(
    'lkt_import_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_import_p_referenced_unit = _import_func(
    'lkt_import_p_referenced_unit',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(AnalysisUnit._c_type)],
    ctypes.c_int
)
_langkit_root_f_imports = _import_func(
    'lkt_langkit_root_f_imports',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_langkit_root_f_decls = _import_func(
    'lkt_langkit_root_f_decls',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_langkit_root_p_fetch_prelude = _import_func(
    'lkt_langkit_root_p_fetch_prelude',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(AnalysisUnit._c_type)],
    ctypes.c_int
)
_lexer_case_rule_f_expr = _import_func(
    'lkt_lexer_case_rule_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lexer_case_rule_f_alts = _import_func(
    'lkt_lexer_case_rule_f_alts',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lexer_case_rule_send_f_sent = _import_func(
    'lkt_lexer_case_rule_send_f_sent',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_lexer_case_rule_send_f_match_size = _import_func(
    'lkt_lexer_case_rule_send_f_match_size',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_null_cond_qualifier_p_as_bool = _import_func(
    'lkt_null_cond_qualifier_p_as_bool',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_binding_pattern_f_decl = _import_func(
    'lkt_binding_pattern_f_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_binding_pattern_f_sub_pattern = _import_func(
    'lkt_binding_pattern_f_sub_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ellipsis_pattern_f_binding = _import_func(
    'lkt_ellipsis_pattern_f_binding',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_extended_pattern_f_sub_pattern = _import_func(
    'lkt_extended_pattern_f_sub_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_extended_pattern_f_details = _import_func(
    'lkt_extended_pattern_f_details',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_filtered_pattern_f_sub_pattern = _import_func(
    'lkt_filtered_pattern_f_sub_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_filtered_pattern_f_predicate = _import_func(
    'lkt_filtered_pattern_f_predicate',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_list_pattern_f_sub_patterns = _import_func(
    'lkt_list_pattern_f_sub_patterns',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_not_pattern_f_sub_pattern = _import_func(
    'lkt_not_pattern_f_sub_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_or_pattern_f_left_sub_pattern = _import_func(
    'lkt_or_pattern_f_left_sub_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_or_pattern_f_right_sub_pattern = _import_func(
    'lkt_or_pattern_f_right_sub_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_paren_pattern_f_sub_pattern = _import_func(
    'lkt_paren_pattern_f_sub_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_tuple_pattern_f_sub_patterns = _import_func(
    'lkt_tuple_pattern_f_sub_patterns',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_type_pattern_f_type_name = _import_func(
    'lkt_type_pattern_f_type_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_field_pattern_detail_f_id = _import_func(
    'lkt_field_pattern_detail_f_id',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_field_pattern_detail_f_expected_value = _import_func(
    'lkt_field_pattern_detail_f_expected_value',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_property_pattern_detail_f_call = _import_func(
    'lkt_property_pattern_detail_f_call',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_property_pattern_detail_f_expected_value = _import_func(
    'lkt_property_pattern_detail_f_expected_value',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_selector_pattern_detail_f_call = _import_func(
    'lkt_selector_pattern_detail_f_call',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_selector_pattern_detail_f_sub_pattern = _import_func(
    'lkt_selector_pattern_detail_f_sub_pattern',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_selector_call_f_quantifier = _import_func(
    'lkt_selector_call_f_quantifier',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_selector_call_f_binding = _import_func(
    'lkt_selector_call_f_binding',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_selector_call_f_selector_call = _import_func(
    'lkt_selector_call_f_selector_call',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_type_ref_p_referenced_decl = _import_func(
    'lkt_type_ref_p_referenced_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_function_type_ref_f_param_types = _import_func(
    'lkt_function_type_ref_f_param_types',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_function_type_ref_f_return_type = _import_func(
    'lkt_function_type_ref_f_return_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_generic_type_ref_f_type_name = _import_func(
    'lkt_generic_type_ref_f_type_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_generic_type_ref_f_args = _import_func(
    'lkt_generic_type_ref_f_args',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_simple_type_ref_f_type_name = _import_func(
    'lkt_simple_type_ref_f_type_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_var_bind_f_name = _import_func(
    'lkt_var_bind_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_var_bind_f_expr = _import_func(
    'lkt_var_bind_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)

# File readers
_dec_ref_file_reader = _import_func(
    'lkt_dec_ref_file_reader',
    [_file_reader], None
)



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
    'lkt_create_event_handler',
    [
        ctypes.py_object,
        _event_handler_destroy_func,
        _event_handler_unit_requested_func,
        _event_handler_unit_parsed_func,
    ],
    _event_handler,
)
_dec_ref_event_handler = _import_func(
    'lkt_dec_ref_event_handler', [_event_handler], None
)

# Unit providers
_dec_ref_unit_provider = _import_func(
    'lkt_dec_ref_unit_provider',
    [_unit_provider], None
)

      
_create_default_provider = _import_func(
    'lkt_create_default_provider',
    [ctypes.POINTER(ctypes.c_char_p)],
    _unit_provider,
)



# Misc
_token_get_kind = _import_func(
    "lkt_token_get_kind", [Token._c_type], ctypes.c_int
)
_token_kind_name = _import_func(
    "lkt_token_kind_name",
    [ctypes.c_int], ctypes.POINTER(ctypes.c_char)
)
_token_sloc_range = _import_func(
    "lkt_token_sloc_range",
    [Token._c_type, ctypes.POINTER(SlocRange._c_type)], None
)
_token_next = _import_func(
    "lkt_token_next",
    [Token._c_type, Token._c_type], None
)
_token_is_equivalent = _import_func(
    "lkt_token_is_equivalent",
    [Token._c_type, Token._c_type], ctypes.c_int
)
_token_previous = _import_func(
    "lkt_token_previous",
    [Token._c_type, Token._c_type], None
)
_token_range_text = _import_func(
    "lkt_token_range_text",
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
    1: Argument,
    2: ErrorLexerCaseRuleAlt,
    3: LexerCaseRuleCondAlt,
    4: LexerCaseRuleDefaultAlt,
    5: MatchBranch,
    6: PatternMatchBranch,
    7: BlockExprClause,
    8: BlockStringLine,
    9: ClassQualifierAbsent,
    10: ClassQualifierPresent,
    11: GrammarRuleDecl,
    12: SyntheticLexerDecl,
    13: NodeDecl,
    14: SelfDecl,
    15: BindingValDecl,
    16: EnumLitDecl,
    17: FieldDecl,
    18: FunParamDecl,
    19: LambdaParamDecl,
    20: DynVarDecl,
    21: MatchValDecl,
    22: ValDecl,
    23: FunDecl,
    24: EnvSpecDecl,
    25: ErrorDecl,
    26: GenericDecl,
    27: GrammarDecl,
    28: LexerDecl,
    29: LexerFamilyDecl,
    30: SynthFunDecl,
    31: SynthParamDecl,
    32: AnyTypeDecl,
    33: EnumClassAltDecl,
    34: FunctionType,
    35: GenericParamTypeDecl,
    36: ClassDecl,
    37: EnumClassDecl,
    38: EnumTypeDecl,
    39: StructDecl,
    40: TraitDecl,
    41: DeclAnnotation,
    42: DeclAnnotationArgs,
    43: DynEnvWrapper,
    44: ElsifBranch,
    45: EnumClassCase,
    46: ExcludesNullAbsent,
    47: ExcludesNullPresent,
    48: AnyOf,
    49: ArrayLiteral,
    50: CallExpr,
    51: LogicPredicate,
    52: LogicPropagateCall,
    53: BinOp,
    54: BlockExpr,
    55: CastExpr,
    56: DotExpr,
    57: ErrorOnNull,
    58: GenericInstantiation,
    59: ErrorGrammarExpr,
    60: GrammarCut,
    61: GrammarDiscard,
    62: GrammarDontSkip,
    63: GrammarList,
    64: GrammarNull,
    65: GrammarOpt,
    66: GrammarOptError,
    67: GrammarOptErrorGroup,
    68: GrammarOptGroup,
    69: GrammarOrExpr,
    70: GrammarPick,
    71: GrammarImplicitPick,
    72: GrammarPredicate,
    73: GrammarRuleRef,
    74: GrammarSkip,
    75: GrammarStopCut,
    76: ParseNodeExpr,
    77: TokenLit,
    78: TokenNoCaseLit,
    79: TokenPatternConcat,
    80: TokenPatternLit,
    81: TokenRef,
    82: Id,
    83: DefId,
    84: ModuleRefId,
    85: RefId,
    86: IfExpr,
    87: Isa,
    88: KeepExpr,
    89: LambdaExpr,
    90: BigNumLit,
    91: CharLit,
    92: NullLit,
    93: NumLit,
    94: BlockStringLit,
    95: SingleLineStringLit,
    96: PatternSingleLineStringLit,
    97: LogicAssign,
    98: LogicExpr,
    99: LogicPropagate,
    100: LogicUnify,
    101: MatchExpr,
    102: NotExpr,
    103: ParenExpr,
    104: RaiseExpr,
    105: SubscriptExpr,
    106: TryExpr,
    107: UnOp,
    108: FullDecl,
    109: GrammarListSep,
    110: Import,
    111: LangkitRoot,
    112: LexerCaseRule,
    113: LexerCaseRuleSend,
    114: ListKindOne,
    115: ListKindZero,
    116: ArgumentList,
    117: BaseLexerCaseRuleAltList,
    118: BaseMatchBranchList,
    119: BlockStringLineList,
    120: CallExprList,
    121: DeclAnnotationList,
    122: ElsifBranchList,
    123: EnumClassAltDeclList,
    124: EnumClassCaseList,
    125: EnumLitDeclList,
    126: ExprList,
    127: AnyOfList,
    128: FullDeclList,
    129: DeclBlock,
    130: GenericParamDeclList,
    131: FunParamDeclList,
    132: GrammarExprList,
    133: GrammarExprListList,
    134: ImportList,
    135: LambdaParamDeclList,
    136: LktNodeList,
    137: PatternDetailList,
    138: PatternList,
    139: RefIdList,
    140: TypeRefList,
    141: SyntheticTypeRefList,
    142: NullCondQualifierAbsent,
    143: NullCondQualifierPresent,
    144: OpAmp,
    145: OpAnd,
    146: OpDiv,
    147: OpEq,
    148: OpGt,
    149: OpGte,
    150: OpLogicAnd,
    151: OpLogicOr,
    152: OpLt,
    153: OpLte,
    154: OpMinus,
    155: OpMult,
    156: OpNe,
    157: OpOr,
    158: OpOrInt,
    159: OpPlus,
    160: AnyTypePattern,
    161: BindingPattern,
    162: BoolPatternFalse,
    163: BoolPatternTrue,
    164: EllipsisPattern,
    165: ExtendedPattern,
    166: FilteredPattern,
    167: IntegerPattern,
    168: ListPattern,
    169: NotPattern,
    170: NullPattern,
    171: OrPattern,
    172: ParenPattern,
    173: RegexPattern,
    174: TuplePattern,
    175: TypePattern,
    176: FieldPatternDetail,
    177: PropertyPatternDetail,
    178: SelectorPatternDetail,
    179: SelectorCall,
    180: DefaultListTypeRef,
    181: FunctionTypeRef,
    182: GenericTypeRef,
    183: SimpleTypeRef,
    184: VarBind,
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


      
def _coerce_bytes(label, value, what='a bytes string', or_none=False):
    """
    Take bytes (forwarded as-is to C) but also accept text (encoded using
    the system encoding).
    """
    if value is None and or_none:
        return None
    elif isinstance(value, bytes):
        return value
    elif isinstance(value, str):
        return value.encode()
    else:
        raise TypeError('`{}` argument must be {} (got {})'
                        .format(label, what, _type_fullname(type(value))))



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

        from liblktlang import App


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

    

