## vim: filetype=makopython

## This template emits declarations that closely follow the ones in the module
## that this stub describes: please refer to module_py.mako for more details.

<%namespace name="astnode_types" file="astnode_types_py.mako" />
<%namespace name="exts"          file="/extensions.mako" />
<%namespace name="struct_types"  file="struct_types_py.mako" />

import argparse
import sys
from typing import (
    Any, AnyStr, Callable, ClassVar, Dict, IO, Iterator, List, Optional as Opt,
    Tuple, Type, TypeVar, Union
)


<%
    root_astnode_name = pyapi.root_astnode_name
%>


% for enum_type in ctx.enum_types:
class ${enum_type.py_helper}(object):
    ${py_doc(enum_type, 4)}
    % for v in enum_type.values:
    ${v.name.lower}: str
    % endfor
% endfor


default_grammar_rule: str


% for e in ctx.sorted_exception_types:
class ${e.name.camel}(Exception):
    pass
% endfor


${exts.include_extension(ctx.ext('python_api', 'mypy_exceptions'))}


class AnalysisContext(object):
    ${py_doc('langkit.analysis_context_type', 4)}

    def __init__(self,
                 charset: Opt[str] = None,
                 unit_provider: Opt[UnitProvider] = None,
                 with_trivia: bool = True,
                 tab_stop: int = ${ctx.default_tab_stop},
                 *,
                 _c_value: Any = None) -> None:
        ${py_doc('langkit.create_context', 8, or_pass=True)}

    def __eq__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...

    def get_from_file(self,
                      filename: AnyStr,
                      charset: Opt[str] = None,
                      reparse: bool = False,
                      rule: str = default_grammar_rule) -> AnalysisUnit:
        ${py_doc('langkit.get_unit_from_file', 8, or_pass=True)}

    def get_from_buffer(self,
                        filename: AnyStr,
                        buffer: AnyStr,
                        charset: Opt[str] = None,
                        reparse: bool = False,
                        rule: str = default_grammar_rule) -> AnalysisUnit:
        ${py_doc('langkit.get_unit_from_buffer', 8, or_pass=True)}

    def get_from_provider(
        self,
        name: AnyStr,
        kind: str,
        charset: Opt[str] = None,
        reparse: bool = False
    ) -> AnalysisUnit:
        ${py_doc('langkit.get_unit_from_provider', 8, or_pass=True)}

    def discard_errors_in_populate_lexical_env(self,
                                               discard: bool) -> None:
        ${py_doc('langkit.context_discard_errors_in_populate_lexical_env', 8,
                 or_pass=True)}

class AnalysisUnit(object):
    ${py_doc('langkit.analysis_unit_type', 4)}

    class TokenIterator(object):
        ${py_doc('langkit.python.AnalysisUnit.TokenIterator', 8)}

        def __init__(self, first: Token) -> None: ...
        def __iter__(self) -> AnalysisUnit.TokenIterator: ...
        def __next__(self) -> Token: ...
        def next(self) -> Token: ...

    def __init__(self, context: AnalysisContext, c_value: Any) -> None: ...

    def __eq__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...

    @property
    def context(self) -> AnalysisContext:
        ${py_doc('langkit.unit_context', 8, or_pass=True)}

    def reparse(self,
                buffer: Opt[AnyStr] = None,
                charset: Opt[str] = None) -> None:
        ${py_doc('langkit.unit_reparse_generic', 8, or_pass=True)}

    def populate_lexical_env(self) -> None:
        ${py_doc('langkit.unit_populate_lexical_env', 8, or_pass=True)}

    @property
    def root(self) -> ${root_astnode_name}:
        ${py_doc('langkit.unit_root', 8, rtype=T.root_node, or_pass=True)}

    @property
    def first_token(self) -> Token:
        ${py_doc('langkit.unit_first_token', 8, or_pass=True)}

    @property
    def last_token(self) -> Token:
        ${py_doc('langkit.unit_last_token', 8, or_pass=True)}

    @property
    def text(self) -> str:
        ${py_doc('langkit.unit_text', 8, or_pass=True)}

    @property
    def token_count(self) -> int:
        ${py_doc('langkit.unit_token_count', 8, or_pass=True)}

    @property
    def trivia_count(self) -> int:
        ${py_doc('langkit.unit_trivia_count', 8, or_pass=True)}

    def lookup_token(self, sloc: Sloc) -> Token:
        ${py_doc('langkit.unit_lookup_token', 8, or_pass=True)}

    def iter_tokens(self) -> AnalysisUnit.TokenIterator:
        ${py_doc('langkit.python.AnalysisUnit.iter_tokens', 8, or_pass=True)}

    @property
    def filename(self) -> str:
        ${py_doc('langkit.unit_filename', 8, or_pass=True)}

    @property
    def diagnostics(self) -> List[Diagnostic]:
        ${py_doc('langkit.python.AnalysisUnit.diagnostics', 8, or_pass=True)}

    def __repr__(self) -> str: ...


class Sloc(object):
    ${py_doc('langkit.sloc_type', 4)}

    line: int
    column: int

    def __init__(self, line: int, column: int) -> None: ...
    def __bool__(self) -> bool: ...
    def __nonzero__(self) -> bool: ...
    def __lt__(self, other: Sloc) -> bool: ...
    def __eq__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...
    def __str__(self) -> str: ...
    def __repr__(self) -> str: ...


class SlocRange(object):
    ${py_doc('langkit.sloc_range_type', 4)}

    start: Sloc
    end: Sloc

    def __init__(self, start: Sloc, end: Sloc) -> None: ...
    def __bool__(self) -> bool: ...
    def __nonzero__(self) -> bool: ...
    def __lt__(self, other: SlocRange) -> bool: ...
    def __eq__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...
    def __str__(self) -> str: ...
    def __repr__(self) -> str: ...


class Diagnostic(object):
    ${py_doc('langkit.diagnostic_type', 4)}

    sloc_range: SlocRange
    message: str

    def __init__(self, sloc_range: SlocRange, message: str) -> None: ...

    @property
    def as_text(self) -> str: ...

    def __str__(self) -> str: ...
    def __repr__(self) -> str: ...


class Token(object):
    ${py_doc('langkit.token_reference_type', 4)}

    @property
    def next(self) -> Opt[Token]:
        ${py_doc('langkit.token_next', 8, or_pass=True)}

    @property
    def previous(self) -> Opt[Token]:
        ${py_doc('langkit.token_previous', 8, or_pass=True)}

    def range_until(self, other: Token) -> Iterator[Token]:
        ${py_doc('langkit.token_range_until', 8, or_pass=True)}

    def is_equivalent(self, other: Token) -> bool:
        ${py_doc('langkit.token_is_equivalent', 8, or_pass=True)}

    @property
    def kind(self) -> str:
        ${py_doc('langkit.token_kind', 8, or_pass=True)}

    @property
    def is_trivia(self) -> bool:
        ${py_doc('langkit.token_is_trivia', 8, or_pass=True)}

    @property
    def index(self) -> int:
        ${py_doc('langkit.token_index', 8, or_pass=True)}

    @property
    def text(self) -> str:
        ${py_doc('langkit.token_text', 8, or_pass=True)}

    @classmethod
    def text_range(cls, first: Token, last: Token) -> str:
        ${py_doc('langkit.token_range_text', 8, or_pass=True)}

    @property
    def sloc_range(self) -> SlocRange:
        ${py_doc('langkit.token_sloc_range', 8, or_pass=True)}

    def __eq__(self, other: Any) -> bool:
        ${py_doc('langkit.python.Token.__eq__', 8, or_pass=True)}

    def __hash__(self) -> int: ...
    def __repr__(self) -> str: ...

    def __lt__(self, other: Opt[Token]) -> bool:
        ${py_doc('langkit.python.Token.__lt__', 8, or_pass=True)}

    def __le__(self, other: Opt[Token]) -> bool: ...
    def __gt__(self, other: Opt[Token]) -> bool: ...
    def __ge__(self, other: Opt[Token]) -> bool: ...

    def to_data(self) -> dict:
        ${py_doc('langkit.python.Token.to_data', 8, or_pass=True)}


class UnitProvider(object):
    ${py_doc('langkit.unit_provider_type', 4)}

    def __init__(self, c_value: Any) -> None:
        ${py_doc('langkit.python.UnitProvider.__init__', 8, or_pass=True)}

${exts.include_extension(
   ctx.ext('python_api', 'unit_providers', 'mypy_methods')
)}


class ${root_astnode_name}(object):
    ${py_doc(T.root_node, 4)}

    is_list_type: ClassVar[bool]

    ${astnode_types.mypy_field_decls(T.root_node, or_pass=False)}

    def __init__(self,
                 c_value: Any,
                 node_c_value: Any,
                 metadata: Any,
                 rebindings: Any) -> None: ...
    def __eq__(self, other: Any) -> bool: ...
    def __ne__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...

    @property
    def kind_name(self) -> str:
        ${py_doc('langkit.node_kind', 8, or_pass=True)}

    @property
    def is_token_node(self) -> bool:
        ${py_doc('langkit.node_is_token_node', 8, or_pass=True)}

    @property
    def is_synthetic(self) -> bool:
        ${py_doc('langkit.node_is_synthetic', 8, or_pass=True)}

    @property
    def sloc_range(self) -> SlocRange:
        ${py_doc('langkit.node_sloc_range', 8, or_pass=True)}

    @property
    def text(self) -> str:
        ${py_doc('langkit.node_text', 8, or_pass=True)}

    @property
    def image(self) -> str:
        ${py_doc('langkit.node_image', 8, or_pass=True)}

    def lookup(self, sloc: Sloc) -> Opt[${root_astnode_name}]:
        ${py_doc('langkit.lookup_in_node', 8, or_pass=True)}

    def __bool__(self) -> bool:
        ${py_doc('langkit.python.root_node.__bool__', 8, or_pass=True)}

    def __nonzero__(self) -> bool: ...

    def __iter__(self) -> Iterator[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.__iter__', 8, or_pass=True)}

    def __len__(self) -> int:
        ${py_doc('langkit.python.root_node.__len__', 8, or_pass=True)}

    def __getitem__(self, key: int) -> Opt[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.__getitem__', 8, or_pass=True)}

    def iter_fields(self) -> Iterator[Tuple[str, ${root_astnode_name}]]:
        ${py_doc('langkit.python.root_node.iter_fields', 8, or_pass=True)}

    def dump_str(self) -> str:
        ${py_doc('langkit.python.root_node.dump_str', 8, or_pass=True)}

    def dump(self, indent: str = '', file: IO[str] = sys.stdout) -> None:
        ${py_doc('langkit.python.root_node.dump', 8, or_pass=True)}

    def findall(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> List[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.findall', 8, or_pass=True)}

    def find(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> Opt[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.find', 8, or_pass=True)}

    def finditer(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> Iterator[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.finditer', 8, or_pass=True)}

    @property
    def parent_chain(self) -> List[${root_astnode_name}]:
        ${py_doc('langkit.python.root_node.parent_chain', 8, or_pass=True)}

    def __repr__(self) -> str: ...

    @property
    def entity_repr(self) -> str: ...

    @property
    def tokens(self) -> Iterator[Token]:
        ${py_doc('langkit.python.root_node.tokens', 8, or_pass=True)}

    def to_data(self) -> Union[list, dict]:
        ${py_doc('langkit.python.root_node.to_data', 8, or_pass=True)}

    def to_json(self) -> str:
        ${py_doc('langkit.python.root_node.to_json', 8, or_pass=True)}

    def is_a(self, *types: Type[${root_astnode_name}]) -> bool:
        ${py_doc('langkit.python.root_node.is_a', 8, or_pass=True)}

    T = TypeVar('T', bound=${root_astnode_name})

    def cast(self, typ: Type[T]) -> T:
        ${py_doc('langkit.python.root_node.cast', 8, or_pass=True)}


% for astnode in ctx.astnode_types:
    % if astnode != T.root_node:
${astnode_types.mypy_decl(astnode)}
    % endif
% endfor


% for struct_type in ctx.struct_types:
    % if struct_type.exposed and \
         not struct_type.is_entity_type and \
         not struct_type is T.env_md:
${struct_types.mypy_decl(struct_type)}
    % endif
% endfor


${exts.include_extension(ctx.ext('mypy_python'))}


class App(object):
    parser: argparse.ArgumentParser
    args: argparse.Namespace
    u: AnalysisUnit
    units: Dict[str, AnalysisUnit]
    ctx: AnalysisContext

    @property
    def description(self) -> str: ...

    def __init__(self, args: Opt[List[str]]) -> None: ...
    def add_arguments(self) -> None: ...
    def create_unit_provider(self) -> Opt[UnitProvider]: ...
    def process_unit(self, unit: AnalysisUnit) -> None: ...

    @classmethod
    def run(cls, args: Opt[List[str]]=None) -> None: ...

    ${exts.include_extension(ctx.ext('python_api', 'mypy_app_exts'))}
