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
    root_astnode_name = pyapi.type_public_name(T.root_node)
%>


% for enum_type in ctx.enum_types:
class ${enum_type.py_helper}(object):
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
    def __init__(self,
                 charset: Opt[str] = None,
                 unit_provider: Opt[UnitProvider] = None,
                 with_trivia: bool = True,
                 tab_stop: int = ${ctx.default_tab_stop},
                 *,
                 _c_value: Any = None) -> None: ...

    def __eq__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...

    def get_from_file(self,
                      filename: AnyStr,
                      charset: Opt[str] = None,
                      reparse: bool = False,
                      rule: str = default_grammar_rule) -> AnalysisUnit: ...
    def get_from_buffer(self,
                        filename: AnyStr,
                        buffer: AnyStr,
                        charset: Opt[str] = None,
                        reparse: bool = False,
                        rule: str = default_grammar_rule) -> AnalysisUnit: ...
    def get_from_provider(
        self,
        name: AnyStr,
        kind: str,
        charset: Opt[str] = None,
        reparse: bool = False
    ) -> AnalysisUnit: ...

    def discard_errors_in_populate_lexical_env(self,
                                               discard: bool) -> None: ...

class AnalysisUnit(object):
    class TokenIterator(object):
        def __init__(self, first: Token) -> None: ...
        def __iter__(self) -> AnalysisUnit.TokenIterator: ...
        def __next__(self) -> Token: ...
        def next(self) -> Token: ...

    def __init__(self, context: AnalysisContext, c_value: Any) -> None: ...

    def __eq__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...

    context: AnalysisContext

    def reparse(self,
                buffer: Opt[AnyStr] = None,
                charset: Opt[str] = None) -> None: ...

    def populate_lexical_env(self) -> None: ...

    @property
    def root(self) -> ${root_astnode_name}: ...

    @property
    def first_token(self) -> Token: ...

    @property
    def last_token(self) -> Token: ...

    @property
    def text(self) -> str: ...

    @property
    def token_count(self) -> int: ...

    @property
    def trivia_count(self) -> int: ...

    def lookup_token(self, sloc: Sloc) -> Token: ...
    def iter_tokens(self) -> AnalysisUnit.TokenIterator: ...

    @property
    def filename(self) -> str: ...

    @property
    def diagnostics(self) -> List[Diagnostic]: ...

    def __repr__(self) -> str: ...


class Sloc(object):
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
    sloc_range: SlocRange
    message: str

    def __init__(self, sloc_range: SlocRange, message: str) -> None: ...

    @property
    def as_text(self) -> str: ...

    def __str__(self) -> str: ...
    def __repr__(self) -> str: ...


class Token(object):

    @property
    def next(self) -> Opt[Token]: ...

    @property
    def previous(self) -> Opt[Token]: ...

    def range_until(self, other: Token) -> Iterator[Token]: ...
    def is_equivalent(self, other: Token) -> bool: ...

    @property
    def kind(self) -> str: ...

    @property
    def is_trivia(self) -> bool: ...

    @property
    def index(self) -> int: ...

    @property
    def text(self) -> str: ...

    @classmethod
    def text_range(cls, first: Token, last: Token) -> str: ...

    @property
    def sloc_range(self) -> SlocRange: ...

    def __eq__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...
    def __repr__(self) -> str: ...
    def __lt__(self, other: Opt[Token]) -> bool: ...
    def __le__(self, other: Opt[Token]) -> bool: ...
    def __gt__(self, other: Opt[Token]) -> bool: ...
    def __ge__(self, other: Opt[Token]) -> bool: ...

    def to_data(self) -> dict: ...


class UnitProvider(object):

    def __init__(self, c_value: Any) -> None: ...

${exts.include_extension(
   ctx.ext('python_api', 'unit_providers', 'mypy_methods')
)}


class ${root_astnode_name}(object):
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
    def kind_name(self) -> str: ...

    @property
    def is_token_node(self) -> bool: ...

    @property
    def is_synthetic(self) -> bool: ...

    @property
    def sloc_range(self) -> SlocRange: ...

    @property
    def text(self) -> str: ...

    @property
    def image(self) -> str: ...

    def lookup(self, sloc: Sloc) -> Opt[${root_astnode_name}]: ...

    def __bool__(self) -> bool: ...
    def __nonzero__(self) -> bool: ...
    def __iter__(self) -> Iterator[${root_astnode_name}]: ...
    def __len__(self) -> int: ...
    def __getitem__(self, key: int) -> Opt[${root_astnode_name}]: ...

    def iter_fields(self) -> Iterator[Tuple[str, ${root_astnode_name}]]: ...

    def dump_str(self) -> str: ...
    def dump(self, indent: str = '', file: IO[str] = sys.stdout) -> None: ...

    def findall(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> List[${root_astnode_name}]: ...

    def find(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> Opt[${root_astnode_name}]: ...

    def finditer(
        self,
        ast_type_or_pred: Union[Type[${root_astnode_name}],
                                Callable[[${root_astnode_name}], bool]],
        **kwargs: Any
    ) -> Iterator[${root_astnode_name}]: ...

    @property
    def parent_chain(self) -> List[${root_astnode_name}]: ...

    def __repr__(self) -> str: ...

    @property
    def entity_repr(self) -> str: ...

    @property
    def tokens(self) -> Iterator[Token]: ...

    def to_data(self) -> Union[list, dict]: ...
    def to_json(self) -> str: ...

    def is_a(self, *types: Type[${root_astnode_name}]) -> bool: ...

    T = TypeVar('T', bound=${root_astnode_name})

    def cast(self, typ: Type[T]) -> T: ...


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
    ctx: AnalysisContext

    @property
    def description(self) -> str: ...

    def __init__(self) -> None: ...
    def add_arguments(self) -> None: ...
    def create_unit_provider(self) -> Opt[UnitProvider]: ...
    def process_files(self) -> None: ...

    @classmethod
    def run(cls) -> None: ...

    ${exts.include_extension(ctx.ext('python_api', 'mypy_app_exts'))}
