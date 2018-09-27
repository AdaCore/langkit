"""
This file contains the logic for the compilation context for langkit. This is
the main hook into langkit, insofar as this is the gate through which an
external language creator will actually trigger the code emission. For example,
this is the way it is done for the ada language::

    from ada_parser import ada_lexer, ada_grammar
    context = CompileCtx(... ada_lexer, ada_grammar...)
    ...
    context.emit(...)
"""

from __future__ import absolute_import, division, print_function

import ast
from collections import defaultdict
from contextlib import contextmanager
from distutils.spawn import find_executable
from functools import reduce
from glob import glob
from io import StringIO
import os
from os import path
import subprocess
import sys

from funcy import keep

from langkit import caching, names
from langkit.ada_api import AdaAPISettings
from langkit.c_api import CAPISettings
from langkit.diagnostics import (Context, Severity, WarningSet,
                                 check_source_language)
import langkit.documentation
from langkit.envs import EnvSpec
from langkit.expressions import PropertyDef
from langkit.passes import (
    ASTNodePass, EnvSpecPass, GlobalPass, GrammarRulePass, MajorStepPass,
    PassManager, PropertyPass, StopPipeline, errors_checkpoint_pass
)
from langkit.template_utils import add_template_dir
from langkit.utils import (Colors, TopologicalSortError, printcol,
                           topological_sort)


compile_ctx = None


def get_context(or_none=False):
    """
    Returns the current compilation context. Meant to be used by the rest of
    LangKit, in any code that has been called as part of the CompileCtx.emit
    primitive.

    :param bool or_none: If True, return None when there is no context.
        Otherwise, raise an assertion error when there is no context.

    :rtype: CompileCtx
    """
    assert or_none or compile_ctx is not None, (
        'Get context has been called in a state in which the compile context'
        ' is not set'
    )
    return compile_ctx


@contextmanager
def global_context(ctx):
    """
    Context manager that temporarily make "ctx" global.

    :param CompileContext ctx: Context to make global.
    """
    global compile_ctx
    old_ctx = compile_ctx
    compile_ctx = ctx
    yield
    compile_ctx = old_ctx


def copy_file(from_path, to_path):
    """
    Helper to copy a source file.

    Return whether the file has been updated.

    :param str from_path: Path of the file to copy.
    :param str to_path: Destination path.
    """
    with open(from_path, 'rb') as f:
        content = f.read()
    write_source_file(to_path, content)


def write_source_file(file_path, source, post_process=None):
    """
    Helper to write a source file.

    Return whether the file has been updated.

    :param str file_path: Path of the file to write.
    :param str source: Content of the file to write.
    :param post_process: If provided, callable used to transform the source
        file content just before writing it.
    :type post_process: None | (str) -> str

    :rtype: bool
    """
    context = get_context()
    if post_process:
        source = post_process(source)
    if (not os.path.exists(file_path) or
            context.cache.is_stale(file_path, source)):
        if context.verbosity.debug:
            printcol('Rewriting stale source: {}'.format(file_path),
                     Colors.OKBLUE)
        with open(file_path, 'wb') as f:
            f.write(source)
        return True
    return False


def write_cpp_file(file_path, source, post_process=None):
    """
    Helper to write a C/C++ source file.

    :param str file_path: Path of the file to write.
    :param str source: Content of the file to write.
    """
    if write_source_file(file_path, source, post_process):
        if find_executable('clang-format'):
            subprocess.check_call(['clang-format', '-i', file_path])


ADA_SPEC = "spec"
ADA_BODY = "body"


def write_ada_file(out_dir, source_kind, qual_name, content,
                   post_process=None):
    """
    Helper to write an Ada file.

    :param str out_dir: The complete path to the directory in which we want to
        write the file.
    :param str source_kind: One of the constants ADA_SPEC or ADA_BODY,
        determining whether the source is a spec or a body.
    :param list[names.Name] qual_name: The qualified name of the Ada spec/body,
        as a list of Name components.
    :param str content: The source content to write to the file.
    """
    assert source_kind in (ADA_SPEC, ADA_BODY)
    file_name = '{}.{}'.format('-'.join(n.lower for n in qual_name),
                               'ads' if source_kind == ADA_SPEC else 'adb')
    file_path = os.path.join(out_dir, file_name)

    # If there are too many lines, which triggers obscure debug info bugs,
    # strip empty lines.
    lines = content.splitlines()
    if len(lines) > 200000:
        content = '\n'.join(l for l in lines if l.strip())

    # TODO: no tool is able to pretty-print a single Ada source file
    write_source_file(file_path, content, post_process)


class Verbosity(object):
    """
    Helper object to handle verbosity level of notifications during code
    generation.
    """

    NONE = 0
    INFO = 1
    DEBUG = 2

    NAMES = ('none', 'info', 'debug')

    def __init__(self, level):
        """
        Create a verbosity level holder.

        :param level: Verbosity level. Can be either the lower-case name for
            this level or the corresponding integer constant.
        :type level: str|int
        """
        if isinstance(level, str):
            if level not in self.NAMES:
                raise ValueError('Invalid verbosity level: {}'.format(level))
            self.level = self._get(level)
        else:
            if level not in [self._get(name) for name in self.NAMES]:
                raise ValueError('Invalid verbosity level: {}'.format(level))
            self.level = level

    @classmethod
    def _get(cls, name):
        """
        Return the integer constant corresponding to the lower-case "name"
        verbosity level.

        :param str name: Verbosity level name.
        :rtype: int
        """
        return getattr(cls, name.upper())

    def __eq__(self, other):
        return isinstance(other, Verbosity) and self.level == other.level

    def __getattr__(self, name):
        """
        Assuming "name" is a lower-case verbosity level name, return whether
        this instance has a level that is either equal or above it.

        :param str name: Lower-case verbosity level name to compare.
        :rtype: bool
        """
        if name in self.NAMES:
            return self.level >= self._get(name)
        else:
            raise AttributeError()

    def __str__(self):
        for name in self.NAMES:
            if self.level == self._get(name):
                return name
        assert False

    def __repr__(self):
        return str(self)

    @classmethod
    def choices(cls):
        """
        Return a list of instances for all available verbosity levels.

        :rtype: list[Verbosity]
        """
        return [
            cls(getattr(cls, name.upper()))
            for name in cls.NAMES
        ]


class LibraryEntity(object):
    """
    Reference to an entity in the generated library.
    """
    def __init__(self, unit_fqn, entity_name):
        """
        Create a reference to an entity in the generated library.

        :param str unit_fqn: Fully qualified name for the unit that contains
            the referenced entity. For instance: "Libfoolang.My_Unit".
        :param str entity_name: Simple name for the entity that is referenced.
        """
        self.unit_fqn = unit_fqn
        self.entity_name = entity_name

    @property
    def fqn(self):
        """
        Fully qualified name for the referenced entity.

        For instance: "Libfoolang.My_Unit.My_Entity".

        :rtype: str
        """
        return '{}.{}'.format(self.unit_fqn, self.entity_name)


class CompileCtx(object):
    """State holder for native code emission."""

    def __init__(self, lang_name, lexer, grammar,
                 lib_name=None, short_name=None,
                 c_symbol_prefix=None,
                 enable_python_api=True,
                 default_charset='utf-8',
                 default_tab_stop=8,
                 verbosity=Verbosity('none'),
                 template_lookup_extra_dirs=None,
                 env_hook_subprogram=None,
                 default_unit_provider=None,
                 symbol_canonicalizer=None,
                 documentations=None):
        """Create a new context for code emission.

        :param str lang_name: string (mixed case and underscore: see
            langkit.names.Name) for the Name of the target language.

        :param lexer: A lexer for the target language.
        :type lexer: langkit.lexer.Lexer

        :param grammar: A grammar for the target language.
        :type grammar: langkit.parsers.Grammer

        :param lib_name: If provided, must be a string (mixed case and
            underscore: see langkit.names.Name), otherwise set to
            "Lib<lang_name>lang". It is used for the filenames, package names,
            etc.  in the generated library.
        :type lib_name: str or None

        :param str|None short_name: If provided, must be a string (mixed case
            and underscore: see langkit.names.Name). It will be used where
            a short name for the library is requested, for instance for the
            shortcut module name in the generated playground script.

        :param c_symbol_prefix: Valid C identifier used as a prefix for all
            top-level declarations in the generated C API.  If not provided,
            set to the name of the language in lower case.  Empty string stands
            for no prefix.
        :type c_symbol_prefix: str or None

        :param bool enable_python_api: If True (which is the default),
            generates a Python API for the generated library.

        :param str default_charset: In the generated library, this will be the
            default charset to use to scan input source files.

        :param int default_tab_stop: Tabulation stop to use as a default value
            in the analysis context constructor.

        :param Verbosity verbosity: Amount of messages to display on standard
            output. None by default.

        :param [str]|None template_lookup_extra_dirs: A list of
            extra directories to add to the directories used by mako for
            template lookup. This is useful if you want to render custom
            code as part of the compilation process.

        :param LibraryEntity|None env_hook_subprogram: If provided, define a
            subprogram to call as the environment hook.

            The environment hook is a subprogram provided by a language
            specification and that can perform arbitrarily complex computations
            and changes to a whole analysis context. It can be invoked in
            environment specifications: see EnvSpec's call_env_hook argument.

            Its intended use case is to implement lexical environment lookups
            across analysis units: when executed on a node that designates
            another unit, the hook can fetch this other unit from the analysis
            context and populate its lexical environment. The logic for
            determining a file name from an analysis unit name is completely
            language dependent, hence the need for a hook.

            The subprogram that implements the hook must have the following
            signature::

                procedure Hook_Func
                  (Unit        : Analysis_Unit;
                   Node        : <root AST node type>;
                   Initial_Env : in out Lexical_Env);

            If the hook is invoked on an node that uses the initial_env EnvSpec
            attribute, the hook can alter it so that it affects the rest of the
            EnvSpec actions.

        :param LibraryEntity|None default_unit_provider: If provided, define a
            Langkit_Support.Unit_Files.Unit_Provider_Access object. This object
            will be used as the default unit provider during the creation of an
            analysis context.

            If None, this disables altogether the unit provider mechanism in
            code generation.

        :param LibraryEntity|None symbol_canonicalizer: If provided, define a
            subprogram to call in order to canonicazie symbol identifiers. Such
            a suprogram must have the following signature::

                function Canonicalize
                  (Name : Text_Type) return Symbolization_Result;

            It takes an identifier name and must return the canonical name for
            it (or an error), so that all equivalent symbols have the same
            canonical name.

            This can be used, for instance, to implement case insensivity.

        :param dict[str, str] documentations: If provided, supply templates to
            document entities. These will be added to the documentations
            available in code generation: see langkit.documentation.
        """
        from langkit.python_api import PythonAPISettings

        self.lang_name = names.Name(lang_name)

        self.lib_name = (
            names.Name('Lib{}lang'.format(self.lang_name.lower))
            if lib_name is None else
            names.Name(lib_name)
        )
        self.short_name = names.Name(short_name) if short_name else None

        self.ada_api_settings = AdaAPISettings(self)
        self.c_api_settings = CAPISettings(
            self,
            (self.lang_name.lower
             if c_symbol_prefix is None else c_symbol_prefix)
        )

        self.default_charset = default_charset
        self.default_tab_stop = default_tab_stop

        self.verbosity = verbosity

        self.set_quex_path()

        self.compiled = False
        """
        Whether the language specification was compiled. This is used to avoid
        doing it multiple times.

        :type: bool
        """

        self.lexer = lexer
        self.lexer.prefix = "{}_TKN_".format(self.lang_name.lower.upper())
        ":type: langkit.lexer.Lexer"

        self.grammar = grammar
        ":type: langkit.parsers.Grammar"

        self.python_api_settings = (
            PythonAPISettings(self, self.c_api_settings)
            if enable_python_api else None
        )

        self.fns = set()
        """
        Set of names (names.Name instances) for all generated parser
        functions. This is used to avoid generating these multiple times.

        :type: set[parsers.Parser]
        """

        self._enum_types = []
        """
        List of all enumeration types.

        :type: list[langkit.compiled_types.EnumType]
        """

        self.astnode_types = []
        """
        List for all ASTnodeType instances, sorted so that A is before B when A
        is a parent class for B. This sorting is important to output
        declarations in dependency order.

        This is computed right after field types inference.

        :type: list[langkit.compiled_types.ASTNodeType]
        """

        self.node_kind_constants = {}
        """
        Mapping: ASTNodeType concrete (i.e. non abstract) instance -> int,
        associating specific constants to be used reliably in bindings.  This
        mapping is built at the beginning of code emission.

        :type: dict[langkit.compiled_types.ASTNodeType, int]
        """

        self.kind_constant_to_node = {}
        """
        Reverse mapping for `node_kind_constants`.

        :type: dict[int, langkit.compiled_types.ASTNodeType]
        """

        self._struct_types = None
        """
        List of all plain struct types.

        :type: list[langkit.compiled_types.StructType]
        """

        self._entity_types = None
        """
        List of all entity types.

        :type: list[langkit.compiled_types.EntityType]
        """

        self.root_grammar_class = None
        """
        The ASTNodeType instance that is the root class for every node used in
        the grammar.

        :type: langkit.compiled_types.ASTNodeType
        """

        self.generic_list_type = None
        """
        The root gammar class subclass that is the base class for all
        automatically generated root list types.

        :type: langkit.compiled_types.ASTNodeType
        """

        self.env_metadata = None
        """
        The StructType instance that will be used as the lexical environment
        metadata type.

        :type: langkit.compiled_types.StructType
        """

        self.list_types = set()
        """
        Set of all ASTNodeType instances for which we generate a corresponding
        list type.

        :type: set[langkit.compiled_types.ASTNodeType]
        """

        self._array_types = None
        """
        Sorted list of all ArrayType instances.

        For each ArrayType instance T, code emission for type definition will
        automatically happen unless T.element_type.should_emit_array_type is
        False. In this case, type definition should be hard-wired in code
        generation.

        :type: list[langkit.compiled_types.ArrayType]
        """

        self._composite_types = None
        """
        Dependency-sorted list of array and struct types.

        :type: list[langkit.compiled_types.CompiledType]
        """

        self.memoized_properties = set()
        """
        Set of all PropertyDef instances that are memoized.

        :type: set[langkit.expressions.base.PropertyDef]
        """

        self.memoization_keys = set()
        """
        Set of all CompiledType instances that are used as key in the hashed
        maps used to implement properties memoization. All of them must be
        hashable.

        :type: set[langkit.compiled_types.CompiledType]
        """

        self.memoization_values = set()
        """
        Set of all CompiledType instances that are used as value in the hashed
        maps used to implement properties memoization. Any type can fit, there
        is no restriction.

        :type: set[langkit.compiled_types.CompiledType]
        """

        self.symbol_literals = {}
        """
        Container for all symbol literals to be used in code generation.

        All "built-in" symbol literals used in parsers and properties are
        pre-computed in each analysis context so that parsing and properties
        evaluation does not need to perform symbol table lookups for them.

        Set of such pre-computed symbols is stored in the generated library in
        an array indexed by an enumeration type. Here, this holds a mapping:
        symbol text -> enumerator, to be used for code generation. See the
        "Symbol_Literals" array in $.Analysis.Analysis_Context_Type.

        This mapping is not supposed to be directly modified anywhere else than
        inside this CompileCtx class. See the add_symbol_literal method to add
        symbols to this mapping. Note that this mapping will be empty until one
        calls the finalize_symbol_literals method.

        :type: dict[str, names.Name]
        """

        self._symbol_literals = set()
        """
        Temporary container for all symbol literal candidates. This is used
        during the collect "pass" for all symbols. When the set is finalized,
        call the finalize_symbol_literals method to create the
        "symbol_literals" mapping.

        This two-pass mechanism is here to make sure we generate deterministic
        enumeration names.

        :type: set[str]
        """

        #
        # Holders for the Ada generated code chunks
        #

        self.generated_parsers = []
        ":type: list[langkit.parsers.GeneratedParser]"

        self.cache = None

        # Internal field for extensions directory
        self._extensions_dir = None

        self.env_metadata = None
        """
        StructType instance used to annotate environment elements. Initialized
        during the typing pass.

        :type: langkit.compiled_types.StructType
        """

        self.has_env_assoc = False
        """
        Whether the env_assoc structure type is created.
        """

        self.has_env_assoc_array = False
        """
        Whether the array type for the env_assoc structure type is created.
        """

        self.has_ref_env = False
        """
        Whether there is a RefEnvs action in environment specs.
        """

        self.template_lookup_extra_dirs = template_lookup_extra_dirs or []

        self.additional_source_files = []
        """
        List of path for file names to include in the generated library.

        :type: list[str]
        """

        self.logic_binders = set()
        """
        Set of tuple of properties for which we want to generate logic binders.
        For each binder, there are potentially two properties: the conversion
        property and the equality property. See langkit.expressions.logic.Bind
        for more information.

        :type: set[(PropertyDef|None, PropertyDef|None)]
        """

        self.env_hook_subprogram = env_hook_subprogram
        self.default_unit_provider = default_unit_provider
        self.symbol_canonicalizer = symbol_canonicalizer

        docs = dict(langkit.documentation.base_langkit_docs)
        if documentations:
            docs.update(documentations)
        self.documentations = langkit.documentation.instantiate_templates(docs)
        """
        Documentation database. Associate a Mako template for each entity to
        document in the generated library.

        :type: dict[str, mako.template.Template]
        """

        self.pretty_print = False
        """
        Whether to pretty print the generated code or not.
        """

        self.parsers_varcontext_stack = []
        """
        Holder for the stack of variables contexts used in parsers code
        emission.
        """

        self.warnings = WarningSet()
        """
        Set of warnings to emit.
        """

        self.with_clauses = defaultdict(list)
        """
        Mapping that binds a list of additional WITH/USE clauses to generate
        for each source file in the generated library. Used to add WITH/USE
        clauses required by extensions. See the `add_with_clause` method.

        :type: dict[(str, str), list[(str, bool, bool)]
        """

        self.sorted_parse_fields = None
        """
        Sorted list of all parsing fields, minus fields that override abstract
        ones. Used to generate the AST node introspection API.

        :type: list[langkit.compiled_types.Field]
        """

        self.ple_unit_root = None
        """
        Node to be used as the PLE unit root, if any.

        :type: ASTNodeType|None
        """

        # Optional callbacks to post-process the content of source files
        self.post_process_ada = None
        self.post_process_cpp = None
        self.post_process_python = None

        self.ref_cats = {names.Name.from_lower('nocat')}
        """
        Set of all env lookup categories, used to optionally discriminate
        referenced envs during env lookup.

        :type: set[names.Name]
        """

    def add_with_clause(self, from_pkg, source_kind, to_pkg, use_clause=False,
                        is_private=False):
        """
        Add a WITH clause for `to_pkg` in the `source_kind` part of the
        `from_pkg` generated package.

        :param str from_pkg: Package to which the WITH clause must be added.
        :param str source_kind: Kind of source file in which the WITH clause
            must be added. Must be eiher ADA_SPEC or ADA_BODY.
        :param str to_pkg: Name of the Ada package to WITH.
        :param bool use_clause: Whether to generate the corresponding USE
            clause.
        :param bool is_private: Whether to generate a "private with" clause.
        """
        assert not use_clause or not is_private, (
            'Cannot generate a private with clause and a use clause for {}'
            ' (from {}:{})'
            .format(to_pkg, source_kind, from_pkg))
        self.with_clauses[(from_pkg, source_kind)].append(
            (to_pkg, use_clause, is_private))

    @property
    def sorted_logic_binders(self):
        return sorted(self.logic_binders, key=lambda x: (
            x[0].name.camel if x[0] else ""
            + x[1].name.camel if x[1] else ""
        ))

    @property
    def main_rule_name(self):
        """
        Shortcut for "self.grammar.main_rule_name".

        :rtype: str
        """
        return self.grammar.main_rule_name

    def sorted_types(self, type_set):
        """
        Turn "type_set" into a list of types sorted by name.

        This is useful during code generation as sorted types keep a consistent
        order for declarations.

        :param set[langkit.compiled_types.CompiledType] type_set: Set of
            CompiledType instances to sort.
        :rtype: list[langkit.compiled_types.CompiledType]
        """
        return sorted(type_set, key=lambda cls: cls.name)

    def do_generate_logic_binder(self, convert_property=None,
                                 eq_property=None):
        """
        Generate a logic binder with the given conversion property.

        If you call this function several times for the same property, only one
        binder will be generaed.

        :param PropertyDef convert_property: The conversion property.
        :param PropertyDef eq_property: The equality property.
        """
        self.logic_binders.add((convert_property, eq_property))

    @property
    def user_rule_names(self):
        """
        Return a sorted list of names for user-available grammar rules.

        :rtype: list[str]
        """
        return sorted(
            name
            for name, parser in self.grammar.rules.items()
            if not parser.is_dont_skip_parser
        )

    def create_enum_node_classes(self):
        """
        Expand all EnumNode subclasses into ASTNodeType instances.
        """
        from langkit.dsl import _EnumNodeMetaclass
        from langkit.compiled_types import create_enum_node_types
        for enum_type in _EnumNodeMetaclass.enum_types:
            create_enum_node_types(enum_type)

    def compute_types(self):
        """
        Compute various information related to compiled types, that needs to be
        available for code generation.
        """
        from langkit.compiled_types import CompiledTypeRepo, StructType, T
        from langkit.dsl import _StructMetaclass

        # Make sure the language spec tagged at most one metadata struct.
        # Register it, if there is one.
        user_env_md = None
        for st in _StructMetaclass.struct_types:
            if st._is_env_metadata:
                assert user_env_md is None
                user_env_md = st._type

        # If the language spec provided no env metadata struct, create a
        # default one.
        if user_env_md is None:
            CompiledTypeRepo.env_metadata = StructType(
                names.Name('Metadata'), None, None, []
            )
        else:
            CompiledTypeRepo.env_metadata = user_env_md
        self.check_env_metadata(CompiledTypeRepo.env_metadata)

        # Get the list of ASTNodeType instances from CompiledTypeRepo
        entity = CompiledTypeRepo.root_grammar_class.entity

        self.astnode_types = list(CompiledTypeRepo.astnode_types)
        self.list_types.update(
            t.element_type for t in CompiledTypeRepo.pending_list_types
        )

        self.generic_list_type = self.root_grammar_class.generic_list_type
        self.env_metadata = CompiledTypeRepo.env_metadata

        # The Group lexical environment operation takes an array of lexical
        # envs, so we always need to generate the corresponding array type.
        CompiledTypeRepo.array_types.add(T.LexicalEnv.array)

        # Likewise for the entity array type (LexicalEnv.get returns it) and
        # for the root node array type (some primitives need that).
        CompiledTypeRepo.array_types.add(entity.array)
        CompiledTypeRepo.array_types.add(
            CompiledTypeRepo.root_grammar_class.array)

        # Sort them in dependency order as required but also then in
        # alphabetical order so that generated declarations are kept in a
        # relatively stable order. This is really useful for debugging
        # purposes.
        self.astnode_types.sort(key=lambda n: n.hierarchical_name)

        # Check that the environment hook is bound if the language spec uses
        # it.
        if self.env_hook_subprogram is None:
            for t in self.astnode_types:
                with t.diagnostic_context:
                    check_source_language(
                        t.env_spec is None or not t.env_spec.env_hook_enabled,
                        'Cannot invoke the environment hook if'
                        ' CompileContext.bind_env_hook has not been called'
                    )

        # We need a hash function for the metadata structure as the
        # Langkit_Support.Lexical_Env generic package requires it.
        T.env_md.require_hash_function()

        # We expose a hash function for public entities, so we must generate
        # the underlying required helpers.
        T.entity.require_hash_function()

    def check_ple_unit_root(self):
        """
        Check that if the "ple_unit_root" node annotation is used, it is valid.
        """
        # Locate the PLE_unit root (if any), checking that we at most one such
        # node annotation.
        for n in self.astnode_types:
            if not n.annotations.ple_unit_root:
                continue

            with n.diagnostic_context:
                if self.ple_unit_root:
                    check_source_language(
                        False, 'Only one PLE unit root is allowed: {}'
                               .format(self.ple_unit_root.dsl_name)
                    )
                check_source_language(
                    not n.subclasses,
                    'No node can derive from PLE unit roots: here we have'
                    ' {}'.format(', '.join(c.dsl_name for c in n.subclasses))
                )
                check_source_language(
                    not n.synthetic,
                    'Synthetic nodes cannot be PLE unit roots'
                )
                self.ple_unit_root = n

        if self.ple_unit_root is None:
            return

        check_source_language(
            self.ple_unit_root in self.list_types,
            'At least one parser must create lists of PLE unit roots'
        )
        ple_unit_root_list = self.ple_unit_root.list

        # Check that there is no subclass for lists of PLE unit roots
        for subcls in ple_unit_root_list.subclasses:
            with subcls.diagnostic_context:
                check_source_language(False, 'Lists of PLE unit roots'
                                             ' cannot be subclassed')

        # Finally, check that the only way to get a PLE unit root is as a child
        # of a list node that is itself the root of a tree.
        for n in self.astnode_types:
            for f in n.get_parse_fields():
                with f.diagnostic_context:
                    check_source_language(
                        ple_unit_root_list not in f._types_from_parser,
                        '{} cannot appear anywhere in trees except as a root'
                        ' node'.format(ple_unit_root_list.dsl_name)
                    )
                    check_source_language(
                        self.ple_unit_root not in f._types_from_parser,
                        '{} cannot appear anywhere in trees except as a child'
                        ' of {} nodes'.format(self.ple_unit_root.dsl_name,
                                              ple_unit_root_list.dsl_name)
                    )

    def check_concrete_subclasses(self, astnode):
        """
        Emit an error if `astnode` is abstract and has no concrete subclass.

        :param ASTNodeType astnode: AST node to check.
        """
        # It's fine to have no list type, so as a special case we allow the
        # generic list type to have no concrete subclass.
        if astnode.is_generic_list_type or not astnode.abstract:
            return

        check_source_language(
            astnode.concrete_subclasses,
            '{} is abstract and has no concrete subclass'.format(
                astnode.dsl_name
            )
        )

    def check_env_metadata(self, cls):
        """
        Perform legality checks on `cls`, the env metadata struct.

        :param StructType cls: Environment metadata struct type.
        """
        from langkit.compiled_types import resolve_type

        with cls.diagnostic_context:
            name = cls.dsl_name
            check_source_language(
                name == 'Metadata',
                'The environment metadata struct type must be called'
                ' "Metadata" (here: {})'.format(name)
            )

        for field in cls.get_fields():
            with field.diagnostic_context:
                typ = resolve_type(field.type)
                check_source_language(
                    typ.is_bool_type or typ.is_ast_node,
                    'Environment metadata fields can be only booleans or AST'
                    ' nodes'
                )

    def all_properties(self, *args, **kwargs):
        """
        Returns an iterator on all the properties. *args and **kwargs are
        forwarded to the call to get_properties that is done on every astnode
        type.

        :rtype: seq[PropertyDef]
        """
        for astnode in self.astnode_types:
            for prop in astnode.get_properties(*args, **kwargs):
                yield prop

    def properties_callgraphs(
        self, forwards_converter=lambda expr, to_prop: to_prop,
        backwards_converter=lambda expr, from_prop: from_prop,
    ):
        """
        Compute forwards and backwards properties callgraphs.

        The forwards callgraph is a mapping::

           Caller property -> set of called properties

        While the backwards one is::

           Called property -> set of caller properties

        This takes care of overriding properties: if C calls A and B overrides
        A, then we consider that C calls both A and B. Note that this considers
        references to properties in logic expressions as calls.

        :param forwards_converter: Function to customize what the forwards call
            graph contains. It is its result that is added to the returned set.
            The given resolved expression, which comes from the caller property
            is the expression that references the given called property.
        :type forwards_converter: (ResolvedExpression, PropertyDef) -> T

        :param backwards_converter: Likewise for the backwards callgraph.
            The given resolved expression, which comes from the given caller
            property is the expression that references the called property.
        :type forwards_converter: (ResolvedExpression, PropertyDef) -> T

        :return: A tuple for 1) the forwards callgraph 2) the backwards one.
        :rtype: (dict[PropertyDef, set[T]], dict[PropertyDef, set[T]])
        """
        def add_forward(from_prop, to_prop, expr):
            backwards.setdefault(to_prop, set())
            forwards[from_prop].add(forwards_converter(expr, to_prop))
            backwards[to_prop].add(backwards_converter(expr, from_prop))
            for over_prop in to_prop.all_overriding_properties:
                add_forward(from_prop, over_prop, expr)

        def traverse_expr(expr):
            for ref_prop in expr.flat_subexprs(
                lambda e: isinstance(e, PropertyDef)
            ):
                add_forward(prop, ref_prop, expr)
            for subexpr in expr.flat_subexprs():
                traverse_expr(subexpr)

        forwards = {}
        backwards = {}

        for prop in self.all_properties(include_inherited=False):
            forwards.setdefault(prop, set())
            backwards.setdefault(prop, set())
            if prop.constructed_expr:
                traverse_expr(prop.constructed_expr)

        return (forwards, backwards)

    def compute_uses_entity_info_attr(self):
        """
        Pass that will compute the `uses_entity_info` attribute for every
        property.  This will determine whether it is necessary to pass along
        entity information or not.
        """
        from langkit.expressions import FieldAccess

        # For each property that uses entity info, extend that attribute to the
        # whole property set, as it changes the signature of the generated
        # subprograms.
        props_using_einfo = sorted(
            self.all_properties(lambda p: p._uses_entity_info,
                                include_inherited=False),
            key=lambda p: p.qualname
        )
        for prop in props_using_einfo:
            for p in prop.property_set():
                with Context(
                    'By inheritance from {} to {}'.format(prop.qualname,
                                                          p.struct.dsl_name),
                    p.location
                ):
                    p.set_uses_entity_info()

        all_props = list(self.all_properties(include_inherited=False))

        # Then, clearly tag all properties that don't use entity info
        for prop in all_props:
            prop._uses_entity_info = bool(prop._uses_entity_info)

        # Now that we determined entity info usage for all properties, make
        # sure that calls to properties that require entity info are made on
        # entities.

        def process_expr(expr):
            if isinstance(expr, FieldAccess.Expr):
                context_mgr = (
                    expr.abstract_expr.diagnostic_context
                    if expr.abstract_expr else
                    Context('', None, '')
                )

                with context_mgr:
                    check_source_language(
                        not expr.node_data.uses_entity_info
                        or expr.node_data.optional_entity_info
                        or expr.implicit_deref,
                        'Call to {} must be done on an entity'.format(
                            expr.node_data.qualname
                        ),
                        severity=Severity.non_blocking_error
                    )

            for subexpr in expr.flat_subexprs():
                process_expr(subexpr)

        for prop in all_props:
            with prop.diagnostic_context:
                if prop.constructed_expr:
                    process_expr(prop.constructed_expr)

    def compute_uses_envs_attr(self):
        """
        Pass to compute the `uses_envs` attribute for every property.

        This will determine if public properties need to automatically call
        Populate_Lexical_Env.
        """
        _, backwards = self.properties_callgraphs()

        queue = sorted(self.all_properties(lambda p: p._uses_envs,
                                           include_inherited=False),
                       key=lambda p: p.qualname)

        # Propagate the "uses envs" attribute in the backwards call graph
        while queue:
            prop = queue.pop(0)
            for caller in backwards[prop]:
                if not caller._uses_envs:
                    caller.set_uses_envs()
                    queue.append(caller)

        # For all unreached nodes, tag them as not using envs
        for prop in self.all_properties(include_inherited=False):
            prop._uses_envs = bool(prop._uses_envs)

    def warn_unused_private_properties(self):
        """
        Check that all private properties are actually used: if one is not,
        it is useless, so emit a warning for it.
        """
        from langkit.expressions import resolve_property
        from langkit.parsers import Predicate

        forwards_strict, _ = self.properties_callgraphs()

        # Compute the callgraph with flattened subclassing information:
        # consider only root properties.
        forwards = defaultdict(set)
        for prop, called in forwards_strict.items():
            root = prop.root_property
            forwards[root].update(c.root_property for c in called)

        # Compute the set of properties that are transitively called by a
        # public property or by Predicate parsers in the grammar. Assume that
        # internal properties are used.

        # First compute the set of properties called by Predicate parsers
        called_by_grammar = set()

        def visit_parser(parser):
            if isinstance(parser, Predicate):
                called_by_grammar.add(resolve_property(parser.property_ref))
            for child in parser.children:
                visit_parser(child)

        for rule in self.grammar.rules.values():
            visit_parser(rule)

        # The first is for strict analysis while the second one simplifies
        # properties to their root.
        reachable_by_public_strict = set()
        reachable_by_public = set()

        def compute_reachable(reachable_set, forward_map):
            queue = {p for p in forward_map if p.is_public or p.is_internal}
            queue.update(called_by_grammar)

            # Don't forget to tag properties used as entity/env resolvers as
            # reachable.
            for astnode in self.astnode_types:
                if astnode.env_spec:
                    queue.update(
                        action.resolver for action in astnode.env_spec.actions
                        if action.resolver
                    )

            while queue:
                prop = queue.pop()
                reachable_set.add(prop)
                queue.update(p for p in forward_map[prop]
                             if p not in reachable_set)
        compute_reachable(reachable_by_public_strict, forwards_strict)
        compute_reachable(reachable_by_public, forwards)

        # Get properties that were explicitly marked as "no-warning" by the
        # user.
        ignore_props = set(self.all_properties(lambda p: not p.warn_on_unused))

        # The unused private properties are the ones that are not part of this
        # set.
        unreachable_private_strict = (
            set(forwards_strict) - reachable_by_public_strict
        )
        unreachable_private = (
            (set(forwards) - reachable_by_public) - ignore_props
        )
        assert all(p.is_private for p in unreachable_private_strict)

        # Now determine the set of unused abstraction: it's all root properties
        # that are unused in the strict analysis but used in the other one.
        unused_abstractions = {
            p.root_property for p in
            (unreachable_private_strict - unreachable_private) - ignore_props
        }

        def warn(unused_set, message):
            sorted_set = sorted((p.qualname, p) for p in unused_set)
            for _, p in sorted_set:
                with p.diagnostic_context:
                    check_source_language(False, message,
                                          severity=Severity.warning)

        warn(unreachable_private, 'This private property is unused')
        warn(unused_abstractions, 'This private abstraction is unused')

    def warn_unreachable_base_properties(self):
        """
        Emit a warning for properties that can never be executed because they
        are defined on an abstract node and all concrete subclassed have it
        overriden.
        """
        unreachable = []

        for astnode in self.astnode_types:
            for prop in astnode.get_properties(include_inherited=False):
                # As we process whole properties set in one round, just focus
                # on root properties. And of course only on dispatching
                # properties.
                if prop.base_property or not prop.dispatching:
                    continue

                # Also focus on properties for which we emit code (concrete
                # ones and the ones with a runtime check).
                props = [p for p in prop.property_set()
                         if not p.abstract or p.abstract_runtime_check]

                # Set of concrete nodes that can reach this property
                nodes = set(astnode.concrete_subclasses)

                # Process properties in reverse hierarchical order to process
                # leaf properties before parent ones.
                for p in reversed(props):
                    reaching_p = set(p.struct.concrete_subclasses) & nodes
                    if not reaching_p:
                        unreachable.append(p)
                    nodes = nodes - reaching_p

        unreachable.sort(key=lambda p: p.location)
        for p in unreachable:
            with p.diagnostic_context:
                check_source_language(
                    False,
                    'Unreachable property: all concrete subclasses override'
                    ' it',
                    severity=Severity.warning
                )

    def render_template(self, *args, **kwargs):
        # Kludge: to avoid circular dependency issues, do not import parsers
        # until needed.
        # TODO: If the render method was dynamically bound, like the compile
        # context, rather than being explicitly redefined in every module, we
        # could avoid this, maybe.
        from langkit.parsers import render
        return render(*args, **kwargs)

    def emit(self, file_root='.', generate_lexer=True, main_source_dirs=set(),
             main_programs=set(), annotate_fields_types=False,
             check_only=False, no_property_checks=False,
             warnings=None, generate_unparser=False, properties_logging=False,
             generate_astdoc=True, generate_gdb_hook=True,
             post_process_ada=None, post_process_cpp=None,
             post_process_python=None):
        """
        Generate sources for the analysis library. Also emit a tiny program
        useful for testing purposes.

        :param str file_root: (optional) Path of the directory in which the
            library should be generated. The default is the current directory.

        :param bool generate_lexer: (optional) Whether to invoke Quex to
            generate the lexer source code. Will do by default. As this can
            take time, it is useful to disable it during testing.

        :param set[str] main_source_dirs: List of source directories to use in
            the project file for mains. Source directories must be relative to
            the mains project file directory (i.e. $BUILD/src).

        :param set[str] main_programs: List of names for programs to build in
            addition to the generated library. To each X program, there must be
            a X.adb source file in the $BUILD/src directory.

        :param bool annotate_fields_types: Whether to try and annotate the
            type of fields in the grammar. If this is True, this will
            actually modify the file in which ASTNodeType instances are
            defined, and annotate empty field definitions.

        :param bool check_only: If true, only perform validity checks: stop
            before code emission. This is useful for IDE hooks.

        :param bool no_property_checks: If True, do not emit safety checks in
            the generated code for properties. Namely, this disables null
            checks on field access.

        :param WarningSet warnings: Set of enabled warnings.

        :param bool generate_unparser: Whether to generate a pretty printer for
            the given grammar.

        :param bool propeties_logging: Whether to instrument properties code to
            do logging.

        :param bool generate_astdoc: Whether to generate the HTML documentation
            for AST nodes, their fields and their properties.

        :param bool generate_gdb_hook: Whether to generate the
            ".debug_gdb_scripts" section. Good for debugging, but better to
            disable for releases.
        """
        if self.extensions_dir:
            add_template_dir(self.extensions_dir)
        for dirpath in keep(self.template_lookup_extra_dirs):
            add_template_dir(dirpath)

        self.no_property_checks = no_property_checks
        self.generate_unparser = generate_unparser
        self.properties_logging = properties_logging
        self.generate_astdoc = generate_astdoc
        self.generate_gdb_hook = generate_gdb_hook
        if warnings:
            self.warnings = warnings

        if post_process_ada:
            self.post_process_ada = post_process_ada
        if post_process_cpp:
            self.post_process_cpp = post_process_cpp
        if post_process_python:
            self.post_process_python = post_process_python

        from langkit.unparsers import Unparsers
        self.unparsers = Unparsers(self)

        if self.generate_unparser:
            self.warnings.enable(self.warnings.unparser_bad_grammar)

        # Automatically add all source files in the "extensions/src" directory
        # to the generated library project.
        self.extensions_src_dir = None
        if self.extensions_dir:
            src_dir = path.join(self.extensions_dir, 'src')
            if path.isdir(src_dir):
                self.extensions_src_dir = src_dir
                for filename in os.listdir(src_dir):
                    filepath = path.join(src_dir, filename)
                    if path.isfile(filepath) and not filename.startswith("."):
                        self.additional_source_files.append(filepath)

        self.compile(check_only=check_only,
                     annotate_fields_types=annotate_fields_types)
        if check_only:
            return
        with global_context(self):
            self._emit(file_root, generate_lexer, main_source_dirs,
                       main_programs)

        self.documentations.report_unused()

    def compile(self, check_only=False, annotate_fields_types=False):
        with global_context(self):
            self._compile(check_only, annotate_fields_types)

    def write_ada_module(self, out_dir, template_base_name, qual_name,
                         has_body=True):
        """
        Write an Ada module (both spec and body) using a standardized scheme
        for finding the corresponding templates.

        :param str out_dir: The out directory for the generated module.

        :param str template_base_name: The base name for the template,
            basically everything that comes before the _body_ada/_spec_ada
            component, including the directory.

        :param list[names.Name] qual_name: Qualified name for the Ada module,
            as a list of "simple" package names. The base library name is
            automatically prepended to that list, so every generated module
            will be a child module of the base library module.

        :param bool has_body: If true, generate a body for this unit.
        """
        for kind in [ADA_SPEC] + ([ADA_BODY] if has_body else []):
            qual_name_str = '.'.join(n.camel_with_underscores
                                     for n in qual_name)
            with_clauses = self.with_clauses[(qual_name_str, kind)]
            with names.camel_with_underscores:
                write_ada_file(
                    out_dir=out_dir,
                    source_kind=kind,
                    qual_name=[self.lib_name] + qual_name,
                    content=self.render_template(
                        "{}{}_ada".format(
                            template_base_name +
                            # If the base name ends with a /, we don't
                            # put a "_" separator.
                            ("" if template_base_name.endswith("/") else "_"),
                            kind
                        ),
                        with_clauses=with_clauses,
                    ),
                    post_process=self.post_process_ada
                )

    @property
    def composite_types(self):
        assert self._composite_types is not None
        return self._composite_types

    @property
    def array_types(self):
        assert self._array_types is not None
        return self._array_types

    @property
    def struct_types(self):
        assert self._struct_types is not None
        return self._struct_types

    @property
    def entity_types(self):
        assert self._entity_types is not None
        return self._entity_types

    @property
    def enum_types(self):
        from langkit.compiled_types import CompiledTypeRepo

        enum_types = CompiledTypeRepo.enum_types

        if self._enum_types:
            assert len(self._enum_types) == len(enum_types), (
                'CompileCtx.enum_types called too early: more enum types were'
                ' added')
        else:
            self._enum_types = list(enum_types)
            enum_types.sort(key=lambda et: et.name)

        return self._enum_types

    def _compile(self, check_only=False, annotate_fields_types=False):
        """
        Compile the language specification: perform legality checks and type
        inference.
        """
        # Compile the first time, do nothing next times
        if self.compiled:
            return
        self.compiled = True

        assert self.grammar, "Set grammar before compiling"

        from langkit.compiled_types import CompiledTypeRepo
        from langkit.parsers import Parser

        self.root_grammar_class = CompiledTypeRepo.root_grammar_class

        pass_manager = PassManager()
        pass_manager.add(
            MajorStepPass('Compiling the grammar'),
            GlobalPass('check token families',
                       self.lexer.check_token_families),
            GlobalPass('check main parsing rule',
                       self.grammar.check_main_rule),
            GlobalPass('warn on unreferenced parsing rules',
                       self.grammar.warn_unreferenced_parsing_rules),
            EnvSpecPass('create internal properties for env specs',
                        EnvSpec.create_properties,
                        iter_metaclass=True),
            GlobalPass('create enum node classes',
                       CompileCtx.create_enum_node_classes),
            GrammarRulePass('compute fields types',
                            lambda p: p.compute_fields_types()),

            GrammarRulePass('Compute dont skip rules',
                            lambda p: p.traverse_dontskip(self.grammar)),

            # This cannot be done before as the "compute fields type" pass will
            # create AST list types.
            GlobalPass('compute types', CompileCtx.compute_types),
            GlobalPass('check PLE unit root', CompileCtx.check_ple_unit_root),
            ASTNodePass('validate AST node fields',
                        lambda _, astnode: astnode.validate_fields(),
                        auto_context=False),
            ASTNodePass('reject abstract AST nodes with no concrete'
                        ' subclasses', CompileCtx.check_concrete_subclasses),
            GlobalPass('compute AST node constants',
                       CompileCtx.compute_astnode_constants),
            errors_checkpoint_pass,

            MajorStepPass('Compiling properties'),
            PropertyPass('prepare abstract expressions',
                         PropertyDef.prepare_abstract_expression),
            PropertyPass('freeze abstract expressions',
                         PropertyDef.freeze_abstract_expression),
            PropertyPass('compute property attributes',
                         PropertyDef.compute_property_attributes),
            PropertyPass('construct and type expressions',
                         PropertyDef.construct_and_type_expression),
            PropertyPass('check overriding types',
                         PropertyDef.check_overriding_types),
            PropertyPass('check properties return types',
                         PropertyDef.check_return_types),
            GlobalPass('compute uses entity info attribute',
                       CompileCtx.compute_uses_entity_info_attr),
            GlobalPass('compute uses envs attribute',
                       CompileCtx.compute_uses_envs_attr),
            EnvSpecPass('check env specs', EnvSpec.check_spec),
            GlobalPass('warn on unused private properties',
                       CompileCtx.warn_unused_private_properties),
            GlobalPass('warn on unreachable base properties',
                       CompileCtx.warn_unreachable_base_properties),
            PropertyPass('warn on undocumented public properties',
                         PropertyDef.warn_on_undocumented_public_property),
            GlobalPass('compute composite types',
                       CompileCtx.compute_composite_types),
            ASTNodePass('expose public structs and arrays types in APIs',
                        CompileCtx.expose_public_api_types,
                        auto_context=False),
            GlobalPass('lower properties dispatching',
                       CompileCtx.lower_properties_dispatching),
            GlobalPass('check memoized properties',
                       CompileCtx.check_memoized),
            errors_checkpoint_pass,

            GrammarRulePass('compute nodes parsers correspondence',
                            self.unparsers.compute),
            ASTNodePass('warn imprecise field type annotations',
                        lambda _, astnode:
                        astnode.warn_imprecise_field_type_annotations()),
            GlobalPass('log node parsers correspondence',
                       self.unparsers.check_nodes_to_rules),
            GlobalPass('finalize unparsers code generation',
                       self.unparsers.finalize),

            StopPipeline('check only', disabled=not check_only),

            MajorStepPass('Prepare code emission'),

            GrammarRulePass('register parsers symbol literals',
                            Parser.add_symbol_literals),
            # Past this point, the set of symbol literals is frozen
            GlobalPass('finalize symbol literals',
                       CompileCtx.finalize_symbol_literals),

            GrammarRulePass('compile grammar rule', Parser.compile),
            PropertyPass('render property', PropertyDef.render_property),
            GlobalPass('annotate fields types',
                       CompileCtx.annotate_fields_types,
                       disabled=not annotate_fields_types),
            errors_checkpoint_pass,
        )

        with names.camel_with_underscores:
            pass_manager.run(self)

    def _emit(self, file_root, generate_lexer, main_source_dirs,
              main_programs):
        """
        Emit native code for all the rules in this grammar as a library:
        a library specification and the corresponding implementation.  Also
        emit a tiny program that can parse starting with any parsing rule for
        testing purposes.
        """
        lib_name_low = self.ada_api_settings.lib_name.lower()

        include_path = path.join(file_root, "include")
        src_path = path.join(file_root, "include", lib_name_low)
        lib_path = path.join(file_root, "lib")
        share_path = path.join(file_root, "share", lib_name_low)

        if not path.exists(file_root):
            os.mkdir(file_root)

        if self.verbosity.info:
            printcol("File setup...", Colors.OKBLUE)

        for d in ["include",
                  "include/{}".format(lib_name_low),
                  "share",
                  "share/{}".format(lib_name_low),
                  "obj", "src", "bin",
                  "lib", "lib/gnat"]:
            p = path.join(file_root, d)
            if not path.exists(p):
                os.mkdir(p)

        self.cache = caching.Cache(
            os.path.join(file_root, 'obj', 'langkit_cache')
        )

        # Create the project file for the generated library
        main_project_file = os.path.join(
            lib_path, "gnat",
            "{}.gpr".format(self.ada_api_settings.lib_name.lower()),
        )
        write_source_file(
            main_project_file,
            self.render_template(
                "project_file",
                lib_name=self.ada_api_settings.lib_name,
                os_path=os.path,
                quex_path=os.environ['QUEX_PATH'],
            )
        )

        if self.generate_astdoc:
            from langkit import astdoc

            f = StringIO()
            astdoc.write_astdoc(self, f)
            write_source_file(os.path.join(share_path, 'ast-types.html'),
                              f.read())

        if self.verbosity.info:
            printcol("Generating sources... ", Colors.OKBLUE)

        ada_modules = [
            # Top (pure) package
            ('pkg_main', '', False),
            # Unit for initialization primitives
            ('pkg_init', 'Init', True),
            # Unit for declarations used by Analysis and Implementation
            ('pkg_common', 'Common', True),
            # Unit for public analysis primitives
            ('pkg_analysis', 'Analysis', True),
            # Unit for converters between public Ada types and C API-level ones
            ('pkg_c', 'C', True),
            # Unit for converters between public and implementation types
            ('pkg_converters', 'Converters', False),
            # Unit for implementation of analysis primitives
            ('pkg_implementation', 'Implementation', True),
            # Unit for AST introspection
            ('pkg_introspection', 'Introspection', True),
            # Unit for AST node iteration primitives
            ('pkg_iterators', 'Iterators', True),
            # Unit for AST rewriting primitives
            ('pkg_rewriting', 'Rewriting', True),
            # Unit for AST unparsing primitives
            ('pkg_unparsing', 'Unparsing', True),
            # Unit for AST implementation of unparsing primitives
            ('pkg_unparsing_impl', 'Unparsing_Implementation', True),
            # Unit for all parsers
            ('parsers/pkg_main', 'Parsers', True),
            # Units for the lexer
            ('pkg_lexer', 'Lexer', True),
            ('pkg_lexer_impl', 'Lexer_Implementation', True),
            # Unit for debug helpers
            ('pkg_debug', 'Debug', True),
        ]

        for template_base_name, qual_name, has_body in ada_modules:
            qual_name = ([names.Name(n) for n in qual_name.split('.')]
                         if qual_name else [])
            self.write_ada_module(src_path, template_base_name, qual_name,
                                  has_body)

        with names.camel_with_underscores:
            write_ada_file(
                path.join(file_root, "src"), ADA_BODY, [names.Name('Parse')],
                self.render_template("main_parse_ada"),
                self.post_process_ada
            )

        with names.lower:
            # ... and the Quex C interface
            write_cpp_file(path.join(src_path, "quex_interface.h"),
                           self.render_template(
                               "lexer/quex_interface_header_c"),
                           self.post_process_cpp)
            write_cpp_file(path.join(src_path, "quex_interface.c"),
                           self.render_template(
                               "lexer/quex_interface_body_c"),
                           self.post_process_cpp)

        imain_project_file = os.path.join(file_root, "src", "mains.gpr")
        write_source_file(
            imain_project_file,
            self.render_template(
                "mains_project_file",
                lib_name=self.ada_api_settings.lib_name,
                source_dirs=main_source_dirs,
                main_programs=main_programs
            )
        )

        # Emit C API
        self.emit_c_api(src_path, include_path)

        # Emit python API
        if self.python_api_settings:
            python_path = path.join(file_root, "python")
            if not path.exists(python_path):
                os.mkdir(python_path)
            self.emit_python_api(python_path)

            playground_file = os.path.join(file_root, "bin", "playground")
            write_source_file(
                playground_file,
                self.render_template(
                    "python_api/playground_py",
                    module_name=self.python_api_settings.module_name
                ),
                self.post_process_python
            )
            os.chmod(playground_file, 0o775)

            setup_py_file = os.path.join(file_root, 'python', 'setup.py')
            write_source_file(
                setup_py_file,
                self.render_template('python_api/setup_py'),
                self.post_process_python
            )

        # Emit GDB helpers initialization script
        gdbinit_path = os.path.join(file_root, 'gdbinit.py')
        lib_name = self.ada_api_settings.lib_name.lower()
        write_source_file(
            gdbinit_path,
            self.render_template(
                'gdb_py',
                langkit_path=os.path.dirname(os.path.dirname(__file__)),
                lib_name=lib_name,
                prefix=(self.short_name.lower
                        if self.short_name else lib_name),
            ),
            self.post_process_python
        )

        # Emit the ".debug_gdb_scripts" section if asked to
        if self.generate_gdb_hook:
            write_source_file(
                os.path.join(src_path, 'gdb.c'),
                self.render_template('gdb_c', gdbinit_path=gdbinit_path,
                                     os_name=os.name),
                self.post_process_cpp
            )

        # Add any sources in $lang_path/extensions/support if it exists
        if self.ext('support'):
            for f in glob(path.join(self.ext('support'), "*.ad*")):
                copy_file(f, src_path)

        if self.verbosity.info:
            printcol("Compiling the quex lexer specification", Colors.OKBLUE)

        # Generating the lexer C code with Quex is quite long: do it only when
        # the Quex specification changed from last build.
        quex_file = os.path.join(src_path,
                                 "{}.qx".format(self.lang_name.lower))
        quex_spec = self.lexer.emit()
        if (
            write_source_file(quex_file, quex_spec) and
            generate_lexer
        ):
            quex_py_file = path.join(os.environ["QUEX_PATH"], "quex-exe.py")
            subprocess.check_call([sys.executable, quex_py_file, "-i",
                                   quex_file,
                                   "-o", "quex_lexer",
                                   "--buffer-element-size", "4",
                                   "--token-id-offset",  "0x1000",
                                   "--language", "C",
                                   "--no-mode-transition-check",
                                   "--single-mode-analyzer",
                                   "--token-memory-management-by-user",
                                   "--token-policy", "single",
                                   "--token-id-prefix", self.lexer.prefix],
                                  cwd=src_path)

        self.cache.save()

    def emit_c_api(self, src_path, include_path):
        """
        Generate header and binding body for the external C API.

        :param str include_path: The include path.
        :param str src_path: The source path.
        """
        def render(template_name):
            return self.render_template(template_name)

        with names.lower:
            write_cpp_file(
                path.join(include_path,
                          "{}.h".format(self.c_api_settings.lib_name)),
                render("c_api/header_c"),
                self.post_process_cpp
            )

        self.write_ada_module(
            src_path, "c_api/pkg_main",
            [names.Name(n) for n in 'Implementation.C'.split('.')]
        )

    def emit_python_api(self, python_path):
        """
        Generate the Python binding module.

        :param str python_path: The directory in which the Python module will
            be generated.
        """
        package_dir = os.path.join(python_path,
                                   self.python_api_settings.module_name)
        if not os.path.isdir(package_dir):
            os.mkdir(package_dir)

        def strip_white_lines(code):
            tree = ast.parse(code)
            # Create an assoc of lines to a boolean flag indicating whether the
            # line is in a multiline string literal or not.
            lines = [[l, False] for l in code.splitlines()]

            # Find all the strings in the AST
            for s in (a for a in ast.walk(tree) if isinstance(a, ast.Str)):
                end_line = s.lineno
                start_line = end_line - len(s.s.splitlines()) + 1
                for l in range(start_line + 1, end_line):
                    lines[l - 1][1] = True

            return "\n".join(
                l[0] for l in lines
                if (not all(c.isspace() for c in l[0])) or l[1]
            )

        def pretty_print(code):
            if self.verbosity.debug:
                printcol('Pretty printing Python code', Colors.OKBLUE)
            if not self.pretty_print:
                return code

            try:
                from yapf.yapflib.yapf_api import FormatCode
                return FormatCode(code)[0]
            except ImportError:
                check_source_language(
                    False,
                    "Yapf not available, using autopep8 to pretty-print "
                    "Python code",
                    severity=Severity.warning
                )

            try:
                from autopep8 import fix_code
                return fix_code(code)
            except ImportError:
                check_source_language(
                    False,
                    "autopep8 not available, cannot pretty-print Python code",
                    severity=Severity.warning
                )
                return code

        with names.camel:
            code = self.render_template(
                "python_api/module_py",
                c_api=self.c_api_settings,
                pyapi=self.python_api_settings,
            )

            # If pretty-printing failed, write the original code anyway in
            # order to ease debugging.
            exc = None
            try:
                pp_code = pretty_print(strip_white_lines(code))
            except SyntaxError as exc:
                pp_code = code

            write_source_file(os.path.join(package_dir, '__init__.py'),
                              pp_code,
                              self.post_process_python)
            if exc:
                raise exc

    @property
    def extensions_dir(self):
        """
        Returns the absolute path to the extension dir, if it exists on the
        disk, or None.
        """
        return self._extensions_dir

    @extensions_dir.setter
    def extensions_dir(self, ext_dir):
        # Only set the extensions dir if this directory exists
        if os.path.isdir(ext_dir):
            self._extensions_dir = os.path.abspath(ext_dir)

    def ext(self, *args):
        """
        Return an extension path, relative to the extensions dir, given
        strings/names arguments, only if the extension file exists, so that
        you can do::

            ext('a', 'b', 'c')
            # returns 'a/b/c'

        :param [str|names.Name] args: The list of components to constitute the
                                      extension's path.

        :rtype: str
        """
        args = [a.lower if isinstance(a, names.Name) else a for a in args]
        if self.extensions_dir:
            ret = path.join(*args)
            if path.isfile(path.join(self.extensions_dir, ret)):
                return ret

    def set_quex_path(self):
        """
        If the QUEX_PATH environment variable is defined, do nothing.
        Otherwise, look for the "quex" program and determine Quex's "share"
        install directory: define the QUEX_PATH environment variable with it.
        """
        if 'QUEX_PATH' in os.environ:
            return

        try:
            quex_bin = subprocess.check_output(['which', 'quex']).strip()
        except subprocess.CalledProcessError:
            printcol('Cannot find the "quex" program. Please define the'
                     ' QUEX_PATH environment variable to Quex\'s "share"'
                     ' install directory', Colors.FAIL)
            raise

        os.environ['QUEX_PATH'] = os.path.join(
            os.path.dirname(os.path.dirname(quex_bin)),
            'share', 'quex'
        )

    def add_symbol_literal(self, name):
        """
        Add "name" to the list of symbol literals.

        This must not be called after finalize_symbol_literals is invoked.

        :type name: str
        """
        assert isinstance(self._symbol_literals, set)
        self._symbol_literals.add(name)

    @property
    def sorted_symbol_literals(self):
        """
        Return the list of symbol literals sorted in enumeration order.

        :rtype: list[(str, str)]
        """
        return sorted(self.symbol_literals.items(),
                      key=lambda kv: kv[1])

    def finalize_symbol_literals(self):
        """
        Collect all symbol literals provided to "add_symbol_literal" and create
        the "symbol_literals" mapping out of it.
        """
        assert isinstance(self._symbol_literals, set)
        symbols = self._symbol_literals
        self._symbol_literals = None

        for i, name in enumerate(sorted(symbols)):
            # Replace all non-alphabetic characters with underscores
            tmp_1 = (c if c.isalpha() else '_' for c in name.lower())

            # Remove consecutive underscores
            tmp_2 = reduce(
                lambda s, c: s if s.endswith('_') and c == '_' else s + c,
                tmp_1
            )

            # Remove leading/trailing underscores, and add 'Symbol' prefix
            candidate_name = names.Name('Symbol') + names.Name.from_lower(
                tmp_2.strip('_')
            )

            # If the candidate is already used, add an unique number
            if candidate_name in self.symbol_literals.values():
                candidate_name = candidate_name + names.Name(str(i))

            self.symbol_literals[name] = candidate_name

    def annotate_fields_types(self):
        """
        Modify the Python files where the node types are defined, to annotate
        empty Field() definitions.
        """
        # Only import lib2to3 if the users needs it
        import lib2to3.main

        astnodes_files = {
            n.location.file
            for n in self.astnode_types
            if n.location is not None
        }

        lib2to3.main.main(
            "langkit",
            ["-f", "annotate_fields_types",
             "--no-diff", "-w"] + list(astnodes_files)
        )

    def compute_astnode_constants(self):
        """
        Compute several constants for the current set of AST nodes.
        """
        # Compute the set of "kind" constants
        for i, astnode in enumerate(
            (astnode
             for astnode in self.astnode_types
             if not astnode.abstract),
            # Start with 1: the constant 0 is reserved as an
            # error/uninitialized code.
            start=1
        ):
            self.node_kind_constants[astnode] = i
            self.kind_constant_to_node[i] = astnode

        # Compute the list of parse fields, for introspection. Also compute
        # their indexes.
        self.sorted_parse_fields = []
        for n in self.astnode_types:
            i = 0
            for f in n.get_parse_fields():

                # Compute the index
                if f.abstract:
                    assert f._index in (None, -1)
                    f._index = -1
                else:
                    if f._index is None:
                        f._index = i
                    else:
                        assert f._index == i
                    i += 1

                # Register the field
                if (f.abstract or not f.overriding) and f.struct is n:
                    self.sorted_parse_fields.append(f)

    def compute_composite_types(self):
        """
        Check that struct and array types are valid and compute related lists.

        Today this only checks that there is no inclusing loop between these
        types. For instance: (1) is an array of (2) and (2) is a struct that
        contains (1).
        """
        from langkit.compiled_types import CompiledTypeRepo

        def dependencies(typ):
            """
            Return dependencies for the given compiled type that are relevant
            to the topological sort of composite types.
            """
            if typ.is_struct_type:
                # A struct type depends on the type of its fields
                result = [f.type for f in typ.get_fields()]

                # For non-root entity types, also add a dependency on the
                # parent entity type so that parents are declared before their
                # children.
                if typ.is_entity_type and not typ.element_type.is_root_node:
                    result.append(typ.element_type.base.entity)

            elif typ.is_array_type:
                result = [typ.element_type]

            else:
                assert False, 'Invalid composite type: {}'.format(typ.dsl_name)

            # Filter types that are relevant for dependency analysis
            return [t for t in result if t.is_struct_type or t.is_array_type]

        # Collect existing types and make sure we don't create other ones later
        # by accident.
        struct_types = CompiledTypeRepo.struct_types
        array_types = CompiledTypeRepo.array_types
        CompiledTypeRepo.struct_types = None
        CompiledTypeRepo.array_types = None

        # Sort the struct and array types by dependency order
        types_and_deps = (
            [(st, dependencies(st)) for st in struct_types]
            + [(at, dependencies(at)) for at in array_types])
        try:
            self._composite_types = topological_sort(types_and_deps)
        except TopologicalSortError as exc:
            message = ['Invalid composition of types:']
            for i, item in enumerate(exc.loop):
                next_item = (exc.loop[i + 1]
                             if i + 1 < len(exc.loop) else
                             exc.loop[0])
                message.append('  * {} contains a {}'
                               .format(item.dsl_name, next_item.dsl_name))
            check_source_language(False, '\n'.join(message))

        self._array_types = [t for t in self._composite_types
                             if t.is_array_type]
        self._struct_types = [t for t in self._composite_types
                              if t.is_struct_type]
        self._entity_types = [t for t in self._composite_types
                              if t.is_entity_type]

    def expose_public_api_types(self, astnode):
        """
        Tag all struct and array types referenced by the public API as exposed.
        This also emits non-blocking errors for all types that are exposed in
        the public API whereas they should not.
        """
        from langkit.compiled_types import ArrayType, Field, StructType

        def expose(t, to_internal, for_field, type_use, traceback):
            """
            Recursively tag "t" and all the types it references as exposed.
            """
            def check(predicate, descr):
                with for_field.diagnostic_context:
                    text_tb = (
                        ' (from: {})'.format(
                            ' -> '.join(traceback[:-1])
                        ) if len(traceback) > 1 else ''
                    )
                    check_source_language(
                        predicate,
                        '{} is {}, which is forbidden in public API{}'.format(
                            type_use, descr, text_tb
                        ),
                        severity=Severity.non_blocking_error
                    )

            if t.exposed:
                # If the type is already exposed, there is nothing to *check*,
                # but we still need to set the converter flags below.
                pass

            elif t.is_entity_type:
                # Allow all entity types to be exposed, and don't try to expose
                # internals, unlike for regular structs.
                pass

            elif isinstance(t, ArrayType):
                # Don't allow public arrays of arrays
                check(
                    not isinstance(t.element_type, ArrayType),
                    '{}, an array of arrays'.format(t.dsl_name)
                )

                # Reject public arrays of bare AST nodes
                check(
                    not t.element_type.is_ast_node,
                    '{}, an array of bare AST nodes'.format(t.dsl_name)
                )

                expose(t.element_type, to_internal, for_field, 'element type',
                       traceback + ['array of {}'.format(t.dsl_name)])

            elif isinstance(t, StructType):
                # Expose all record fields
                for f in t.get_fields():
                    # Reject public arrays of bare AST nodes
                    check(
                        not f.type.is_ast_node,
                        '{}, a bare AST node struct field'.format(f.qualname)
                    )
                    expose(f.type, to_internal, for_field, 'field type',
                           traceback + ['{} structures'.format(t.dsl_name)])
                    f.type.used_in_public_struct = True

            else:
                # Only array and struct types have their "_exposed" attribute
                # inferred. We consider all other ones to have a static value,
                # so complain if we reach a type that must not be exposed.
                check(t.exposed, t.dsl_name)
                return

            # Propagate the need of converters to exposed types. We can't rely
            # on the above recursive calls to expose if ``t`` was already
            # exposed.
            if to_internal:
                if not t.to_internal_converter_required:
                    for et in t.exposed_types:
                        expose(et, to_internal, for_field, 'exposed type',
                               traceback)
                    t.to_internal_converter_required = True
            else:
                if not t.to_public_converter_required:
                    for et in t.exposed_types:
                        expose(et, to_internal, for_field, 'exposed type',
                               traceback)
                    t.to_public_converter_required = True

            t.exposed = True

        for f in astnode.get_abstract_node_data(
            predicate=lambda f: f.is_public,
            include_inherited=False
        ):
            # Parse fields that are bare AST nodes are manually wrapped to
            # return entities in the public API. Bare AST nodes themselves
            # belong to the private API, so avoid exposing them.
            if isinstance(f, Field) and f.type.is_ast_node:
                continue

            expose(f.type, False, f,
                   'return type' if f.is_property else 'type',
                   [f.qualname])
            for arg in f.natural_arguments:
                expose(arg.type, True, f, '"{}" argument'.format(arg.dsl_name),
                       [f.qualname])
            if f.is_property:
                for dv in f.dynamic_vars:
                    expose(dv.type, True, f,
                           '"{}" dynamic variable'.format(dv.dsl_name),
                           [f.qualname])

    def lower_properties_dispatching(self):
        """
        Lower all dispatching properties.

        For each set of related dispatching properties, create a wrapper one
        that does manual dispatching based on AST node kinds and make all the
        other ones non-dispatching and private.
        """
        from langkit.compiled_types import Argument
        from langkit.expressions import (Entity, FieldAccess, LocalVars, Match,
                                         Self, construct)

        ignored_props = set()
        redirected_props = {}
        wrapper_props = set()

        # Iterate on AST nodes in hierarchical order, so that we meet root
        # properties before the overriding ones. As processing root properties
        # will remove the dispatching attribute of all overriding ones, we will
        # not process the same property twice.
        for astnode in self.astnode_types:
            for prop in astnode.get_properties(
                lambda p: p.dispatching,
                include_inherited=False
            ):
                if prop in ignored_props:
                    continue

                # `prop` must the ultimate base property: see the above comment
                prop_set = prop.property_set()
                assert prop_set[0] == prop

                # Because of the way they integrate in code generation,
                # external properties need to use tag-based dispatching, so
                # don't lower dispatching for any set of properties that
                # contain an external one.
                if any(p.external for p in prop_set):
                    ignored_props.update(prop_set)
                    continue

                def static_name(prop):
                    return prop.struct.name + prop.name

                static_props = list(prop_set)
                static_props.sort(key=lambda p: p.struct.hierarchical_name)

                # After the transformation, only the dispatching property will
                # require an untyped wrapper, so just remember if we need at
                # least one and make sure we generate at most one per property
                # hierarchy.
                requires_untyped_wrapper = any(p.requires_untyped_wrapper
                                               for p in static_props)
                for p in static_props:
                    p._requires_untyped_wrapper = False
                prop._requires_untyped_wrapper = requires_untyped_wrapper

                # The root property will be re-purposed as dispatching
                # function, so if it wasn't abstract, create a clone that the
                # dispatcher will redirect to.
                #
                # Note that in this context, we consider abstract properties
                # with a runtime check as concrete, as we do generate a body
                # for them. Because of this, we can here create a concrete
                # property that has an abstract runtime check.
                root_static = None
                prop.reset_inheritance_info()
                if not prop.abstract or prop.abstract_runtime_check:
                    root_static = PropertyDef(
                        expr=None, prefix=None, name=prop.name,
                        type=prop.type,
                        doc=prop.doc,
                        public=False,
                        dynamic_vars=prop.dynamic_vars,
                        uses_entity_info=prop.uses_entity_info,
                        uses_envs=prop.uses_envs,
                        optional_entity_info=prop.optional_entity_info,
                    )
                    static_props[0] = root_static

                    # Transfer arguments from the dispatcher to the new static
                    # property, then regenerate arguments in the dispatcher.
                    root_static.arguments = prop.arguments
                    prop.arguments = [
                        Argument(arg.name, arg.type, arg.is_artificial,
                                 arg.default_value)
                        for arg in prop.natural_arguments
                    ]
                    prop.build_dynamic_var_arguments()

                    root_static.constructed_expr = prop.constructed_expr
                    prop.constructed_expr = None

                    root_static.vars = prop.vars
                    prop.vars = LocalVars()

                    root_static.abstract_runtime_check = (
                        prop.abstract_runtime_check)
                    prop.abstract_runtime_check = False

                    root_static._has_self_entity = prop._has_self_entity

                    root_static.struct = prop.struct
                    root_static.location = prop.location

                else:
                    # If there is no runtime check for abstract properties, the
                    # set of concrete properties should cover the whole
                    # hierarchy tree. Just remove the future dispatcher from
                    # the list of statically dispatched properties.
                    static_props.pop(0)

                prop.is_dispatcher = True

                # Make sure all static properties are public, not dispatching
                # anymore, and assign them another name so that they don't
                # override each other in the generated code.
                for p in static_props:
                    p._is_public = False

                    p.prefix = None
                    p._name = static_name(p)
                    PropertyDef.name.fget.reset(p)

                    # Now that "root_static" is properly renamed, we can add it
                    # to its owning ASTNodeType instance.
                    if p == root_static:
                        prop.struct.add_field(p)

                    p._abstract = False
                    p.reset_inheritance_info()

                    redirected_props[p] = prop

                # Now turn the root property into a dispatcher
                wrapper_props.add(prop)
                prop._abstract = False

                # If at least one property this dispatcher calls uses entity
                # info, then we must consider that the dispatcher itself uses
                # it (same for using environments). We must do this by hand
                # since by the time we run this expansion pass, these
                # attributes are already initialized by propagation through the
                # callgraph.
                prop._uses_entity_info = any(p.uses_entity_info
                                             for p in prop_set)
                prop._uses_envs = any(p.uses_envs for p in prop_set)

                with prop.bind(bind_dynamic_vars=True), \
                        Self.bind_type(prop.struct):
                    outer_scope = prop.get_scope()
                    self_arg = construct(Entity
                                         if prop.uses_entity_info else
                                         Self)
                    matchers = []
                    for p in reversed(static_props):
                        match_var = prop.vars.create_scopeless(
                            'Match_Var',
                            (p.struct.entity
                             if prop.uses_entity_info
                             else p.struct)
                        )
                        with outer_scope.new_child() as inner_scope:
                            inner_scope.add(match_var)
                            # When this field access gets evaluated, we know
                            # that the receiver is not null as we already tried
                            # to get its kind, so we can perform an unsafe
                            # field access on it.
                            static_call = FieldAccess.Expr(
                                receiver_expr=match_var.ref_expr,
                                node_data=p,
                                arguments=[construct(arg.var)
                                           for arg in prop.natural_arguments],
                                implicit_deref=p.uses_entity_info,
                                unsafe=True
                            )
                            matchers.append(Match.Matcher(
                                match_var.ref_expr, static_call, inner_scope
                            ))

                    prop.constructed_expr = Match.Expr(self_arg, matchers)

        # Now that all relevant properties have been transformed, update all
        # references to them so that we always call the wrapper. Note that we
        # don't have to do this for property expressions are we are supposed to
        # have already directed to root properties at resolved expression
        # construction time.
        for astnode in self.astnode_types:
            if astnode.env_spec:
                for env_action in astnode.env_spec.actions:
                    env_action.rewrite_property_refs(redirected_props)

    def generate_actions_for_hierarchy(self, node_var, kind_var,
                                       actions_for_astnode):
        """
        Generate a sequence of Ada statements/nested CASE blocks to execute
        some actions on an AST node depending on its kind.

        This method is useful to avoid generating the same statements over and
        over for multiple AST node kinds. For instance, given a root AST node
        `A` a field `f` on `A`, and its derivations `B`, `C` and `D`, if one
        wants to perform clean-up on some AST node fields, there is no need to
        generate specific code for `B.f`, `C.f` and `D.f` when we could just
        generate code for `A.f`.

        :param str|None node_var: Name of the variable that holds the AST node
            to process. None if the generated code must work only on the kind.
        :param str kind_var: Name of the variable that holds the kind of the
            AST node to process. Holding it in a variables is handy to avoid
            computing it multiple times.
        :param (ASTNodeType) -> str actions_for_astnode: Function that return
            the actions (i.e. Ada statements as a single string) to perform for
            the given AST node type. Note that these actions should be specific
            to the AST node, i.e. they should not overlap with actions for any
            parent AST node.
        :type actions_for_astnode: (ASTNodeType) -> str
        """

        class Matcher(object):
            """
            Holder for "when ... =>" clauses in a CASE block.
            """

            def __init__(self, astnode, actions):
                self.astnode = astnode
                """
                AST node that `self` matches.
                :type: ASTNodeType
                """

                self.actions = actions
                """
                List of actions specific to this matched AST node.
                :type: str
                """

                self.inner_case = Case(astnode)
                """
                Case instance for nodes that are more specific than `astnode`.
                :type: Case
                """

            @staticmethod
            def new_node_var(astnode):
                """
                Return the variable name that will hold the casted value for
                the matched AST node.

                :rtype: names.Name
                """
                return names.Name('N') + astnode.name

        class Case(object):
            """
            Holder for a generated CASE blocks.
            """

            def __init__(self, astnode):
                self.astnode = astnode
                """
                Most specific type for this CASE block's input expression.
                :type: ASTNodeType
                """

                self.matchers = []
                """
                List of matchers for this CASE block.
                :type: list[Matcher]
                """

        root_node = self.root_grammar_class

        result = []
        """
        List of strings for the sequence of Ada statements to return.
        :type: list[str]
        """

        case_stack = [Case(root_node)]
        """
        Stack of Case instances for the Case tree we are currently building.
        First element is for the top-level CASE node while the last element is
        for the currently inner-most CASE node.
        :type: list[Case]
        """

        def build_cases(astnode):
            """
            Build the tree of CASE blocks for `astnode` and all its subclasses.
            """
            # Don't bother processing classes unless they actually have
            # concrete subclasses, otherwise we would be producing dead code.
            if not astnode.concrete_subclasses:
                return

            to_pop = False

            if astnode == root_node:
                # As a special case, emit actions for the root node outside of
                # the top-level CASE block as we don't need to dispatch on
                # anything for them: they always must be applied.
                actions = actions_for_astnode(astnode, node_var)
                if actions:
                    result.append(actions)

            else:
                # If there are actions for this node, add a matcher for them
                # and process the subclasses in a nested CASE block.
                actions = actions_for_astnode(
                    astnode, Matcher.new_node_var(astnode)
                )
                if actions:
                    m = Matcher(astnode, actions)
                    case_stack[-1].matchers.append(m)
                    case_stack.append(m.inner_case)
                    to_pop = True

            for subcls in astnode.subclasses:
                build_cases(subcls)

            if to_pop:
                case_stack.pop()

        def print_case(case, node_var):
            """
            Render a tree of CASE blocks and append them to `result`.

            :param Case case: CASE block to render.
            :param str node_var: Name of the variable that holds the node on
                which this CASE must dispatch.
            """
            if not case.matchers:
                return

            result.append('case {} ({}) is'.format(
                case.astnode.ada_kind_range_name, kind_var
            ))
            for m in case.matchers:
                result.append(
                    'when {} =>'.format(m.astnode.ada_kind_range_name)
                )
                if node_var is None:
                    new_node_var = None
                else:
                    new_node_type = m.astnode.name.camel_with_underscores
                    new_node_var = m.new_node_var(m.astnode)
                    result.append("""
                       declare
                          {new_node_var} : constant {new_node_type} :=
                             {new_node_type} ({node_var});
                       begin
                    """.format(
                        node_var=node_var,
                        new_node_type=new_node_type,
                        new_node_var=new_node_var,
                    ))
                result.append(m.actions)
                print_case(m.inner_case, new_node_var)
                if node_var is not None:
                    result.append('end;')

            result.append('when others => null;')
            result.append('end case;')

        with names.camel_with_underscores:
            build_cases(root_node)
            assert len(case_stack) == 1
            print_case(case_stack[0], node_var)

        return '\n'.join(result) or 'null;'

    @property
    def has_memoization(self):
        """
        Return whether one property is memoized.

        :rtype: bool
        """
        has_keys = bool(self.memoization_keys)
        has_values = bool(self.memoization_values)
        assert has_keys == has_values, (
            'Either there is no memoized property, either we do, in which case'
            ' there must be at least one key type and one key value'
        )
        return has_keys

    def check_memoized(self):
        """
        Check that various invariants for memoized properties are respected.
        Also register involved types in the memoization machinery.
        """
        from langkit.compiled_types import T

        class Annotation(object):
            """
            Analysis annotation for a property.
            """

            def __init__(self, reason=None, call_chain=[]):
                """
                :param str|None reason: None if this property can be memoized.
                    Otherwise, error message to indicate why.
                :param list[PropertyDef] call_chain: When this property cannot
                    be memoized because of transitivity, chain of properties
                    that led to this decision.
                """
                assert (reason is None) == (not call_chain)
                self.reason = reason
                self.call_chain = call_chain

            @property
            def memoizable(self):
                """
                Return whether the property this annotation relates to is
                considered to be memoizable (for now).

                :rtype: bool
                """
                return self.reason is None

            def with_call(self, prop):
                """
                Return a copy of `self` with `prop` appened to its call chain.

                :rtype: Annotation
                """
                if self.reason is None:
                    return self
                else:
                    return Annotation(self.reason, self.call_chain + [prop])

            def simpler_than(self, other):
                """
                Return whether `self` is at least as simple than `other`.

                This assumes that both `self.memoizable` and `other.memoizable`
                are false.  An annotation is consider simpler if its call chain
                is not bigger.

                :rtype: bool
                """
                assert not self.memoizable and not other.memoizable
                return (other.reason is not None and
                        len(self.call_chain) <= len(other.call_chain))

            def __repr__(self):
                return '<Annotation {} ({})>'.format(
                    self.memoizable,
                    ', '.join(p.qualname for p in self.call_chain)
                )

        _, back_graph = self.properties_callgraphs()
        annotations = {prop: Annotation() for prop in back_graph}

        # First check that properties can be memoized without considering
        # callgraph-transitive evidence that they cannot (but collect all
        # information).
        for astnode in self.astnode_types:
            for prop in astnode.get_properties(include_inherited=False):
                with prop.diagnostic_context:

                    tr_reason = prop.transitive_reason_for_no_memoization
                    if tr_reason is not None and not prop.call_memoizable:
                        annotations[prop] = Annotation(tr_reason, [prop])

                    if not prop.memoized:
                        continue

                    reason = prop.reason_for_no_memoization
                    check_source_language(reason is None, reason)

                    self.memoized_properties.add(prop)
                    prop.struct.add_as_memoization_key(self)
                    if prop.uses_entity_info:
                        T.entity_info.add_as_memoization_key(self)
                    for arg in prop.arguments:
                        check_source_language(
                            arg.type.hashable,
                            'This property cannot be memoized because argument'
                            ' {} (of type {}) is not hashable'.format(
                                arg.name.lower, arg.type.dsl_name
                            ),
                        )
                        arg.type.add_as_memoization_key(self)
                    prop.type.add_as_memoization_value(self)

        # Now do the propagation of callgraph-transitive evidence
        queue = {p for p, a in annotations.items() if not a.memoizable}
        while queue:
            callee = queue.pop()
            for caller in back_graph[callee]:
                callee_annot = annotations[callee].with_call(caller)
                caller_annot = annotations[caller]

                # Stop propagation on properties that state that they can
                # handle memoization safety.
                if caller.call_memoizable:
                    continue

                if (not callee_annot.memoizable and
                        (caller_annot.memoizable or
                            callee_annot.simpler_than(caller_annot))):
                    annotations[caller] = callee_annot
                    queue.add(caller)

        for prop, annot in sorted(annotations.items(),
                                  key=lambda p: p[0].qualname):
            if not prop.memoized or annot.memoizable:
                continue

            message = 'Property cannot be memoized '
            if annot.call_chain:
                message += '(in {}: {}, call chain is: {})'.format(
                    annot.call_chain[0].qualname,
                    annot.reason,
                    ' -> '.join(p.qualname for p in reversed(annot.call_chain))
                )
            else:
                message += '({})'.format(annot.reason)

            with prop.diagnostic_context:
                check_source_language(False, message,
                                      severity=Severity.non_blocking_error)

    astnode_kind_set = langkit.utils.astnode_kind_set

    collapse_concrete_nodes = staticmethod(
        langkit.utils.collapse_concrete_nodes
    )
