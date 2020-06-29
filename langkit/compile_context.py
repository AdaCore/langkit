"""
This file contains the logic for the compilation context for Langkit. This is
the main hook into Langkit, insofar as this is the gate through which an
external language creator will actually trigger the code emission. For example,
this is the way it is done for the Ada language::

    from ada_parser import ada_lexer, ada_grammar
    context = CompileCtx(... ada_lexer, ada_grammar...)
    ...
    context.emit(...)
"""
from __future__ import annotations

from collections import defaultdict
from contextlib import contextmanager
from functools import reduce
import importlib
import os
from os import path
from typing import Any, Callable, Dict, List

from funcy import lzip

from langkit import documentation, names, utils
from langkit.ada_api import AdaAPISettings
from langkit.c_api import CAPISettings
from langkit.coverage import GNATcov
from langkit.diagnostics import (Context, Location, Severity, WarningSet,
                                 check_source_language, context_stack)
from langkit.utils import (TopologicalSortError, collapse_concrete_nodes,
                           memoized, memoized_with_default, topological_sort)


compile_ctx = None


def get_context(or_none=False):
    """
    Return the current compilation context. Meant to be used by the rest of
    Langkit, in any code that has been called as part of the CompileCtx.emit
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


ADA_SPEC = "spec"
ADA_BODY = "body"


class Verbosity:
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


class UnparseScript:
    """
    Sequence of actions to generate concrete syntax DSL.
    """

    def __init__(self, spec):
        self.actions = self.parse(spec)

    @staticmethod
    def parse(spec):
        result = []

        # Do the parsing itself
        for action in spec.split(','):
            if ':' not in action:
                key = action
                value = None
            else:
                key, value = action.split(':', 1)
            result.append((key, value))

        # Validate actions
        if not len(result):
            raise ValueError('At least one action expected')
        for i, (action, arg) in enumerate(result):
            if i == 0 and action != 'to':
                raise ValueError('First action must be to:')

            if action in ('to', 'import'):
                if arg is None:
                    raise ValueError('Missing argument for {}:'.format(action))
            elif action in ('nodes', 'lexer', 'grammar'):
                if arg is not None:
                    raise ValueError('Unexpected argument for {}:'
                                     .format(action))
            else:
                raise ValueError('Unknown action: {}:'.format(action))

        return result


class LibraryEntity:
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


class GeneratedException:
    """
    Describe an exception in generated libraries.
    """

    def __init__(self, doc_section, package, name):
        """
        :param str doc_section: Section in the documentation where this
            exception occurs.
        :param list[names.Name] package: Ada package in which this exception is
            originally defined.
        :param names.Name name: Name for this exception.
        """
        self.doc_section = doc_section
        self.package = package
        self.name = name

    @property
    def doc_entity(self):
        """
        Name of the documentation entry for this exception.

        :rtype: str
        """
        return '{}.{}'.format(self.doc_section, self.name.lower)

    @property
    def qualname(self):
        """
        Fully qualified name to the exception declaration (in Ada).

        :rtype: str
        """
        return '{}.{}'.format('.'.join(str(p) for p in self.package),
                              self.name)

    @property
    def kind_name(self):
        """
        Return the enumeration name corresponding to an exception.

        :rtype: names.Name
        """
        return names.Name('Exception') + self.name


class CompileCtx:
    """State holder for native code emission."""

    def __init__(self, lang_name, lexer, grammar,
                 lib_name=None, short_name=None,
                 c_symbol_prefix=None,
                 default_charset='utf-8',
                 default_tab_stop=8,
                 verbosity=Verbosity('none'),
                 template_lookup_extra_dirs=None,
                 default_unit_provider=None,
                 symbol_canonicalizer=None,
                 documentations=None,
                 show_property_logging=False,
                 lkt_file=None,
                 types_from_lkt=False):
        """Create a new context for code emission.

        :param str lang_name: string (mixed case and underscore: see
            langkit.names.Name) for the Name of the target language.

        :param lexer: A lexer for the target language.
        :type lexer: langkit.lexer.Lexer

        :param grammar: A grammar for the target language. If left to None,
            fetch the grammar in the Lktlang source.
        :type None|grammar: langkit.parsers.Grammar

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

        :param bool show_property_logging: If true, any property that has been
            marked with tracing activated will be traced on stdout by default,
            without need for any config file.

        :param None|str lkt_file: Optional name of the file to contain Lktlang
            definitions for this language.

        :param bool types_from_lkt: When loading definitions from Lktlang
            files, whether to load type definitions. This is not done by
            default during the transition from our Python DSL to Lktlang.
        """
        from langkit.python_api import PythonAPISettings
        from langkit.ocaml_api import OCamlAPISettings
        from langkit.unparsers import Unparsers

        self.lang_name = names.Name(lang_name)

        self.lib_name = (
            names.Name('Lib{}lang'.format(self.lang_name.lower))
            if lib_name is None else
            names.Name(lib_name)
        )
        self.short_name = names.Name(short_name) if short_name else None
        self.short_name_or_long = self.short_name or self.lib_name

        self.ada_api_settings = AdaAPISettings(self)
        self.c_api_settings = CAPISettings(
            self,
            (self.lang_name.lower
             if c_symbol_prefix is None else c_symbol_prefix)
        )
        self.c_api_settings.lib_name = self.lib_name.lower

        self.default_charset = default_charset
        self.default_tab_stop = default_tab_stop

        self.verbosity = verbosity

        self.compiled = False
        """
        Whether the language specification was compiled. This is used to avoid
        doing it multiple times.

        :type: bool
        """

        self.lkt_units = None
        if lkt_file is None:
            assert grammar, 'LKT spec required when no grammar is provided'
        else:
            from langkit.lkt_lowering import load_lkt
            self.lkt_units = load_lkt(lkt_file)

        self.lexer = lexer
        ":type: langkit.lexer.Lexer"

        self.grammar = grammar
        ":type: langkit.parsers.Grammar"

        self.python_api_settings = PythonAPISettings(self, self.c_api_settings)
        self.types_from_lkt = types_from_lkt

        self.ocaml_api_settings = (
            OCamlAPISettings(self, self.c_api_settings)
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

        self.synthetic_nodes = None
        """
        Sub-sequence of `self.astnode_types` for all nodes that are synthetic.

        This is computed right after `self.astnode_types`.

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

        self.exception_types = {}
        """
        Mapping of all exception types. Keys are lower-case exception names.

        :type: dict[str, GeneratedException]
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

        self.default_unit_provider = default_unit_provider
        self.symbol_canonicalizer = symbol_canonicalizer

        docs = dict(documentation.base_langkit_docs)
        if documentations:
            docs.update(documentations)
        self.documentations = documentation.instantiate_templates(docs)
        """
        Documentation database. Associate a Mako template for each entity to
        document in the generated library.

        :type: dict[str, mako.template.Template]
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

        self.sorted_properties = None
        """
        Sorted list of public properties. Used to generate the property
        introspection API.

        :type: list[langkit.expressions.PropertyDef]
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

        self.nfa_start = None
        """
        Intermediate representation for the lexer state machine (NFA).

        :type: langkit.lexer.regexp.NFAState
        """

        self.unparsers = Unparsers(self)
        """
        :type: langkit.unparsers.Unparsers
        """

        self.emitter = None
        """
        During code emission, corresponding instance of Emitter. None the rest
        of the time.

        :type: None|langkit.emitter.Emitter
        """

        self.gnatcov = None
        """
        During code emission, GNATcov instance if coverage is enabled. None
        otherwise.

        :type: None|langkit.coverage.GNATcov
        """

        self.show_property_logging = show_property_logging

        # Register builtin exception types
        self._register_builtin_exception_types()

    @contextmanager
    def lkt_context(self, lkt_node):
        """
        Context manager to set the diagnostic context to the given node.

        :param liblktlang.LKNode lkt_node: Node to use as a reference for this
            diagnostic context.
        """
        # Invalid type passed here will fail much later and only if a
        # check_source_language call fails. To ease debugging, check that
        # "lkt_node" has the right type here.
        import liblktlang
        assert isinstance(lkt_node, liblktlang.LKNode)

        context_stack.append(Location.from_lkt_node(lkt_node))

        try:
            yield
        finally:
            context_stack.pop()

    @staticmethod
    def lkt_doc(full_decl):
        """
        Return the documentation attached to the ``full_decl`` node, if any.

        :param liblktlang.FullDecl full_decl: Declaration to process.
        :rtype: None|str
        """
        return '\n'.join(l.text for l in full_decl.f_doc)

    def _register_builtin_exception_types(self):
        """
        Register exception types for all builtin exceptions.
        """
        for namespace, exception_name in [
            (None, 'native_exception'),
            (None, 'precondition_failure'),
            (None, 'property_error'),
            (None, 'invalid_unit_name_error'),
            (None, 'invalid_symbol_error'),
            (None, 'stale_reference_error'),
            (None, 'unknown_charset'),
            (None, 'invalid_input'),
            ('introspection', 'invalid_field'),
            ('introspection', 'node_data_evaluation_error'),
            ('rewriting', 'template_format_error'),
            ('rewriting', 'template_args_error'),
            ('rewriting', 'template_instantiation_error')
        ]:
            doc_section = 'langkit'
            package = [names.Name('Langkit_Support'),
                       names.Name('Errors')]
            if namespace:
                doc_section = '{}.{}'.format(doc_section, namespace)
                package.append(names.Name.from_lower(namespace))

            assert exception_name not in self.exception_types
            self.exception_types[exception_name] = GeneratedException(
                doc_section, package, names.Name.from_lower(exception_name)
            )

    @property
    def exceptions_by_section(self):
        """
        Return exceptions grouped by "section".

        We compute sections from documentation entries: none for
        'langkit.EXCEPTION_NAME' and SECTION for
        'langkit.SECTION.EXCEPTION_NAME'.

        :rtype: list[(None|str, list[str, names.Name])]
        """
        sections = defaultdict(list)

        for e in self.sorted_exception_types:
            # Remove the 'langkit.' prefix
            no_prefix = e.doc_entity.split('.', 1)[1]

            section_name = (
                '' if '.' not in no_prefix else
                no_prefix.split('.')[0].replace('_', ' ').capitalize())

            sections[section_name].append(e)

        return sorted(sections.items())

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

    @property
    def sorted_exception_types(self):
        """
        Turn "exception_types" into a sorted list.

        This is required during code generation to preserve a stable output.

        :rtype: list[(str, names.Name)]
        """
        return sorted(self.exception_types.values(),
                      key=lambda e: e.doc_entity)

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

    @staticmethod
    def grammar_rule_api_name(rule):
        """
        Return the API name of the given grammar rule name.

        :type rule: str
        :rtype: names.Name
        """
        return names.Name.from_lower(rule + '_rule')

    @property
    def main_rule_api_name(self):
        """
        Return the API name of the grammar's main rule.

        :rtype: names.Name
        """
        return self.grammar_rule_api_name(self.grammar.main_rule_name)

    def compute_types(self):
        """
        Compute various information related to compiled types, that needs to be
        available for code generation.
        """
        from langkit.compiled_types import (CompiledTypeRepo, EnumType,
                                            StructType, T)
        from langkit.dsl import _StructMetaclass
        from langkit.expressions.base import construct_compile_time_known

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

        self.synthetic_nodes = [n for n in self.astnode_types
                                if n.synthetic]

        # We need a hash function for the metadata structure as the
        # Langkit_Support.Lexical_Env generic package requires it.
        T.env_md.require_hash_function()

        # We expose a hash function for public entities, so we must generate
        # the underlying required helpers.
        T.entity.require_hash_function()

        # Create the type for grammar rules
        EnumType(name='GrammarRule',
                 location=None,
                 doc="Gramar rule to use for parsing.",
                 value_names=[self.grammar_rule_api_name(n)
                              for n in self.grammar.user_defined_rules],
                 is_builtin_type=True)

        # Now that all types are known, construct default values for fields
        for st in CompiledTypeRepo.struct_types:
            for f in st.get_abstract_node_data():
                if f.abstract_default_value is not None:
                    f.default_value = construct_compile_time_known(
                        f.abstract_default_value
                    )

    def compute_optional_field_info(self):
        """
        For every parse field, find out if it is an optional field or not, i.e.
        whether it is ever produced from a parser of the user grammar that can
        create a null node.
        """
        from langkit.parsers import (Defer, DontSkip, List, Null, Opt, Or,
                                     Predicate, Skip, _Extract, _Transform)

        @memoized_with_default(False)
        def can_produce_null(parser):
            if isinstance(parser, Opt):
                # If parser is an Opt parser and is not set to produce an enum
                # alternative, it means that field is optional.
                return not parser._booleanize
            elif isinstance(parser, Null):
                return True
            elif isinstance(parser, Or):
                return any(can_produce_null(p) for p in parser.parsers)
            elif isinstance(parser, Defer):
                return can_produce_null(parser.parser)
            elif isinstance(parser, List):
                return False
            elif isinstance(parser, DontSkip):
                return can_produce_null(parser.subparser)
            elif isinstance(parser, Skip):
                return False
            elif isinstance(parser, Predicate):
                return can_produce_null(parser.parser)
            elif isinstance(parser, _Transform):
                return False
            elif isinstance(parser, _Extract):
                return can_produce_null(parser.parser.parsers[parser.index])
            else:
                raise NotImplementedError("Unhandled parser {}".format(parser))

        all_parse_fields = [
            field
            for node_type in self.astnode_types
            for field in node_type.get_parse_fields(include_inherited=False)
        ]

        for field in all_parse_fields:
            field._is_optional = False
            for parser in field.parsers_from_transform:
                if can_produce_null(parser):
                    field._is_optional = True

    def check_ple_unit_root(self):
        """
        Check that if the "ple_unit_root" node annotation is used, it is valid.

        If so, add a "is_env_populated" field to it.
        """
        from langkit.compiled_types import ASTNodeType, T, UserField

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
                        ple_unit_root_list not in f.precise_types,
                        '{} cannot appear anywhere in trees except as a root'
                        ' node'.format(ple_unit_root_list.dsl_name)
                    )
                    check_source_language(
                        self.ple_unit_root not in f.precise_types,
                        '{} cannot appear anywhere in trees except as a child'
                        ' of {} nodes'.format(self.ple_unit_root.dsl_name,
                                              ple_unit_root_list.dsl_name)
                    )

        is_env_populated_flag = UserField(
            type=T.Bool,
            doc='Whether this PLE unit root was processed by'
                ' Populate_Lexical_Env.',
            public=False
        )
        is_env_populated_flag._name = is_env_populated_flag._original_name = (
            ASTNodeType.is_env_populated_name
        )
        is_env_populated_flag._indexing_name = (
            ASTNodeType.is_env_populated_indexing_name
        )
        self.ple_unit_root.add_field(is_env_populated_flag)

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
        Return an iterator on all the properties. *args and **kwargs are
        forwarded to the call to get_properties that is done on every astnode
        type.

        :rtype: seq[PropertyDef]
        """
        from langkit.compiled_types import CompiledTypeRepo
        for astnode in self.astnode_types:
            for prop in astnode.get_properties(*args, **kwargs):
                yield prop

        # Compute properties for non-astnode types
        for _, typ in CompiledTypeRepo.type_dict.items():
            if not typ.is_ast_node:
                for prop in typ.get_properties(*args, **kwargs):
                    yield prop

    @memoized
    def properties_logging(self):
        """
        Return whether logging is activated for any properties in the compile
        context.
        """
        return any(prop.activate_tracing for prop in self.all_properties)

    def properties_callgraphs(self):
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
        from langkit.expressions import PropertyDef

        def add_forward(from_prop, to_prop):
            backwards.setdefault(to_prop, set())
            forwards[from_prop].add(to_prop)
            backwards[to_prop].add(from_prop)
            for over_prop in to_prop.all_overriding_properties:
                add_forward(from_prop, over_prop)

        def traverse_expr(expr):
            for ref_prop in expr.flat_subexprs(
                lambda e: isinstance(e, PropertyDef)
            ):
                add_forward(prop, ref_prop)
            for subexpr in expr.flat_subexprs():
                traverse_expr(subexpr)

        forwards = {}
        backwards = {}

        for prop in self.all_properties(include_inherited=False):
            forwards.setdefault(prop, set())
            backwards.setdefault(prop, set())

            # For dispatchers, add calls to the dispatched properties
            if prop.is_dispatcher:
                for _, static_prop in prop.dispatch_table:
                    add_forward(prop, static_prop)

            # For regular properties, add calls from the property expression
            elif prop.constructed_expr:
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
                with Context(p.location):
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
                    Context(None)
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

    def warn_on_undocumented(self, node):
        """
        Emit a warning if ``node`` is not documented.
        """
        # Ignore nodes that are created during the expansion of enum nodes:
        # users cannot add documentation for these.
        if node.base and node.base.is_enum_node:
            return

        # Likewise for the very abstract generic list type
        elif node.is_generic_list_type:
            return

        WarningSet.undocumented_nodes.warn_if(
            not node._doc, 'This node lacks documentation')

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

    _template_extensions_fns: List[Callable[[CompileCtx], Dict[str, Any]]] = []
    """
    List of functions to create the default template environment.

    :type: list[(CompileCtx) -> dict[str, object]]
    """

    _template_extensions_frozen = False
    """
    Whether at least one context has requested the list of template extensions.
    Once it's true, one cannot register template extensions anymore.

    :type: bool
    """

    @property  # type: ignore
    @memoized
    def template_extensions(self):
        """
        Return the set of template extensions evaluated for this context.

        :rtype: dict[str, object]
        """
        CompileCtx._template_extensions_frozen = True

        from langkit.common import comment_box, string_repr
        assert self.emitter
        base_env = {
            'comment_box': comment_box,
            'string_repr': string_repr,
            'Name':        names.Name,
            'ada_doc':     documentation.ada_doc,
            'c_doc':       documentation.c_doc,
            'py_doc':      documentation.py_doc,
            'ocaml_doc':   documentation.ocaml_doc,
            'ada_c_doc':   documentation.ada_c_doc,
            'emitter':     self.emitter,
        }
        for fn in CompileCtx._template_extensions_fns:
            ext_env = fn(self)
            for k, v in ext_env.items():
                assert k not in base_env, (
                    'Duplicate key in renderer env: {}'.format(k)
                )
                base_env[k] = v
        return base_env

    @property  # type: ignore
    @memoized
    def renderer(self):
        """
        Return the default renderer for this context.
        """
        from langkit import template_utils
        return template_utils.Renderer(self.template_extensions)

    def render_template(self, *args, **kwargs):
        """
        Shortcut for ``self.renderer.render(*args, **kwargs)``.
        """
        return self.renderer.render(*args, **kwargs)

    @classmethod
    def register_template_extensions(cls, exts_fn):
        """
        Register a set of mako template env extensions.

        :param exts_fn: Function to be evaluated the first time the renderer is
            created.
        :type exts_fn: (CompileCtx) -> dict[str, object]
        """
        assert not cls._template_extensions_frozen
        CompileCtx._template_extensions_fns.append(exts_fn)

    @staticmethod
    def load_plugin_pass(pass_or_name):
        """
        Load a plug-in pass.

        :param str|langkit.passes.AbstractPass pass_or_name: Name of the pass
            to load (``MODULE.CALLABLE`` syntax). If it is already a pass
            object, just return it.
        :rtype: langkit.passes.AbstractPass
        """
        from langkit.passes import AbstractPass

        if isinstance(pass_or_name, AbstractPass):
            return pass_or_name

        module_name, constructor_name = pass_or_name.rsplit('.', 1)
        module = importlib.import_module(module_name)
        constructor = getattr(module, constructor_name)
        result = constructor()
        assert isinstance(result, AbstractPass)
        return result

    def emit(self, lib_root, check_only=False, warnings=None,
             report_unused_documentation_entries=False,
             default_max_call_depth=1000, **kwargs):
        """
        Compile the DSL and emit sources for the generated library.

        :param str lib_root: Path of the directory in which the library should
            be generated.

        :param bool check_only: If true, only perform validity checks: stop
             before code emission. This is useful for IDE hooks. False by
             default.

        :param None|WarningSet warnings: If provided, white list of warnings to
            emit.

        :param bool annotate_fields_types: If true, annotate the
             type of fields in the grammar. This will actually modify the file
             in which ASTNodeType instances are defined, and annotate empty
             field definitions. False by default.

        :param bool generate_unparser: If true, generate a pretty printer for
            the given grammar. False by default.

        :param list[str] plugin_passes: List of passes to add as plugins to the
            compilation pass manager. List item must be a name matching the
            following pattern: ``MODULE.CALLABLE`` where ``MODULE`` is the name
            of a module that can be imported, and ``CALLABLE`` is the name of a
            callable inside the module to import. This callable must accept no
            argument and return an instance of a
            ``langkit.passes.AbstractPass`` subclass.

        :param bool report_unused_documentation_entries: Whether to emit
            warnings about unused documentation entries.

        :param int max_call_depth: Default maximum number of recursive calls
            allowed in property calls. This is used as a mitigation against
            infinite recursions.

        See langkit.emitter.Emitter's constructor for other supported keyword
        arguments.
        """

        assert self.emitter is None

        if warnings:
            self.warnings = warnings

        self.generate_unparser = kwargs.pop('generate_unparser', False)
        annotate_fields_types = kwargs.pop('annotate_fields_types', False)
        self.default_max_call_depth = default_max_call_depth

        self.report_unused_documentation_entries = (
            report_unused_documentation_entries
        )

        if kwargs.get('coverage', False):
            self.gnatcov = GNATcov(self)

        # Load plugin passes
        plugin_passes = [self.load_plugin_pass(p)
                         for p in kwargs.pop('plugin_passes', [])]

        # Compute the list of passes to run:

        # First compile the DSL
        all_passes = self.compilation_passes

        # Then, if requested, emit code for the generated library
        if not check_only:
            all_passes.append(
                self.prepare_code_emission_pass(lib_root, **kwargs))

            all_passes.extend(self.code_emission_passes(annotate_fields_types))

            # Run plugin passes at the end of the pipeline
            all_passes.extend(plugin_passes)

        # We can now run the pipeline
        with names.camel_with_underscores, global_context(self):
            try:
                self.run_passes(all_passes)
                if not check_only and self.emitter is not None:
                    self.emitter.cache.save()
            finally:
                self.emitter = None

    def lower_lkt(self):
        """
        Run the lkt lowering passes over lkt input files.
        """

        if self.lexer is None:
            from langkit.lkt_lowering import create_lexer
            self.lexer = create_lexer(self, self.lkt_units)

        if self.grammar is None:
            from langkit.lkt_lowering import create_grammar
            self.grammar = create_grammar(self, self.lkt_units)

        if self.types_from_lkt and self.lkt_units:
            from langkit.lkt_lowering import create_types
            create_types(self, self.lkt_units)

    def prepare_compilation(self):
        """
        Prepare this context to compile the DSL.

        TODO: this pass seems like a weird grab bag of verifications and a
        potentially useless assignment. See if it can be removed later.
        """
        from langkit.compiled_types import CompiledTypeRepo

        # Compilation cannot happen more than once
        assert not self.compiled

        # Make sure user provided a grammar
        assert self.grammar, 'Set grammar before compiling'

        self.root_grammar_class = CompiledTypeRepo.root_grammar_class

        if self.generate_unparser:
            self.warnings.enable(self.warnings.unparser_bad_grammar)

    def prepare_code_emission_pass(self, lib_root, **kwargs):
        """
        Return a pass to prepare this context for code emission.
        """
        from langkit.emitter import Emitter
        from langkit.passes import GlobalPass

        def pass_fn(ctx):
            ctx.emitter = Emitter(self, lib_root, ctx.extensions_dir, **kwargs)

        return GlobalPass('prepare code emission', pass_fn)

    def compile(self, generate_unparser=False):
        """
        Compile the DSL.

        :param bool generate_unparser: If true, generate a pretty printer for
            the given grammar. False by default.
        """
        self.generate_unparser = generate_unparser

        with global_context(self):
            self.run_passes(self.compilation_passes)

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

    @property
    def compilation_passes(self):
        """
        Return the list of passes to compile the DSL.
        """
        from langkit.envs import EnvSpec
        from langkit.expressions import PropertyDef
        from langkit.lexer import Lexer
        from langkit.parsers import Grammar, Parser
        from langkit.passes import (
            ASTNodePass, EnvSpecPass, GlobalPass, GrammarPass, GrammarRulePass,
            LexerPass, MajorStepPass, PropertyPass, errors_checkpoint_pass
        )

        # RA22-015: in order to allow bootstrap, we need to import liblktlang
        # only if we are about to process LKT grammar rules.
        def lower_grammar_rules(ctx):
            if not ctx.grammar._all_lkt_rules:
                return
            from langkit.lkt_lowering import lower_grammar_rules
            lower_grammar_rules(ctx)

        return [
            MajorStepPass('LKT processing'),
            GlobalPass('lower lkt', CompileCtx.lower_lkt),
            GlobalPass('prepare compilation', CompileCtx.prepare_compilation),

            MajorStepPass('Compiling the lexer'),
            LexerPass('check token families', Lexer.check_token_families),
            LexerPass('compile lexer rules', Lexer.compile_rules),

            MajorStepPass('Compiling the grammar'),
            GlobalPass('lower Lkt parsing rules', lower_grammar_rules),
            GrammarPass('check main parsing rule', Grammar.check_main_rule),
            GrammarPass('warn on unreferenced parsing rules',
                        Grammar.warn_unreferenced_parsing_rules),
            EnvSpecPass('create internal properties for env specs',
                        EnvSpec.create_properties,
                        iter_metaclass=True),
            EnvSpecPass('register categories', EnvSpec.register_categories,
                        iter_metaclass=True),
            GrammarRulePass('compute parser types', Parser.compute_types),
            GrammarRulePass('freeze parser types', Parser.freeze_types),
            GrammarRulePass('check type of top-level grammar rules',
                            Parser.check_toplevel_rules),

            GrammarRulePass('compute dont skip rules',
                            lambda p: p.traverse_dontskip(self.grammar)),

            # This cannot be done before as the "compute fields type" pass will
            # create AST list types.
            GlobalPass('compute types', CompileCtx.compute_types),
            ASTNodePass('check inferred field types',
                        lambda _, node: node.check_inferred_field_types()),
            ASTNodePass('validate AST node fields',
                        lambda _, astnode: astnode.validate_fields(),
                        auto_context=False),
            GlobalPass('compute optional field info',
                       CompileCtx.compute_optional_field_info),
            ASTNodePass('reject abstract AST nodes with no concrete'
                        ' subclasses', CompileCtx.check_concrete_subclasses),
            errors_checkpoint_pass,

            MajorStepPass('Compiling properties'),
            PropertyPass('compute base properties',
                         PropertyDef.compute_base_property),
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
            PropertyPass('check properties returning node types',
                         PropertyDef.check_returned_nodes),
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
            ASTNodePass('warn on undocumented nodes',
                        CompileCtx.warn_on_undocumented),
            GlobalPass('compute composite types',
                       CompileCtx.compute_composite_types),
            ASTNodePass('expose public structs and arrays types in APIs',
                        CompileCtx.expose_public_api_types,
                        auto_context=False),
            GlobalPass('lower properties dispatching',
                       CompileCtx.lower_properties_dispatching),
            GlobalPass('check memoized properties',
                       CompileCtx.check_memoized),
            GlobalPass('compute AST node constants',
                       CompileCtx.compute_astnode_constants),
            errors_checkpoint_pass,

            MajorStepPass('Computing precise types'),
            ASTNodePass('compute precise fields types',
                        lambda _, n: n.compute_precise_fields_types()),
            GlobalPass('check PLE unit root', CompileCtx.check_ple_unit_root),

            GrammarRulePass('compile parsers', Parser.compile),
            GrammarRulePass('compute nodes parsers correspondence',
                            self.unparsers.compute),
            ASTNodePass('warn imprecise field type annotations',
                        lambda _, astnode:
                        astnode.warn_imprecise_field_type_annotations()),
            GlobalPass('log node parsers correspondence',
                       self.unparsers.check_nodes_to_rules),
            GlobalPass('finalize unparsers code generation',
                       self.unparsers.finalize),
        ]

    def code_emission_passes(self, annotate_fields_types):
        """
        Return the list of passes to emit sources for the generated library.
        """
        from langkit.emitter import Emitter
        from langkit.expressions import PropertyDef
        from langkit.parsers import Parser
        from langkit.passes import (
            EmitterPass, GlobalPass, GrammarRulePass, MajorStepPass,
            PropertyPass, errors_checkpoint_pass
        )

        from langkit.dsl_unparse import unparse_lang

        return [
            MajorStepPass('Prepare code emission'),

            GrammarRulePass('register parsers symbol literals',
                            Parser.add_symbol_literals),
            # Past this point, the set of symbol literals is frozen
            GlobalPass('finalize symbol literals',
                       CompileCtx.finalize_symbol_literals),

            GrammarRulePass('render parsers code', Parser.render_parser),
            PropertyPass('render property', PropertyDef.render_property),
            GlobalPass('annotate fields types',
                       CompileCtx.annotate_fields_types,
                       disabled=not annotate_fields_types),
            errors_checkpoint_pass,

            MajorStepPass('Generate library sources'),
            EmitterPass('setup directories', Emitter.setup_directories),
            EmitterPass('emit astdoc', Emitter.emit_astdoc),
            EmitterPass('generate lexer DFA', Emitter.generate_lexer_dfa),
            EmitterPass('emit Ada sources', Emitter.emit_ada_lib),
            EmitterPass('emit mains', Emitter.emit_mains),
            EmitterPass('emit C API', Emitter.emit_c_api),
            EmitterPass('emit Python API', Emitter.emit_python_api),
            EmitterPass('emit Python playground',
                        Emitter.emit_python_playground),
            EmitterPass('emit GDB helpers', Emitter.emit_gdb_helpers),
            EmitterPass('emit OCaml API', Emitter.emit_ocaml_api),
            EmitterPass('emit library project file',
                        Emitter.emit_lib_project_file),
            EmitterPass('instrument for code coverage',
                        Emitter.instrument_for_coverage),

            GlobalPass('report unused documentation entries',
                       lambda ctx: ctx.documentations.report_unused(),
                       disabled=not self.report_unused_documentation_entries),

            GlobalPass('RA22-015: Unparse language to concrete syntax',
                       unparse_lang),
        ]

    def run_passes(self, passes):
        """
        Run the given passes through the pass manager.

        :param list[langkit.passes.AbstractPass]: List of compilation passes to
            go through.
        """
        from langkit.passes import PassManager
        pass_manager = PassManager()
        pass_manager.add(*passes)
        pass_manager.run(self)

    @property
    def extensions_dir(self):
        """
        Return the absolute path to the extension dir, if it exists on the
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
        strings/names arguments, only if the extension file/dir exists, so that
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
            p = path.join(self.extensions_dir, ret)
            if path.isfile(p) or path.isdir(p):
                return ret

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

            # Remove leading/trailing underscores, and add 'Precomputed_Symbol'
            # prefix.
            candidate_name = (names.Name('Precomputed_Symbol') +
                              names.Name.from_lower(tmp_2.strip('_')))

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

        # Compute the list of parse fields and public properties, for
        # introspection. Also compute parse field indexes.
        self.sorted_parse_fields = []
        self.sorted_properties = []
        for n in self.astnode_types:
            i = 0
            for f in n.get_parse_fields():

                # Compute the index
                if f.abstract or f.null:
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

            for p in n.get_properties(predicate=lambda p: p.is_public,
                                      include_inherited=False):
                if not p.overriding:
                    self.sorted_properties.append(p)

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
        from langkit.expressions import PropertyDef

        redirected_props = {}

        # Iterate on AST nodes in hierarchical order, so that we meet root
        # properties before the overriding ones. As processing root properties
        # will remove the dispatching attribute of all overriding ones, we will
        # not process the same property twice.
        for astnode in self.astnode_types:
            for prop in astnode.get_properties(
                lambda p: p.dispatching,
                include_inherited=False
            ):
                # `prop` must be the ultimate base property: see the above
                # comment.
                prop_set = prop.property_set()
                assert prop_set[0] == prop

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

                # The root property will be re-purposed as a dispatching
                # function, so if it wasn't abstract, create a clone that the
                # dispatcher will redirect to.
                #
                # Note that in this context, we consider abstract properties
                # with a runtime check as concrete, as we do generate a body
                # for them. Because of this, we can here create a concrete
                # property that has an abstract runtime check.
                root_static = None
                prop.is_dispatcher = True
                prop.reset_inheritance_info()

                # Assign the dispatcher a new name so that it does not conflict
                # with the root property in the generated code.
                prop_name = prop.name
                prop._name = names.Name('Dispatcher') + prop.name

                if not prop.abstract or prop.abstract_runtime_check:
                    root_static = PropertyDef(
                        expr=None, prefix=None, name=None,
                        type=prop.type,
                        doc=prop.doc,
                        public=False,
                        dynamic_vars=prop.dynamic_vars,
                        uses_entity_info=prop.uses_entity_info,
                        uses_envs=prop.uses_envs,
                        optional_entity_info=prop.optional_entity_info,
                    )
                    static_props[0] = root_static

                    # Add this new property to its structure for code
                    # generation. Make sure it is registered under a name that
                    # is different from the dispatcher so that both are present
                    # in the structures' field dict.
                    root_static._name = prop_name
                    root_static._original_name = prop._original_name
                    root_static._indexing_name = (
                        '[root-static]{}'.format(prop.indexing_name)
                    )
                    prop.struct.add_field(root_static)

                    # Transfer arguments from the dispatcher to the new static
                    # property, then regenerate arguments in the dispatcher.
                    root_static.arguments = prop.arguments
                    prop.arguments = [
                        Argument(arg.name, arg.type, arg.is_artificial,
                                 arg.abstract_default_value)
                        for arg in prop.natural_arguments
                    ]
                    prop.build_dynamic_var_arguments()

                    root_static.constructed_expr = prop.constructed_expr
                    prop.expected_type = prop.type
                    prop.constructed_expr = None

                    root_static.vars = prop.vars
                    prop.vars = None

                    root_static.abstract_runtime_check = (
                        prop.abstract_runtime_check)
                    prop.abstract_runtime_check = False

                    root_static._has_self_entity = prop._has_self_entity

                    root_static.struct = prop.struct
                    root_static.location = prop.location
                    prop.is_artificial_dispatcher = True

                else:
                    # If there is no runtime check for abstract properties, the
                    # set of concrete properties should cover the whole
                    # hierarchy tree. Just remove the future dispatcher from
                    # the list of statically dispatched properties.
                    static_props.pop(0)

                # Determine for each static property the set of concrete nodes
                # we should dispatch to it.
                dispatch_types, remainder = collapse_concrete_nodes(
                    prop.struct, reversed([p.struct for p in static_props]))
                assert not remainder
                prop.dispatch_table = lzip(reversed(dispatch_types),
                                           static_props)
                # TODO: emit a warning for unreachable properties earlier in
                # the compilation pipeline. Here we can see them with an empty
                # set of types in the dispatch table.

                # Make sure all static properties are private and not
                # dispatching anymore. Also remove their prefixes: their names
                # are already decorated and these properties will not be used
                # in public APIs (only dispatching ones are).
                for p in static_props:
                    p.prefix = None
                    p._is_public = False
                    p._abstract = False
                    p.reset_inheritance_info()
                    p.dispatcher = prop
                    redirected_props[p] = prop

                # Now turn the root property into a dispatcher
                prop._abstract = False
                prop.constructed_expr = None

                # If at least one property this dispatcher calls uses entity
                # info, then we must consider that the dispatcher itself uses
                # it (same for using environments). We must do this by hand
                # since by the time we run this expansion pass, these
                # attributes are already initialized by propagation through the
                # callgraph.
                prop._uses_entity_info = any(p.uses_entity_info
                                             for p in prop_set)
                prop._uses_envs = any(p.uses_envs for p in prop_set)

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
                                       actions_for_astnode,
                                       public_nodes=False):
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
        :param (ASTNodeType, str) -> str actions_for_astnode: Function that
            takes a specific node type to process and the name of the variable
            to hold the node instance to process. It must return the actions
            (i.e. Ada statements as a single string) to perform for this node.
            Note that these actions should be specific to the node type, i.e.
            they should not overlap with actions for any parent node.
        :type actions_for_astnode: (ASTNodeType) -> str
        :param bool public_nodes: Whether `node_var` is a type from the public
            API. Otherwise (the default), assume it is an internal node.
        """

        class Matcher:
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

        class Case:
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
                    new_node_type = (
                        m.astnode.entity.api_name.camel_with_underscores
                        if public_nodes else
                        m.astnode.name.camel_with_underscores)
                    new_node_var = m.new_node_var(m.astnode)

                    # Declare a new variable to hold the node subtype to
                    # process in this matcher.
                    new_node_expr = ('{node_var}.As_{new_node_type}'
                                     if public_nodes else
                                     str(node_var))
                    result.append('declare')
                    result.append(
                        ('{new_node_var} : constant'
                         ' {namespace}{new_node_type} := ' +
                         new_node_expr + ';').format(
                            node_var=node_var,
                            new_node_type=new_node_type,
                            new_node_var=new_node_var,

                            # Public node names sometimes clash with
                            # introspection enumerations. Adding namespace
                            # helps generating correct code.
                            namespace='Analysis.' if public_nodes else ''))
                    result.append('begin')

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

        class Annotation:
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

    TypeSet = utils.TypeSet

    astnode_kind_set = utils.astnode_kind_set

    collapse_concrete_nodes = staticmethod(
        utils.collapse_concrete_nodes
    )
