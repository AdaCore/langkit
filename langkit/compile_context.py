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

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import ast
from collections import defaultdict
from contextlib import contextmanager
from distutils.spawn import find_executable
from glob import glob
import inspect
import itertools
import os
from os import path
import shutil
import subprocess
import sys

from funcy import keep
from mako.lookup import TemplateLookup

from langkit import caching, names, template_utils
from langkit.ada_api import AdaAPISettings
from langkit.c_api import CAPISettings
from langkit.diagnostics import Severity, check_source_language
import langkit.documentation
from langkit.expressions import PropertyDef
from langkit.passes import (
    ASTNodePass, GlobalPass, GrammarRulePass, MajorStepPass, PassManager,
    PropertyPass, StopPipeline, errors_checkpoint_pass
)
from langkit.utils import Colors, printcol

compile_ctx = None


def get_context():
    """
    Returns the current compilation context. Meant to be used by the rest of
    LangKit, in any code that has been called as part of the CompileCtx.emit
    primitive.

    :rtype: CompileCtx
    """
    assert compile_ctx is not None, (
        "Get context has been called in a state in which the compile context"
        " is not set"
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


def write_cpp_file(file_path, source):
    with open(file_path, "wb") as out_file:
        if find_executable("clang-format"):
            p = subprocess.Popen(["clang-format"], stdin=subprocess.PIPE,
                                 stdout=out_file)
            p.communicate(source)
            assert p.returncode == 0
        else:
            out_file.write(source)


ADA_SPEC = "spec"
ADA_BODY = "body"


def write_ada_file(out_dir, source_kind, qual_name, content):
    """
    Helper to write an Ada file.

    :param str out_dir: The complete path to the directory in which we want to
        write the file.
    :param str source_kind: One of the constants ADA_SPEC or ADA_BODY,
        determining whether the source is a spec or a body.
    :param list[str] qual_name: The qualified name of the Ada spec/body,
        as a list of string components.
    :param str content: The source content to write to the file.
    """
    assert source_kind in (ADA_SPEC, ADA_BODY)
    file_name = "{}.{}".format("-".join(qual_name).lower(),
                               "ads" if source_kind == ADA_SPEC else "adb")
    file_path = os.path.join(out_dir, file_name)

    # TODO: no tool is able to pretty-print a single Ada source file
    with open(file_path, "wb") as out_file:
            out_file.write(content)


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
        if isinstance(level, basestring):
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
                 lib_name=None,
                 library_fields_all_public=False,
                 c_symbol_prefix=None,
                 enable_python_api=True,
                 default_charset='utf-8',
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

        :param bool library_fields_all_public: Whether private fields are
            exposed anyway in the various generated APIs. Doing so is useful
            for debugging.

        :param c_symbol_prefix: Valid C identifier used as a prefix for all
            top-level declarations in the generated C API.  If not provided,
            set to the name of the language in lower case.  Empty string stands
            for no prefix.
        :type c_symbol_prefix: str or None

        :param bool enable_python_api: If True (which is the default),
            generates a Python API for the generated library.

        :param str default_charset: In the generated library, this will be the
            default charset to use to scan input source files.

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

                function Canonicalize (Name : Text_Type) return Text_Type;

            It takes an identifier name and must return the canonical name for
            it, so that all equivalent symbols have the same canonical name.

            This can be used, for instance, to implement case insensivity.

        :param dict[str, str] documentations: If provided, supply templates to
            document entities. These will be added to the documentations
            available in code generation: see langkit.documentation.
        """
        from langkit.python_api import PythonAPISettings

        self.lang_name = names.Name(lang_name)

        lib_name = (
            names.Name('Lib{}lang'.format(self.lang_name.lower))
            if lib_name is None else
            names.Name(lib_name)
        )

        self.ada_api_settings = AdaAPISettings(
            lib_name.camel_with_underscores
        )
        self.c_api_settings = CAPISettings(
            self, lib_name.lower,
            (self.lang_name.lower
             if c_symbol_prefix is None else c_symbol_prefix)
        )

        self.library_fields_all_public = library_fields_all_public

        self.default_charset = default_charset

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
            PythonAPISettings(lib_name.lower, self.c_api_settings)
            if enable_python_api else None
        )

        self.fns = set()
        """
        Set of names (names.Name instances) for all generated parser
        functions. This is used to avoid generating these multiple times.

        :type: set[parsers.Parser]
        """

        self.types = set()
        """
        Set of CompiledType subclasses: all such subclasses must register
        themselves here when their add_to_context method is invoked. This
        field too is used to avoid multiple generation issues.

        :type: set[langkit.compiled_types.CompiledType]
        """

        self.enum_types = set()
        """
        Set of EnumType subclasses, registered by EnumType.add_to_context.

        :type: set[langkit.compiled_types.CompiledType]
        """

        self.astnode_types = []
        """
        List for all ASTnode subclasses (ASTNode excluded), sorted so that A
        is before B when A is a parent class for B. This sorting is important
        to output declarations in dependency order.
        This is computed right after field types inference.

        :type: list[langkit.compiled_types.ASTNode]
        """

        self.node_kind_constants = {}
        """
        Mapping: ASTNode concrete (i.e. non abstract) subclass -> int,
        associating specific constants to be used reliably in bindings.  This
        mapping is built at the beginning of code emission.

        :type: dict[langkit.compiled_types.ASTNode, int]
        """

        self._struct_types = []
        """
        List of all plain struct types.

        :type: list[langkit.compiled_types.Struct]
        """

        self.root_grammar_class = None
        """
        The ASTNode subclass that is the root class for every node used in
        the grammar.

        :type: langkit.compiled_types.ASTNode
        """

        self.generic_list_type = None
        """
        The root gammar class subclass that is the base class for all
        automatically generated root list types.

        :type: langkit.compiled_types.ASTNode
        """

        self.env_metadata = None
        """
        The Struct subclass that will be used as the lexical environment
        metadata type.

        :type: langkit.compiled_types.Struct
        """

        self.list_types = set()
        """
        Set of all ASTNode subclasses (ASTNode included) for which we
        generate a corresponding list type.

        :type: set[langkit.compiled_types.ASTNode]
        """

        self.array_types = set()
        """
        Set of all ArrayType subclasses.

        For each ArrayType subclass T, code emission for type definition will
        automatically happen in the AST.Types packages unless
        T.element_type().should_emit_array_type is False. In this case, type
        definition should be hard-wired in the AST package.

        :type: set[langkit.compiled_types.ArrayType]
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
        Struct subclass used to annotate environment elements. Initialized
        during the typing pass.

        :type: langkit.compiled_types.Struct
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
        For the moment, there is just one property that handles the conversion
        in the bind, but ultimately there will also be a property to check for
        equality.

        :type: set[(PropertyDef, )]
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
            CompiledType subclasses to sort.
        :rtype: list[langkit.compiled_types.CompiledType]
        """
        return sorted(type_set, key=lambda cls: cls.name())

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

        This list contains all rules that return AST nodes.

        :rtype: list[str]
        """
        from langkit.compiled_types import ASTNode
        return sorted(
            name
            for name, parser in self.grammar.rules.items()
            if issubclass(parser.get_type(), ASTNode)
        )

    def compute_types(self):
        """
        Compute various information related to compiled types, that needs to be
        available for code generation.
        """

        # Get the list of ASTNode types from the Struct metaclass
        from langkit.compiled_types import LexicalEnvType, StructMetaclass
        entity = StructMetaclass.root_grammar_class.entity()

        self.astnode_types = list(StructMetaclass.astnode_types)

        self.root_grammar_class = StructMetaclass.root_grammar_class
        self.generic_list_type = self.root_grammar_class.generic_list_type
        self.env_metadata = StructMetaclass.env_metadata

        # The Group lexical environment operation takes an array of lexical
        # envs, so we always need to generate the corresponding array type.
        self.array_types.add(LexicalEnvType.array_type())

        # Likewise for the entity array type: LexicalEnv.get returns it.
        self.array_types.add(entity.array_type())

        # Sort them in dependency order as required but also then in
        # alphabetical order so that generated declarations are kept in a
        # relatively stable order. This is really useful for debugging
        # purposes.
        keys = {
            cls: cls.hierarchical_name()
            for cls in self.astnode_types
        }
        self.astnode_types.sort(key=lambda cls: keys[cls])

        # Check that the environment hook is bound if the language spec uses
        # it.
        if self.env_hook_subprogram is None:
            for t in self.astnode_types:
                with t.diagnostic_context():
                    check_source_language(
                        t.env_spec is None or not t.env_spec.env_hook_enabled,
                        'Cannot invoke the environment hook if'
                        ' CompileContext.bind_env_hook has not been called'
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

    def properties_callgraphs(self):
        """
        Return forwards and backwards callgraphs for all properties.

        This takes care of overriding properties. In other words, if C calls A
        and B overrides A, then we consider that C calls both A and B. Note
        that this considers references to properties in logic expressions as
        calls.

        :rtype: (dict[PropertyDef, set[PropertyDef]],
                 dict[PropertyDef, set[PropertyDef]])
        """
        def add_forward(from_prop, to_prop):
            backwards.setdefault(to_prop, set())
            forwards[from_prop].add(to_prop)
            backwards[to_prop].add(from_prop)
            for over_prop in to_prop.overriding_properties:
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
            if prop.constructed_expr:
                traverse_expr(prop.constructed_expr)

        return (forwards, backwards)

    def compute_uses_env_attribute(self):
        """
        Pass that will compute the `uses_envs` attribute for every property.
        This will determine whether it is necessary to pass along env
        rebindings or not.
        """

        _, backwards = self.properties_callgraphs()

        def propagate(prop):
            """
            Propagate the `uses_envs` attribute to callers.
            """
            for p in prop.property_set():
                p.set_uses_env()

            for bw_link in backwards.get(prop, set()):
                if not bw_link.uses_envs:
                    propagate(bw_link)

        # Propagate computed attribute
        for prop in self.all_properties(lambda p: not p.base_property
                                        and p.uses_envs, False):
            propagate(prop)

    def warn_unused_private_properties(self):
        """
        Check that all private properties are actually used: if one is not,
        it is useless, so emit a warning for it.
        """

        forwards_strict, _ = self.properties_callgraphs()

        # Compute the callgraph with flattened subclassing information:
        # consider only root properties.
        forwards = defaultdict(set)
        for prop, called in forwards_strict.items():
            root = prop.root_property
            forwards[root].update(c.root_property for c in called)

        # Compute the set of properties that are transitively called by a
        # public property. Assume that internal properties are used.

        # The first is for strict analysis while the second one simplifies
        # properties to their root.
        reachable_by_public_strict = set()
        reachable_by_public = set()

        def compute_reachable(reachable_set, forward_map):
            queue = {p for p in forward_map if p.is_public or p.is_internal}
            while queue:
                prop = queue.pop()
                reachable_set.add(prop)
                queue.update(p for p in forward_map[prop]
                             if p not in reachable_set)
        compute_reachable(reachable_by_public_strict, forwards_strict)
        compute_reachable(reachable_by_public, forwards)

        # The unused private properties are the ones that are not part of this
        # set.
        unreachable_private_strict = (
            set(forwards_strict) - reachable_by_public_strict
        )
        unreachable_private = set(forwards) - reachable_by_public
        assert all(p.is_private for p in unreachable_private_strict)

        # Now determine the set of unused abstraction: it's all root properties
        # that are unused in the strict analysis but used in the other one.
        unused_abstractions = {
            p.root_property for p in
            (unreachable_private_strict - unreachable_private)
        }

        def warn(unused_set, message):
            sorted_set = sorted((p.qualname, p) for p in unused_set)
            for _, p in sorted_set:
                with p.diagnostic_context():
                    check_source_language(False, message,
                                          severity=Severity.warning)

        warn(unreachable_private, 'This private property is unused')
        warn(unused_abstractions, 'This private abstraction is unused')

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
             gdb_helpers_prefix=None):
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
            actually modify the file in which ASTNode subclasses are
            defined, and annotate empty field definitions.

        :param bool check_only: If true, only perform validity checks: stop
            before code emission. This is useful for IDE hooks.

        :param bool no_property_checks: If True, do not emit safety checks in
            the generated code for properties. Namely, this disables null
            checks on field access.

        :param str|None gdb_helpers_prefix: If provided, it must be the prefix
            to used for the name of GDB helper commands. In this case, create
            GDB helpers to help debugging of the generated library. If left to
            None, nothing is done with respect to GDB helpers.
        """
        dir_path = path.join(
            path.dirname(path.realpath(__file__)), "templates"
        )
        template_utils.template_lookup = TemplateLookup(
            directories=keep([dir_path, self.extensions_dir]
                             + self.template_lookup_extra_dirs),
            strict_undefined=True
        )

        self.no_property_checks = no_property_checks
        self.gdb_helpers_prefix = gdb_helpers_prefix

        # Automatically add all source files in the "extensions/src" directory
        # to the generated library project.
        if self.extensions_dir:
            src_dir = path.join(self.extensions_dir, 'src')
            if path.isdir(src_dir):
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

        :param list[str] qual_name: Qualified name for the Ada module,
            as a list of strings. The base library name is automatically
            prepended to that list, so every generated module will be a
            child module of the base library module.

        :param bool has_body: If true, generate a body for this unit.
        """
        for kind in [ADA_SPEC] + ([ADA_BODY] if has_body else []):
            with names.camel_with_underscores:
                write_ada_file(
                    out_dir=out_dir,
                    source_kind=kind,
                    qual_name=[self.ada_api_settings.lib_name] + qual_name,
                    content=self.render_template(
                        "{}{}_ada".format(
                            template_base_name +
                            # If the base name ends with a /, we don't
                            # put a "_" separator.
                            ("" if template_base_name.endswith("/") else "_"),
                            kind
                        ),
                    )
                )

    @property
    def struct_types(self):
        # Here we're skipping Struct because it's not a real type in
        # generated code. We're also putting env_metadata and entity in
        # the beginning and in the right dependency order (the metadata type
        # before the entity type).
        #
        # TODO: Using a dependency order topological sort wouldn't hurt at
        # some point.

        from langkit.compiled_types import StructMetaclass

        if self._struct_types:
            # TODO: A better solution at some point would be having a
            # "freezable list" for struct_types (and every list of types for
            # that matter) and raising an error if some code tries to add to it
            # after the freeze point.
            assert (
                len(self._struct_types) == len(StructMetaclass.struct_types)
            ), (
                "CompileCtx.struct_types called too early: more struct types "
                "were added"
            )

        else:
            entity = StructMetaclass.root_grammar_class.entity()
            env_md = StructMetaclass.env_metadata
            self._struct_types = [
                t for t in StructMetaclass.struct_types
                if t not in [env_md, entity]
            ]
            self._struct_types.insert(0, entity)
            self._struct_types.insert(0, env_md)

        return self._struct_types

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

        from langkit.parsers import Parser

        pass_manager = PassManager()
        pass_manager.add(
            MajorStepPass('Compiling the grammar'),
            GlobalPass('check main parsing rule',
                       self.grammar.check_main_rule),
            GlobalPass('warn on unreferenced parsing rules',
                       self.grammar.warn_unreferenced_parsing_rules),
            GrammarRulePass('compute fields types',
                            lambda p: p.compute_fields_types()),
            GlobalPass('compute types', CompileCtx.compute_types),
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
            GlobalPass('Compute uses envs attribute',
                       CompileCtx.compute_uses_env_attribute),
            ASTNodePass('check env spec properties',
                        lambda context, astnode:
                            astnode.env_spec
                            and astnode.env_spec.check_properties()),
            ASTNodePass('check resolved ASTnode subclasses',
                        lambda _, astnode: astnode.check_resolved()),
            GlobalPass('warn on unused private properties',
                       CompileCtx.warn_unused_private_properties),
            ASTNodePass('expose public structs and arrays types in APIs',
                        CompileCtx.expose_public_api_types,
                        auto_context=False),
            errors_checkpoint_pass,

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
            GlobalPass('compute ASTNode kind constants',
                       CompileCtx.compute_node_kind_constants),

            # Now that all Struct subclasses referenced by the grammar have
            # been typed, iterate over all declared subclasses to register the
            # ones that are unreachable from the grammar.  TODO: this kludge
            # will eventually disappear as part of OC22-016.
            GlobalPass('add structs to context',
                       CompileCtx.add_structs_to_context),
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
                  "include/langkit_support",
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
        with open(main_project_file, "w") as f:
            f.write(self.render_template(
                "project_file",
                lib_name=self.ada_api_settings.lib_name,
                os_path=os.path,
                quex_path=os.environ['QUEX_PATH'],
            ))

        # Copy langkit_support sources files to the include prefix and
        # create its own project file.
        from os.path import dirname, abspath, join
        lngk_support_dir = join(dirname(abspath(__file__)), "support")

        for f in itertools.chain(glob(join(lngk_support_dir, "*.adb")),
                                 glob(join(lngk_support_dir, "*.ads"))):
            shutil.copy(f, join(include_path, "langkit_support"))
        shutil.copy(join(lngk_support_dir, "langkit_support_installed.gpr"),
                    join(lib_path, "gnat", "langkit_support.gpr"))

        # Copy adalog files. TODO: This is kludgeish to the extreme, and is
        # only a workaround the fact you can't with regular projects from
        # library projects.
        adalog_dir = join(dirname(abspath(__file__)), "adalog")
        for f in glob(join(adalog_dir, "src", "*.ad*")):
            shutil.copy(f, join(include_path, lib_name_low))

        # Copy additional source files from the language specification
        for filepath in self.additional_source_files:
            filename = os.path.basename(filepath)
            shutil.copy(filepath, join(src_path, filename))

        with open(os.path.join(share_path, 'ast-types.html'), 'w') as f:
            from langkit import astdoc
            astdoc.write_astdoc(self, f)

        if self.verbosity.info:
            printcol("Generating sources... ", Colors.OKBLUE)

        ada_modules = [
            # Top (pure) package
            ("pkg_main",         [], False),
            # Unit for initialization primitives
            ("pkg_init",         ["init"], True),
            # Unit for analysis primitives
            ("pkg_analysis",     ["analysis"], True),
            # Unit for all parsers
            ("parsers/pkg_main", ["analysis", "parsers"], True),
            # Unit for the lexer
            ("lexer/pkg_lexer",  ["lexer"], True),
            # Unit for debug helpers
            ("pkg_debug",        ["debug"], True),
        ]

        for template_base_name, qual_name, has_body in ada_modules:
            self.write_ada_module(src_path, template_base_name, qual_name,
                                  has_body)

        with names.camel_with_underscores:
            write_ada_file(
                path.join(file_root, "src"), ADA_BODY, ["parse"],
                self.render_template("interactive_main_ada", _self=self)
            )

        with names.lower:
            # ... and the Quex C interface
            write_cpp_file(path.join(src_path, "quex_interface.h"),
                           self.render_template(
                               "lexer/quex_interface_header_c",
                               _self=self))
            write_cpp_file(path.join(src_path, "quex_interface.c"),
                           self.render_template(
                               "lexer/quex_interface_body_c",
                               _self=self))

        imain_project_file = os.path.join(file_root, "src", "mains.gpr")
        with open(imain_project_file, "w") as f:
            f.write(self.render_template(
                "mains_project_file",
                lib_name=self.ada_api_settings.lib_name,
                source_dirs=main_source_dirs,
                main_programs=main_programs
            ))

        # Emit C API
        self.emit_c_api(src_path, include_path)

        # Emit python API
        if self.python_api_settings:
            python_path = path.join(file_root, "python")
            if not path.exists(python_path):
                os.mkdir(python_path)
            self.emit_python_api(python_path)

            playground_file = os.path.join(file_root, "bin", "playground")
            with open(playground_file, "w") as f:
                f.write(self.render_template(
                    "python_api/playground_py",
                    module_name=self.python_api_settings.module_name
                ))

            os.chmod(playground_file, 0o775)

        # Emit GDB helpers initialization script
        if self.gdb_helpers_prefix:
            with open(os.path.join(file_root, 'gdbinit'), 'w') as f:
                f.write(self.render_template(
                    'gdb',
                    langkit_path=os.path.dirname(os.path.dirname(__file__)),
                    lib_name=self.ada_api_settings.lib_name.lower(),
                    astnode_names={node.name().lower
                                   for node in self.astnode_types},
                    prefix=self.gdb_helpers_prefix,
                ))

        # Add any sources in $lang_path/extensions/support if it exists
        if self.ext('support'):
            for f in glob(join(self.ext('support'), "*.ad*")):
                shutil.copy(f, src_path)

        if self.verbosity.info:
            printcol("Compiling the quex lexer specification", Colors.OKBLUE)

        quex_file = os.path.join(src_path,
                                 "{}.qx".format(self.lang_name.lower))
        quex_spec = self.lexer.emit()
        with open(quex_file, 'w') as f:
            f.write(quex_spec)

        # Generating the lexer C code with Quex is quite long: do it only when
        # the Quex specification changed from last build.
        if generate_lexer and self.cache.is_stale('quex_specification',
                                                  quex_spec):
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
            return self.render_template(template_name, _self=self)

        with names.lower:
            write_cpp_file(
                path.join(include_path,
                          "{}.h".format(self.c_api_settings.lib_name)),
                render("c_api/header_c")
            )

        self.write_ada_module(src_path, "c_api/pkg_analysis",
                              ["Analysis", "C"])

    def emit_python_api(self, python_path):
        """
        Generate the Python binding module.

        :param str python_path: The directory in which the Python module will
            be generated.
        """

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

        module_filename = "{}.py".format(self.python_api_settings.module_name)

        with names.camel:
            with open(os.path.join(python_path, module_filename), "w") as f:
                code = self.render_template(
                    "python_api/module_py", _self=self,
                    c_api=self.c_api_settings,
                    pyapi=self.python_api_settings,
                )

                # If pretty-printing failed, write the original code anyway in
                # order to ease debugging.
                try:
                    pp_code = pretty_print(strip_white_lines(code))
                except SyntaxError:
                    f.write(code)
                    raise
                f.write(pp_code)

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
                      key=lambda (s, n): n)

    def finalize_symbol_literals(self):
        """
        Collect all symbol literals provided to "add_symbol_literal" and create
        the "symbol_literals" mapping out of it.
        """
        assert isinstance(self._symbol_literals, set)
        symbols = self._symbol_literals
        self._symbol_literals = None

        i = 1
        for name in sorted(symbols):
            # Create a candidate name for this symbol: replace all
            # non-alphabetic characters with underscores and remove
            # leading/trailing/consecutive underscores.
            candidate_lower_name = ''
            last_is_alpha = False
            for c in name.lower():
                if 'a' <= c <= 'z':
                    candidate_lower_name += c
                    last_is_alpha = True
                else:
                    if last_is_alpha:
                        candidate_lower_name += '_'
                    last_is_alpha = False

            candidate_name = names.Name.from_lower(
                candidate_lower_name.strip('_')
            )

            # If we have no candidate or if the candidate is already used, fall
            # back to an unique number.
            if not candidate_name or candidate_name in self.symbol_literals:
                enum_name = names.Name(str(i))
                i += 1
            else:
                enum_name = candidate_name

            self.symbol_literals[name] = names.Name('Symbol') + enum_name

    def annotate_fields_types(self):
        """
        Modify the Python files where the node types are defined, to annotate
        empty Field() definitions.
        """
        # Only import lib2to3 if the users needs it
        import lib2to3.main

        astnodes_files = {
            path.abspath(inspect.getsourcefile(n))
            for n in self.astnode_types
        }

        lib2to3.main.main(
            "langkit",
            ["-f", "annotate_fields_types",
             "--no-diff", "-w"] + list(astnodes_files)
        )

    def compute_node_kind_constants(self):
        """
        Compute kind constants for all ASTNode concrete subclasses.
        """
        for i, astnode in enumerate(
            (astnode
             for astnode in self.astnode_types
             if not astnode.abstract),
            # Start with 1: the constant 0 is reserved as an
            # error/uninitialized code.
            start=1
        ):
            self.node_kind_constants[astnode] = i

    def add_structs_to_context(self):
        """
        Make sure all Struct subclasses (including ASTNode ones) are added to
        the context.
        """
        for t in self.struct_types + self.astnode_types:
            t.add_to_context()

    def expose_public_api_types(self, astnode):
        """
        Tag all struct and array types referenced by the public API as exposed.
        This also emits non-blocking errors for all types that are exposed in
        the public API whereas they should not.
        """
        from langkit.compiled_types import ArrayType, Struct

        # All code must ignore _exposed attributes when the following is true
        if self.library_fields_all_public:
            return

        def expose(t, for_field, type_use, traceback):
            """
            Recursively tag "t" and all the types it references as exposed.
            """
            if t._exposed:
                return

            if issubclass(t, Struct) and not t.is_ast_node():
                for f in t.get_abstract_fields(include_inherited=False):
                    expose(f.type, f, 'type', traceback + [f.qualname])

            elif issubclass(t, ArrayType):
                expose(t.element_type(), for_field, 'element type',
                       traceback + ['array of {}'.format(t.name().camel)])

            else:
                # Only struct and array types have their "_exposed" attribute
                # inferred. We consider all other ones to have a static value,
                # so complain if we reach a type that must not be exposed.
                with for_field.diagnostic_context():
                    text_tb = (
                        ' (from: {})'.format(
                            ' -> '.join(traceback[:-1])
                        ) if len(traceback) > 1 else ''
                    )
                    check_source_language(
                        t._exposed,
                        "{} is {}, which is forbidden in public API{}".format(
                            type_use, t.name().camel, text_tb
                        ),
                        severity=Severity.non_blocking_error
                    )
                return

            t._exposed = True

        for f in astnode.get_abstract_fields(
            predicate=lambda f: f.is_public,
            include_inherited=False
        ):
            expose(f.type, f,
                   'return type' if f.is_property else 'type',
                   [f.qualname])
            for arg in f.explicit_arguments:
                expose(arg.type, f, '"{}" argument'.format(arg.name),
                       [f.qualname])
