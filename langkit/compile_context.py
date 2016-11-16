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

from __future__ import absolute_import

from contextlib import contextmanager
import difflib
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
from langkit.diagnostics import (
    Severity, check_source_language, errors_checkpoint
)
from langkit.expressions import PropertyDef
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


class CompileCtx():
    """State holder for native code emission."""

    def __init__(self, lang_name, lexer, grammar,
                 lib_name=None,
                 library_fields_all_public=False,
                 c_symbol_prefix=None,
                 enable_python_api=True,
                 default_charset='utf-8',
                 verbosity=Verbosity('none'),
                 template_lookup_extra_dirs=None,
                 env_hook_subprogram=None):
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

        :param (str, str)|None env_hook_subprogram: If provided, define a
            subprogram to call as the environment hook. The first string is the
            name of the Ada unit in which this subprogram is defined. The
            second one is the name of the subprogram itself.

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
        self.c_api_settings = CAPISettings(lib_name.lower,
                                           (self.lang_name.lower
                                            if c_symbol_prefix is None else
                                            c_symbol_prefix))

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

        # Mapping: rule name -> Parser instances.
        # TODO: why do we need this? The grammar already has such a mapping.
        self.rules_to_fn_names = {}

        self.lexer = lexer
        ":type: langkit.lexer.Lexer"

        self.grammar = grammar
        ":type: langkit.parsers.Grammar"

        self.main_rule_name = grammar.main_rule_name

        self.python_api_settings = (
            PythonAPISettings(lib_name.lower, self.c_api_settings)
            if enable_python_api else None
        )

        self.fns = set()
        """
        Set of names (names.Name instances) for all generated parser
        functions. This is used to avoid generating these multiple times.

        :type: set[names.Name]
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

        self.struct_types = []
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

        self.env_element = None
        """
        Shortcut to the EnvElement class, so that it can easily be used in
        templates.

        :type: langkit.compiled_types.EnvElement
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

        self.env_element = None
        """
        Shortcut for langkit.compiled_types.EnvElement. Initialized
        during the typing pass.

        :type: langkit.compiled_types.EnvElement
        """

        self.annotate_fields_types = False
        """
        Whether to run the 2to3 field annotation pass.
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
            for name, parser in self.rules_to_fn_names.items()
            if issubclass(parser.get_type(), ASTNode)
        )

    def compute_types(self):
        """
        Compute various information related to compiled types, that needs to be
        available for code generation.
        """

        # Get the list of ASTNode types from the Struct metaclass
        from langkit.compiled_types import (
            EnvElement, LexicalEnvType, StructMetaclass
        )

        self.astnode_types = list(StructMetaclass.astnode_types)

        # Here we're skipping Struct because it's not a real type in
        # generated code. We're also putting env_metadata and EnvElement in
        # the beginning and in the right dependency order (the metadata
        # type before the env element type).
        # TODO: Using a dependency order topological sort wouldn't hurt at
        # some point.
        self.struct_types = [
            t for t in StructMetaclass.struct_types
            if t not in [EnvElement, StructMetaclass.env_metadata]
        ]
        self.struct_types.insert(0, EnvElement)

        if StructMetaclass.env_metadata:
            self.struct_types = (
                [StructMetaclass.env_metadata] + self.struct_types
            )

        self.root_grammar_class = StructMetaclass.root_grammar_class
        self.generic_list_type = self.root_grammar_class.generic_list_type
        self.env_metadata = StructMetaclass.env_metadata
        self.env_element = EnvElement

        # The Group lexical environment operation takes an array of lexical
        # envs, so we always need to generate the corresponding array type.
        self.array_types.add(LexicalEnvType.array_type())

        # Likewise for the EnvElement array type: LexicalEnv.get returns it.
        # No need to bind anything if the language specification did not
        # specify any EnvElement, though.
        if self.env_element:
            self.array_types.add(EnvElement.array_type())

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

    def compute_properties(self):
        """
        Compute information related to ASTNode's properties. This needs to be a
        global analysis because we want to compute which properties need to be
        dispatching, and this is determined not only by the context of one
        node, but by whether the parent has a property with the same name.
        """

        for pass_fn in PropertyDef.compilation_passes():
            for astnode in self.astnode_types:
                for prop in astnode.get_properties(include_inherited=False):
                    with prop.diagnostic_context():
                        pass_fn(prop)

                if astnode.env_spec:
                    # Env specs generate properties, and have some invariants
                    # to check after properties have been properly computed.
                    astnode.env_spec.prepare()

    def render_template(self, *args, **kwargs):
        # Kludge: to avoid circular dependency issues, do not import parsers
        # until needed.
        # TODO: If the render method was dynamically bound, like the compile
        # context, rather than being explicitly redefined in every module, we
        # could avoid this, maybe.
        from langkit.parsers import render
        return render(*args, **kwargs)

    def emit(self, file_root='.', generate_lexer=True, main_programs=set(),
             annotate_fields_types=False):
        """
        Generate sources for the analysis library. Also emit a tiny program
        useful for testing purposes.

        :param str file_root: (optional) Path of the directory in which the
            library should be generated. The default is the current directory.

        :param bool generate_lexer: (optional) Whether to invoke Quex to
            generate the lexer source code. Will do by default. As this can
            take time, it is useful to disable it during testing.

        :param set[str] main_programs: List of names for programs to build in
            addition to the generated library. To each X program, there must be
            a X.adb source file in the $BUILD/src directory.

        :param bool annotate_fields_types: Whether to try and annotate the
            type of fields in the grammar. If this is True, this will
            actually modify the file in which ASTNode subclasses are
            defined, and annotate empty field definitions.
        """
        dir_path = path.join(
            path.dirname(path.realpath(__file__)), "templates"
        )
        template_utils.template_lookup = TemplateLookup(
            directories=keep([dir_path, self.extensions_dir]
                             + self.template_lookup_extra_dirs),
            strict_undefined=True
        )

        # Automatically add all source files in the "extensions/src" directory
        # to the generated library project.
        if self.extensions_dir:
            src_dir = path.join(self.extensions_dir, 'src')
            if path.isdir(src_dir):
                for filename in os.listdir(src_dir):
                    filepath = path.join(src_dir, filename)
                    if path.isfile(filepath) and not filename.startswith("."):
                        self.additional_source_files.append(filepath)

        self.annotate_fields_types = annotate_fields_types
        self.compile()
        with global_context(self):
            self._emit(file_root, generate_lexer, main_programs)

    def compile(self):
        with global_context(self):
            self._compile()

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
                        _self=self
                    )
                )

    def _compile(self):
        """
        Compile the language specification: perform legality checks and type
        inference.
        """
        # Compile the first time, do nothing next times
        if self.compiled:
            return
        self.compiled = True

        assert self.grammar, "Set grammar before compiling"

        if not self.grammar.rules.get(self.main_rule_name, None):
            close_matches = difflib.get_close_matches(
                self.main_rule_name, self.grammar.rules.keys()
            )

            with self.grammar.context():
                check_source_language(
                    False,
                    'Invalid rule name specified for main rule: "{}". '
                    '{}'.format(
                        self.main_rule_name,
                        'Did you mean "{}"?'.format(close_matches[0])
                        if close_matches else ""
                    )
                )

        unreferenced_rules = self.grammar.get_unreferenced_rules()

        check_source_language(
            not unreferenced_rules, "The following parsing rules are not "
            "used: {}".format(", ".join(sorted(unreferenced_rules))),
            severity=Severity.warning
        )

        if self.verbosity.info:
            printcol("Compiling the grammar...", Colors.OKBLUE)

        with names.camel_with_underscores:
            # Compute the type of fields for types used in the grammar
            for r_name, r in self.grammar.rules.items():
                r.compute_fields_types()

        # Compute type information, so that it is available for further
        # compilation stages.
        self.compute_types()
        errors_checkpoint()

        with names.camel_with_underscores:
            # Compute properties information, so that it is available for
            # further compilation stages.
            self.compute_properties()
            errors_checkpoint()

            for r_name, r in self.grammar.rules.items():
                r.compile()
                self.rules_to_fn_names[r_name] = r

        unresolved_types = set([t for t in self.astnode_types
                                if not t.is_type_resolved])
        check_source_language(
            not unresolved_types,
            "The following ASTNode subclasses are not type resolved. They are"
            " not used by the grammar, and their types not annotated:"
            " {}".format(", ".join(t.name().camel for t in unresolved_types))
        )

        astnodes_files = {
            path.abspath(inspect.getsourcefile(n)) for n in self.astnode_types
        }

        if self.annotate_fields_types:
            # Only import lib2to3 if the users needs it
            import lib2to3.main

            lib2to3.main.main(
                "langkit",
                ["-f", "annotate_fields_types",
                 "--no-diff", "-w"] + list(astnodes_files)
            )

        for i, astnode in enumerate(
            (astnode
             for astnode in self.astnode_types
             if not astnode.abstract),
            # Compute kind constants for all ASTNode concrete subclasses.
            # Start with 1: the constant 0 is reserved as an
            # error/uninitialized code.
            start=1
        ):
            self.node_kind_constants[astnode] = i

        # Now that all Struct subclasses referenced by the grammar have been
        # typed, iterate over all declared subclasses to register the ones that
        # are unreachable from the grammar.  TODO: this kludge will eventually
        # disappear as part of OC22-016.
        for t in self.struct_types + self.astnode_types:
            t.add_to_context()

        errors_checkpoint()

    def _emit(self, file_root, generate_lexer, main_programs):
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

        with file(os.path.join(share_path, 'ast-types.txt'), 'w') as f:
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
            ("pkg_analysis_interfaces", ["analysis_interfaces"], True),
            # Unit for analysis unit conversions hack
            ("pkg_analysis_internal", ["analysis", "internal"], False),
            # Unit for analysis primitives
            ("pkg_analysis",     ["analysis"], True),
            # Unit for the root AST node
            ("pkg_ast",          ["ast"], True),
            # Unit for generic AST lists
            ("pkg_ast_list",     ["ast", "list"], True),
            # Unit for all derived AST nodes
            ("pkg_ast_types",    ["ast", "types"], True),
            # Unit for all parsers
            ("parsers/pkg_main", ["ast", "types", "parsers"], True),
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
                main_programs=main_programs
            ))

        self.emit_c_api(src_path, include_path)
        if self.python_api_settings:
            python_path = path.join(file_root, "python")
            if not path.exists(python_path):
                os.mkdir(python_path)
            self.emit_python_api(python_path)

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
                                   "--token-policy", "single"],
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
        self.write_ada_module(src_path, "c_api/pkg_ast", ["AST", "C"])
        self.write_ada_module(src_path, "c_api/pkg_ast_types",
                              ["AST", "Types", "C"])

    def emit_python_api(self, python_path):
        """
        Generate the Python binding module.

        :param str python_path: The directory in which the Python module will
            be generated.
        """
        module_filename = "{}.py".format(self.python_api_settings.module_name)

        with names.camel:
            with open(os.path.join(python_path, module_filename), "w") as f:
                f.write(self.render_template(
                    "python_api/module_py", _self=self,
                    c_api=self.c_api_settings,
                    pyapi=self.python_api_settings,
                ))

    @property
    def extensions_dir(self):
        """
        Returns the absolute path to the extension dir, if it exists on the
        disk, or None.
        """
        return self._extensions_dir

    @extensions_dir.setter
    def extensions_dir(self, ext_dir):
        # only set the extensions dir if this directory exists
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
