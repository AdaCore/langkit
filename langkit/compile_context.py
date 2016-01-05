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

from collections import defaultdict
from distutils.spawn import find_executable
from glob import glob
import itertools
import os
from os import path, environ
import shutil
import subprocess
import sys

from langkit import astdoc, caching, names
from langkit.ada_api import AdaAPISettings
from langkit.c_api import CAPISettings
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


class CompileCtx():
    """State holder for native code emission."""

    def __init__(self, lang_name, main_rule_name, lexer, grammar,
                 lib_name=None,
                 c_symbol_prefix=None,
                 enable_python_api=True,
                 verbose=False):
        """Create a new context for code emission.

        :param str lang_name: string (mixed case and underscore: see
            langkit.names.Name) for the Name of the target language.

        :param str main_rule_name: Name for the grammar rule that will be used
            as an entry point when parsing units.

        :param lexer: A lexer for the target language.
        :type lexer: langkit.lexer.Lexer

        :param grammar: A grammar for the target language.
        :type grammar: langkit.parsers.Grammer

        :param lib_name: If provided, must be a string (mixed case and
            underscore: see langkit.names.Name), otherwise set to
            "Lib<lang_name>lang". It is used for the filenames, package names,
            etc.  in the generated library.
        :type lib_name: str or None

        :param c_symbol_prefix: Valid C identifier used as a prefix for all
            top-level declarations in the generated C API.  If not provided,
            set to the name of the language in lower case.  Empty string stands
            for no prefix.
        :type c_symbol_prefix: str or None

        :param bool enable_python_api: If True (which is the default),
            generates a Python API for the generated library.

        :param bool verbose: If True (which is not the default), print various
            debug messages on standard output.
        """
        from langkit.python_api import PythonAPISettings

        self.lang_name = names.Name(lang_name)
        self.main_rule_name = main_rule_name

        lib_name = (
            names.Name('Lib{}lang'.format(
                self.lang_name.camel_with_underscores
            ))
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
        self.verbose = verbose

        # Mapping: rule name -> Parser instances.
        # TODO: why do we need this? The grammar already has such a mapping.
        self.rules_to_fn_names = {}

        self.lexer = lexer
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

        :type: set[names.Name]
        """

        self.types = set()
        """
        Set of CompiledType subclasses: all such subclasses must register
        themselves here when their add_to_context method is invoked. This
        field too is used to avoid multiple generation issues.

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

        self.list_types = set()
        """
        Set of all ASTNode subclasses (ASTNode included) for which we
        generate a corresponding list type.

        :type: set[langkit.compiled_types.ASTNode]
        """

        self.array_types = set()
        """
        Set of all CompiledType for which we generate a corresponding array
        type.

        :type: set[langkit.compiled_types.CompiledType]
        """

        #
        # Holders for the Ada generated code chunks
        #

        self.enum_declarations = []
        """
        List of TypeDeclaration instances for all enumeration types used in
        AST node fields and all ASTNode subclasses.

        :type: list[langkit.compiled_types.TypeDeclaration]
        """

        self.array_types_declarations = []
        """
        List of types for all array types.

        :type: list[langkit.compiled_types.CompiledTypes]
        """

        self.struct_types_declarations = []
        """
        List of TypeDeclaration instances for all Struct derivations
        (excluding ASTNode derivations). These contain the type full
        declarations.

        :type: list[langkit.compiled_types.TypeDeclaration]
        """

        self.astnode_types_declarations = []
        """
        List of TypeDeclaration instances for all ASTNode derivations
        (excluding ASTList derivations). These contain the type full
        declarations.

        :type: list[langkit.compiled_types.TypeDeclaration]
        """

        self.incomplete_types_declarations = []
        """
        List of TypeDeclaration instances for all ASTNode derivations. These
        only contain forward declarations so that full declarations have
        access to all declared types.

        :type: list[langkit.compiled_types.TypeDeclaration]
        """

        self.list_types_declarations = []
        """
        List of TypeDeclaration instances for all ASTList derivations. These
        don't need forward declarations.

        :type: list[langkit.compiled_types.TypeDeclaration]
        """

        self.primitives_bodies = []
        """
        List of strings for all ASTNode subclasses primitives body

        :type: list[str]
        """

        self.generated_parsers = []
        ":type: list[langkit.parsers.GeneratedParser]"

        #
        # Holders for the C external API generated code chunks
        #

        self.node_kind_constants = {}
        """
        Mapping: ASTNode concrete (i.e. non abstract) subclass -> int,
        associating specific constants to be used reliably in bindings.  This
        mapping is built at the beginning of code emission.

        :type: dict[langkit.compiled_types.ASTNode, int]
        """

        self.c_astnode_primitives = defaultdict(list)
        """
        Mapping: ASTNode subclass -> GeneratedFunction instances for all
        subclass field accessors.

        :type: dict[langkit.compiled_types.ASTNode,
                    list[langkit.compiled_types.GeneratedFunction]]
        """

        self.c_astnode_field_types = {}
        """
        Mapping: CompiledType -> string (C declarations) for types used in
        AST node fields.

        :type: dict[langkit.compiled_types.CompiledType, str]
        """

        self.c_astnode_field_types_ada = {}
        """
        Likewise but for Ada declarations

        :type: dict[langkit.compiled_types.CompiledType, str]
        """

        self.c_array_types = {}
        """
        Mapping: CompiledType -> string (C declarations) for all array types

        :type: dict[langkit.compiled_types.CompiledType, str]
        """

        self.c_array_types_ada = {}
        """
        Likewise but for Ada declarations.

        :type: dict[langkit.compiled_types.CompiledType, str]
        """

        #
        # Corresponding holders for the Python API
        #

        self.py_struct_classes = {}
        """
        Mapping: Struct subclass -> string (generated Python code Struct
        subclass declarations).

        :type: dict[langkit.compiled_types.Struct, str]
        """

        self.py_field_types = {}
        """
        Mapping CompiledType -> string (Python declarations) for types used
        in Struct fields.

        :type: dict[langkit.compiled_types.CompiledType, str]
        """

        self.py_array_types = {}
        """
        Mapping: CompiledType -> string (Python declarations) for all array
        types.

        :type: dict[langkit.compiled_types.CompiledType, str]
        """

        self.cache = None

        # Internal field for extensions directory
        self._extensions_dir = None

    def compute_types(self):
        """
        Compute various information related to compiled types, that needs to be
        available for code generation.
        """

        # Get the list of ASTNode types from the Struct metaclass
        from langkit.compiled_types import StructMetaClass

        # Skipping the first element which is ASTNode, because it is not a
        # real type in the generated library.
        self.astnode_types = list(StructMetaClass.astnode_types)[1:]

        # Skipping the first element which is Struct, because it is not a
        # real type in the generated library.
        self.struct_types = list(StructMetaClass.struct_types)[1:]

        self.root_grammar_class = StructMetaClass.root_grammar_class

        # Sort them in dependency order as required but also then in
        # alphabetical order so that generated declarations are kept in a
        # relatively stable order. This is really useful for debugging
        # purposes.
        keys = {
            cls: ".".join(cls.name().base_name
                          for cls in cls.get_inheritance_chain())
            for cls in self.astnode_types
        }
        self.astnode_types.sort(key=lambda cls: keys[cls])

    def compute_properties(self):
        """
        Compute information related to ASTNode's properties. This needs to be a
        global analysis because we want to compute which properties need to be
        dispatching, and this is determined not only by the context of one
        node, but by whether the parent has a property with the same name.
        """
        for astnode_type in self.astnode_types:
            for prop in astnode_type.get_properties(include_inherited=False):
                prop.compute(astnode_type)

    def render_template(self, *args, **kwargs):
        # Kludge: to avoid circular dependency issues, do not import parsers
        # until needed.
        # TODO: If the render method was dynamically bound, like the compile
        # context, rather than being explicitly redefined in every module, we
        # could avoid this, maybe.
        from langkit.parsers import render
        return render(*args, **kwargs)

    def emit(self, file_root="."):
        global compile_ctx
        try:
            compile_ctx = self
            self._emit(file_root)
        finally:
            compile_ctx = None

    def write_ada_module(self, out_dir, template_base_name, qual_name,
                         in_library=True):
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

        :param bool in_library: If true, the module will be considered as
            part of the generated library hierarchy, and will be a child of the
            parent package.
        """
        for kind in [ADA_SPEC, ADA_BODY]:
            with names.camel_with_underscores:
                write_ada_file(
                    out_dir=out_dir,
                    source_kind=kind,
                    qual_name=([self.ada_api_settings.lib_name] if in_library
                               else []) + qual_name,
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

    def _emit(self, file_root):
        """
        Emit native code for all the rules in this grammar as a library:
        a library specification and the corresponding implementation.  Also
        emit a tiny program that can parse starting with any parsing rule for
        testing purposes.
        """
        assert self.grammar, "Set grammar before calling emit"

        # Compute type information, so that it is available for further
        # compilation stages.
        self.compute_types()

        # Compute properties information, so that it is available for further
        # compilation stages.
        self.compute_properties()

        lib_name_low = self.ada_api_settings.lib_name.lower()

        include_path = path.join(file_root, "include")
        src_path = path.join(file_root, "include", lib_name_low)
        lib_path = path.join(file_root, "lib")
        share_path = path.join(file_root, "share", lib_name_low)

        if not path.exists(file_root):
            os.mkdir(file_root)

        printcol("File setup ...", Colors.OKBLUE)

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
                quex_path=os.environ["QUEX_PATH"],
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

        printcol("Compiling the grammar...", Colors.OKBLUE)

        with names.camel_with_underscores:
            for r_name, r in self.grammar.rules.items():
                r.compute_fields_types()

            for r_name, r in self.grammar.rules.items():
                r.compile()
                self.rules_to_fn_names[r_name] = r

        for astnode_type in self.astnode_types:
            assert astnode_type.is_type_resolved, (
                "ASTNode subclass {} is not type resolved. It is probably "
                "not used by the grammar, and its type not annotated".format(
                    astnode_type.name()
                )
            )

        for i, astnode in enumerate(
            (astnode
             for astnode in self.astnode_types
             if not astnode.abstract),
            # Compute kind constants for all ASTNode concrete subclasses.
            # Start with 2: the constant 0 is reserved as an
            # error/uninitialized code and the constant 1 is reserved for all
            # ASTList nodes.
            start=2
        ):
            self.node_kind_constants[astnode] = i

        with file(os.path.join(share_path, 'ast-types.txt'), 'w') as f:
            astdoc.write_astdoc(self, f)

        # Now that all Struct subclasses referenced by the grammar have been
        # typed, iterate over all declared subclasses to register the ones that
        # are unreachable from the grammar.  TODO: this kludge will eventually
        # disappear as part of OC22-016.
        for t in self.astnode_types + self.struct_types:
            t.add_to_context()

        printcol("Generating sources... ", Colors.OKBLUE)

        self.write_ada_module(src_path, "root_ast_type", ["ast"],
                              in_library=False)
        self.write_ada_module(src_path, "root_ast_list_type", ["ast", "list"],
                              in_library=False)

        ada_modules = [
            # unit for all derived AST nodes
            ("main", []),
            # unit for the lexer
            ("lexer/lexer", ["lexer"]),
            # unit for all parsers
            ("parsers/main", ["parsers"]),
        ]

        for template_base_name, qual_name in ada_modules:
            self.write_ada_module(src_path, template_base_name, qual_name)

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

        imain_project_file = os.path.join(file_root, "src", "parse.gpr")
        with open(imain_project_file, "w") as f:
            f.write(self.render_template(
                "parse_project_file",
                lib_name=self.ada_api_settings.lib_name,
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

        printcol("Compiling the quex lexer specification", Colors.OKBLUE)

        quex_file = os.path.join(src_path,
                                 "{}.qx".format(self.lang_name.lower))
        quex_spec = self.lexer.emit()
        with open(quex_file, 'w') as f:
            f.write(quex_spec)

        # Generating the lexer C code with Quex is quite long: do it only when
        # the Quex specification changed from last build.
        if self.cache.is_stale('quex_specification', quex_spec):
            quex_py_file = path.join(environ["QUEX_PATH"], "quex-exe.py")
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

        self.write_ada_module(src_path, "c_api/", ["C"])

    def emit_python_api(self, python_path):
        """
        Generate the Python binding module.

        :param str python_path: The directory in which the Python module will
            be generated.
        """
        module_filename = "{}.py".format(self.python_api_settings.module_name)

        # Collect ASTNode subclass declarations preserving "astnode_types"'s
        # order so that dependencies comes first.
        astnode_subclass_decls = filter(bool, [
            self.py_struct_classes.get(cls, None)
            for cls in self.astnode_types + self.struct_types
        ])

        with names.camel:
            with open(os.path.join(python_path, module_filename), "w") as f:
                f.write(self.render_template(
                    "python_api/module_py", _self=self,
                    c_api=self.c_api_settings,
                    pyapi=self.python_api_settings,
                    astnode_subclass_decls=astnode_subclass_decls,
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
        Return an extension file's absolute path, given strings/names
        arguments, so that you can do::

            ext('a', 'b', 'c')
            # returns '$lang_dir/extensions/a/b/c'

        :param [str|names.Name] args: The list of components to constitute the
                                      extension's path.

        :rtype: str
        """
        args = [a.lower if isinstance(a, names.Name) else a for a in args]
        if self.extensions_dir:
            ret = os.path.join(self.extensions_dir, *args)
            return ret if os.path.isfile(ret) else None
