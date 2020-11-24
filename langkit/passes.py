"""
Helpers to manage compilation passes.
"""

from __future__ import annotations

from typing import Callable, List, TYPE_CHECKING

from langkit.compiled_types import ASTNodeType, CompiledTypeRepo
from langkit.diagnostics import errors_checkpoint
from langkit.emitter import Emitter
from langkit.envs import EnvSpec
from langkit.expressions import PropertyDef
from langkit.lexer import Lexer
from langkit.parsers import Grammar, Parser
from langkit.utils import Colors, printcol


# Do this only during typing to avoid circular dependencies
if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx


class PassManager:
    """
    Holder for compilation passes. Handles passes sequential execution.
    """

    frozen: bool
    """
    If true, adding new passes is forbidden.
    """

    passes: List[AbstractPass]
    """
    List of passes to run.
    """

    def __init__(self) -> None:
        self.frozen = False
        self.passes = []

    def add(self, *passes: AbstractPass) -> None:
        """
        Add the given passes to the execution pipeline.

        :param passes: Pass to append.
        """
        assert not self.frozen, ('Invalid attempt to add a pass after pipeline'
                                 ' execution')
        self.passes.extend(passes)

    def run(self, context: CompileCtx) -> None:
        """
        Run through the execution pipeline.
        """
        assert not self.frozen, 'Invalid attempt to run the pipeline twice'
        self.frozen = True

        for p in self.passes:
            if p.disabled:
                if context.verbosity.debug:
                    printcol('Skipping pass: {}'.format(p.name), Colors.YELLOW)
                continue
            if isinstance(p, StopPipeline):
                if context.verbosity.info:
                    printcol('Stopping pipeline execution: {}'.format(p.name),
                             Colors.OKBLUE)
                return
            else:
                if (not isinstance(p, MajorStepPass)
                        and context.verbosity.debug):  # no-code-coverage
                    printcol('Running pass: {}'.format(p.name), Colors.YELLOW)
                p.run(context)


class AbstractPass:
    """
    Base class for all specialized compilation passes.

    Subclasses are required only to override the "run" method.
    """

    name: str
    """
    Informative name for users to be used in logging.
    """

    disabled: bool
    """
    If True, do not run this pass. This makes it convenient to selectively
    disable passes in big PassManager.add calls.
    """

    is_optional: bool
    """
    If true, the pass can be activated/deactivated via the command line
    arguments.
    """

    doc: str

    def __init__(self, name: str, disabled: bool = False) -> None:
        self.name = name
        self.disabled = disabled
        self.is_optional = False

    def optional(self, doc: str, disabled: bool = True) -> AbstractPass:
        """
        Expression chain method to make a pass optional. Make this pass
        optional, with assorted doc, and return it, so that it's easy to use in
        an expression context.
        """
        from langkit.documentation import format_text

        self.is_optional = True
        self.disabled = disabled
        self.doc = format_text(doc, 4)
        return self

    def run(self, context: CompileCtx) -> None:
        raise NotImplementedError()


class MajorStepPass(AbstractPass):
    """
    Dummy concrete pass that just prints a message.

    This is useful to display global compilation progression to users.
    """

    message: str
    """
    Message to display.
    """

    def __init__(self, message: str) -> None:
        super().__init__('')
        self.message = message

    def run(self, context: CompileCtx) -> None:
        if context.verbosity.info:
            printcol('{}...'.format(self.message), Colors.OKBLUE)


class GlobalPass(AbstractPass):
    """
    Concrete pass to run on the context itself.
    """

    def __init__(self,
                 name: str,
                 pass_fn: Callable[[CompileCtx], None],
                 disabled: bool = False) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        self.pass_fn(context)


class EmitterPass(AbstractPass):
    """
    Concrete pass to run on the emitter (for code generation).
    """

    def __init__(self,
                 name: str,
                 pass_fn: Callable[[Emitter, CompileCtx], None],
                 disabled: bool = False) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        self.pass_fn(context.emitter, context)


class EmbedIpythonPass(AbstractPass):  # no-code-coverage
    """
    Small utility pass that allows to embed an IPython interpreter at a given
    point in the pipeline. This can be useful to inspect the state of
    compilation and data structures for example.
    """

    def __init__(self) -> None:
        super().__init__("Embed IPython")

    def run(self, context: CompileCtx) -> None:
        from IPython import embed
        embed(header="Langkit debug prompt")


class LexerPass(AbstractPass):
    """
    Concrete pass to run on the whole lexer.
    """

    def __init__(self,
                 name: str,
                 pass_fn: Callable[[Lexer, CompileCtx], None],
                 disabled: bool = False) -> None:
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass.
        :type (langkit.lexer.Lexer, langkit.compile_context.CompileCtx) -> None

        :param bool disabled: See AbstractPass.
        """
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        self.pass_fn(context.lexer, context)


class GrammarPass(AbstractPass):
    """
    Concrete pass to run on the whole grammar.
    """

    def __init__(self,
                 name: str,
                 pass_fn: Callable[[Grammar, CompileCtx], None],
                 disabled: bool = False) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        self.pass_fn(context.grammar, context)


class GrammarRulePass(AbstractPass):
    """
    Concrete pass to run on each grammar rule.
    """

    def __init__(self,
                 name: str,
                 pass_fn: Callable[[Parser], None],
                 disabled: bool = False) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        # Sort grammar rules by name, so that the pass order is deterministic
        for name, rule in sorted(context.grammar.rules.items()):
            with rule.diagnostic_context:
                self.pass_fn(rule)


class ASTNodePass(AbstractPass):
    """
    Concrete pass to run on each ASTNodeType subclass.
    """

    auto_context: bool
    """
    If True, setup a diagnostic context for the current AST node.
    """

    def __init__(self,
                 name: str,
                 pass_fn: Callable[[CompileCtx, ASTNodeType], None],
                 disabled: bool = False,
                 auto_context: bool = True) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn
        self.auto_context = auto_context

    def run(self, context: CompileCtx) -> None:
        for astnode in context.astnode_types:
            if self.auto_context:
                with astnode.diagnostic_context:
                    self.pass_fn(context, astnode)
            else:
                self.pass_fn(context, astnode)


class EnvSpecPass(AbstractPass):
    """
    Concrete pass to run on each EnvSpec instance.
    """

    iter_metaclass: bool
    """
    If True, iterate on the AST nodes in CompiledTypeRepo.astnode_types.
    Otherwise, iterate on the context's list of AST node types.
    """

    def __init__(self,
                 name: str,
                 pass_fn: Callable[[EnvSpec, CompileCtx], None],
                 disabled: bool = False,
                 iter_metaclass: bool = False) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn
        self.iter_metaclass = iter_metaclass

    def run(self, context: CompileCtx) -> None:
        astnode_types = (CompiledTypeRepo.astnode_types
                         if self.iter_metaclass else
                         context.astnode_types)
        for astnode in astnode_types:
            env_spec = astnode.env_spec
            if env_spec is None:
                continue
            self.pass_fn(env_spec, context)


class PropertyPass(AbstractPass):
    """
    Concrete pass to run on each PropertyDef instance.
    """

    def __init__(self,
                 name: str,
                 pass_fn: Callable[[PropertyDef, CompileCtx], None],
                 disabled: bool = False) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        for prop in context.all_properties(include_inherited=False):
            with prop.diagnostic_context:
                self.pass_fn(prop, context)


class StopPipeline(AbstractPass):
    """
    Dummy concrete pass to abort the execution of a pipeline.

    Used with the "disabled" attribute, this is useful to conditionnaly limit
    the set of passes to be executed.
    """
    pass


errors_checkpoint_pass = GlobalPass('errors checkpoint',
                                    lambda _: errors_checkpoint())
