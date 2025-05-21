"""
Helpers to manage compilation passes.
"""

from __future__ import annotations

import abc
import argparse
from typing import Any, Callable, Iterable, Sequence, TYPE_CHECKING

from langkit.compile_context import Verbosity
from langkit.compiled_types import ASTNodeType
from langkit.diagnostics import Location, error, errors_checkpoint
from langkit.emitter import Emitter
from langkit.envs import EnvSpec
from langkit.expressions import PropertyDef
from langkit.lexer import Lexer
from langkit.parsers import Grammar, Parser
from langkit.utils import Colors, PluginLoader, PluginLoadingError, printcol


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

    passes: list[AbstractPass]
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
        assert not self.frozen, (
            "Invalid attempt to add a pass after pipeline" " execution"
        )
        self.passes.extend(passes)

    def run(self, context: CompileCtx) -> None:
        """
        Run through the execution pipeline.
        """
        assert not self.frozen, "Invalid attempt to run the pipeline twice"
        self.frozen = True

        for p in self.passes:
            if p.disabled:
                if context.verbosity >= Verbosity.debug:
                    printcol("Skipping pass: {}".format(p.name), Colors.YELLOW)
                continue
            if isinstance(p, StopPipeline):
                if context.verbosity >= Verbosity.info:
                    printcol(
                        "Stopping pipeline execution: {}".format(p.name),
                        Colors.OKBLUE,
                    )
                return
            else:
                if (
                    not isinstance(p, MajorStepPass)
                    and context.verbosity >= Verbosity.debug
                ):  # no-code-coverage
                    printcol("Running pass: {}".format(p.name), Colors.YELLOW)
                p.run(context)

    @staticmethod
    def add_args(
        args: argparse.ArgumentParser,
        dest: str = "pass_activations",
    ) -> None:
        """
        Register --pass-on/--pass-off arguments in ``args``.

        Parsing these two options will yield a mapping from pass names to a
        boolean that determines whether this pass should be enabled or
        disabled and store that mapping in the "dest" argument namespace
        attribute.
        """

        class Action(argparse.Action):
            def __init__(
                self,
                option_strings: list[str],
                dest: str,
                nargs: int | str | None = None,
                const: object = None,
                default: object = None,
                type: (
                    Callable[[str], object] | argparse.FileType | None
                ) = None,
                choices: Iterable[object] | None = None,
                required: bool = False,
                help: str | None = None,
                metavar: str | None = None,
            ):
                assert isinstance(const, bool)
                super().__init__(
                    option_strings,
                    dest,
                    nargs,
                    const,
                    default,
                    type,
                    choices,
                    required,
                    help,
                    metavar,
                )

            def __call__(
                self,
                parser: argparse.ArgumentParser,
                namespace: argparse.Namespace,
                values: str | Sequence[Any] | None,
                option_string: str | None = None,
            ) -> None:
                assert isinstance(values, str)
                mapping = getattr(namespace, self.dest)
                mapping[values] = self.const

        args.add_argument(
            "--pass-on",
            help="Activate an optional pass by name.",
            dest=dest,
            default={},
            action=Action,
            const=True,
        )
        args.add_argument(
            "--pass-off",
            help="Deactivate an optional pass by name.",
            dest=dest,
            action=Action,
            const=False,
        )


class AbstractPass(abc.ABC):
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

    def optional(
        self,
        doc: str,
        disabled: bool = True,
    ) -> AbstractPass:
        """
        Expression chain method to make a pass optional. Make this pass
        optional, with assorted doc, and return it, so that it's easy to use in
        an expression context.
        """
        from langkit.documentation import format_text

        self.is_optional = True
        self.disabled = disabled
        self.doc = format_text(None, doc, 4)
        return self

    def __repr__(self) -> str:
        return f"<{type(self).__name__}: {self.name}>"

    @abc.abstractmethod
    def run(self, context: CompileCtx) -> None: ...

    @staticmethod
    def load_plugin_passes(
        plugin_loader: PluginLoader,
        refs: list[str],
    ) -> list[AbstractPass]:
        """
        Load the given list of plugin compilation passes.
        """
        # Load plugin passes
        try:
            # See the comment above PluginLoader.load
            return [
                plugin_loader.load(r, AbstractPass)  # type: ignore
                for r in refs
            ]
        except PluginLoadingError as exc:
            error(str(exc), location=Location.nowhere)


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
        super().__init__("")
        self.message = message

    def run(self, context: CompileCtx) -> None:
        if context.verbosity >= Verbosity.info:
            printcol("{}...".format(self.message), Colors.OKBLUE)


class GlobalPass(AbstractPass):
    """
    Concrete pass to run on the context itself.
    """

    def __init__(
        self,
        name: str,
        pass_fn: Callable[[CompileCtx], None],
        disabled: bool = False,
    ) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        self.pass_fn(context)


class EmitterPass(AbstractPass):
    """
    Concrete pass to run on the emitter (for code generation).
    """

    def __init__(
        self,
        name: str,
        pass_fn: Callable[[Emitter, CompileCtx], None],
        disabled: bool = False,
    ) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        assert context.emitter is not None
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

    def __init__(
        self,
        name: str,
        pass_fn: Callable[[Lexer, CompileCtx], None],
        disabled: bool = False,
    ) -> None:
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass.
        :type (langkit.lexer.Lexer, langkit.compile_context.CompileCtx) -> None

        :param bool disabled: See AbstractPass.
        """
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        assert context.lexer is not None
        self.pass_fn(context.lexer, context)


class GrammarPass(AbstractPass):
    """
    Concrete pass to run on the whole grammar.
    """

    def __init__(
        self,
        name: str,
        pass_fn: Callable[[Grammar, CompileCtx], None],
        disabled: bool = False,
    ) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        assert context.grammar is not None
        self.pass_fn(context.grammar, context)


class GrammarRulePass(AbstractPass):
    """
    Concrete pass to run on each grammar rule.
    """

    def __init__(
        self,
        name: str,
        pass_fn: Callable[[Parser], None],
        disabled: bool = False,
    ) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        # Sort grammar rules by name, so that the pass order is deterministic
        assert context.grammar is not None
        for name, rule in sorted(context.grammar.rules.items()):
            self.pass_fn(rule)


class ASTNodePass(AbstractPass):
    """
    Concrete pass to run on each ASTNodeType subclass.
    """

    auto_context: bool
    """
    If True, setup a diagnostic context for the current AST node.
    """

    def __init__(
        self,
        name: str,
        pass_fn: Callable[[CompileCtx, ASTNodeType], None],
        disabled: bool = False,
    ) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        for astnode in context.node_types:
            self.pass_fn(context, astnode)


class EnvSpecPass(AbstractPass):
    """
    Concrete pass to run on each EnvSpec instance.
    """

    def __init__(
        self,
        name: str,
        pass_fn: Callable[[EnvSpec, CompileCtx], None],
        disabled: bool = False,
    ) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        node_types = (
            context.pending_node_types
            if context.pending_node_types
            else context.node_types
        )
        for t in node_types:
            env_spec = t.env_spec
            if env_spec is None:
                continue
            self.pass_fn(env_spec, context)


class PropertyPass(AbstractPass):
    """
    Concrete pass to run on each PropertyDef instance.
    """

    def __init__(
        self,
        name: str,
        pass_fn: Callable[[PropertyDef, CompileCtx], None],
        disabled: bool = False,
    ) -> None:
        super().__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context: CompileCtx) -> None:
        for prop in context.all_properties(include_inherited=False):
            self.pass_fn(prop, context)


class StopPipeline(AbstractPass):
    """
    Dummy concrete pass to abort the execution of a pipeline.

    Used with the "disabled" attribute, this is useful to conditionnaly limit
    the set of passes to be executed.
    """

    pass


errors_checkpoint_pass = GlobalPass(
    "errors checkpoint", lambda _: errors_checkpoint()
)
