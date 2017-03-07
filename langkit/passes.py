"""
Helpers to manage compilation passes.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from langkit.diagnostics import errors_checkpoint
from langkit.utils import Colors, printcol


class PassManager(object):
    """
    Holder for compilation passes. Handles passes sequential execution.
    """

    def __init__(self):
        self.frozen = False
        self.passes = []

    def add(self, *passes):
        """
        Add the given passes to the execution pipeline.

        :param list[AbstractPass] passes: Pass to append.
        """
        assert not self.frozen, ('Invalid attempt to add a pass after pipeline'
                                 ' execution')
        self.passes.extend(passes)

    def run(self, context):
        """
        Run through the execution pipeline.

        :type context: langkit.compile_context.CompileCtx context
        """
        assert not self.frozen, 'Invalid attempt to run the pipeline twice'
        self.frozen = True

        for p in self.passes:
            if p.disabled:
                continue
            if isinstance(p, StopPipeline):
                if context.verbosity.info:
                    printcol('Stopping pipeline execution: {}'.format(p.name),
                             Colors.OKBLUE)
                return
            else:
                p.run(context)


class AbstractPass(object):
    """
    Base class for all specialized compilation passes.

    Subclasses are required only to override the "run" method.
    """

    def __init__(self, name, disabled=False):
        """
        :param str name: Informative name for users to be used in logging.
        :param bool disabled: If True, do not run this pass. This makes it
            convenient to selectively disable passes in big PassManager.add
            calls.
        """
        self.name = name
        self.disabled = disabled

    def run(self, context):
        raise NotImplementedError()


class MajorStepPass(AbstractPass):
    """
    Dummy concrete pass that just prints a message.

    This is useful to display global compilation progression to users.
    """

    def __init__(self, message):
        super(MajorStepPass, self).__init__(None)
        self.message = message

    def run(self, context):
        if context.verbosity.info:
            printcol('{}...'.format(self.message), Colors.OKBLUE)


class GlobalPass(AbstractPass):
    """
    Concrete pass to run on the context itself.
    """

    def __init__(self, name, pass_fn, disabled=False):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass.
        :type (langkit.compile_context.CompileCtx) -> None

        :param bool disabled: See AbstractPass.
        """
        super(GlobalPass, self).__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context):
        self.pass_fn(context)


class GrammarRulePass(AbstractPass):
    """
    Concrete pass to run on each grammar rule.
    """

    def __init__(self, name, pass_fn, disabled=False):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass. Called once
            per grammar rule. This function must take the context and the rule
            parser itself.
        :type (langkit.compile_context.CompileCtx,
               langkit.parsers.Parser) -> None

        :param bool disabled: See AbstractPass.
        """
        super(GrammarRulePass, self).__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context):
        for name, rule in context.grammar.rules.items():
            with rule.diagnostic_context():
                self.pass_fn(rule, context)


class ASTNodePass(AbstractPass):
    """
    Concrete pass to run on each ASTNode subclass.
    """

    def __init__(self, name, pass_fn, disabled=False, auto_context=True):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass. Called once
            per ASTNode subclass. This function must take the context and the
            ASTnode subclass.
        :type (langkit.compile_context.CompileCtx,
               langkit.compile_context.ASTNode) -> None

        :param bool disabled: See AbstractPass.

        :param bool auto_context: If True, setup a diagnostic context for the
            current AST node.
        """
        super(ASTNodePass, self).__init__(name, disabled)
        self.pass_fn = pass_fn
        self.auto_context = auto_context

    def run(self, context):
        for astnode in context.astnode_types:
            if self.auto_context:
                with astnode.diagnostic_context():
                    self.pass_fn(context, astnode)
            else:
                self.pass_fn(context, astnode)


class PropertyPass(AbstractPass):
    """
    Concrete pass to run on each PropertyDef instance.
    """

    def __init__(self, name, pass_fn, disabled=False):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass. Called once
            per PropertyDef instance. This function must take the PropertyDef
            instance and the context.
        :type (langkit.expressions.base.PropertyDef,
               langkit.compile_context.CompileCtx) -> None

        :param bool disabled: See AbstractPass.
        """
        super(PropertyPass, self).__init__(name, disabled)
        self.pass_fn = pass_fn

    def run(self, context):
        for astnode in context.astnode_types:
            for prop in astnode.get_properties(include_inherited=False):
                with prop.diagnostic_context():
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
