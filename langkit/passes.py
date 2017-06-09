"""
Helpers to manage compilation passes.
"""

from __future__ import absolute_import, division, print_function

from langkit.compiled_types import StructMetaclass
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
                        and context.verbosity.debug):
                    printcol('Running pass: {}'.format(p.name), Colors.YELLOW)
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


class EmbedIpythonPass(AbstractPass):
    """
    Small utility pass that allows to embed an IPython interpreter at a given
    point in the pipeline. This can be useful to inspect the state of
    compilation and data structures for example.
    """

    def __init__(self):
        super(EmbedIpythonPass, self).__init__("Embed IPython")

    def run(self, context):
        from IPython import embed
        embed(header="Langkit debug prompt")


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
                self.pass_fn(rule)


class ASTNodePass(AbstractPass):
    """
    Concrete pass to run on each ASTNodeType subclass.
    """

    def __init__(self, name, pass_fn, disabled=False, auto_context=True):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass. Called once
            per ASTNodeType subclass. This function must take the context and
            the ASTnodeType subclass.
        :type (langkit.compile_context.CompileCtx,
               langkit.compile_context.ASTNodeType) -> None

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


class EnvSpecPass(AbstractPass):
    """
    Concrete pass to run on each EnvSpec instance.
    """

    def __init__(self, name, pass_fn, disabled=False, iter_metaclass=False):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass. Called once
            per EnvSpec instance. This function must take the EnvSpec instance
            itself and the context.
        :type (langkit.envs.EnvSpec,
               langkit.compile_context.CompileCtx) -> None

        :param bool disabled: See AbstractPass.

        :param bool iter_metaclass: If True, iterate on the AST nodes in
            StructMetaclass.astnode_types. Otherwise, iterate on the context's
            list of AST node types.
        """
        super(EnvSpecPass, self).__init__(name, disabled)
        self.pass_fn = pass_fn
        self.iter_metaclass = iter_metaclass

    def run(self, context):
        astnode_types = (StructMetaclass.astnode_types
                         if self.iter_metaclass else
                         context.astnode_types)
        for astnode in astnode_types:
            env_spec = astnode.env_spec
            if env_spec is None:
                continue
            assert env_spec.ast_node is not None

            # Process EnvSpec instance only once, for the top-most subclass
            # that has it.
            if env_spec.ast_node is astnode:
                self.pass_fn(env_spec, context)


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
