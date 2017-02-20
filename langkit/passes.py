"""
Helpers to manage compilation passes.
"""

from __future__ import absolute_import


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
            p.run(context)


class AbstractPass(object):
    """
    Base class for all specialized compilation passes.

    Subclasses are required only to override the "run" method.
    """

    def __init__(self, name):
        """
        :param str name: Informative name for users to be used in logging.
        """
        self.name = name

    def run(self, context):
        raise NotImplementedError()


class GlobalPass(AbstractPass):
    """
    Concrete pass to run on the context itself.
    """

    def __init__(self, name, pass_fn):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass.
        :type (langkit.compile_context.CompileCtx) -> None
        """
        super(GlobalPass, self).__init__(name)
        self.pass_fn = pass_fn

    def run(self, context):
        if context.verbosity.debug:
            print('Global pass {}'.format(self.name))
        self.pass_fn(context)


class GrammarRulePass(AbstractPass):
    """
    Concrete pass to run on each grammar rule.
    """

    def __init__(self, name, pass_fn):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass. Called once
            per grammar rule. This function must take the context, the rule
            name and the rule parser itself.
        :type (langkit.compile_context.CompileCtx,
               str,
               langkit.parsers.Parser) -> None
        """
        super(GrammarRulePass, self).__init__(name)
        self.pass_fn = pass_fn

    def run(self, context):
        for name, rule in context.grammar.rules.items():
            with rule.diagnostic_context():
                if context.verbosity.debug:
                    print('Grammar pass {} on {}'.format(self.name, name))
                self.pass_fn(rule, context, name)


class ASTNodePass(AbstractPass):
    """
    Concrete pass to run on each ASTNode subclass.
    """

    def __init__(self, name, pass_fn):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass. Called once
            per ASTNode subclass. This function must take the context and the
            ASTnode subclass.
        :type (langkit.compile_context.CompileCtx,
               langkit.compile_context.ASTNode) -> None
        """
        super(ASTNodePass, self).__init__(name)
        self.pass_fn = pass_fn

    def run(self, context):
        for astnode in context.astnode_types:
            with astnode.diagnostic_context():
                if context.verbosity.debug:
                    print('ASTNode pass {} on {}'.format(self.name,
                                                         astnode.name().camel))
                self.pass_fn(context, astnode)


class PropertyPass(AbstractPass):
    """
    Concrete pass to run on each PropertyDef instance.
    """

    def __init__(self, name, pass_fn):
        """
        :param str name: See AbstractPass.

        :param pass_fn: Function to be run when executing the pass. Called once
            per PropertyDef instance. This function must take the PropertyDef
            instance and the context.
        :type (langkit.expressions.base.PropertyDef,
               langkit.compile_context.CompileCtx) -> None
        """
        super(PropertyPass, self).__init__(name)
        self.pass_fn = pass_fn

    def run(self, context):
        for astnode in context.astnode_types:
            for prop in astnode.get_properties(include_inherited=False):
                with prop.diagnostic_context():
                    if context.verbosity.debug:
                        print('Property pass {} on {}'.format(
                            self.name, prop.qualname
                        ))
                    self.pass_fn(prop, context)
