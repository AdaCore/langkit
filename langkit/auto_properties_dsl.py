"""
Sphinx extension to generate documentation for the properties DSL.
"""

from __future__ import (absolute_import, division, print_function)

from docutils import nodes
import docutils.parsers.rst
from docutils.statemachine import StringList
from sphinx.util.docstrings import prepare_docstring

from langkit.expressions import AbstractExpression


class AutoPropertiesDSL(docutils.parsers.rst.Directive):
    """
    Directive to generate a definition list for all DSL constructors.
    """

    required_arguments = 1

    def _prepare_docstring(self, docstring):
        """
        Remove anything that appears after a line that starts with ":". This
        makes it possible to remove directives like ":param XXX:" which are
        intended for Langkit developpers, not for users.
        """
        result = prepare_docstring(docstring)
        for i, line in enumerate(result):
            if line.startswith(':'):
                return '\n'.join(result[:i]).rstrip().split('\n')
        return result

    def _parse(self, strlist, dest_block):
        self.state.nested_parse(StringList(strlist), 0, dest_block)

    def _process_one(self, doc_expr, document):
        def_list_item = nodes.definition_list_item()

        # Create a target for this documentation entry so that the rest of
        # the documentation can reference it (see `properties_dsl_ref`
        # below`).
        target_id = 'properties-dsl-{}'.format(doc_expr.name)
        target_node = nodes.target('', '',
                                   ids=[target_id],
                                   names=[target_id])
        document.note_explicit_target(target_node)

        term = nodes.term()
        term_label = '**{}**'.format(doc_expr.name)
        if doc_expr.is_attribute:
            term_label = r'{}.\ {}'.format(doc_expr.prefix_name, term_label)

        argspec = doc_expr.argspec
        if argspec is None:
            pass
        elif len(argspec) == 0:
            term_label += r'\ ()'
        else:
            term_label += r'\ (\ *{}*\ )'.format(', '.join(argspec))
        self._parse([term_label], term)

        definition = nodes.definition()
        doc = doc_expr.doc or '*Not yet documented*'
        self._parse(self._prepare_docstring(doc), definition)

        def_list_item.append(target_node)
        def_list_item.append(term)
        def_list_item.append(definition)

        return def_list_item

    def run(self):
        document = self.state.document
        def_list = nodes.definition_list()
        result = [def_list]

        what_str, = self.arguments
        what_list = {
            'attr': AbstractExpression.attrs_dict.values(),
            'cls': AbstractExpression.constructors,
        }
        what = what_list[what_str]

        for doc_expr in sorted(what, key=lambda doc_expr: doc_expr.name):
            def_list.append(self._process_one(doc_expr, document))

        return result


def properties_dsl_ref(name, rawtext, text, lineno, inliner, options={},
                       content=[]):
    """
    Role to create a reference to the definition of a DSL constructor.

    The input text must be the name of the constructor (for instance: `all` to
    reference the `.all` attribute constructor).
    """
    label = nodes.literal('', '.{}'.format(text))
    ref = nodes.reference(rawtext, '', label,
                          refid='properties-dsl-{}'.format(text),
                          **options)
    return [ref], []
