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

    def _parse(self, strlist, dest_block):
        self.state.nested_parse(StringList(strlist), 0, dest_block)

    def run(self):
        document = self.state.document
        def_list = nodes.definition_list()
        result = [def_list]

        for name, attr_expr in sorted(AbstractExpression.attrs_dict.items()):
            def_list_item = nodes.definition_list_item()

            # Create a target for this documentation entry so that the rest of
            # the documentation can reference it (see `properties_dsl_ref`
            # below`).
            target_id = 'properties-dsl-{}'.format(name)
            target_node = nodes.target('', '',
                                       ids=[target_id],
                                       names=[target_id])
            document.note_explicit_target(target_node)

            term = nodes.term()
            term_label = r'expr.\ **{}**'.format(name)
            argspec = attr_expr.argspec
            if argspec is None:
                pass
            elif len(argspec) == 0:
                term_label += r'\ ()'
            else:
                term_label += r'\ (\ *{}*\ )'.format(', '.join(argspec))
            self._parse([term_label], term)

            definition = nodes.definition()
            doc = attr_expr.doc or '*Not yet documented*'
            self._parse(prepare_docstring(doc), definition)

            def_list_item.append(target_node)
            def_list_item.append(term)
            def_list_item.append(definition)

            def_list.append(def_list_item)

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
