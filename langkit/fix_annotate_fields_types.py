from __future__ import absolute_import, division, print_function

from lib2to3 import fixer_base
from lib2to3.fixer_util import Call, KeywordArg, Name, Attr
from lib2to3.pygram import python_symbols as syms
from lib2to3.pytree import Node

from langkit.compiled_types import CompiledTypeMetaclass
from langkit.utils import memoized


def build_attr(prefix, suffix):
    prefix = prefix if isinstance(prefix, Node) else Name(prefix)
    suffix = suffix if isinstance(suffix, Node) else Name(suffix)
    return Node(syms.power, Attr(prefix, suffix))

# TODO: Add a warning when the default type repo is not imported in the
# source files. Might be difficult since it can be imported transitively!
# Semantic analysis of Python code is hard!


class FixAnnotateFieldsTypes(fixer_base.BaseFix):
    """
    2to3 fixer that will annotate the types of fields that are not
    annotated already.
    """

    PATTERN = "power< 'Field' trailer<'('')'> >"

    @memoized
    def astnodes(self):
        """
        Create a map from AST node subclasses names to types.

        :rtype: dict[str, ASTNodeType]
        """
        nodes = {}
        for astnode_type in CompiledTypeMetaclass.astnode_types:
            nodes[astnode_type.__name__] = astnode_type

        return nodes

    def transform(self, node, results):
        klass = node.parent.parent.parent.parent
        assert klass.type == syms.classdef, (
            "Internal error in the fields annotator"
        )

        ast_node_name = klass.children[1].value
        ast_node = self.astnodes()[ast_node_name]
        ":type: langkit.compiled_types.ASTNodeType"

        field_name = node.parent.children[0].value
        field = ast_node.get_abstract_fields_dict()[field_name]
        field_type_name = field.type.__name__

        # This assumes that a typerepo instance of name T is available in
        # the environment in which nodes are defined.
        type_expr = build_attr("T", field_type_name)
        if field.type.is_list_type:
            type_expr = Call(build_attr(
                build_attr("T", field.type.element_type.__name__),
                "list_type"
            ))

        return Call(Name(" Field"),
                    [KeywordArg(Name("type"), type_expr)])
