"""
Check that the callgraph is correct in the presence of property dispatchers.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import AbstractKind, Self, langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):

    @langkit_property(public=True)
    def foo():
        return Self.bar.as_bare_entity

    @langkit_property(kind=AbstractKind.abstract, return_type=T.FooNode)
    def bar():
        pass


class Example(FooNode):
    token_node = True

    @langkit_property()
    def bar():
        return Self


class NullExample(Example):
    @langkit_property()
    def bar():
        return Self


ctx = emit_and_print_errors(lkt_file='foo.lkt')
assert ctx is not None


def find_path(from_property, to_property):
    """
    Look for a path in the properties call graph from one property to another.
    """
    ctx.compute_properties_callgraphs()
    graph = ctx.properties_forwards_callgraph

    visited = set()

    def helper(prop, path):
        # If we already processed this property, we know it will not help us
        # reach `to_property`. If we are already processing it, do not allow
        # recursion and just return None as "we don't know yet".
        if prop in visited:
            return None
        visited.add(prop)

        # If we reached `to_property`, return the computed path
        path.append(prop)
        if prop is to_property:
            return path

        # Otherwise, continue seaching to the properties that `prop` calls
        for callee in graph:
            result = helper(callee, path)
            if result:
                return result

    return helper(from_property, [])


assert find_path(FooNode.foo, Example.bar)
print('Done')
