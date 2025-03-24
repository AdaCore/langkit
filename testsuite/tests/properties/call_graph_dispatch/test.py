"""
Check that the callgraph is correct in the presence of property dispatchers.
"""

from langkit.compiled_types import T


def find_path(ctx, from_property, to_property):
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


def get_property(type_name, prop_name):
    t = getattr(T, type_name)
    return t.get_abstract_node_data_dict()[prop_name]


def main(ctx):
    foo = get_property("FooNode", "foo")
    bar = get_property("Example", "bar")
    if not find_path(ctx, foo, bar):
        raise RuntimeError
    print("Done")
