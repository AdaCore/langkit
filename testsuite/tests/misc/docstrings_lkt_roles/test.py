"""
Check that the typeref langkit doc directive is correctly emitted in generated
APIs.
"""


from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    """
    Log this docstring. This is a typeref: :typeref:`FooNode`.
    """

    @langkit_property(public=True)
    def prop():
        return 1


class Example(FooNode):
    pass


emit_and_print_errors(lkt_file='foo.lkt')

# Log docstrings
for path in [
    "build/src/libfoolang-analysis.ads",
    "build/libfoolang.h",
    "build/ocaml/libfoolang.mli",
    "build/python/libfoolang/__init__.py",
]:
    with open(path) as f:
        for line in f.readlines():
            if 'Log this docstring' in line:
                print(f"{path}:")
                print(f"  {line.strip()}")

print('Done')
