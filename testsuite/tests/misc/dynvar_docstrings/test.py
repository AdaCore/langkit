"""
Test creating an array literal whose element_type is the node type
defined by the enclosing class.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import DynamicVariable, Self, ignore, langkit_property

from utils import emit_and_print_errors


v_nodoc = DynamicVariable("v_nodoc", T.Bool)
v_oneline = DynamicVariable("v_oneline", T.Bool, doc="One line doc.")
v_multiline = DynamicVariable("v_multiline", T.Bool, doc="""
    Doc that contains
    multiple lines.
""")
v_multiparag = DynamicVariable("v_multiparag", T.Bool, doc="""
    This is a very complicated doc.

    So complicated that it requires several paragraphs to be decently
    described.

    * It even has to contain...
    * Bullet points!
    * But...

    .. attention::

       This is just an example.
""")


class FooNode(ASTNode):

    # One property to include all dynamic vars, to check the reformatting of
    # each docstring.
    @langkit_property(
        public=True,
        dynamic_vars=[v_nodoc, v_oneline, v_multiline, v_multiparag],
    )
    def p1():
        """
        This is the doc for the property itself.
        """
        ignore(v_nodoc, v_oneline, v_multiline, v_multiparag)
        return Self

    # One property that demonstrates that the doc is left unchanged for a dyn
    # var with no docstring.
    @langkit_property(public=True, dynamic_vars=[v_nodoc])
    def p2():
        """
        This is the doc for the property itself.
        """
        ignore(v_nodoc)
        return Self

    # One property with no docstring at all
    # mention thedoc of the inherited dynamic var.
    @langkit_property(public=True, dynamic_vars=[v_nodoc])
    def p3():
        ignore(v_nodoc)
        return Self


class Example(FooNode):
    token_node = True


emit_and_print_errors(lkt_file="expected_concrete_syntax.lkt")

print("Reading property docs in generated code...")
with open("build/src/libfoolang-analysis.ads") as f:

    # Tracks whether we are processing the declaration of a property function.
    # Used to ignore all comments but the ones of these functions.
    in_property = False

    for line in f:
        line = line.strip()
        if line.startswith("function P_P"):
            in_property = True
            print("")
            print(f"== Doc for {line.split()[1]} ==")
            print("")

        elif in_property:
            if line.startswith("--"):
                print(line)
            elif not line:
                in_property = False

print('Done')
