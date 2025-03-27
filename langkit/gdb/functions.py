import gdb

from langkit.gdb.context import Context


class Match(gdb.Function):
    """
    Convenience function to match an AST node.

    This function expects two arguments: the GDB image of the node and the node
    value to match. For instance::

        $XXXmatch("<Identifier p.adb:4:25-4:32>", Node_Variable)
    """

    def __init__(self, context: Context):
        self.context = context
        self.name = "{}match".format(context.prefix)
        super().__init__(self.name)

    def invoke(self, *args: gdb.Value) -> bool:
        if len(args) != 2:
            print(
                "{}: 2 arguments expected, got {} instead".format(
                    self.name, len(args)
                )
            )
            return False

        image, node_value = args
        return str(node_value) == image.string()
