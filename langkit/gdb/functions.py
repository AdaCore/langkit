from __future__ import absolute_import, division, print_function

import gdb


class Match(gdb.Function):
    """
    Convenience function to match an AST node.

    This function expects two arguments: the GDB image of the node and the node
    value to match. For instance::

        $XXXmatch("<Identifier p.adb:4:25-4:32>", Node_Variable)
    """

    def __init__(self, context):
        self.context = context
        super(Match, self).__init__('{}match'.format(context.prefix))

    def invoke(self, *args):
        if len(args) != 2:
            print('{}: 2 arguments expected, got {} instead'.format(len(args)))
            return False

        image, node_value = args
        return str(node_value) == image.string()
