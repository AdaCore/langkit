import gdb

from langkit.debug_info import Property
from langkit.gdb.context import Context
from langkit.gdb.state import Binding, ExpressionEvaluation
from langkit.names import Name
from langkit.utils import Colors, col


def expr_repr(expr: ExpressionEvaluation) -> str:
    """
    Return a colored repr for an expression.
    """
    return col(expr.expr_repr, Colors.CYAN)


def name_repr(expr: Binding) -> str:
    """
    Return a colored repr for a binding name.
    """
    return col(expr.dsl_name, Colors.GREEN)


def prop_repr(prop: Property) -> str:
    """
    Return a colored repr for a property name.

    :type prop: langkit.gdb.debug_info.Property
    :rtype: str
    """
    prefix = '[dispatcher]'
    if prop.name.startswith(prefix):
        name = prop.name[len(prefix):]
        label = 'dispatch property for {}'.format(name)
    else:
        label = prop.name
    return col(col(label, Colors.RED), Colors.BOLD)


def adaify_name(context: Context, name: str) -> str:
    """
    Turn a symbol name like a__b into an Ada-like name such as A.B.
    Also strip the $LIB_NAME.Analysis prefix, if present.
    """
    pfx = context.analysis_prefix
    if name.startswith(pfx):
        name = name[len(pfx):]
    chunks = name.split('__')
    return '.'.join(Name.from_lower(c).camel_with_underscores
                    for c in chunks)


def dereference_fat_array_ptr(fat_ptr: gdb.Value) -> gdb.Value:
    """
    Dereference an array reference materialized as a fat pointer.
    """
    lower_bound = int(fat_ptr['P_BOUNDS']['LB0'])
    upper_bound = int(fat_ptr['P_BOUNDS']['UB0'])
    array_ptr = fat_ptr['P_ARRAY']
    element_type = array_ptr.type.target().target()
    array_ptr_type = element_type.array(lower_bound, upper_bound).pointer()
    return array_ptr.cast(array_ptr_type).dereference()
