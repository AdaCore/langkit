from __future__ import absolute_import, division, print_function


def inc_ref(var):
    """
    Shortcut for expression rendering.

    If `var` is a ref-counted variable, return Ada code to inc-ref it
    (one statement including trailing semi-colon). Otherwise, return an empty
    string.

    :type var: langkit.expressions.VariableExpr
    :rtype: str
    """
    return 'Inc_Ref ({});'.format(var.name) if var.type.is_refcounted() else ''


def assign_var(var, expr, requires_incref=True):
    """
    Shortcut for expression rendering.

    Return Ada code to assign the given `expr` to the given `var`.  If
    `requires_incref` is True and `var` is a ref-counted variable, also emit
    code to inc-ref it. The result is a sequence of Ada statements, including
    trailing semi-colon.

    :type var: langkit.expressions.VariableExpr
    :type expr: str
    :type requires_incref: bool
    :rtype: str
    """
    return '{} := {}; {}'.format(var.name, expr,
                                 inc_ref(var) if requires_incref else '')


def array_aggr(exprs):
    """
    Shortcut to format an array aggregate expression.

    :type exprs: list[str]
    :rtype: str
    """
    return ('({})'.format(', '.join(['{}'] * len(exprs)))
            if exprs else
            '(1 .. 0 => <>)')
