from __future__ import annotations

import langkit.expressions as E


def inc_ref(var: E.VariableExpr | E.LocalVars.LocalVar) -> str:
    """
    Shortcut for expression rendering.

    If ``var`` is a ref-counted variable, return Ada code to inc-ref it
    (one statement including trailing semi-colon). Otherwise, return an empty
    string.
    """
    var_name = (
        var.codegen_name if isinstance(var, E.LocalVars.LocalVar) else var.name
    )
    return f"Inc_Ref ({var_name});" if var.type.is_refcounted else ""


def assign_var(
    var: E.VariableExpr | E.LocalVars.LocalVar,
    expr: str,
    requires_incref: bool = True,
) -> str:
    """
    Shortcut for expression rendering.

    Return Ada code to assign the given `expr` to the given `var`.  If
    `requires_incref` is True and `var` is a ref-counted variable, also emit
    code to inc-ref it. The result is a sequence of Ada statements, including
    trailing semi-colon.
    """
    var_name = (
        var.codegen_name if isinstance(var, E.LocalVars.LocalVar) else var.name
    )
    return f"{var_name} := {expr}; {inc_ref(var) if requires_incref else ''}"


def array_aggr(exprs: list[str]) -> str:
    """
    Shortcut to format an array aggregate expression.
    """
    return (
        "({})".format(", ".join(["{}"] * len(exprs)))
        if exprs
        else "(1 .. 0 => <>)"
    )
