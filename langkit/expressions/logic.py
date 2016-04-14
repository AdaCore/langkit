from langkit.compiled_types import LogicVarType, EquationType
from langkit.expressions.base import (
    AbstractExpression, construct, ResolvedExpression, PropertyDef
)


class Domain(AbstractExpression):
    """
    Define the domain of a logical variable. Several important properties about
    this expression:

    This is the entry point into the logic DSL. A variable of LogicVarType
    *must* have a domain defined in the context of an equation. If it doesnt,
    it's solution set is empty, and thus the only possible value for it is
    undefined.

    If an equation is defined in which the only constraint on variables is
    their domains, then, for a set A, B, .., N of logical variables, with
    domains DA, DB, .., DN, the set of solutions will be of the product of the
    set of every domains.

    So for example, in the equation::
        Domain(A, [1, 2]) and Domain(B, [1, 2])

    The set of solutions is::
        [(1, 1), (1, 2), (2, 1), (2, 2)]

    The 'or' operator acts like concatenation on domains of logic variable, so
    for example::

        Domain(A, [1, 2]) or Domain(A, [3, 4])

    is equivalent to (but slower than) Domain(A, [1, 2, 3, 4]).

    You can define an equation that is invalid, in that not every equation has
    a domain, and, due to runtime dispatch , we cannot statically predict if
    that's gonna happen. Thus, trying to solve such an equation will result in
    an error.
    """

    class Expr(ResolvedExpression):

        def __init__(self, domain, logic_var_expr):
            self.domain = domain
            self.logic_var_expr = logic_var_expr
            self.res_var = PropertyDef.get().vars.create("Var", EquationType)

        @property
        def type(self):
            return EquationType

        def render_pre(self):
            return "\n".join([
                self.domain.render_pre(),
                self.logic_var_expr.render_pre(), """
                declare
                    A : Eq_Node.Raw_Member_Array (1 .. Length ({domain}));
                begin
                    for J in 0 .. Length ({domain}) loop
                        A (J) := Get ({domain}, J);
                    end loop;

                    {res_var} := Member ({logic_var}, A);
                end;
                """.format(logic_var=self.logic_var_expr.render_expr(),
                           domain=self.domain.render_expr(),
                           res_var=self.res_var.name)
            ])

        def render_expr(self):
            return self.res_var.name

    def __init__(self, logic_var_expr, domain):
        super(Domain, self).__init__()

        self.domain = domain
        ":type: AbstractExpression"

        self.logic_var_expr = logic_var_expr
        ":type: AbstractExpression"

    def construct(self):
        return Domain.Expr(
            construct(
                self.domain, lambda d: d.is_collection(),
                "Type given to LogicVar must be collection type, got {}"
            ),
            construct(self.logic_var_expr, LogicVarType)
        )
