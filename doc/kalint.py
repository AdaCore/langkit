import sys

import libkaleidoscopelang as lkl


class ExecutionError(Exception):
    def __init__(self, sloc_range: lkl.SlocRange, message: str):
        self.sloc_range = sloc_range
        self.message = message


class Interpreter:
    # Mapping: enumerators for the Operator type -> callables to perform the
    # operations themselves.
    BINOPS = {
        lkl.OperatorPlus: lambda x, y: x + y,
        lkl.OperatorMinus: lambda x, y: x - y,
        lkl.OperatorMult: lambda x, y: x * y,
        lkl.OperatorDiv: lambda x, y: x / y,
    }

    def __init__(self) -> None:
        # The following dict keeps track of function declarations found so far
        self.functions: dict[str, lkl.FunctionNode] = {}

    # Method for the Interpreter class
    def execute(self, root: lkl.KaleidoscopeNodeList) -> None:
        for node in root:
            if isinstance(node, lkl.FunctionNode):
                self.functions[node.f_proto.f_name.text] = node

            elif isinstance(node, lkl.ExternDecl):
                raise ExecutionError(
                    node.sloc_range, "External declarations are not supported"
                )

            elif isinstance(node, lkl.TopLevelExpr):
                print(self.evaluate(node.f_expr))

            else:
                # There should be no other kind of node at top-level
                assert False

    # Method for the Interpreter class
    def evaluate(
        self,
        expr: lkl.Expr,
        env: dict[str, float] | None = None,
    ) -> float:
        local_env = env or {}
        if env is None:
            env = {}

        if isinstance(expr, lkl.Number):
            return float(expr.text)

        elif isinstance(expr, lkl.Identifier):
            try:
                return local_env[expr.text]
            except KeyError:
                raise ExecutionError(
                    expr.sloc_range, f"Unknown identifier: {expr.text}"
                )

        elif isinstance(expr, lkl.BinaryExpr):
            lhs = self.evaluate(expr.f_lhs, local_env)
            rhs = self.evaluate(expr.f_rhs, local_env)
            return self.BINOPS[type(expr.f_op)](lhs, rhs)

        elif isinstance(expr, lkl.CallExpr):
            name = expr.f_callee.text
            try:
                func = self.functions[name]
            except KeyError:
                raise ExecutionError(
                    expr.f_callee.sloc_range, f"No such function: '{name}'"
                )
            formals = func.f_proto.f_args
            actuals = expr.f_args

            # Check that the call is consistent with the function prototype
            if len(formals) != len(actuals):
                raise ExecutionError(
                    expr.sloc_range,
                    f"'{name}' expects {len(formals)} arguments, but got"
                    f" {len(actuals)} ones",
                )

            # Evaluate arguments and then evaluate the call itself
            new_env = {
                f.text: self.evaluate(a, local_env)
                for f, a in zip(formals, actuals)
            }
            result = self.evaluate(func.f_body, new_env)
            return result

        else:
            # There should be no other kind of node in expressions
            assert False


def print_error(
    filename: str,
    sloc_range: lkl.SlocRange,
    message: str,
) -> None:
    line = sloc_range.start.line
    column = sloc_range.start.column
    print(f"In {filename}, line {line}:", file=sys.stderr)
    with open(filename) as f:
        # Get the corresponding line in the source file and display it
        for _ in range(sloc_range.start.line - 1):
            f.readline()
        print(f"  {f.readline().rstrip()}", file=sys.stderr)
        print(f"  {' ' * (column - 1)}^", file=sys.stderr)
    print(f"Error: {message}", file=sys.stderr)


def execute(filename: str) -> None:
    ctx = lkl.AnalysisContext()
    unit = ctx.get_from_file(filename)
    if unit.diagnostics:
        for diag in unit.diagnostics:
            print_error(filename, diag.sloc_range, diag.message)
            sys.exit(1)
    root = unit.root
    assert isinstance(root, lkl.KaleidoscopeNodeList)
    try:
        Interpreter().execute(root)
    except ExecutionError as exc:
        print_error(filename, exc.sloc_range, exc.message)
        sys.exit(1)


execute(sys.argv[1])
