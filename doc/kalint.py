import sys
import libkaleidoscopelang as lkl

class ExecutionError(Exception):
    def __init__(self, sloc_range, message):
        self.sloc_range = sloc_range
        self.message = message

class Interpreter(object):
    # Mapping: enumerators for the Operator type -> callables to perform the
    # operations themselves.
    BINOPS = {lkl.OperatorPlus:  lambda x, y: x + y,
              lkl.OperatorMinus: lambda x, y: x - y,
              lkl.OperatorMult:  lambda x, y: x * y,
              lkl.OperatorDiv:   lambda x, y: x / y}

    def __init__(self):
        # Mapping: function name -> FunctionNode instance
        self.functions = {}

    # Method for the Interpreter class
    def execute(self, ast):
        assert isinstance(ast, lkl.KaleidoscopeNodeList)
        for node in ast:
            if isinstance(node, lkl.FunctionNode):
                self.functions[node.f_proto.f_name.text] = node

            elif isinstance(node, lkl.ExternDecl):
                raise ExecutionError(
                    node.sloc_range,
                    'External declarations are not supported'
                )

            elif isinstance(node, lkl.Expr):
                print (self.evaluate(node))

            else:
                # There should be no other kind of node at top-level
                assert False
                
    # Method for the Interpreter class
    def evaluate(self, node, env=None):
        if env is None:
            env = {}

        if isinstance(node, lkl.Number):
            return float(node.text)

        elif isinstance(node, lkl.Identifier):
            try:
                return env[node.text]
            except KeyError:
                raise ExecutionError(
                    node.sloc_range,
                    'Unknown identifier: {}'.format(node.text)
                )

        elif isinstance(node, lkl.BinaryExpr):
            lhs = self.evaluate(node.f_lhs, env)
            rhs = self.evaluate(node.f_rhs, env)
            return self.BINOPS[type(node.f_op)](lhs, rhs)

        elif isinstance(node, lkl.CallExpr):
            name = node.f_callee.text
            try:
                func = self.functions[name]
            except KeyError:
                raise ExecutionError(
                    node.f_callee.sloc_range,
                    'No such function: "{}"'.format(name)
                )
            formals = func.f_proto.f_args
            actuals = node.f_args

            # Check that the call is consistent with the function prototype
            if len(formals) != len(actuals):
                raise ExecutionError(
                    node.sloc_range,
                    '"{}" expects {} arguments, but got {} ones'.format(
                        node.f_callee.f_name.text,
                        len(formals), len(actuals)
                    )
                )

            # Evaluate arguments and then evaluate the call itself
            new_env = {f.text: self.evaluate(a, env)
                       for f, a in zip(formals, actuals)}
            result = self.evaluate(func.f_body, new_env)
            return result

        else:
            # There should be no other kind of node in expressions
            assert False

def print_error(filename, sloc_range, message):
    line = sloc_range.start.line
    column = sloc_range.start.column
    print ('In {}, line {}:'.format(filename, line), file=sys.stderr)
    with open(filename) as f:
        # Get the corresponding line in the source file and display it
        for _ in range(sloc_range.start.line - 1):
            f.readline()
        print ('  {}'.format(f.readline().rstrip()), file=sys.stderr)
        print ('  {}^'.format(' ' * (column - 1)), file=sys.stderr)
    print ('Error: {}'.format(message), file=sys.stderr)

def execute(filename):
    ctx = lkl.AnalysisContext()
    unit = ctx.get_from_file(filename)
    if unit.diagnostics:
        for diag in unit.diagnostics:
            print_error(filename, diag.sloc_range, diag.message)
            sys.exit(1)
    try:
        Interpreter().execute(unit.root)
    except ExecutionError as exc:
        print_error(filename, exc.sloc_range, exc.message)
        sys.exit(1)

execute(sys.argv[1])
