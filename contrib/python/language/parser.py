from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, EnumNode, abstract
from langkit.parsers import Grammar, Or, List, Pick, Opt, _
from language.lexer import python_lexer as L


def newlines():
    return _(List(P.nl, empty_valid=True, discard=False))


class VarArgsFlag(EnumNode):
    qualifier = True


class KwArgsFlag(EnumNode):
    qualifier = True


class PythonNode(ASTNode):
    pass


class Decorator(PythonNode):
    dec_name = Field()
    arg_list = Field()


class Decorated(PythonNode):
    decorators = Field()
    defn = Field()


class FileNode(PythonNode):
    statements = Field()


class FuncDef(PythonNode):
    name = Field()
    parameters = Field()
    body = Field()


class Params(PythonNode):
    single_params = Field()


class SingleParam(PythonNode):
    is_varargs = Field()
    is_kwargs = Field()
    name = Field()
    default_value = Field()


@abstract
class Stmt(PythonNode):
    pass


class AugAssignStmt(Stmt):
    l_value = Field()
    op = Field()
    r_value = Field()


class AssignStmt(Stmt):
    l_value = Field()
    r_values = Field()


class PrintStmt(Stmt):
    exprs = Field()


class StreamPrintStmt(Stmt):
    stream_expr = Field()
    exprs = Field()


class DelStmt(Stmt):
    exprs = Field()


class PassStmt(Stmt):
    pass


class BreakStmt(Stmt):
    pass


class ContinueStmt(Stmt):
    pass


class ReturnStmt(Stmt):
    exprs = Field()


class RaiseStmt(Stmt):
    exprs = Field()


class ImportName(Stmt):
    imported_names = Field()


class ImportFrom(Stmt):
    rel_name = Field()
    imported = Field()


class RelName(PythonNode):
    dots = Field()
    name = Field()


class ImportStar(PythonNode):
    pass


class AsNameNode(PythonNode):
    imported = Field()
    as_name = Field()


@abstract
class Expr(PythonNode):
    pass


class DottedName(Expr):
    prefix = Field()
    suffix = Field()


class CallExpr(Expr):
    prefix = Field()
    suffix = Field()


class SubscriptExpr(Expr):
    prefix = Field()
    suffix = Field()


class GlobalStmt(Stmt):
    names = Field()


class ExecStmt(Stmt):
    expr = Field()
    in_list = Field()


class AssertStmt(Stmt):
    test_expr = Field()
    msg = Field()


class ElsePart(PythonNode):
    statements = Field()


class IfStmt(Stmt):
    cond_test = Field()
    statements = Field()
    elif_branchs = Field()
    else_part = Field()


class ElifBranch(Stmt):
    cond_test = Field()
    statements = Field()


class WhileStmt(Stmt):
    cond_test = Field()
    statements = Field()
    else_part = Field()


class ForStmt(Stmt):
    bindings = Field()
    expr = Field()
    statements = Field()
    else_part = Field()


class TryStmt(Stmt):
    statements = Field()
    except_parts = Field()
    else_part = Field()
    finally_part = Field()


class ExceptPart(PythonNode):
    as_name = Field()
    statements = Field()


class WithStmt(Stmt):
    bindings = Field()
    statements = Field()


class IfExpr(Expr):
    expr = Field()
    cond = Field()
    else_expr = Field()


class OrOp(Expr):
    left = Field()
    right = Field()


class AndOp(Expr):
    left = Field()
    right = Field()


class NotOp(Expr):
    expr = Field()


class CompOpKind(EnumNode):
    alternatives = [
        'lt', 'gt', 'eq', 'gte', 'lte', 'diamond', 'noteq',
        'in', 'notin', 'is', 'isnot'
    ]


class CompOp(Expr):
    left = Field()
    op = Field()
    right = Field()


class OrExpr(Expr):
    left = Field()
    right = Field()


class XorExpr(Expr):
    left = Field()
    right = Field()


class AndExpr(Expr):
    left = Field()
    right = Field()


class ShiftExpr(Expr):
    left = Field()
    op = Field()
    right = Field()


class ArithExpr(Expr):
    left = Field()
    op = Field()
    right = Field()


class Factor(Expr):
    op = Field()
    expr = Field()


class Term(Expr):
    left = Field()
    op = Field()
    right = Field()


class Power(Expr):
    left = Field()
    right = Field()


class DictAssoc(PythonNode):
    key = Field()
    value = Field()


class YieldExpr(Expr):
    exprs = Field()


class ListGen(Expr):
    expr = Field()
    comprehension = Field()


class TupleLit(Expr):
    exprs = Field()


class ListComp(Expr):
    expr = Field()
    comprehension = Field()


class ListLit(Expr):
    exprs = Field()


class SetComp(Expr):
    expr = Field()
    comprehension = Field()


class SetLit(Expr):
    exprs = Field()


class DictComp(Expr):
    assoc = Field()
    comprehension = Field()


class DictLit(Expr):
    assocs = Field()


class InlineEval(Expr):
    exprs = Field()


class LambdaDef(Expr):
    args = Field()
    expr = Field()


class EllipsisExpr(Expr):
    pass


class SliceExpr(Expr):
    first = Field()
    last = Field()


class ExtSliceExpr(SliceExpr):
    stride = Field()


class ClasssDef(Stmt):
    name = Field()
    bases = Field()
    statements = Field()


@abstract
class Arg(PythonNode):
    pass


class ArgAssoc(Arg):
    name = Field()
    expr = Field()


class ArgGen(Arg):
    expr = Field()
    comprehension = Field()


class VarArgs(Arg):
    expr = Field()


class KwArgs(Arg):
    expr = Field()


@abstract
class Comprehension(PythonNode):
    pass


class CompFor(Comprehension):
    exprs = Field()
    target = Field()
    comp = Field()


class CompForL(Comprehension):
    exprs = Field()
    target = Field()
    comp = Field()


class CompIf(PythonNode):
    test = Field()
    comp = Field()


def TrailList(el, sep, empty_valid=False):
    return Pick(List(el, sep=sep, empty_valid=empty_valid), Opt(sep))


class Name(Expr):
    id = Field()


class NumberLit(Expr):
    num = Field()


class StringLit(Expr):
    str = Field()


class ConcatStringLit(Expr):
    first_str = Field()
    subsequent_str = Field()


class Dot(Expr):
    pass


class NL(PythonNode):
    pass


python_grammar = Grammar('main_rule')
P = python_grammar

python_grammar.add_rules(
    name=Name(L.Identifier(keep=True)),
    number=NumberLit(L.Number(keep=True)),
    string=StringLit(L.String(keep=True)),
    cat_string=ConcatStringLit(P.string, List(P.string)),
    nl=NL(L.Newline()),
    main_rule=FileNode(
        List(newlines(), P.stmt, newlines()), L.Termination()
    ),
    decorator=Decorator(
        '@', P.dotted_name, Opt('(', P.arg_list, ')'), L.Newline()
    ),
    decorators=List(P.decorator),
    decorated=Decorated(P.decorators, Or(P.class_def, P.func_def)),
    func_def=FuncDef('def', P.name, P.parameters, ':', P.suite),
    parameters=Pick('(', Opt(P.varargslist), ')'),
    varargslist=Params(
        List(
            SingleParam(
                Opt('*').as_bool(VarArgsFlag), Opt('**').as_bool(KwArgsFlag),
                P.fpdef, Opt('=', P.test)
            ),
            empty_valid=True, sep=","
        ),
    ),
    fpdef=Or(P.name, Pick('(', P.name_list, ')')),
    name_list=TrailList(P.name, sep=','),
    stmt=Or(P.simple_stmt, P.compound_stmt),
    simple_stmt=Pick(Or(P.small_stmt, TrailList(P.small_stmt, sep=';')),
                     L.Newline()),
    small_stmt=(
        P.expr_stmt | P.print_stmt | P.del_stmt | P.pass_stmt | P.flow_stmt
        | P.import_stmt | P.global_stmt | P.exec_stmt | P.assert_stmt
    ),
    expr_stmt=Or(
        AugAssignStmt(
            P.test_list,
            Or('+=', '-=', '*=', '/=', '%=', '&=',
               '|=', '^=', '<<=', '>>=', '**=', '//='),
            Or(P.yield_expr, P.test_list)
        ),
        AssignStmt(
            P.test_list, List(Pick('=', Or(P.yield_expr, P.test_list)))
        ),
        P.test_list
    ),
    print_stmt=Or(
        PrintStmt('print', P.test_list),
        StreamPrintStmt('print', '>>', P.test, ',', P.test_list)
    ),
    del_stmt=DelStmt('del', P.expr_list),
    pass_stmt=PassStmt('pass'),
    flow_stmt=Or(
        P.break_stmt, P.continue_stmt, P.return_stmt, P.raise_stmt,
        P.yield_stmt
    ),
    break_stmt=BreakStmt('break'),
    continue_stmt=ContinueStmt('continue'),
    return_stmt=ReturnStmt('return', Opt(P.test_list)),
    yield_stmt=P.yield_expr,
    raise_stmt=RaiseStmt('raise', Opt(P.test_list)),
    import_stmt=Or(P.import_name, P.import_from),
    import_name=ImportName('import', P.dotted_as_names),
    dot=Dot('.'),
    import_from=ImportFrom(
        'from',
        Or(P.dotted_name, RelName(List(P.dot), Opt(P.dotted_name))),
        'import',
        Or(ImportStar('*'),
           Pick('(', P.import_as_names, ')'),
           P.import_as_names),
    ),
    as_name=AsNameNode(P.name, 'as', P.name),
    dotted_as_name=AsNameNode(P.dotted_name, 'as', P.name),
    import_as_names=TrailList(Or(P.as_name, P.name), sep=','),
    dotted_as_names=TrailList(Or(P.dotted_as_name, P.dotted_name), sep=','),
    dotted_name=DottedName(P.dotted_name, '.', P.name) | P.name,
    global_stmt=GlobalStmt('global', P.name_list),
    exec_stmt=ExecStmt('exec', P.expr, Opt('in', P.test_list)),
    assert_stmt=AssertStmt('assert', P.test, Opt(',', P.test)),
    compound_stmt=(
        P.if_stmt | P.while_stmt | P.for_stmt | P.try_stmt |
        P.with_stmt | P.func_def | P.class_def | P.decorated
    ),
    else_part=ElsePart('else', ':', P.suite),
    if_stmt=IfStmt(
        'if', P.test, ':', P.suite,
        List('elif', ElifBranch(P.test, ':', P.suite), empty_valid=True),
        Opt(P.else_part)
    ),
    while_stmt=WhileStmt('while', P.test, ':', P.suite, Opt(P.else_part)),
    for_stmt=ForStmt(
        'for', P.expr_list, 'in', P.test_list, ':', P.suite,
        Opt(P.else_part)
    ),
    try_stmt=TryStmt(
        'try', ':', P.suite,

        List(ExceptPart(
            'except', Opt(AsNameNode(P.test, Opt('as', P.test))), ':',
            P.suite
        ), empty_valid=True),

        Opt(P.else_part),
        Opt('finally', ':', P.suite),
    ),
    with_stmt=WithStmt('with', List(P.with_item, sep=','), ":", P.suite),
    with_item=AsNameNode(P.test, Opt('as', P.expr)),
    suite=Or(
        Pick(newlines(), L.Indent(),
             List(newlines(), P.stmt, newlines()),
             L.Dedent()),
        P.simple_stmt,
    ),
    test=Or(
        P.lambdef,
        IfExpr(P.or_test, 'if', P.or_test, 'else', P.test),
        P.or_test,
    ),
    or_test=Or(OrOp(P.or_test, 'or', P.and_test), P.and_test),
    and_test=Or(AndOp(P.and_test, 'and', P.not_test), P.not_test),
    not_test=Or(NotOp('not', P.not_test), P.comparison),
    comparison=Or(
        CompOp(
            P.comparison,
            CompOpKind.alt_lt('<')
            | CompOpKind.alt_gt('>')
            | CompOpKind.alt_eq('==')
            | CompOpKind.alt_gte('>=')
            | CompOpKind.alt_lte('<=')
            | CompOpKind.alt_diamond('<>')
            | CompOpKind.alt_noteq('!=')
            | CompOpKind.alt_in('in')
            | CompOpKind.alt_notin('not', 'in')
            | CompOpKind.alt_isnot('is', 'not')
            | CompOpKind.alt_is('is'),
            P.expr
        ),
        P.expr
    ),
    expr=Or(OrExpr(P.expr, '|', P.xor_expr), P.xor_expr),
    xor_expr=Or(XorExpr(P.xor_expr, '^', P.and_expr), P.and_expr),
    and_expr=Or(AndExpr(P.and_expr, '&', P.shift_expr), P.shift_expr),
    shift_expr=Or(
        ShiftExpr(P.shift_expr, Or('<<', '>>'), P.arith_expr),
        P.arith_expr
    ),
    arith_expr=Or(ArithExpr(P.arith_expr, Or('+', '-'), P.term), P.term),
    term=Or(Term(P.term, Or('*', '/', '%', '//'), P.factor), P.factor),
    factor=Or(Factor(Or('+', '-', '~'), P.factor), P.power),
    power=Or(Power(P.atom_expr, '**', P.factor), P.atom_expr),
    atom_expr=Or(
        DottedName(P.atom_expr, ".", P.name),
        CallExpr(P.atom_expr, '(', P.arg_list, ')'),
        SubscriptExpr(P.atom_expr, '[', P.subscript_list, ']'),
        P.atom
    ),
    dict_assoc=DictAssoc(P.test, ':', P.test),
    yield_expr=YieldExpr('yield', Opt(P.test_list)),
    atom=Or(
        Pick('(', P.yield_expr, ')'),
        ListGen('(', P.test, P.list_for, ')'),
        TupleLit('(', Opt(P.test_list), ')'),
        ListComp('[', P.test, P.list_for, ']'),
        ListLit('[', P.empty_test_list, ']'),
        SetComp('{', P.test, P.comp_for, '}'),
        P.set_lit,
        DictComp('{', P.dict_assoc, P.comp_for, '}'),
        DictLit('{', TrailList(P.dict_assoc, sep=','), '}'),
        InlineEval('`', P.test_list, '`'),
        P.name,
        P.number,
        P.cat_string,
        P.string
    ),
    set_lit=SetLit('{', P.empty_test_list, '}'),
    lambdef=LambdaDef('lambda', P.varargslist, ':', P.test),
    subscript_list=TrailList(P.subscript, sep=","),
    subscript=Or(
        EllipsisExpr('.', '.', '.'),
        ExtSliceExpr(Opt(P.test), ':', Opt(P.test), ':', Opt(P.test)),
        SliceExpr(Opt(P.test), ':', Opt(P.test)),
        P.test,
    ),
    expr_list=TrailList(P.expr, ','),
    test_list=Or(P.test, TrailList(P.test, ',')),
    empty_test_list=TrailList(P.test, ',', empty_valid=True),
    class_def=ClasssDef(
        'class', P.name, Opt('(', Opt(P.test_list), ')'), ':', P.suite
    ),
    arg_list=TrailList(Or(
        ArgGen(P.test, P.comp_for),
        ArgAssoc(Opt(P.test, '='), P.test),
        VarArgs('*', P.test),
        KwArgs('**', P.test),
    ), sep=",", empty_valid=True),

    list_iter=Or(P.list_for, P.list_if),
    list_for=CompForL(
        'for', P.expr_list, 'in', P.test_list, Opt(P.list_iter)
    ),
    list_if=CompIf('if', P.test, Opt(P.list_iter)),

    comp_iter=Or(P.comp_for, P.comp_if),
    comp_for=CompFor(
        'for', P.expr_list, 'in', P.or_test, Opt(P.comp_iter)
    ),
    comp_if=CompIf('if', P.test, Opt(P.comp_iter))
)
