from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import No, Property, Self, new_env_assoc
from langkit.parsers import Grammar, List, Opt, Or, Pick, _
from language.lexer import python_lexer as L


def newlines():
    return _(List(P.nl, empty_valid=True))


class PythonNode(ASTNode):
    pass


class VarArgsFlag(PythonNode):
    enum_node = True
    qualifier = True


class KwArgsFlag(PythonNode):
    enum_node = True
    qualifier = True


@abstract
class Stmt(PythonNode):
    pass


class Decorator(PythonNode):
    dec_name = Field(type=T.Name)
    arg_list = Field(type=T.Arg.list)


class Decorated(Stmt):
    decorators = Field(type=T.Decorator.list)
    defn = Field(type=T.DefStmt)


class FileNode(PythonNode):
    statements = Field(type=T.PythonNode.list)


@abstract
class DefStmt(Stmt):
    env_spec = EnvSpec(add_env())


class FuncDef(DefStmt):
    name = Field(type=T.Id)
    parameters = Field(type=T.Params)
    body = Field(type=T.PythonNode)


class Params(PythonNode):
    single_params = Field(type=T.SingleParam.list)


class SingleParam(PythonNode):
    is_varargs = Field(type=T.VarArgsFlag)
    is_kwargs = Field(type=T.KwArgsFlag)
    name = Field(type=T.PythonNode)
    default_value = Field(type=T.Expr)

    env_spec = EnvSpec(
        add_to_env(Self.name.match(
            lambda i=T.Id: new_env_assoc(key=i.sym, value=Self).singleton,
            lambda l=T.Id.list: l.map(
                lambda i: new_env_assoc(key=i.sym, value=Self)
            ),
            lambda _: No(T.env_assoc.array)
        ))
    )


class AugAssignStmt(Stmt):
    l_value = Field(type=T.Expr.list)
    op = Field(type=T.Op)
    r_value = Field(type=T.PythonNode)


class AssignStmt(Stmt):
    l_value = Field(type=T.Expr.list)
    r_values = Field(type=T.PythonNode.list)

    env_spec = EnvSpec(
        add_to_env(Self.l_value.filtermap(
            lambda e: new_env_assoc(key=e.cast_or_raise(T.Id).sym, value=Self),
            lambda e: e.is_a(T.Id),
        ))
    )


class PrintStmt(Stmt):
    exprs = Field(type=T.Expr.list)


class StreamPrintStmt(Stmt):
    stream_expr = Field(type=T.Expr)
    exprs = Field(type=T.Expr.list)


class DelStmt(Stmt):
    exprs = Field(type=T.Expr.list)


class PassStmt(Stmt):
    pass


class BreakStmt(Stmt):
    pass


class ContinueStmt(Stmt):
    pass


class ReturnStmt(Stmt):
    exprs = Field(type=T.Expr.list)


class RaiseStmt(Stmt):
    exprs = Field(type=T.Expr.list)


class ImportName(Stmt):
    imported_names = Field(type=T.PythonNode.list)


class ImportFrom(Stmt):
    rel_name = Field(type=T.PythonNode)
    imported = Field(type=T.PythonNode)


class RelName(PythonNode):
    dots = Field(type=T.Dot.list)
    name = Field(type=T.Name)


class ImportStar(PythonNode):
    pass


class AsNameNode(PythonNode):
    imported = Field(type=T.Expr)
    as_name = Field(type=T.Expr)


@abstract
class Expr(PythonNode):
    pass


@abstract
class Name(Expr):
    pass


class DottedName(Name):
    prefix = Field(type=T.Expr)
    suffix = Field(type=T.Id)


class CallExpr(Expr):
    prefix = Field(type=T.Expr)
    suffix = Field(type=T.Arg.list)


class SubscriptExpr(Expr):
    prefix = Field(type=T.Expr)
    suffix = Field(type=T.Expr.list)


class GlobalStmt(Stmt):
    names = Field(type=T.Id.list)


class ExecStmt(Stmt):
    expr = Field(type=T.Expr)
    in_list = Field(type=T.Expr.list)


class AssertStmt(Stmt):
    test_expr = Field(type=T.Expr)
    msg = Field(type=T.Expr)


class ElsePart(PythonNode):
    statements = Field(type=T.PythonNode)


class IfStmt(Stmt):
    cond_test = Field(type=T.Expr)
    statements = Field(type=T.PythonNode)
    elif_branchs = Field(type=T.ElifBranch.list)
    else_part = Field(type=T.ElsePart)


class ElifBranch(Stmt):
    cond_test = Field(type=T.Expr)
    statements = Field(type=T.PythonNode)


class WhileStmt(Stmt):
    cond_test = Field(type=T.Expr)
    statements = Field(type=T.PythonNode)
    else_part = Field(type=T.ElsePart)


class ForStmt(Stmt):
    bindings = Field(type=T.Expr.list)
    expr = Field(type=T.Expr.list)
    statements = Field(type=T.PythonNode)
    else_part = Field(type=T.ElsePart)


class TryStmt(Stmt):
    statements = Field(type=T.PythonNode)
    except_parts = Field(type=T.ExceptPart.list)
    else_part = Field(type=T.ElsePart)
    finally_part = Field(type=T.PythonNode)


class ExceptPart(PythonNode):
    as_name = Field(type=T.AsNameNode)
    statements = Field(type=T.PythonNode)


class WithStmt(Stmt):
    bindings = Field(type=T.AsNameNode.list)
    statements = Field(type=T.PythonNode)


class IfExpr(Expr):
    expr = Field(type=T.Expr)
    cond = Field(type=T.Expr)
    else_expr = Field(type=T.Expr)


class OrOp(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)


class AndOp(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)


class NotOp(Expr):
    expr = Field(type=T.Expr)


class CompOpKind(PythonNode):
    enum_node = True
    alternatives = [
        'lt', 'gt', 'eq', 'gte', 'lte', 'diamond', 'noteq',
        'in', 'notin', 'is', 'isnot'
    ]


class CompOp(Expr):
    left = Field(type=T.Expr)
    op = Field(type=T.CompOpKind)
    right = Field(type=T.Expr)


class OrExpr(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)


class XorExpr(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)


class AndExpr(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)


class Op(PythonNode):
    token_node = True


@abstract
class BinOp(Expr):
    left = Field(type=T.Expr)
    op = Field(type=T.Op)
    right = Field(type=T.Expr)


class ArithExpr(BinOp):
    pass


class Term(BinOp):
    pass


class ShiftExpr(BinOp):
    pass


class Factor(Expr):
    op = Field(type=T.Op)
    expr = Field(type=T.Expr)


class Power(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)


class DictAssoc(PythonNode):
    key = Field(type=T.Expr)
    value = Field(type=T.Expr)


class YieldExpr(Expr):
    exprs = Field(type=T.Expr.list)


class ListGen(Expr):
    expr = Field(type=T.Expr)
    comprehension = Field(type=T.CompForL)


class TupleLit(Expr):
    exprs = Field(type=T.Expr.list)


class ListComp(Expr):
    expr = Field(type=T.Expr)
    comprehension = Field(type=T.CompForL)


class ListLit(Expr):
    exprs = Field(type=T.Expr.list)


class SetComp(Expr):
    expr = Field(type=T.Expr)
    comprehension = Field(type=T.CompFor)


class SetLit(Expr):
    exprs = Field(type=T.Expr.list)


class DictComp(Expr):
    assoc = Field(type=T.DictAssoc)
    comprehension = Field(type=T.CompFor)


class DictLit(Expr):
    assocs = Field(type=T.DictAssoc.list)


class InlineEval(Expr):
    exprs = Field(type=T.Expr.list)


class LambdaDef(Expr):
    args = Field(type=T.Params)
    expr = Field(type=T.Expr)


class EllipsisExpr(Expr):
    pass


class SliceExpr(Expr):
    first = Field(type=T.Expr)
    last = Field(type=T.Expr)


class ExtSliceExpr(SliceExpr):
    stride = Field(type=T.Expr)


class ClassDef(DefStmt):
    name = Field(type=T.Id)
    bases = Field(type=T.Expr.list)
    statements = Field(type=T.PythonNode)


@abstract
class Arg(PythonNode):
    pass


class ArgAssoc(Arg):
    name = Field(type=T.Expr)
    expr = Field(type=T.Expr)


class ArgGen(Arg):
    expr = Field(type=T.Expr)
    comprehension = Field(type=T.CompFor)


class VarArgs(Arg):
    expr = Field(type=T.Expr)


class KwArgs(Arg):
    expr = Field(type=T.Expr)


@abstract
class Comprehension(PythonNode):
    pass


class CompFor(Comprehension):
    exprs = Field(type=T.Expr.list)
    target = Field(type=T.Expr)
    comp = Field(type=T.PythonNode)


class CompForL(Comprehension):
    exprs = Field(type=T.Expr.list)
    target = Field(type=T.Expr.list)
    comp = Field(type=T.PythonNode)


class CompIf(PythonNode):
    test = Field(type=T.Expr)
    comp = Field(type=T.PythonNode)


def TrailList(el, sep, empty_valid=False):
    return Pick(List(el, sep=sep, empty_valid=empty_valid), Opt(sep))


class Id(Name):
    token_node = True

    sym = Property(
        Self.symbol, doc="Shortcut to get the symbol of this node"
    )


class NumberLit(Expr):
    token_node = True


class StringLit(Expr):
    token_node = True


class ConcatStringLit(Expr):
    first_str = Field(type=T.StringLit)
    subsequent_str = Field(type=T.StringLit.list)


class Dot(Expr):
    pass


class NL(PythonNode):
    pass


python_grammar = Grammar('main_rule')
P = python_grammar

python_grammar.add_rules(
    name=Id(L.Identifier),
    number=NumberLit(L.Number),
    string=StringLit(L.String),
    cat_string=ConcatStringLit(P.string, List(P.string)),
    nl=NL(L.Newline),
    main_rule=FileNode(
        List(newlines(), P.stmt, newlines()), L.Termination
    ),
    decorator=Decorator(
        '@', P.dotted_name, Opt('(', P.arg_list, ')'), L.Newline
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
                     L.Newline),
    small_stmt=(
        P.expr_stmt | P.print_stmt | P.del_stmt | P.pass_stmt | P.flow_stmt
        | P.import_stmt | P.global_stmt | P.exec_stmt | P.assert_stmt
    ),
    expr_stmt=Or(
        AugAssignStmt(
            P.test_list,
            Op(Or('+=', '-=', '*=', '/=', '%=', '&=',
                  '|=', '^=', '<<=', '>>=', '**=', '//=')),
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
        Pick(newlines(), L.Indent,
             List(newlines(), P.stmt, newlines()),
             L.Dedent),
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
        ShiftExpr(P.shift_expr, Op(Or('<<', '>>')), P.arith_expr),
        P.arith_expr
    ),
    arith_expr=Or(ArithExpr(P.arith_expr, Op(Or('+', '-')), P.term), P.term),
    term=Or(Term(P.term, Op(Or('*', '/', '%', '//')), P.factor), P.factor),
    factor=Or(Factor(Op(Or('+', '-', '~')), P.factor), P.power),
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
    test_list=TrailList(P.test, ','),
    empty_test_list=TrailList(P.test, ',', empty_valid=True),
    class_def=ClassDef(
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
