from __future__ import absolute_import, division, print_function

from langkit.dsl import (
    ASTNode, AbstractField, Field, NullField, T, abstract
)
from langkit.parsers import Grammar, List, NoBacktrack as cut, Opt, Or

from language.lexer import lkt_lexer as Lex


@abstract
class LKNode(ASTNode):
    """
    Root node class for lkt AST nodes.
    """
    pass


class LangkitRoot(LKNode):
    """
    For the moment, root node of a lkt compilation unit.
    """
    decls = Field()


class FullDecl(LKNode):
    """
    Container for an lkt declaration. Contains the decl node plus the
    documentation and annotations.
    """
    doc = Field()
    decl_annotations = Field()
    decl = Field()


@abstract
class Decl(LKNode):
    """
    Base class for declarations. Encompasses regular declarations as well as
    special declarations such as grammars, grammar rules, etc.
    """
    name = AbstractField(type=T.Id)


@abstract
class Expr(LKNode):
    """
    Base class for expressions. Encompasses regular expressions as well as
    special expressions (grammar expressions, etc).
    """
    pass


@abstract
class BaseGrammarDecl(Decl):
    """
    Base class for all declarations related to grammars.
    """


class GrammarDecl(BaseGrammarDecl):
    """
    Declaration of a language's grammar.
    """
    name = Field()
    rules = Field()


class GrammarRuleDecl(Decl):
    """
    Declaration of a grammar rule inside of a grammar.
    """
    name = Field()
    expr = Field()


@abstract
class GrammarExpr(Expr):
    """
    Base class for expressions related to grammars.
    """
    pass


class ParseNodeExpr(GrammarExpr):
    """
    Expression for the parsing of a Node.
    """
    node_name = Field()
    sub_exprs = Field()


class GrammarRuleRef(GrammarExpr):
    """
    Grammar expression for a reference to another grammar rule.
    """
    node_name = Field()


class DotExpr(Expr):
    """
    Dotted expression.
    """
    prefix = Field()
    suffix = Field()


class NullCondDottedName(DotExpr):
    """
    Null conditional dotted expression.
    """
    pass


class Id(Expr):
    """
    Identifier.
    """
    token_node = True


class TokenLit(GrammarExpr):
    """
    Grammar expression for a token literal.
    """
    token_node = True


class GrammarPick(GrammarExpr):
    """
    Grammar expression to pick the significant parse out of a list of parses
    (will automatically discard token results).
    """
    exprs = Field()


class GrammarImplicitPick(GrammarPick):
    """
    Implicit pick operation.
    """
    pass


class GrammarToken(GrammarExpr):
    """
    Grammar expression for a token reference.
    """
    token_name = Field()
    expr = Field()


class GrammarOrExpr(GrammarExpr):
    """
    Grammar `Or` expression (disjunctive choice between several grammar
    options).
    """
    sub_exprs = Field()


class GrammarOpt(GrammarExpr):
    """
    Grammar expression for an optional parsing result.
    """
    expr = Field()


class GrammarOptGroup(GrammarExpr):
    """
    Grammar expression for a group of optional parsing results.
    """
    expr = Field()


class GrammarCut(GrammarExpr):
    """
    Grammar expression for a cut.
    """
    pass


class GrammarNull(GrammarExpr):
    """
    Grammar expression to parse a null node.
    """
    name = Field()


class GrammarSkip(GrammarExpr):
    """
    Grammar expression (error recovery) to skip a parsing result.
    """
    name = Field()


class GrammarPredicate(GrammarExpr):
    """
    Grammar expression for a predicate: Only parse something if the predicate
    (that is a reference to a node property) returns True.
    """
    expr = Field()
    prop_ref = Field()


class GrammarDontSkip(GrammarExpr):
    """
    Grammar expression (error recovery) to ensure that any nested skip parser
    calls won't skip certain parse results.
    """
    expr = Field()
    dont_skip = Field()


class GrammarList(GrammarExpr):
    """
    Grammar expression to parse lists of results. Results can be separated by a
    separator. List can be empty ('*') or not ('+').
    """
    kind = Field()
    expr = Field()
    sep = Field()


class ListKind(LKNode):
    """
    Kind for list parser expressions.
    """
    enum_node = True
    alternatives = ["one", "zero"]


class ClassDecl(Decl):
    """
    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.
    """
    name = Field()
    base_class = Field()
    decls = Field()


class DocComment(LKNode):
    """
    Node for one line of documentation attached to a node.
    """
    token_node = True


class Doc(LKNode):
    """
    Documentation attached to a decl node.
    """
    lines = Field()


class FunDecl(Decl):
    """
    Function declaration.
    """
    name = Field()
    args = Field()
    return_type = Field()
    body = Field()


@abstract
class BaseValDecl(Decl):
    """
    Abstract class for named values declarations, such as arguments, local
    value bindings, fields, etc.
    """
    name = Field()
    type = AbstractField(type=T.TypeRef)


class FunArgDecl(BaseValDecl):
    """
    Function argument declaration.
    """
    type = Field()
    default_val = Field()


class LambdaArgDecl(BaseValDecl):
    """
    Function argument declaration.
    """
    type = Field()
    default_val = Field()


class FieldDecl(BaseValDecl):
    """
    Field declaration.
    """
    type = Field()


@abstract
class TypeRef(LKNode):
    """
    Base class for a reference to a type.
    """
    pass


class SimpleTypeRef(TypeRef):
    """
    Simple reference to a type.
    """
    type_name = Field()


class GenericTypeRef(TypeRef):
    """
    Reference to a generic type.
    """
    type_name = Field()
    params = Field()


class NullLit(Expr):
    """
    Null literal expression.
    """
    token_node = True


class ArrayLiteral(Expr):
    """
    Literal for an array value.
    """
    exprs = Field()


class Isa(Expr):
    """
    Isa expression.
    """
    expr = Field()
    dest_type = Field()


class DeclAnnotation(LKNode):
    """
    Compile time annotation attached to a declaration.
    """
    name = Field()
    params = Field()


class Param(LKNode):
    """
    Parameter for function calls or for annotations.
    """
    name = Field()
    value = Field()


class ParenExpr(Expr):
    """
    Parenthesized expression.
    """
    expr = Field()


class CallExpr(Expr):
    """
    Call expression.
    """
    name = Field()
    args = Field()


class NullCondCallExpr(CallExpr):
    """
    Null conditional call expression.
    """
    pass


class GenericInstantiation(Expr):
    """
    Generic instantiation.
    """
    name = Field()
    args = Field()


class ErrorOnNull(Expr):
    """
    Expression that throws an error if LHS is null.
    """
    expr = Field()


class LambdaExpr(Expr):
    """
    Lambda expression.
    """
    params = Field()
    body = Field()


class TryExpr(Expr):
    """
    Try expression.
    """
    try_expr = Field()
    or_expr = Field()


class RaiseExpr(Expr):
    """
    Raise expression.
    """
    except_expr = Field()


class IfExpr(Expr):
    """
    If expression.
    """
    cond_expr = Field()
    then_expr = Field()
    alternatives = Field()
    else_expr = Field()


class ElsifBranch(LKNode):
    """
    Elsif branch of an if expression.
    """
    cond_expr = Field()
    then_expr = Field()


class BlockExpr(Expr):
    """
    Block expression.
    """
    val_defs = Field()
    expr = Field()


class MatchExpr(Expr):
    """
    Binary operator expression.
    """
    match_expr = Field()
    branches = Field()


class MatchBranch(LKNode):
    """
    Branch inside a match expression.
    """
    decl = Field()
    expr = Field()


class MatchValDecl(BaseValDecl):
    """
    Value declaration in a match branch.
    """
    type = Field()


class BinOp(Expr):
    """
    Binary operator expression.
    """
    left = Field()
    op = Field()
    right = Field()


class ValDecl(BaseValDecl):
    """
    Value declaration.
    """
    type = Field()
    val = Field()


class Op(LKNode):
    """
    Operator in a binary operator expression.
    """
    enum_node = True

    alternatives = ["and", "or", "plus", "minus", "eq", "mult", "div",
                    "lt", "gt", "lte", "gte", "amp"]


class StringLit(Expr):
    """
    String literal expression.
    """
    token_node = True


class NumLit(Expr):
    """
    Number literal expression.
    """
    token_node = True


class BindDecl(BaseValDecl):
    """
    Dynamic var bind expression.
    """
    expr = Field()
    type = NullField()


class LogicExpr(Expr):
    """
    Class for logic expressions (any ``basic_expr`` starting with %).
    """
    expr = Field()


lkt_grammar = Grammar('main_rule')
G = lkt_grammar
lkt_grammar.add_rules(
    main_rule=LangkitRoot(
        G.decls, Lex.Termination
    ),
    id=Id(Lex.Identifier),

    doc_comment=DocComment(Lex.DocComment),

    doc=Doc(List(G.doc_comment, empty_valid=True)),

    grammar_decl=GrammarDecl(
        "grammar", G.id,
        "{", List(G.grammar_rule, empty_valid=True), "}"
    ),
    grammar_rule=GrammarRuleDecl(G.id, "<-", G.grammar_expr),
    grammar_primary=Or(
        G.token_literal,
        G.grammar_cut,
        G.grammar_skip,
        G.grammar_null,
        G.grammar_list_expr,
        G.grammar_token,
        G.parse_node_expr,
        G.grammar_opt,
        G.grammar_or_expr,
        G.grammar_rule_ref,
        G.grammar_pick,
    ),
    grammar_expr=Or(
        GrammarDontSkip(
            G.grammar_expr,
            "|>", Lex.Identifier("dont_skip"),
            "(", G.grammar_expr, ")"
        ),
        GrammarPredicate(
            G.grammar_expr,
            "|>", Lex.Identifier("when"),
            "(", G.basic_name, ")"
        ),
        G.grammar_primary
    ),

    grammar_pick=GrammarPick(
        "(", List(G.grammar_expr, empty_valid=False), ")"
    ),

    grammar_implicit_pick=GrammarImplicitPick(
        List(G.grammar_expr, empty_valid=False)
    ),

    grammar_opt=Or(
        GrammarOpt("?", G.grammar_expr),
        GrammarOptGroup("?", "(", List(G.grammar_expr, empty_valid=True), ")"),
    ),

    grammar_cut=GrammarCut("/"),
    grammar_or_expr=GrammarOrExpr(
        "or", "(",
        Opt("|"), List(List(G.grammar_expr), sep="|"),
        ")"
    ),
    token_literal=TokenLit(Lex.String),
    parse_node_expr=ParseNodeExpr(
        G.id, "(", List(G.grammar_expr, empty_valid=True), ")"
    ),
    grammar_rule_ref=GrammarRuleRef(G.id),
    grammar_list_expr=GrammarList(
        Or(ListKind.alt_one("list+"), ListKind.alt_zero("list*")),
        "(",

        # Main list expr
        G.grammar_implicit_pick | G.grammar_expr,

        # Separator
        Opt(",", G.grammar_expr), ")",
    ),

    grammar_skip=GrammarSkip(
        Lex.Identifier(match_text="skip"), "(", G.type_ref, ")"
    ),

    grammar_null=GrammarNull(
        "null", "(", G.type_ref, ")"
    ),

    grammar_token=GrammarToken(
        "@", G.id, Opt("(", G.token_literal, ")")
    ),

    class_decl=ClassDecl(
        "class", G.id, Opt(":", G.type_ref), "{",
        G.decls,
        "}"
    ),

    fun_decl=FunDecl(
        "fun", G.id,
        "(", G.fun_arg_list, ")",
        ":", G.type_ref,
        Opt("=", G.expr)
    ),

    lambda_arg_decl=LambdaArgDecl(
        G.id, Opt(":", G.type_ref), Opt("=", G.expr)
    ),

    fun_arg_decl=FunArgDecl(G.id, ":", G.type_ref, Opt("=", G.expr)),

    fun_arg_list=List(G.fun_arg_decl, empty_valid=True, sep=","),
    lambda_arg_list=List(G.lambda_arg_decl, empty_valid=True, sep=","),

    field_decl=FieldDecl(
        G.id,
        ":",
        G.type_ref
    ),

    decl=FullDecl(
        G.doc, List(G.decl_annotation, empty_valid=True),
        Or(
            G.class_decl,
            G.fun_decl,
            G.grammar_decl,
            G.grammar_rule,
            G.field_decl
        ),
    ),

    type_ref=Or(
        GenericTypeRef(
            G.basic_name,
            "[", List(G.type_ref, empty_valid=False, sep=","), "]"
        ),
        SimpleTypeRef(G.basic_name),
    ),

    decls=List(G.decl, empty_valid=True),

    val_decl=ValDecl(
        "val", G.id, Opt(":", G.type_ref), "=", G.expr
    ),

    bind_decl=BindDecl("bind", G.id, "=", G.expr),

    block=BlockExpr(
        "{",
        # TODO: Add discard/ignore in the list
        List(Or(G.val_decl, G.bind_decl), empty_valid=False),
        G.expr,
        "}"
    ),

    expr=Or(
        BinOp(G.expr, Or(Op.alt_or("or"), Op.alt_and("and")), G.rel),
        G.rel
    ),


    rel=Or(
        BinOp(G.rel, Or(Op.alt_lte("<="),
                        Op.alt_lt("<"),
                        Op.alt_gte(">="),
                        Op.alt_gt(">"),
                        Op.alt_eq("=")), G.arith_1),
        G.arith_1
    ),

    arith_1=Or(
        BinOp(G.arith_1, Or(Op.alt_plus("+"), Op.alt_minus("-"),
                            Op.alt_amp("&")), G.arith_2),
        G.arith_2
    ),

    arith_2=Or(
        BinOp(G.arith_2,
              Or(Op.alt_mult("*"), Op.alt_div("/")), G.isa_or_primary),
        G.isa_or_primary
    ),

    isa_or_primary=Or(
        Isa(G.primary, "isa", G.type_ref),
        G.primary
    ),

    primary=Or(
        G.null,
        G.lambda_expr,
        G.array_literal,
        G.block,
        G.if_expr,
        G.raise_expr,
        G.try_expr,
        G.num_lit,
        G.string_lit,
        G.logic,
    ),

    match_expr=MatchExpr(
        "match", G.expr, "{",
        List(MatchBranch("case",
                         MatchValDecl(G.id, Opt(":", G.type_ref)),
                         "=>", G.expr)),
        "}"
    ),

    num_lit=NumLit(Lex.Number),
    string_lit=StringLit(Lex.String),

    if_expr=IfExpr(
        "if", G.expr, "then", G.expr,
        List(ElsifBranch("elif", G.expr, "then", G.expr), empty_valid=True),
        "else", G.expr
    ),

    raise_expr=RaiseExpr("raise", G.expr),
    try_expr=TryExpr("try", G.expr, Opt("or", G.expr)),

    array_literal=ArrayLiteral(
        "[", List(G.expr, sep=",", empty_valid=True), "]"
    ),

    logic=Or(
        LogicExpr("%", G.basic_expr),
        G.basic_expr
    ),

    basic_expr=Or(
        CallExpr(G.basic_expr, "(", G.params, ")"),
        NullCondCallExpr(G.basic_expr, "?", "(", G.params, ")"),
        GenericInstantiation(G.basic_expr, "[", G.params, "]"),
        ErrorOnNull(G.basic_expr, "!"),
        DotExpr(G.basic_expr, ".", G.id),
        NullCondDottedName(G.basic_expr, "?", ".", G.id),
        G.term
    ),

    term=Or(
        ParenExpr("(", G.expr, ")"),
        G.match_expr,
        G.id
    ),

    basic_name=Or(
        DotExpr(G.basic_name, ".", G.id),
        G.id
    ),


    lambda_expr=LambdaExpr("(", G.lambda_arg_list, ")", "=>", cut(), G.expr),

    null=NullLit("null"),

    params=List(G.param, sep=",", empty_valid=True),

    decl_annotation=DeclAnnotation(
        "@", G.id, Opt("(", G.params, ")")
    ),

    param=Param(Opt(G.id, "="), G.expr),
)
