from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract
from langkit.parsers import Grammar, List, Opt, Or

from language.lexer import lkt_lexer as Lex


@abstract
class LKNode(ASTNode):
    """
    Root node class for lkt AST nodes.
    """
    pass


class LangkitRoot(LKNode):
    decls = Field()


@abstract
class Decl(LKNode):
    pass


@abstract
class Expr(LKNode):
    pass


class GrammarDecl(Decl):
    name = Field()
    rules = Field()


class GrammarRuleDecl(Decl):
    name = Field()
    expr = Field()


@abstract
class GrammarExpr(Expr):
    pass


class ParseNodeExpr(GrammarExpr):
    node_name = Field()
    sub_exprs = Field()


class GrammarRuleRef(GrammarExpr):
    node_name = Field()


@abstract
class Name(Expr):
    pass


class DottedName(Name):
    prefix = Field()
    suffix = Field()


class Id(Name):
    token_node = True


class TokenLit(GrammarExpr):
    token_node = True


class GrammarToken(GrammarExpr):
    token_name = Field()
    expr = Field()


class GrammarOrExpr(GrammarExpr):
    sub_exprs = Field()


class GrammarOpt(GrammarExpr):
    expr = Field()


class GrammarOptGroup(GrammarExpr):
    expr = Field()


class GrammarCut(GrammarExpr):
    pass


class GrammarNull(GrammarExpr):
    name = Field()


class GrammarSkip(GrammarExpr):
    name = Field()


class GrammarPredicate(GrammarExpr):
    expr = Field()
    prop_ref = Field()


class GrammarDontSkip(GrammarExpr):
    expr = Field()
    dont_skip = Field()


class GrammarList(GrammarExpr):
    kind = Field()
    expr = Field()
    sep = Field()


class ListKind(LKNode):
    enum_node = True
    alternatives = ["one", "zero"]


class ClassDecl(LKNode):
    """
    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.
    """
    name = Field()
    base_class = Field()
    decls = Field()


lkt_grammar = Grammar('main_rule')
G = lkt_grammar
lkt_grammar.add_rules(
    main_rule=LangkitRoot(
        List(G.root_decl, empty_valid=True), Lex.Termination
    ),
    root_decl=Or(G.grammar_decl, G.regular_decl),
    id=Id(Lex.Identifier),

    dotted_name=Or(
        DottedName(G.dotted_name, ".", G.id),
        G.id
    ),

    grammar_decl=GrammarDecl(
        "grammar", G.id, "is", List(G.grammar_rule, empty_valid=True), "end"
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
            "(", G.dotted_name, ")"
        ),
        G.grammar_primary
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
        "(", G.grammar_expr, Opt(",", G.grammar_expr), ")",
    ),

    grammar_skip=GrammarSkip(
        Lex.Identifier(match_text="skip"), "(", G.id, ")"
    ),

    grammar_null=GrammarNull(
        Lex.Identifier(match_text="skip"), "(", G.id, ")"
    ),

    grammar_token=GrammarToken(
        "@", G.id, Opt("(", G.token_literal, ")")
    ),

    class_decl=ClassDecl(
        "class", G.id, Opt(":", G.dotted_name), "is",
        G.decls,
        "end"
    ),

    regular_decl=Or(
        G.class_decl,
    ),

    decls=List(G.regular_decl, empty_valid=True),
)
