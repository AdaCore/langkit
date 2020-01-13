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
    """
    For the moment, root node of a lkt compilation unit.
    """
    decls = Field()


@abstract
class Decl(LKNode):
    """
    Base class for declarations. Encompasses regular declarations as well as
    special declarations such as grammars, grammar rules, etc.
    """
    pass


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


@abstract
class Name(Expr):
    """
    Name referencing an entity.
    """
    pass


class DottedName(Name):
    """
    Dotted qualified name.
    """
    prefix = Field()
    suffix = Field()


class Id(Name):
    """
    Identifier.
    """
    token_node = True


class TokenLit(GrammarExpr):
    """
    Grammar expression for a token literal.
    """
    token_node = True


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
        "grammar", G.id, "{", List(G.grammar_rule, empty_valid=True), "}"
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
        "class", G.id, Opt(":", G.dotted_name), "{",
        G.decls,
        "}"
    ),

    regular_decl=Or(
        G.class_decl,
    ),

    decls=List(G.regular_decl, empty_valid=True),
)
