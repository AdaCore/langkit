from __future__ import absolute_import, division, print_function

from langkit.dsl import (
    ASTNode, AbstractField, Annotations, Field, NullField, Struct, T,
    UserField, abstract, synthetic
)
from langkit.envs import EnvSpec, add_env, add_to_env_kv, do
from langkit.expressions import (
    AbstractProperty, Entity, If, No, Not, Or, Property, PropertyError,
    Self, String, Var, ignore, langkit_property
)
from langkit.parsers import (
    Discard, Grammar, List, NoBacktrack as cut, Opt, Or as GOr
)


from language.lexer import lkt_lexer as Lex


class TypingResult(Struct):
    """
    Result for a call to ``Expr.expr_type``. The result can be either the
    expression's type, or an error message. Only one field can have a value. If
    both fields are null, means that ``expr_type`` has been called on a non
    regular expression.
    """
    expr_type = UserField(
        T.TypeDecl.entity, default_value=No(T.TypeDecl.entity)
    )
    error_message = UserField(
        T.String, default_value=No(T.String)
    )


@abstract
class LKNode(ASTNode):
    """
    Root node class for lkt AST nodes.
    """

    @langkit_property(return_type=T.Decl.entity)
    def root_get(entity_name=T.Symbol):
        return Self.unit.root.node_env.get_first(entity_name).cast(T.Decl)

    @langkit_property(return_type=T.TypeDecl.entity)
    def get_builtin_type(entity_name=T.Symbol):
        return Self.root_get(entity_name).cast(T.TypeDecl)

    int_type = Property(
        Self.get_builtin_type('Int'), public=True,
        doc="Unit method. Return the integer builtin type."
    )

    bool_type = Property(
        Self.get_builtin_type('Boolean'), public=True,
        doc="Unit method. Return the boolean builtin type."
    )

    bigint_type = Property(
        Self.get_builtin_type('BigInt'), public=True,
        doc="Unit method. Return the big integer builtin type."
    )

    string_type = Property(
        Self.get_builtin_type('String'), public=True,
        doc="Unit method. Return the string builtin type."
    )

    symbol_type = Property(
        Self.get_builtin_type('Symbol'), public=True,
        doc="Unit method. Return the string builtin type."
    )


class LangkitRoot(LKNode):
    """
    For the moment, root node of a lkt compilation unit.
    """
    decls = Field(type=T.FullDecl.list)

    @langkit_property(external=True,
                      uses_entity_info=False,
                      uses_envs=False,
                      return_type=T.Bool)
    def fetch_prelude():
        """
        External property that will fetch the prelude unit, containing
        predefined types and values.
        """
        pass

    env_spec = EnvSpec(
        do(Self.fetch_prelude)
    )


class FullDecl(LKNode):
    """
    Container for an lkt declaration. Contains the decl node plus the
    documentation and annotations.
    """
    doc = Field(type=T.Doc)
    decl_annotations = Field(type=T.DeclAnnotation.list)
    decl = Field(type=T.Decl)


@abstract
class Decl(LKNode):
    """
    Base class for declarations. Encompasses regular declarations as well as
    special declarations such as grammars, grammar rules, etc.
    """
    annotations = Annotations(custom_short_image=True)

    syn_name = AbstractField(type=T.DefId)
    name = Property(Self.syn_name._.symbol, public=True, doc="""
        Return the symbol corresponding to the name of this declaration.
    """)

    env_spec = EnvSpec(add_to_env_kv(Entity.name, Self))

    @langkit_property(return_type=T.Decl)
    def assert_bare():
        return If(
            Entity.info.rebindings == No(T.EnvRebindings),
            Entity.node,
            PropertyError(T.Decl, "Type decl has rebindings but shouldn't")
        )


@abstract
class Expr(LKNode):
    """
    Base class for expressions. Encompasses regular expressions as well as
    special expressions (grammar expressions, etc).
    """

    @langkit_property(public=True)
    def in_type_ref():
        """
        Return whether this expression is part of a type reference.
        """
        return Not(Self.parents.find(lambda p: p .is_a(T.TypeRef)).is_null)

    in_decl_annotation = Property(
        Not(Self.parents
            .find(lambda p: p .is_a(T.DeclAnnotation)).is_null)
    )

    in_grammar_rule = Property(
        Not(Self.parents
            .find(lambda p: p .is_a(T.GrammarRuleDecl)).is_null)
    )

    @langkit_property()
    def designated_scope():
        # NOTE: for the moment, every dotted expression has a typed value on
        # the RHS, and we'll use the type to go search valid fields and methods
        # on it. We need to add support for packages/namespaces someday.
        return Entity.expr_type.expr_type._.type_scope

    @langkit_property(return_type=T.TypeDecl.entity, activate_tracing=True)
    def expected_type():
        return Entity.parent.match(
            lambda fun_decl=T.FunDecl: fun_decl.return_type.designated_type,

            lambda fun_arg_decl=T.FunArgDecl: fun_arg_decl.get_type,

            lambda match_branch=T.MatchBranch:
            match_branch.parent.parent.cast_or_raise(T.Expr)
            .expr_type.expr_type,

            lambda bin_op=T.BinOp: If(
                # For those operators, there is no expected type flowing from
                # upward: we use the type of the other operand, if it exists.

                # NOTE: In some cases none of the operands will have a type, as
                # in "0 = 0" (because int literals are polymorphic). We
                # consider this an edge case for the moment. Possible solutions
                # would be either forcing the user to qualify the type in those
                # cases, or having no polymorphic expressions at all (which
                # seems impractical for null).
                bin_op.op.is_a(Op.alt_lte, Op.alt_gte, Op.alt_gt, Op.alt_lt,
                               Op.alt_eq),

                If(Entity == bin_op.left, bin_op.left, bin_op.right)
                .expr_type_impl(No(T.TypeDecl.entity), False).expr_type,

                # For other operators, the return type is the same as the type
                # of both operands, so we use the upward flowing type.
                bin_op.expected_type

            ),

            lambda elsif_branch=T.ElsifBranch:
            elsif_branch.parent.cast_or_raise(T.Expr).expected_type,

            lambda if_expr=T.IfExpr: If(
                # If we're resolving the type of the condition expression, then
                # the result type needs to be a boolean.
                Entity == if_expr.cond_expr,
                Self.bool_type,

                # Else, we're in a branch, use the expected type
                if_expr.expected_type
            ),

            lambda _: No(T.TypeDecl.entity)
        )

    @langkit_property(public=True)
    def is_regular_expr():
        """
        Return whether this expression is a regular expression that can be
        evaluated at runtime, in particular:

        * Not part of a type reference.
        * Not part of a declaration annotation.
        * Not a defining identifier.

        TODO: List  to be expanded probably to take into account grammar
        expressions.
        """
        return Not(Or(
            Entity.in_type_ref,
            Entity.is_a(T.DefId),
            Entity.in_decl_annotation,
            Entity.in_grammar_rule
        ))

    @langkit_property(return_type=TypingResult, public=True)
    def expr_type():
        """
        Return the type of this expression, if it is a regular expression (see
        ``is_regular_expr``), null otherwise.
        """
        return If(
            Entity.is_regular_expr,
            Entity.expr_type_impl(Entity.expected_type),
            No(TypingResult),
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def check_expected_type(expected_type=T.TypeDecl.entity,
                            raise_if_no_type=(T.Bool, True)):
        """
        Check that there is an expected type, return it, or raise an error
        otherwise.
        """
        return expected_type.then(
            lambda et: et,
            default_val=If(raise_if_no_type,
                           PropertyError(T.TypeDecl.entity, "no type"),
                           expected_type),
            # TODO: Enhance diagnostics quality ...
        )

    @langkit_property(return_type=TypingResult)
    def expr_type_impl(expected_type=T.TypeDecl.entity,
                       raise_if_no_type=(T.Bool, True)):
        """
        Overriding type specific implementation for Expr.expr_type.
        """
        return TypingResult.new(
            expr_type=Entity.check_expected_type(expected_type,
                                                 raise_if_no_type),
        )

    @langkit_property(return_type=T.Decl.entity, public=True)
    def referenced_decl():
        """
        Return the declaration referenced by this expression, if applicable,
        null otherwise.
        """
        return No(T.Decl.entity)


class LexerDecl(Decl):
    """
    Declaration of a language's lexer.
    """
    syn_name = Field(type=T.DefId)
    rules = Field(type=T.LKNode.list)


class LexerCaseRule(LKNode):
    """
    Lexer construct to introduce a conditional lexing action.
    """
    expr = Field(type=T.GrammarExpr)
    alts = Field(type=T.BaseLexerCaseRuleAlt.list)


@abstract
class BaseLexerCaseRuleAlt(LKNode):
    """
    Base class for the different kind of alternatives allowed in a case rule.
    """
    send = AbstractField(type=T.LexerCaseRuleSend)


class LexerCaseRuleCondAlt(BaseLexerCaseRuleAlt):
    """
    Alternative of a case rule which sends the token only if the kind of the
    previous token is among a given set.
    """
    cond_exprs = Field(type=T.RefId.list)
    send = Field(type=T.LexerCaseRuleSend)


class LexerCaseRuleDefaultAlt(BaseLexerCaseRuleAlt):
    """
    Default alternative of a case rule which sends the token if all the
    previous alternatives failed.
    """
    send = Field(type=T.LexerCaseRuleSend)


class LexerCaseRuleSend(LKNode):
    """
    Lexer construction used by case alternatives to represent the token to send
    if that alternative was chosen.
    """
    sent = Field(type=T.RefId)
    match_size = Field(type=T.NumLit)


@abstract
class BaseGrammarDecl(Decl):
    """
    Base class for all declarations related to grammars.
    """


class GrammarDecl(BaseGrammarDecl):
    """
    Declaration of a language's grammar.
    """
    syn_name = Field(type=T.DefId)
    rules = Field(type=T.FullDecl.list)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env()
    )


class GrammarRuleDecl(Decl):
    """
    Declaration of a grammar rule inside of a grammar.
    """
    syn_name = Field(type=T.DefId)
    expr = Field(type=T.GrammarExpr)


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
    node_name = Field(type=T.TypeRef)
    sub_exprs = Field(type=T.GrammarExpr.list)


class GrammarRuleRef(GrammarExpr):
    """
    Grammar expression for a reference to another grammar rule.
    """
    node_name = Field(type=T.RefId)


class GrammarDiscard(GrammarExpr):
    """
    Grammar expression to discard the match.
    """
    expr = Field(type=T.GrammarExpr)


class DotExpr(Expr):
    """
    Dotted expression.
    """
    prefix = Field(type=T.Expr)
    suffix = Field(type=T.RefId)


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


class DefId(Id):
    """
    Defining identifier.
    """
    pass


class RefId(Id):
    """
    Reference identifier.
    """

    @langkit_property()
    def referenced_decl():
        return Entity.scope.get_first(Self.symbol).cast(T.Decl)

    @langkit_property(return_type=TypingResult)
    def expr_type_impl(expected_type=T.TypeDecl.entity,
                       raise_if_no_type=(T.Bool, True)):
        ignore(raise_if_no_type)
        id_type = Var(
            Entity.referenced_decl.cast_or_raise(T.BaseValDecl).get_type
        )

        return expected_type.then(
            lambda et: If(
                et.matches(id_type),
                TypingResult.new(expr_type=et),
                et.expected_type_error(
                    String("type ").concat(id_type.syn_name.text)
                )
            ),
            default_val=TypingResult.new(expr_type=id_type)
        )

    @langkit_property()
    def scope():
        return Entity.parent.match(
            lambda de=T.DotExpr: If(
                Entity == de.suffix,
                de.prefix.designated_scope,
                Entity.children_env
            ),
            lambda _: Entity.children_env
        )


class TokenLit(GrammarExpr):
    """
    Grammar expression for a token literal.
    """
    token_node = True


class TokenNoCaseLit(GrammarExpr):
    """
    Grammar expression for a case insensitive token literal.
    """
    lit = Field(type=T.TokenLit)


class TokenPatternLit(GrammarExpr):
    """
    Grammar expression for a pattern literal.
    """
    token_node = True


class GrammarPick(GrammarExpr):
    """
    Grammar expression to pick the significant parse out of a list of parses
    (will automatically discard token results).
    """
    exprs = Field(type=T.GrammarExpr.list)


class GrammarImplicitPick(GrammarPick):
    """
    Implicit pick operation.
    """
    pass


class GrammarToken(GrammarExpr):
    """
    Grammar expression for a token reference.
    """
    token_name = Field(type=T.RefId)
    expr = Field(type=T.TokenLit)


class GrammarOrExpr(GrammarExpr):
    """
    Grammar `Or` expression (disjunctive choice between several grammar
    options).
    """
    sub_exprs = Field(type=T.GrammarExpr.list.list)


class GrammarOpt(GrammarExpr):
    """
    Grammar expression for an optional parsing result.
    """
    expr = Field(type=T.GrammarExpr)


class GrammarOptGroup(GrammarExpr):
    """
    Grammar expression for a group of optional parsing results.
    """
    expr = Field(type=T.GrammarExpr.list)


class GrammarCut(GrammarExpr):
    """
    Grammar expression for a cut.
    """
    pass


class GrammarNull(GrammarExpr):
    """
    Grammar expression to parse a null node.
    """
    name = Field(type=T.TypeRef)


class GrammarSkip(GrammarExpr):
    """
    Grammar expression (error recovery) to skip a parsing result.
    """
    name = Field(type=T.TypeRef)


class GrammarPredicate(GrammarExpr):
    """
    Grammar expression for a predicate: Only parse something if the predicate
    (that is a reference to a node property) returns True.
    """
    expr = Field(type=T.GrammarExpr)
    prop_ref = Field(type=T.Expr)


class GrammarDontSkip(GrammarExpr):
    """
    Grammar expression (error recovery) to ensure that any nested skip parser
    calls won't skip certain parse results.
    """
    expr = Field(type=T.GrammarExpr)
    dont_skip = Field(type=T.GrammarExpr)


class GrammarList(GrammarExpr):
    """
    Grammar expression to parse lists of results. Results can be separated by a
    separator. List can be empty ('*') or not ('+').
    """
    kind = Field(type=T.ListKind)
    node_name = Field(type=T.TypeRef)
    expr = Field(type=T.GrammarExpr)
    sep = Field(type=T.GrammarExpr)


class ListKind(LKNode):
    """
    Kind for list parser expressions.
    """
    enum_node = True
    alternatives = ["one", "zero"]


class DeclBlock(FullDecl.list):
    """
    List of declarations that also introduces a containing lexical scope.
    """
    env_spec = EnvSpec(add_env())


@abstract
class TypeDecl(Decl):
    """
    Abstract base class for type declarations.
    """

    @langkit_property()
    def expected_type_error(got=T.String):
        return TypingResult.new(
            error_message=String("Expected instance of type '")
            .concat(Self.syn_name.text)
            .concat(String("', got ").concat(got))
        )

    @langkit_property()
    def matches(other=T.TypeDecl.entity):
        """
        Return whether ``self`` matches ``other``.
        """
        # TODO: here we can add specific logic wrt. type matching (for example
        # entity/node type equivalence, subtyping, etc). As in ada, we might
        # need several match predicates, for different cases.
        return Entity == other

    @langkit_property(memoized=True)
    def self_decl():
        return SelfDecl.new()

    @langkit_property(memoized=True)
    def node_decl():
        return NodeDecl.new()

    type_scope = AbstractProperty(
        type=T.LexicalEnv, doc="""
        Return the scope for this type, containing methods associated with this
        type.
        """
    )


@abstract
class NamedTypeDecl(TypeDecl):
    """
    Explicit named type declaration.
    """
    decls = AbstractField(type=DeclBlock)

    type_scope = Property(Entity._.decls.children_env)


class GenericDecl(Decl):
    """
    Generic entity declaration.
    """
    generic_formals = Field(type=T.DefId.list)
    decl = Field(type=T.Decl)
    name = Property(Self.decl.name)
    syn_name = NullField()

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env()
    )

    @langkit_property(memoized=True)
    def get_instantiated_type(params=T.TypeDecl.array):
        return InstantiatedGenericType.new(
            inner_type_decl=Self.decl.cast_or_raise(T.TypeDecl),
            params=params
        )


@synthetic
class InstantiatedGenericType(TypeDecl):
    """
    Instantiated generic type.
    """
    inner_type_decl = UserField(type=T.TypeDecl, public=False)
    params = UserField(type=T.TypeDecl.array, public=False)
    syn_name = NullField()

    @langkit_property(memoized=True)
    def get_instantiated_type():
        return InstantiatedGenericTypeInternal.entity.new(
            node=InstantiatedGenericTypeInternal.new(
                inner_type_decl=Self.inner_type_decl
            ),
            info=T.entity_info.new(
                md=No(T.Metadata),
                from_rebound=False,
                rebindings=Entity.info.rebindings
            )
        )

    type_scope = Property(Entity.get_instantiated_type.type_scope)


@synthetic
class InstantiatedGenericTypeInternal(TypeDecl):
    """
    Instantiated generic type.
    """
    inner_type_decl = UserField(type=T.TypeDecl, public=False)
    syn_name = NullField()

    type_scope = Property(Entity.inner_type_decl.as_entity.type_scope)


class EnumTypeDecl(NamedTypeDecl):
    """
    Enum type declaration.
    """

    syn_name = Field(type=T.DefId)
    literals = Field(type=T.EnumLitDecl.list)
    decls = Field(type=DeclBlock)


class StructDecl(NamedTypeDecl):
    """
    Declaration for a LK struct.
    """
    syn_name = Field(type=T.DefId)
    decls = Field(type=DeclBlock)


class ClassDecl(NamedTypeDecl):
    """
    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.
    """
    syn_name = Field(type=T.DefId)
    base_class = Field(type=T.TypeRef)
    decls = Field(type=DeclBlock)


class EnumClassAltDecl(TypeDecl):
    """
    Alternative for an enum class decl.
    """
    syn_name = Field(T.DefId)

    parent_type = Property(Entity.parent.parent.cast_or_raise(T.TypeDecl))

    type_scope = Property(Entity.parent_type.type_scope)


class EnumClassDecl(NamedTypeDecl):
    """
    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.
    """
    syn_name = Field(type=T.DefId)
    alts = Field(type=T.EnumClassAltDecl.list)
    base_class = Field(type=T.TypeRef)
    decls = Field(type=DeclBlock)


class DocComment(LKNode):
    """
    Node for one line of documentation attached to a node.
    """
    token_node = True


class Doc(LKNode):
    """
    Documentation attached to a decl node.
    """
    lines = Field(type=T.DocComment.list)


class FunDecl(Decl):
    """
    Function declaration.
    """
    syn_name = Field(type=T.DefId)
    args = Field(type=T.FunArgDecl.list)
    return_type = Field(type=T.TypeRef)
    body = Field(type=T.Expr)

    owning_type = Property(
        Self.parents.find(lambda t: t.is_a(T.TypeDecl)).cast(T.TypeDecl)
    )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env(),
        add_to_env_kv("self", Self.owning_type._.self_decl),
        add_to_env_kv("node", Self.owning_type._.node_decl),
    )


@abstract
class BaseValDecl(Decl):
    """
    Abstract class for named values declarations, such as arguments, local
    value bindings, fields, etc.
    """
    get_type = AbstractProperty(type=T.TypeDecl.entity)


@synthetic
class SelfDecl(BaseValDecl):
    """
    Synthetic declaration for the implicit "self" variable available in
    properties.
    """

    syn_name = NullField()
    name = Property('self')
    get_type = Property(Entity.parent.cast_or_raise(T.TypeDecl))


@synthetic
class NodeDecl(BaseValDecl):
    """
    Synthetic declaration for the implicit "node" variable available in
    properties.
    """

    syn_name = NullField()
    name = Property('node')
    get_type = Property(Entity.parent.cast_or_raise(T.TypeDecl))


@abstract
class UserValDecl(BaseValDecl):
    """
    Class for user declared val declarations (not synthetic).
    """
    syn_name = Field(type=T.DefId)
    decl_type = AbstractField(type=T.TypeRef)

    get_type = Property(Entity.decl_type.designated_type)


class EnumLitDecl(UserValDecl):
    """
    Enum literal declaration.
    """
    decl_type = NullField()
    get_type = Property(
        Entity.parents.find(lambda t: t.is_a(T.EnumTypeDecl))
        .cast_or_raise(T.TypeDecl)
    )


class FunArgDecl(UserValDecl):
    """
    Function argument declaration.
    """
    decl_type = Field(type=T.TypeRef)
    default_val = Field(type=T.Expr)


class LambdaArgDecl(UserValDecl):
    """
    Function argument declaration.
    """
    decl_type = Field(type=T.TypeRef)
    default_val = Field(type=T.Expr)


class FieldDecl(UserValDecl):
    """
    Field declaration.
    """
    decl_type = Field(type=T.TypeRef)


@abstract
class TypeRef(LKNode):
    """
    Base class for a reference to a type.
    """
    type_name = Field(type=T.Expr)

    designated_entity = Property(
        Entity.type_name.referenced_decl
    )

    designated_type = AbstractProperty(T.TypeDecl.entity)


class SimpleTypeRef(TypeRef):
    """
    Simple reference to a type.
    """

    @langkit_property()
    def designated_type():
        return Entity.designated_entity.cast_or_raise(T.TypeDecl)


class GenericTypeRef(TypeRef):
    """
    Reference to a generic type.
    """
    params = Field(type=T.TypeRef.list)

    @langkit_property()
    def designated_type():
        generic_decl = Var(
            Entity.designated_entity.cast_or_raise(T.GenericDecl)
        )
        return generic_decl.get_instantiated_type(
            Entity.params.map(
                lambda p: p.designated_type.assert_bare
                .cast_or_raise(T.TypeDecl)
            )
        ).as_bare_entity


class ArrayLiteral(Expr):
    """
    Literal for an array value.
    """
    exprs = Field(type=T.Expr.list)


class NotExpr(Expr):
    """
    Boolean negation expression.
    """
    expr = Field(type=T.Expr)


class Isa(Expr):
    """
    Isa expression.
    """
    expr = Field(type=T.Expr)
    dest_type = Field(type=T.TypeRef.list)


class DeclAnnotation(LKNode):
    """
    Compile time annotation attached to a declaration.
    """
    name = Field(type=T.Id)
    params = Field(type=T.Param.list)


class Param(LKNode):
    """
    Parameter for function calls or for annotations.
    """
    name = Field(type=T.RefId)
    value = Field(type=T.Expr)


class ParenExpr(Expr):
    """
    Parenthesized expression.
    """
    expr = Field(type=T.Expr)


class CallExpr(Expr):
    """
    Call expression.
    """
    name = Field(type=T.Expr)
    args = Field(type=T.Param.list)


class NullCondCallExpr(CallExpr):
    """
    Null conditional call expression.
    """
    pass


class GenericInstantiation(Expr):
    """
    Generic instantiation.
    """
    name = Field(type=T.Expr)
    args = Field(type=T.Param.list)


class ErrorOnNull(Expr):
    """
    Expression that throws an error if LHS is null.
    """
    expr = Field(type=T.Expr)


class LambdaExpr(Expr):
    """
    Lambda expression.
    """
    params = Field(type=T.LambdaArgDecl.list)
    body = Field(type=T.Expr)


class TryExpr(Expr):
    """
    Try expression.
    """
    try_expr = Field(type=T.Expr)
    or_expr = Field(type=T.Expr)


class RaiseExpr(Expr):
    """
    Raise expression.
    """
    except_expr = Field(type=T.Expr)


class IfExpr(Expr):
    """
    If expression.
    """
    cond_expr = Field(type=T.Expr)
    then_expr = Field(type=T.Expr)
    alternatives = Field(type=T.ElsifBranch.list)
    else_expr = Field(type=T.Expr)


class ElsifBranch(LKNode):
    """
    Elsif branch of an if expression.
    """
    cond_expr = Field(type=T.Expr)
    then_expr = Field(type=T.Expr)


class BlockExpr(Expr):
    """
    Block expression.
    """
    val_defs = Field(type=T.LKNode.list)
    expr = Field(type=T.Expr)

    env_spec = EnvSpec(add_env())


class MatchExpr(Expr):
    """
    Binary operator expression.
    """
    match_expr = Field(type=T.Expr)
    branches = Field(type=T.MatchBranch.list)


class MatchBranch(LKNode):
    """
    Branch inside a match expression.
    """
    decl = Field(type=T.MatchValDecl)
    expr = Field(type=T.Expr)

    env_spec = EnvSpec(
        add_env()
    )


class MatchValDecl(UserValDecl):
    """
    Value declaration in a match branch.
    """
    decl_type = Field(type=T.TypeRef)


class BinOp(Expr):
    """
    Binary operator expression.
    """
    left = Field(type=T.Expr)
    op = Field(type=T.Op)
    right = Field(type=T.Expr)


class ValDecl(UserValDecl):
    """
    Value declaration.
    """
    decl_type = Field(type=T.TypeRef)
    val = Field(type=T.Expr)


class Op(LKNode):
    """
    Operator in a binary operator expression.
    """
    enum_node = True

    alternatives = ["and", "or", "plus", "minus", "eq", "mult", "div",
                    "lt", "gt", "lte", "gte", "amp"]


@abstract
class Lit(Expr):
    """
    Base class for literals.
    """

    @langkit_property(return_type=TypingResult)
    def expr_type_impl(expected_type=T.TypeDecl.entity,
                       raise_if_no_type=(T.Bool, True)):
        exp_type = Var(
            Entity.check_expected_type(expected_type, raise_if_no_type)
        )
        return If(
            Self.lit_predicate(exp_type),
            TypingResult.new(expr_type=exp_type),
            exp_type._.expected_type_error(got=Self.lit_error_name),
        )

    @langkit_property(return_type=T.Bool)
    def lit_predicate(expected_type=T.TypeDecl.entity):
        """
        Predicate to return whether the expected type is a valid type for this
        literal. Default implementation always returns True.
        """
        ignore(expected_type)
        return True

    lit_error_name = Property(String("<not implemented>"))


class NullLit(Lit):
    """
    Null literal expression.
    """
    token_node = True


class StringLit(Lit):
    """
    String literal expression.
    """
    token_node = True

    @langkit_property()
    def lit_predicate(expected_type=T.TypeDecl.entity):
        return Or(expected_type == Self.string_type,
                  expected_type == Self.symbol_type)

    lit_error_name = Property(String("a string literal"))


class NumLit(Lit):
    """
    Number literal expression.
    """
    token_node = True

    @langkit_property()
    def lit_predicate(expected_type=T.TypeDecl.entity):
        return Or(expected_type == Self.int_type,
                  expected_type == Self.bigint_type)

    lit_error_name = Property(String("a number literal"))


class VarBind(LKNode):
    """
    Dynamic var bind expression.
    """
    name = Field(type=T.RefId)
    expr = Field(type=T.Expr)


class LogicExpr(Expr):
    """
    Class for logic expressions (any ``basic_expr`` starting with %).
    """
    expr = Field(type=T.Expr)


lkt_grammar = Grammar('main_rule')
G = lkt_grammar
lkt_grammar.add_rules(
    main_rule=LangkitRoot(
        G.decls, Lex.Termination
    ),
    id=Id(Lex.Identifier),
    ref_id=RefId(Lex.Identifier),
    def_id=DefId(Lex.Identifier),

    doc_comment=DocComment(Lex.DocComment),

    doc=Doc(List(G.doc_comment, empty_valid=True)),

    lexer_decl=LexerDecl(
        "lexer", G.def_id, "{",
        List(Or(G.decl, G.lexer_case_rule), empty_valid=True),
        "}"
    ),
    grammar_decl=GrammarDecl(
        "grammar", G.def_id,
        "{", List(G.decl, empty_valid=True), "}"
    ),
    grammar_rule=GrammarRuleDecl(G.def_id, "<-", G.grammar_expr),
    lexer_case_rule=LexerCaseRule(
        "match", G.grammar_primary, "{",
            List(G.lexer_case_alt, empty_valid=False),
        "}"
    ),
    lexer_case_alt=GOr(
        LexerCaseRuleCondAlt(
            "if",
            Lex.Identifier(match_text="previous_token"),
            "isa", List(G.ref_id, sep="|", empty_valid=False),
            "then", G.lexer_case_send
        ),
        LexerCaseRuleDefaultAlt(
            "else", G.lexer_case_send
        )
    ),
    lexer_case_send=LexerCaseRuleSend(
        Lex.Identifier(match_text="send"), "(", G.ref_id, ",", G.num_lit, ")"
    ),
    grammar_primary=GOr(
        G.token_literal,
        G.token_no_case_literal,
        G.token_pattern_literal,
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
        G.grammar_discard_expr,
    ),
    grammar_expr=GOr(
        GrammarDontSkip(
            G.grammar_expr,
            ".", Lex.Identifier("dont_skip"),
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

    grammar_opt=GOr(
        GrammarOpt("?", G.grammar_expr),
        GrammarOptGroup("?", "(", List(G.grammar_expr, empty_valid=True), ")"),
    ),

    grammar_cut=GrammarCut("/"),
    grammar_or_expr=GrammarOrExpr(
        "or", "(",
        Opt("|"), List(List(G.grammar_expr), sep="|"),
        ")"
    ),
    grammar_discard_expr=GrammarDiscard(
        "discard", "(", G.grammar_expr, ")"
    ),
    token_literal=TokenLit(Lex.String),
    token_no_case_literal=TokenNoCaseLit(
        Lex.Identifier(match_text="no_case"), "(", G.token_literal, ")"
    ),
    token_pattern_literal=TokenPatternLit(Lex.PString),
    parse_node_expr=ParseNodeExpr(
        G.type_ref, "(", List(G.grammar_expr, empty_valid=True), ")"
    ),
    grammar_rule_ref=GrammarRuleRef(G.ref_id),
    grammar_list_expr=GrammarList(
        GOr(ListKind.alt_one("list+"), ListKind.alt_zero("list*")),
        Opt(G.type_ref),
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
        "@", G.ref_id, Opt("(", G.token_literal, ")")
    ),

    type_decl=Or(
        StructDecl("struct", G.def_id, "{", G.decl_block, "}"),

        EnumClassDecl(
            "enum", "class", G.def_id,
            "(", List(EnumClassAltDecl(G.def_id), sep=","), ")",
            Opt(":", G.type_ref),
            "{",
            G.decl_block,
            "}"
        ),

        ClassDecl(
            "class", G.def_id, Opt(":", G.type_ref), "{",
            G.decl_block,
            "}"
        ),

        EnumTypeDecl(
            "enum", G.def_id, "(", List(G.enum_lit_decl, sep=","), ")",
            "{", G.decl_block, "}"
        ),

    ),

    generic_decl=GenericDecl(
        "generic", "[", List(G.def_id, sep=","), "]", G.bare_decl
    ),

    enum_lit_decl=EnumLitDecl(G.def_id),

    fun_decl=FunDecl(
        "fun", G.def_id,
        "(", G.fun_arg_list, ")",
        ":", G.type_ref,
        Opt("=", G.expr)
    ),

    lambda_arg_decl=LambdaArgDecl(
        G.def_id, Opt(":", G.type_ref), Opt("=", G.expr)
    ),

    fun_arg_decl=FunArgDecl(G.def_id, ":", G.type_ref, Opt("=", G.expr)),

    fun_arg_list=List(G.fun_arg_decl, empty_valid=True, sep=","),
    lambda_arg_list=List(G.lambda_arg_decl, empty_valid=True, sep=","),

    field_decl=FieldDecl(
        G.def_id,
        ":",
        G.type_ref
    ),

    bare_decl=GOr(
        G.generic_decl,
        G.type_decl,
        G.fun_decl,
        G.lexer_decl,
        G.grammar_decl,
        G.grammar_rule,
        G.field_decl,
        G.val_decl
    ),

    decl=FullDecl(
        G.doc, List(G.decl_annotation, empty_valid=True), G.bare_decl
    ),

    type_ref=GOr(
        GenericTypeRef(
            G.basic_name,
            "[", List(G.type_ref, empty_valid=False, sep=","), "]"
        ),
        SimpleTypeRef(G.basic_name),
    ),

    decls=List(G.decl, empty_valid=True),
    decl_block=List(G.decl, empty_valid=True, list_cls=DeclBlock),

    val_decl=ValDecl(
        "val", G.def_id, Opt(":", G.type_ref), "=", G.expr
    ),

    var_bind=VarBind("bind", G.ref_id, "=", G.expr),

    block=BlockExpr(
        "{",
        # TODO: Add discard/ignore in the list
        List(GOr(G.val_decl, G.var_bind), empty_valid=False, sep=";"),
        ";",
        G.expr,
        "}"
    ),

    expr=GOr(
        BinOp(G.expr, GOr(Op.alt_or("or"), Op.alt_and("and")), G.rel),
        G.rel
    ),

    rel=GOr(
        NotExpr("not", G.eq),
        G.eq
    ),

    eq=GOr(
        BinOp(G.eq, GOr(Op.alt_lte("<="),
                        Op.alt_lt("<"),
                        Op.alt_gte(">="),
                        Op.alt_gt(">"),
                        Op.alt_eq("=")), G.arith_1),
        G.arith_1
    ),

    arith_1=GOr(
        BinOp(G.arith_1, GOr(Op.alt_plus("+"),
                             Op.alt_minus("-"),
                             Op.alt_amp("&")), G.arith_2),
        G.arith_2
    ),

    arith_2=GOr(
        BinOp(G.arith_2,
              GOr(Op.alt_mult("*"), Op.alt_div("/")), G.isa_or_primary),
        G.isa_or_primary
    ),

    isa_or_primary=GOr(
        Isa(G.primary, "isa", List(G.type_ref, sep="|", empty_valid=False)),
        G.primary
    ),

    primary=GOr(
        G.lambda_expr,
        G.if_expr,
        G.raise_expr,
        G.try_expr,
        G.logic,
    ),

    match_expr=MatchExpr(
        "match", G.expr, "{",
        List(MatchBranch("case",
                         MatchValDecl(G.def_id, Opt(":", G.type_ref)),
                         "=>", G.expr)),
        "}"
    ),

    num_lit=NumLit(Lex.Number),
    string_lit=StringLit(GOr(Lex.String, Lex.PString)),

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

    logic=GOr(
        LogicExpr("%", G.basic_expr),
        G.basic_expr
    ),

    basic_expr=GOr(
        CallExpr(G.basic_expr, "(", G.params, ")"),
        NullCondCallExpr(G.basic_expr, "?", "(", G.params, ")"),
        GenericInstantiation(G.basic_expr, "[", G.params, "]"),
        ErrorOnNull(G.basic_expr, "!"),
        DotExpr(G.basic_expr, ".", G.ref_id),
        NullCondDottedName(G.basic_expr, "?", ".", G.ref_id),
        G.term
    ),

    term=GOr(
        ParenExpr("(", G.expr, ")"),
        G.match_expr,
        G.null_lit,
        G.ref_id,
        G.block,
        G.num_lit,
        G.string_lit,
        G.array_literal,
    ),

    basic_name=GOr(
        DotExpr(G.basic_name, ".", G.ref_id),
        G.ref_id
    ),


    lambda_expr=LambdaExpr("(", G.lambda_arg_list, ")", "=>", cut(), G.expr),

    null_lit=NullLit("null"),

    params=List(G.param, sep=",", empty_valid=True),

    decl_annotation=DeclAnnotation(
        "@", G.id, Opt("(", G.params, ")")
    ),

    param=Param(Opt(G.ref_id, "="), G.expr),
)
