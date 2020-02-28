from __future__ import absolute_import, division, print_function

from langkit.dsl import (
    ASTNode, AbstractField, Annotations, Field, LookupKind as LK, NullField,
    Struct, T, UserField, abstract, synthetic
)
from langkit.envs import EnvSpec, add_env, add_to_env_kv, do
from langkit.expressions import (
    AbstractProperty, And, CharacterLiteral, Cond, EmptyEnv, Entity, If, No,
    Not, Or, Property, PropertyError, Self, String, Var, ignore,
    langkit_property
)
from langkit.parsers import (Grammar, List, NoBacktrack as cut, Null, Opt,
                             Or as GOr, Pick)


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


class EnvKV(Struct):
    """
    Utility struct that represents a key value pair in a lexical environment.
    """
    key = UserField(T.Symbol)
    value = UserField(T.LKNode)


class ParamMatch(Struct):
    """
    Helper data structure to implement parameter matching.
    """
    has_matched = UserField(type=T.Bool, default_value=True)
    actual = UserField(type=T.Param.entity)
    formal = UserField(type=T.BaseValDecl.entity)


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

    regexp_type = Property(
        Self.get_builtin_type('Regexp'), public=True,
        doc="Unit method. Return the regexp builtin type."
    )

    @langkit_property(external=True,
                      uses_entity_info=False,
                      uses_envs=True,
                      return_type=T.LexicalEnv)
    def env_from_vals_internal(vals=EnvKV.array):
        """
        Internal property that will create a lexical environment from a list of
        key values associations. The lexical environment will not be
        ref-counted, but instead associated with the node's analysis unit, so
        being careful about how this property is used is important.
        """
        pass

    @langkit_property()
    def static_match_params(formals=T.BaseValDecl.entity.array,
                            actuals=T.Param.entity.array):
        """
        Static method. Returns an array of ParamMatch structures, matching the
        actual parameters of a call to the formal parameters of the
        declaration. This will work for types constructors and function calls.
        """
        return actuals.map(lambda i, a: If(
            a.name.is_null,
            formals.at(i).then(lambda f: ParamMatch.new(actual=a, formal=f)),
            a.name.then(
                lambda n: formals.find(lambda f: f.name == n.symbol).then(
                    lambda f: ParamMatch.new(actual=a, formal=f)
                )
            )
        ))

    @langkit_property(return_type=T.String)
    def string_join(strns=T.String.array, sep=T.String):
        """
        Static method. Return the array of strings joined by separator ``sep``.
        """
        arr_len = Var(strns.length)

        return strns.mapcat(lambda i, n: (
            If(i == arr_len - 1, n, n.concat(sep))
        ))

    @langkit_property(external=True, uses_entity_info=False, uses_envs=False,
                      return_type=T.AnalysisUnit)
    def internal_fetch_referenced_unit(name=T.String):
        """
        Return the unit that this unit name designates. Load it if needed.
        """
        pass


class LangkitRoot(LKNode):
    """
    For the moment, root node of a lkt compilation unit.
    """
    imports = Field(type=T.Import.list)
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


class Import(LKNode):
    """
    Statement to import another source file.
    """
    name = Field(type=T.RefId)

    @langkit_property(public=True)
    def referenced_unit():
        """
        Return the unit that this import statements designates. Load it if
        needed.
        """
        return Self.internal_fetch_referenced_unit(Self.name.text)

    env_spec = EnvSpec(
        do(Self.referenced_unit)
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

    full_name = Property(
        Self.name.image, doc="""
        Return the full name of this type, as it should be seen by users/shown
        in diagnostics.
        """
    )

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

            lambda p=T.Param: p.call_expr.match_params().find(
                lambda pm: pm.actual == p
            ).then(lambda pm: pm.formal.get_type),

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
            Entity.in_grammar_rule,
            # TODO: Implement function types
            Entity.referenced_decl.is_a(T.TypeDecl, T.GenericDecl, T.FunDecl)
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
    def check_type(typ=T.TypeDecl.entity, raise_if_no_type=(T.Bool, True)):
        """
        Check that there is an expected type, return it, or raise an error
        otherwise.
        """
        return typ.then(
            lambda et: et,
            default_val=If(raise_if_no_type,
                           PropertyError(T.TypeDecl.entity, "no type"),
                           typ),
            # TODO: Enhance diagnostics quality ...
        )

    @langkit_property(return_type=T.TypeDecl.entity, public=True)
    def expr_context_free_type():
        """
        If the type of this expression can be determined with no bottom up
        context, return it. This will be used by `expr_type_impl`'s default
        implementation.
        """
        return No(T.TypeDecl.entity)

    @langkit_property(return_type=T.Bool)
    def expected_type_predicate(expected_type=T.TypeDecl.entity):
        """
        Predicate to return whether the expected type is a valid type for this
        expression. This is only called if there is no context-free type
        available for this expression.

        .. note::
            Default implementation always returns True.
        """
        ignore(expected_type)
        return True

    @langkit_property()
    def invalid_expected_type_error_name():
        """
        Error string to use as the actual when the expected type was not a
        valid type for this expression. This can only be called if there is no
        context-free type available for this expression.
        """
        return String("<not implemented>")

    @langkit_property(return_type=TypingResult)
    def expr_type_impl(expected_type=T.TypeDecl.entity,
                       raise_if_no_type=(T.Bool, True)):
        """
        Implementation for Expr.expr_type. This is the core of the current type
        system for lkt. Default implementation has the following behavior:

        - If there is a context free type for this expression *and* an expected
          type, then check that they match, if they don't, return an error
          TypingResult.

        - If there is an expected type but no context-free type, check that
          the expected type is valid by calling the expected_type_predicate
          property and return the expected type, otherwise fail.

        - If there is only a context free type but no expected type, return
          the context-free type.

        - If there is none, raise a PropertyError.

        Implementing typing for new constructs should be simple:

        - If the construct has a definite type regardless of the context (for
          example, a variable reference), then just override
          ``expr_context_free_type``.

        - If the construct has no definite type but imposes some constraints on
          the typing - for example, a string literal will require it's argument
          to be either a string or a symbol - override
          ``expected_type_predicate`` and ``invalid_expected_type_error_name``.

        - If the construct has no definite type and no constraints, you have
          nothing to do.

        - If the construct need rules that have not been planned by the above,
          you can still override ``expr_type_impl``. This should not be
          necessary though, and should only be done following a discussion with
          LKT devs, and altering this comment afterwards.
        """
        cf_type = Var(Entity.expr_context_free_type)

        return Cond(
            # We have both a context free type and an expected type: check that
            # they match.
            And(Not(expected_type.is_null), Not(cf_type.is_null)),
            If(
                expected_type.matches(cf_type),
                TypingResult.new(expr_type=expected_type),
                expected_type.expected_type_error(
                    String("type ").concat(cf_type.full_name)
                )
            ),

            Not(expected_type.is_null),
            If(
                Entity.expected_type_predicate(expected_type),
                TypingResult.new(expr_type=expected_type),
                expected_type.expected_type_error(
                    got=Self.invalid_expected_type_error_name
                )
            ),

            Not(cf_type.is_null),
            TypingResult.new(expr_type=cf_type),

            # We don't have both types: check that there is at least one and
            # return it, else, raise an error if raise_if_no_type is true.
            TypingResult.new(expr_type=Entity.check_type(
                expected_type._or(cf_type), raise_if_no_type
            )),
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


class LexerFamilyDecl(Decl):
    """
    Declaration of a token family.
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

    referenced_decl = Property(Entity.suffix.referenced_decl)

    expr_context_free_type = Property(Entity.suffix.expr_context_free_type)


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
    annotations = Annotations(custom_short_image=True)

    @langkit_property()
    def referenced_decl():
        lk = Var(If(Entity.dot_expr_if_suffix.is_null, LK.recursive, LK.flat))
        return Entity.scope.get_first(Self.symbol, lookup=lk).cast(T.Decl)

    expr_context_free_type = Property(
        Entity.referenced_decl.cast_or_raise(T.BaseValDecl).get_type
    )

    dot_expr_if_suffix = Property(
        Entity.parent.cast(T.DotExpr).then(
            lambda de: If(Entity == de.suffix, de, No(T.DotExpr.entity)))
    )

    @langkit_property()
    def scope():
        return Entity.dot_expr_if_suffix.then(
            lambda de: de.prefix.designated_scope,
            default_val=Entity.children_env
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
    list_type = Field(type=T.TypeRef)
    kind = Field(type=T.ListKind)
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
            .concat(Self.full_name)
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

    @langkit_property()
    def get_fun(name=T.Symbol):
        """
        Return the function with name ``name`` for this type.
        """
        return Entity.type_scope.get_first(name).cast_or_raise(T.FunDecl)

    fields = Property(
        No(T.BaseValDecl.entity.array),
        doc="""Return the list of fields for this type"""
    )


class GenericFormalTypeDecl(TypeDecl):
    """
    Declaration of a generic formal type in a generic declaration.
    """
    syn_name = Field(T.TypeDefId)
    type_scope = Property(EmptyEnv)


@abstract
class NamedTypeDecl(TypeDecl):
    """
    Explicit named type declaration.
    """
    decls = AbstractField(type=DeclBlock)

    type_scope = Property(Entity._.decls.children_env)

    fields = Property(Entity.decls.filtermap(
        lambda d: d.decl.cast_or_raise(T.BaseValDecl),
        lambda d: d.decl.is_a(T.BaseValDecl)
    ))


class GenericParamAssoc(Struct):
    """
    Generic parameter association. Associates a generic formal type to an
    actual type.
    """
    formal = UserField(T.GenericFormalTypeDecl)
    actual = UserField(T.TypeDecl)


class GenericDecl(Decl):
    """
    Generic entity declaration.
    """
    annotations = Annotations(rebindable=True)

    generic_formals = Field(type=T.GenericFormalTypeDecl.list)
    decl = Field(type=T.Decl)
    name = Property(Self.decl.name)
    syn_name = NullField()

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env()
    )

    @langkit_property(memoized=True)
    def get_instantiated_type(actuals=T.TypeDecl.array):
        return InstantiatedGenericType.new(
            inner_type_decl=Self.decl.cast_or_raise(T.TypeDecl),
            actuals=actuals
        )

    @langkit_property(memoized=True)
    def get_assocs(actuals=T.TypeDecl.array):
        """
        Return an array of GenericParamAssocs, associating the generic formal
        types of this generic declaration with actuals given as parameter.
        """
        return If(
            Entity.generic_formals.length == actuals.length,
            Entity.generic_formals.map(
                lambda idx, f: GenericParamAssoc.new(
                    formal=f.node, actual=actuals.at(idx)
                )
            ),
            PropertyError(T.GenericParamAssoc.array,
                          "Actuals and formals don't match")
        )

    @langkit_property(memoized=True)
    def get_inst_env(actuals=T.TypeDecl.array):
        """
        Return the lexical environment associating generic formals for this
        generic decl with actuals given as parameters. Used to create the
        rebinding environment for a generic instantiation.
        """
        return Entity.env_from_vals_internal(
            Entity.get_assocs(actuals).map(
                lambda assoc: EnvKV.new(
                    key=assoc.formal.name, value=assoc.actual
                )
            )
        )


@synthetic
class InstantiatedGenericType(TypeDecl):
    """
    Instantiated generic type.
    """
    inner_type_decl = UserField(type=T.TypeDecl, public=False)
    actuals = UserField(type=T.TypeDecl.array, public=False)
    syn_name = NullField()
    name = Property(Self.inner_type_decl.name)

    full_name = Property(
        Self.name.image.concat(String('[')).concat(
            Self.string_join(Self.actuals.map(lambda t: t.full_name),
                             sep=String(", "))
        ).concat(String(']'))
    )

    generic_decl = Property(
        Entity.inner_type_decl.parent.as_entity.cast_or_raise(T.GenericDecl)
    )

    @langkit_property(memoized=True, public=True)
    def get_instantiated_type():
        """
        Return the instantiated type decl with proper rebindings, that can be
        used for typing and code generation.
        """
        return TypeDecl.entity.new(
            node=Self.inner_type_decl,
            info=T.entity_info.new(
                md=No(T.Metadata),
                from_rebound=False,
                rebindings=Entity.info.rebindings.append_rebinding(
                    Self.inner_type_decl.parent.children_env,
                    Entity.generic_decl.get_inst_env(Self.actuals)
                )
            )
        )

    type_scope = Property(Entity.get_instantiated_type.type_scope)

    fields = Property(Entity.get_instantiated_type.fields)


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

    designated_type = AbstractProperty(T.TypeDecl.entity, public=True, doc="""
        Return the type designated by this type ref.
    """)


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

    call_expr = Property(
        Entity.parents.find(lambda p: p.is_a(T.CallExpr)).cast(T.CallExpr)
    )


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

    @langkit_property()
    def called_decl():
        # Implement special resolution for calls to objects via __call__
        rd = Var(Entity.name.referenced_decl)
        call_builtin = Var(
            rd.cast(T.BaseValDecl)._.get_type._.get_fun('__call__')
        )
        return call_builtin.then(lambda a: a, default_val=rd)

    expr_context_free_type = Property(
        Entity.called_decl.then(
            lambda rd: rd.match(
                lambda fd=T.FunDecl: fd.return_type.designated_type,
                lambda td=T.TypeDecl: td,
                lambda _:
                PropertyError(T.TypeDecl.entity, "should not happen"),
            )
        )
    )

    formals = Property(
        Entity.called_decl.match(
            lambda fd=T.FunDecl: fd.args.map(
                lambda p: p.cast_or_raise(T.BaseValDecl)
            ),
            lambda td=T.TypeDecl: td.fields,
            lambda _: PropertyError(T.BaseValDecl.entity.array,
                                    "Should not happen")
        )
    )

    @langkit_property(memoized=True)
    def match_params():
        """
        Returns an array of ParamMatch structures, matching the
        actual parameters of this call expr to the formals of the entity
        designated by the callexpr.
        """
        return Self.static_match_params(Entity.formals, Entity.args.as_array)


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
    args = Field(type=T.TypeRef.list)

    expr_context_free_type = Property(Entity.designated_type)

    @langkit_property(public=True)
    def designated_type():
        """
        Get the type designated by this instantiation.

        NOTE: for the moment we only have generic types, so that's enough. If
        we want generic functions at some point we'll have to revisit.
        """
        generic_decl = Var(
            Entity.name.referenced_decl.cast_or_raise(T.GenericDecl)
        )
        return generic_decl.get_instantiated_type(
            Entity.args.map(
                lambda p: p.designated_type.assert_bare
                .cast_or_raise(T.TypeDecl)
            )
        ).as_bare_entity

    referenced_decl = Property(Entity.designated_type)


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

    get_type = Property(
        # If no type was given for the val declaration, infer it using its
        # expression.
        Entity.decl_type.then(
            lambda tpe: tpe.designated_type,
            default_val=Entity.val.expr_type._.expr_type
        )
    )


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
    pass


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

    @langkit_property(public=True, return_type=T.Bool,
                      external=True, uses_envs=False, uses_entity_info=False)
    def is_prefixed_string():
        """
        Return whether this string is prefixed or not.
        """
        pass

    @langkit_property(public=True, return_type=T.Character,
                      external=True, uses_envs=False, uses_entity_info=False)
    def prefix():
        """
        Return the prefix of this string.
        """
        pass

    @langkit_property(public=True, return_type=T.String,
                      external=True, uses_envs=False, uses_entity_info=False)
    def denoted_value():
        """
        Return the content of the given string literal node.
        """
        pass

    @langkit_property(return_type=T.Bool)
    def is_regexp_literal():
        """
        Return whether this string literal is actually a regexp literal, by
        checking that this string is prefixed by 'p'.
        """
        return Self.prefix == CharacterLiteral('p')

    @langkit_property()
    def expr_context_free_type():
        return Cond(
            Self.is_regexp_literal,
            Self.regexp_type,

            No(T.TypeDecl.entity)
        )

    @langkit_property()
    def expected_type_predicate(expected_type=T.TypeDecl.entity):
        # We don't need to handle the case where this string literal is a
        # regexp literal, because a regexp literal will always have a
        # context-free type and therefore this predicate will never be called.
        return Or(expected_type == Self.string_type,
                  expected_type == Self.symbol_type)

    invalid_expected_type_error_name = Property(String("a string literal"))


class NumLit(Lit):
    """
    Number literal expression.
    """
    token_node = True

    @langkit_property()
    def expected_type_predicate(expected_type=T.TypeDecl.entity):
        return Or(expected_type == Self.int_type,
                  expected_type == Self.bigint_type)

    invalid_expected_type_error_name = Property(String("a number literal"))


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
        G.imports, G.decls, Lex.Termination
    ),
    id=Id(Lex.Identifier),
    ref_id=RefId(Lex.Identifier),
    def_id=DefId(Lex.Identifier),

    import_stmt=Import("import", G.ref_id),
    imports=List(G.import_stmt, empty_valid=True),

    doc_comment=DocComment(Lex.DocComment),

    doc=Doc(List(G.doc_comment, empty_valid=True)),

    lexer_decl=LexerDecl(
        "lexer", G.def_id, "{",
        List(GOr(G.lexer_rule, G.lexer_family_decl), empty_valid=True),
        "}"
    ),
    grammar_decl=GrammarDecl(
        "grammar", G.def_id,
        "{", List(G.decl, empty_valid=True), "}"
    ),
    grammar_rule=GrammarRuleDecl(G.def_id, "<-", G.grammar_expr),
    lexer_rule=GOr(G.decl, G.lexer_case_rule),
    lexer_family_decl=LexerFamilyDecl(
        Lex.Identifier(match_text="family"), G.def_id, "{",
        List(G.lexer_rule, empty_valid=False),
        "}"
    ),
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
        G.grammar_pick,
        G.grammar_list_expr,
        G.token_literal,
        G.token_no_case_literal,
        G.token_pattern_literal,
        G.grammar_cut,
        G.grammar_skip,
        G.grammar_null,
        G.grammar_token,
        G.parse_node_expr,
        G.grammar_opt,
        G.grammar_or_expr,
        G.grammar_rule_ref,
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
        Lex.Identifier("pick"),
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
        # Match either "list" (type inference will determine the list type) or
        # a specific list type.
        GOr(Pick(Lex.Identifier(match_text="list"), Null(TypeRef)),
            G.type_ref),

        GOr(ListKind.alt_one("+"), ListKind.alt_zero("*")),
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
        "generic", "[", List(G.generic_formal_type, sep=","), "]", G.bare_decl
    ),

    generic_formal_type=GenericFormalTypeDecl(G.def_id),

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
        GenericTypeRef(G.basic_name, "[", G.type_list, "]"),
        SimpleTypeRef(G.basic_name),
    ),

    type_list=List(G.type_ref, empty_valid=False, sep=","),

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
        GenericInstantiation(G.basic_expr, "[", G.type_list, "]"),
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

    param=Param(Opt(G.ref_id, "="), G.expr),
    params=List(G.param, sep=",", empty_valid=True),

    decl_annotation=DeclAnnotation(
        "@", G.id, Opt("(", G.params, ")")
    ),

)
