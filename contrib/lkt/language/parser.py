from __future__ import absolute_import, division, print_function

from langkit.dsl import (
    ASTNode, AbstractField, Annotations, Field, LookupKind as LK, NullField,
    Struct, T, UserField, abstract, synthetic
)
from langkit.envs import EnvSpec, add_env, add_to_env, add_to_env_kv, do
from langkit.expressions import (
    AbstractKind, AbstractProperty, And, CharacterLiteral, Cond, EmptyEnv,
    Entity, If, Let, No, Not, Or, Property, PropertyError, Self, String, Try,
    Var, ignore, langkit_property, new_env_assoc
)
from langkit.parsers import (Grammar, List, NoBacktrack as cut, Null, Opt,
                             Or as GOr, Pick)


from language.lexer import lkt_lexer as Lex


class SemanticResult(Struct):
    """
    Result for a call to a semantic property that can return an error.

    In every case, the node field will be populated with the node upon which
    the request was made. Then, the result can be either:

    * ``result_type`` if the property was a type returning property.
    * ``result_ref`` if the property was a reference returning property
    * ``error_message`` if an error was found as part of the resolution
      process.

    Only one of those fields can have a value.

    If all fields are null, it means that ``expr_type`` has been called on a
    non regular expression.

    TODO: Turn this into a real variant record when we have variants.
    """
    node = UserField(T.LKNode, default_value=No(T.LKNode))

    result_type = UserField(
        T.TypeDecl.entity, default_value=No(T.TypeDecl.entity)
    )
    result_ref = UserField(T.Decl.entity, default_value=No(T.Decl.entity))
    has_error = UserField(T.Bool, default_value=False)
    error_message = UserField(T.String, default_value=No(T.String))


class TreeSemanticResult(Struct):
    """
    Collection of semantic results for a subtree. This will carry both:

    * An array of semantic results for all of the subtree's nodes.
    * A flag indicating whether the subtree contained errors or not.
    """
    results = UserField(T.SemanticResult.array)
    has_error = UserField(T.Bool)


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

    @langkit_property()
    def function_type(args_types=T.TypeDecl.array, return_type=T.TypeDecl):

        # We create function types in the unit of the return type of the
        # function, else, if we just create the function types in any unit,
        # `(Int) -> Int` coming from file a.lkt won't be equal to `(Int) ->
        # Int` coming from file b.lkt.
        #
        # This also helps limit the number of function types.
        #
        # TODO: For the moment we don't have support for multiple files. As
        # soon as we do, test that this works correctly.

        return return_type.unit.root.cast(T.LangkitRoot).function_type_helper(
            args_types, return_type
        )

    @langkit_property()
    def expected_type_error(expected=T.TypeDecl.entity, got=T.String):
        return SemanticResult.new(
            node=Self,
            error_message=String("Mismatched types: expected '")
            .concat(expected.full_name)
            .concat(String("', got ").concat(got)),
            has_error=True
        )

    @langkit_property(return_type=T.TreeSemanticResult, public=True)
    def check_semantic():
        """
        Run name and type resolution on all relevant entities in this subtree,
        aggregating and returning the results.

        If you ran this and the result's `has_error` flag is false, then you
        *know* that you can call semantic properties such as `referenced_decl`
        and `expr_type`, and those will never raise.
        """
        # Compute the results for children of this subtree
        children_results = Var(Entity.children.then(
            lambda children: children.map(
                lambda child: child._.check_semantic()
            )
        ))

        # Aggregate the arrays of results for every child of this node
        aggregated_results = Var(children_results.mapcat(
            lambda res: res.results
        ))

        # Store whether any children has errors
        children_have_errors = Var(
            children_results.any(lambda res: res.has_error)
        )

        own_results = Var(Entity.check_semantic_impl())

        return TreeSemanticResult.new(
            results=aggregated_results.concat(own_results.results),
            has_error=Or(children_have_errors, own_results.has_error)
        )

    @langkit_property(return_type=T.TreeSemanticResult)
    def check_semantic_impl():
        """
        Do semantic checking for this node. This will resolve names for RefIds,
        and types for Exprs, and aggregate this in a TreeSemanticResult.
        """

        # TODO: For the moment, for lack of a better more practical solution,
        # we'll catch all errors with Try blocks, should an error happen inside
        # a property computing a result.
        #
        # This is suboptimal because this doesn't distinguish between expected
        # errors due to erroneous input and unexpected errors due to bugs in
        # LKT. To handle this correctly we'd need either:
        #
        # 1. Different exception kinds, so that we can discriminate between
        #    expected an unexpected errors.
        #
        # 2. Error types (more generally sum types), that would allow
        #    propagating failure in a way that is more convenient than what we
        #    can do now (we could do it with bare structs but it would be
        #    tedious and error prone, and is considered not worth it at this
        #    stage).

        err = Var(SemanticResult.new(has_error=True))

        # Compute the results (resolve names + resolve types)
        results = Var(Entity.match(
            lambda r=T.RefId: Let(
                lambda rd=Try(r.referenced_decl, err): rd.singleton.concat(
                    # Don't try to compute the type if we couldn't find the ref
                    If(Not(rd.has_error),
                       Try(r.expr_type, err)._.singleton,
                       No(T.SemanticResult.array))
                ),
            ),
            lambda e=T.Expr: Try(e.expr_type, err)._.singleton,
            lambda _: No(T.SemanticResult.array)
        ))

        return TreeSemanticResult.new(
            results=results,
            # If there is an error in any of the results, has_error will be
            # true.
            has_error=results.any(lambda r: r.has_error)
        )


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

    @langkit_property(memoized=True, return_type=T.FunctionType)
    def function_type_helper(args_types=T.TypeDecl.array,
                             return_type=T.TypeDecl):
        return FunctionType.new(args=args_types, return_type=return_type)

    @langkit_property(return_type=T.SemanticResult.array, public=True)
    def check_legality():
        """
        Run all legality checks on this tree. Return a list of diagnostics (For
        the moment, pending on real discriminated records, SemanticResults
        which necessarily have an error_message).
        """
        res = Var(Entity.check_semantic())

        # Crude sanity check: since for the moment the Try blocks in
        # check_semantic will catch any exception happening in subcalls to
        # semantic properties, we want to check that, if we have internal
        # errors, we also have user errors (eg. diagnostics). If we have an
        # internal error and no user errors, then we caught something we were
        # not supposed to.
        # TODO: Assess whether this is still needed when we migrate to a better
        # error handling mechanism (see check_semantic_impl).

        errs = Var(res.results.filter(lambda r: r.has_error))
        diags = Var(errs.filter(lambda r: Not(r.error_message.is_null)))

        has_internal_errors = Var(errs.any(lambda r: r.error_message.is_null))

        return If(
            has_internal_errors & (diags.length == 0),
            PropertyError(T.SemanticResult.array,
                          "ERROR: internal errors without diagnostics"),
            diags
        )

    env_spec = EnvSpec(do(Self.fetch_prelude))


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
        Return the full name of this decl, as it should be seen by users/shown
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
        # If Self is a type, then use the type's scope to lookup operations
        # directly. TODO: Functions are accessible that way, but we
        # didn't define a semantic for this kind of calls yet.
        return Entity.referenced_decl.result_ref.cast(T.TypeDecl).then(
            lambda td: td.type_scope,
            # Else, take the type of self, and return the type's scope
            default_val=Entity.check_expr_type.type_scope
        )

    @langkit_property(return_type=T.TypeDecl.entity, activate_tracing=True)
    def expected_type():
        return Entity.parent.match(
            lambda fun_decl=T.FunDecl: fun_decl.return_type.designated_type,

            lambda fun_arg_decl=T.FunArgDecl: fun_arg_decl.get_type,

            # Expression of a match branch: return the expected type of the
            # match expression.
            lambda match_branch=T.MatchBranch:
            match_branch.parent.parent.cast_or_raise(T.Expr).check_expr_type,

            lambda bin_op=T.BinOp: If(
                # For those operators, there is no expected type flowing from
                # upward: we use the type of the other operand, if it exists.
                #
                # NOTE: In some cases none of the operands will have a type, as
                # in "0 = 0" (because int literals are polymorphic). We
                # consider this a legality error for the moment. If there is a
                # realistic use case that appears, we'll reconsider this.
                bin_op.op.is_a(Op.alt_lte, Op.alt_gte, Op.alt_gt, Op.alt_lt,
                               Op.alt_eq),

                If(Entity == bin_op.left, bin_op.left, bin_op.right)
                .expr_type_impl(No(T.TypeDecl.entity), False).result_type,

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

            lambda val_decl=T.BaseValDecl: val_decl.get_type(
                no_inference=True
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
            Entity.in_grammar_rule,
            # TODO: Implement function types
            Entity.referenced_decl.result_ref.is_a(T.TypeDecl, T.GenericDecl)
        ))

    @langkit_property(return_type=SemanticResult, public=True)
    def expr_type():
        """
        Return the type of this expression, if it is a regular expression (see
        ``is_regular_expr``), null otherwise.
        """
        return If(
            Entity.is_regular_expr,
            Entity.expr_type_impl(Entity.expected_type),
            No(SemanticResult),
        )

    @langkit_property(return_type=T.TypeDecl.entity, public=True)
    def check_expr_type():
        """
        Return the type of this expression. Assumes that this is a regular and
        valid expression. If this is called on a non regular or non valid
        expression, it will raise an error.
        """
        return If(
            Entity.is_regular_expr,
            Entity.expr_type.result_type.then(
                lambda rt: rt,
                default_val=PropertyError(T.TypeDecl.entity, "Typing error")
            ),
            PropertyError(T.TypeDecl.entity, "non regular expression")
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

    @langkit_property(return_type=SemanticResult)
    def expr_type_impl(expected_type=T.TypeDecl.entity,
                       raise_if_no_type=(T.Bool, True)):
        """
        Implementation for Expr.expr_type. This is the core of the current type
        system for lkt. Default implementation has the following behavior:

        - If there is a context free type for this expression *and* an expected
          type, then check that they match, if they don't, return an error
          SemanticResult.

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

        NOTE: In the absence of a solver based more clever type inference
        system (for now at least), we have to make a decision about the
        direction types commonly flow in LKT. What follows from the previous
        description is that type commonly flow upwards, from parent nodes to
        leaves.
        """
        cf_type = Var(Entity.expr_context_free_type)

        return Cond(
            # We have both a context free type and an expected type: check that
            # they match.
            And(Not(expected_type.is_null), Not(cf_type.is_null)),
            If(
                expected_type.matches(cf_type),
                SemanticResult.new(node=Self, result_type=expected_type),
                Self.expected_type_error(
                    expected_type,
                    String("'").concat(cf_type.full_name).concat(String("'"))
                )
            ),

            Not(expected_type.is_null),
            If(
                Entity.expected_type_predicate(expected_type),
                SemanticResult.new(result_type=expected_type, node=Self),
                Self.expected_type_error(
                    expected_type,
                    got=Self.invalid_expected_type_error_name
                )
            ),

            Not(cf_type.is_null),
            SemanticResult.new(result_type=cf_type, node=Self),

            # We don't have both types: check that there is at least one and
            # return it, else, raise an error if raise_if_no_type is true.
            SemanticResult.new(result_type=Entity.check_type(
                expected_type._or(cf_type), raise_if_no_type
            ), node=Self),
        )

    @langkit_property(return_type=SemanticResult, public=True)
    def referenced_decl():
        """
        Return the declaration referenced by this expression, if applicable,
        null otherwise.
        """
        return No(SemanticResult)

    @langkit_property(return_type=T.Decl.entity, public=True)
    def check_referenced_decl():
        """
        Return the referenced decl of this expr, raise otherwise.
        """
        return Entity.referenced_decl._.result_ref.then(
            lambda rr: rr,
        )._or(PropertyError(T.Decl.entity, "No referenced decl error"))


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

    referenced_decl = Property(SemanticResult.new(
        result_ref=Entity.suffix.check_referenced_decl, node=Self
    ))

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
        return (
            Entity.scope.get_first(Self.symbol, lookup=lk).cast(T.Decl).then(
                lambda d: SemanticResult.new(result_ref=d, node=Self),
                default_val=Self.ref_not_found_error
            )
        )

    expr_context_free_type = Property(
        Entity.check_referenced_decl.cast_or_raise(T.BaseValDecl).get_type
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

    @langkit_property()
    def ref_not_found_error():
        return SemanticResult.new(
            node=Self,
            error_message=String("Cannot find entity '")
            .concat(Self.text).concat(String("' in this scope")),
            has_error=True
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


@synthetic
class FunctionType(TypeDecl):
    """
    Function type.
    """
    args = UserField(T.TypeDecl.array, public=False)
    return_type = UserField(T.TypeDecl, public=False)

    syn_name = NullField()
    full_name = Property(
        String("(").concat(
            Self.string_join(
                Self.args.map(lambda t: t.full_name), sep=String(", ")
            )
        ).concat(String(") -> ")).concat(Self.return_type.full_name)
    )

    type_scope = Property(EmptyEnv)


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

    # Empty env spec: alts are added as part of EnumClassDecl's env_spec
    env_spec = EnvSpec()


class EnumClassDecl(NamedTypeDecl):
    """
    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.
    """
    syn_name = Field(type=T.DefId)
    alts = Field(type=T.EnumClassAltDecl.list)
    base_class = Field(type=T.TypeRef)
    decls = Field(type=DeclBlock)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env(),
        add_to_env(Entity.alts.map(lambda alt: new_env_assoc(
            key=alt.name, val=alt.node, dest_env=Self.children_env
        )))
    )


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


@abstract
class BaseValDecl(Decl):
    """
    Abstract class for named values declarations, such as arguments, local
    value bindings, fields, etc.
    """

    @langkit_property(kind=AbstractKind.abstract,
                      return_type=T.TypeDecl.entity)
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        pass


class FunDecl(BaseValDecl):
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
        # Add node & self if there is an owning type
        add_to_env(Self.owning_type.then(
            lambda ot: [new_env_assoc("self", ot.self_decl),
                        new_env_assoc("node", ot.node_decl)]
        )),
    )

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        return Self.function_type(
            # If there is an owning type, then the type of this function
            # contain: the type of the self argument, which (we imagine, it
            # won't be used yet) will need to be passed "self" explicitly, when
            # passed around as a function object.
            Self.owning_type.then(lambda ot: ot.singleton)
            .concat(Entity.args.map(
                lambda a:
                a.decl_type.designated_type.assert_bare.cast(T.TypeDecl)
            )),
            Entity.return_type.designated_type.assert_bare.cast(T.TypeDecl)
        ).as_entity


@synthetic
class SelfDecl(BaseValDecl):
    """
    Synthetic declaration for the implicit "self" variable available in
    properties.
    """

    syn_name = NullField()
    name = Property('self')

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        return Entity.parent.cast_or_raise(T.TypeDecl)


@synthetic
class NodeDecl(BaseValDecl):
    """
    Synthetic declaration for the implicit "node" variable available in
    properties.
    """

    syn_name = NullField()
    name = Property('node')

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        return Entity.parent.cast_or_raise(T.TypeDecl)


@abstract
class UserValDecl(BaseValDecl):
    """
    Class for user declared val declarations (not synthetic).
    """
    syn_name = Field(type=T.DefId)
    decl_type = AbstractField(type=T.TypeRef)

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        return Entity.decl_type.designated_type


class EnumLitDecl(UserValDecl):
    """
    Enum literal declaration.
    """
    decl_type = NullField()

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        return (
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
    designated_type = AbstractProperty(T.TypeDecl.entity, public=True, doc="""
        Return the type designated by this type ref.
    """)


class SimpleTypeRef(TypeRef):
    """
    Simple reference to a type.
    """
    type_name = Field(type=T.Expr)

    @langkit_property()
    def designated_type():
        return Entity.type_name.check_referenced_decl.cast_or_raise(T.TypeDecl)


class GenericTypeRef(TypeRef):
    """
    Reference to a generic type.
    """
    type_name = Field(type=T.Expr)
    params = Field(type=T.TypeRef.list)

    @langkit_property()
    def designated_type():
        generic_decl = Var(
            Entity.type_name.check_referenced_decl.cast_or_raise(T.GenericDecl)
        )
        return generic_decl.get_instantiated_type(
            Entity.params.map(
                lambda p: p.designated_type.assert_bare
                .cast_or_raise(T.TypeDecl)
            )
        ).as_bare_entity


class FunctionTypeRef(TypeRef):
    """
    Reference to a function type.
    """
    args_types = Field(T.TypeRef.list)
    return_type = Field(T.TypeRef)

    @langkit_property()
    def designated_type():
        return Self.function_type(
            Entity.args_types.map(
                lambda a:
                a.designated_type.assert_bare.cast(T.TypeDecl)
            ),
            Entity.return_type.designated_type.assert_bare.cast(T.TypeDecl)
        ).as_entity


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
    params = Field(type=T.DeclAnnotationParams)


class DeclAnnotationParams(LKNode):
    """
    List of arguments for an annotation with a call syntax. This intermediate
    node is necessary in order to determine after parsing whether there is no
    param list, or if the list is empty.
    """
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
        rd = Var(Entity.name.check_referenced_decl)
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
            Entity.name.check_referenced_decl.cast_or_raise(T.GenericDecl)
        )
        return generic_decl.get_instantiated_type(
            Entity.args.map(
                lambda p: p.designated_type.assert_bare
                .cast_or_raise(T.TypeDecl)
            )
        ).as_bare_entity

    referenced_decl = Property(
        SemanticResult.new(result_ref=Entity.designated_type, node=Self)
    )


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

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        # If no type was given for the val declaration, infer it using its
        # expression.
        return Entity.decl_type.then(
            lambda tpe: tpe.designated_type,
            default_val=If(
                no_inference,
                No(T.TypeDecl.entity),
                Entity.val.check_expr_type
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

    Note that in order to reduce the size of the node type hierarchy, we define
    only one node (StringLit) for all our string literals (only regular strings
    and pattern string literals at the moment). This will also make it easy to
    add new string prefixes in the future.
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
        Return the prefix of this string, or the null character if there is no
        prefix.
        """
        pass

    @langkit_property(public=True, return_type=T.String,
                      external=True, uses_envs=False, uses_entity_info=False)
    def denoted_value():
        """
        Return the content of the given string literal node.
        """
        pass

    @langkit_property(return_type=T.Bool, public=True)
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
        List(GOr(G.lexer_family_decl, G.lexer_rule), empty_valid=True),
        "}"
    ),
    grammar_decl=GrammarDecl(
        "grammar", G.def_id,
        "{", List(G.decl, empty_valid=True), "}"
    ),
    grammar_rule=GrammarRuleDecl(G.def_id, Opt("<-", G.grammar_expr)),
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
        G.field_decl,
        G.val_decl,
        G.grammar_rule,
    ),

    decl=FullDecl(
        G.doc, List(G.decl_annotation, empty_valid=True), G.bare_decl
    ),

    type_ref=GOr(
        GenericTypeRef(G.basic_name, "[", G.type_list, "]"),
        SimpleTypeRef(G.basic_name),
        FunctionTypeRef("(", G.type_list, ")", "->", G.type_ref)
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

    decl_annotation_params=Opt(DeclAnnotationParams("(", G.params, ")")),
    decl_annotation=DeclAnnotation("@", G.id, G.decl_annotation_params),

)
