"""
The Lkt language is still a work in progress, so it does not have a proper
reference manual or some serious documentation for the moment. In the meantime,
this docstring is an attempt to give keys for the concepts behind the language
design.


Annotations:

* We want to use annotations as much as possible to avoid the introduction of
  new syntax for specific cases.


Classes:

* For now, except for builtins, only nodes are allowed as classes: exactly one
  class definition with no base must have the @root_node annotation, and all
  other classes must derive it, or derive one of its subclasses.

Enum types:

* Maps to a discrete value type. Small memory footprint.
* Is just an enum, cannot contain data (enum classes are meant for that, longer
  term).
* Syntax is similar to enum classes ("enum A { case b, c, d }").

Enum classes:

* No class can derive from enum classes (they are "final").
* The @qualifier annotation for enum classes creates automatic Present/Absent
  alternatives, so no explicit alternative is allowed when the @qualifier
  annotation is present.
"""

from langkit.dsl import (
    ASTNode, AbstractField, Annotations, Field, LookupKind as LK, NullField,
    Struct, T, UserField, abstract, synthetic
)
from langkit.envs import (
    EnvSpec, RefKind, add_env, add_to_env, add_to_env_kv, do, handle_children,
    reference
)
from langkit.expressions import (
    AbstractKind, AbstractProperty, And, ArrayLiteral as Array,
    CharacterLiteral, Cond, EmptyEnv, Entity, If, Let, No, Not, Or, Property,
    PropertyError, Self, String as S, Try as _Try, Var, direct_env, ignore,
    langkit_property, new_env_assoc
)
from langkit.parsers import (Grammar, List, NoBacktrack as cut, Null, Opt,
                             Or as GOr, Pick)


from language.lexer import lkt_lexer as Lex


NO_EXC_CATCHING = False
"""
If True, deactivates the catching of exceptions via `Try` blocks. This is
needed because we catch exceptions in semantic passes, and since we have only
one exception type, we'll sometimes silence a useful/informative error.
"""


def Try(expr, *args):
    if NO_EXC_CATCHING:
        return expr
    else:
        return _Try(expr, *args)


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
    exempt_analysis = UserField(
        T.Bool, default_value=False,
        doc="Internal value. Used to skip analysis of some nodes"
    )


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
    has_matched = UserField(type=T.Bool)
    actual = UserField(type=T.Param.entity, default_value=No(T.Param.entity))
    formal = UserField(type=T.FormalParam, default_value=No(T.FormalParam))


@abstract
class LKNode(ASTNode):
    """
    Root node class for lkt AST nodes.
    """

    @langkit_property(return_type=T.Decl.entity)
    def root_get(entity_name=T.Symbol):
        return Self.unit.root.node_env.get_first(
            entity_name).cast_or_raise(T.Decl)

    @langkit_property(return_type=T.NamedTypeDecl.entity)
    def get_builtin_type(entity_name=T.Symbol):
        return Self.root_get(entity_name).cast_or_raise(T.NamedTypeDecl)

    @langkit_property(return_type=T.GenericDecl.entity)
    def get_builtin_gen_decl(entity_name=T.Symbol):
        return Self.root_get(entity_name).cast_or_raise(T.GenericDecl)

    node_type = Property(
        Self.get_builtin_gen_decl('Node'), public=True,
        doc="Unit method. Return the Node base class."
    )

    char_type = Property(
        Self.get_builtin_type('Char'), public=True,
        doc="Unit method. Return the character builtin type."
    )

    int_type = Property(
        Self.get_builtin_type('Int'), public=True,
        doc="Unit method. Return the integer builtin type."
    )

    bool_type = Property(
        Self.get_builtin_type('Bool'), public=True,
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

    property_error_type = Property(
        Self.get_builtin_type('PropertyError'), public=True,
        doc="Unit method. Return the property error builtin type."
    )

    regexp_type = Property(
        Self.get_builtin_type('Regexp'), public=True,
        doc="Unit method. Return the regexp builtin type."
    )

    array_gen_type = Property(
        Self.get_builtin_gen_decl('Array'), public=True,
        doc="Unit method. Return the array builtin generic type."
    )

    array_type = Property(
        Self.array_gen_type.decl.cast(T.NamedTypeDecl), public=True,
        doc="Unit method. Return the array builtin type."
    )

    astlist_gen_type = Property(
        Self.get_builtin_gen_decl('ASTList'),
        public=True,
        doc="Unit method. Return the ASTList builtin generic type."
    )

    astlist_type = Property(
        Self.astlist_gen_type.decl.cast(T.NamedTypeDecl),
        public=True,
        doc="Unit method. Return the ASTList builtin type."
    )

    iterator_gen_trait = Property(
        Self.get_builtin_gen_decl('Iterator'),
        public=True,
        doc="Unit method. Return the Iterator builtin generic trait."
    )

    iterator_trait = Property(
        Self.iterator_gen_trait.decl.cast(T.TraitDecl),
        public=True,
        doc="Unit method. Return the Iterator builtin trait."
    )

    analysis_unit_gen_trait = Property(
        Self.get_builtin_gen_decl('AnalysisUnit'),
        public=True,
        doc="Unit method. Return the ``AnalysisUnit`` builtin generic trait."
    )

    analysis_unit_trait = Property(
        Self.analysis_unit_gen_trait.decl.cast(T.TraitDecl),
        public=True,
        doc="Unit method. Return the ``AnalysisUnit`` builtin trait."
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
    def exempt():
        """
        Convenience function to construct an exempt error from Self.
        """
        return SemanticResult.new(
            node=Self, has_error=True, exempt_analysis=True
        )

    @langkit_property()
    def error(message=T.String):
        """
        Convenience function to construct an error from Self and ``message``.
        """
        return SemanticResult.new(
            node=Self, error_message=message, has_error=True,
        )

    @langkit_property()
    def expected_type_error(expected=T.TypeDecl.entity, got=T.String):
        """
        Return an error of the form::

            Mismatched types: expected ``expected.full_name``, got ``got``

        Where `expected` is a type and ``got`` is a string.
        """
        return Self.error(
            S("Mismatched types: expected `")
            .concat(expected.full_name)
            .concat(S("`, got ").concat(got)),
        )

    @langkit_property()
    def type_mismatch_error(expected=T.TypeDecl.entity, got=T.TypeDecl.entity):
        """
        Return an error of the form::

            Mismatched types: expected ``expected.full_name``, got
            ``got.full_name``

        Where ``expected`` and ``got`` are types.
        """
        return Self.expected_type_error(
            expected,
            S("`").concat(got.full_name).concat(S("`"))
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
        return Entity.check_sem_internal()

    @langkit_property(return_type=T.TreeSemanticResult)
    def check_sem_internal(
        exempted_nodes=(T.LKNode.array, No(T.LKNode.array))
    ):
        """
        Internal recursive visitor for check_semantic pass.
        """

        # First, run a pre-analysis correctness check phase on self. This might
        # produce user diagnostics, as well as diagnostics exempting analysis
        # for some child nodes.
        pre_diagnostics = Var(Entity.check_correctness_pre())
        exemptions = Var(exempted_nodes.concat(pre_diagnostics.filtermap(
            lambda d: d.node, lambda d: d.exempt_analysis
        )))

        # Compute the results for children of this subtree
        children_results = Var(Entity.children.then(
            lambda children: children.map(lambda child: If(
                # For each child, check if we have a diagnostic exempting
                # analysis on this node due to errors. If not, recurse on
                # it.
                # NOTE: This might be computationally expensive because we
                # don't use a map, so this is `O(len_diags * len_children)`. We
                # rely on the fact that the set of diagnostics is probably
                # small. To be verified.
                exemptions.find(lambda n: n == child.node).is_null,
                child._.check_sem_internal(exemptions),
                No(T.TreeSemanticResult)
            ))
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

        post_diagnostics = Var(Entity.check_correctness_post())

        return TreeSemanticResult.new(
            results=pre_diagnostics.concat(aggregated_results)
            .concat(own_results.results)
            .concat(post_diagnostics),
            has_error=Or(
                children_have_errors, own_results.has_error,
                pre_diagnostics.length > 0
            )
        )

    @langkit_property(return_type=T.SemanticResult.array, memoized=True)
    def check_correctness_post():
        """
        Custom hook to implement legality checks for a given node that can
        run after type resolution. If no errors, returns a null array.
        """
        return No(T.SemanticResult.array)

    @langkit_property(return_type=T.SemanticResult.array, memoized=True)
    def check_correctness_pre():
        """
        Custom hook to implement legality checks for a given node that need to
        run *before* type resolution. If no errors, returns a null array.

        .. WARNING: This must *not* raise exceptions, so must make sure that
            any queried semantic results are queried in a safe fashion.
        """
        return No(T.SemanticResult.array)

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
        # Lkt. To handle this correctly we'd need either:
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

    @langkit_property(return_type=T.LKNode.entity)
    def first_no_paren_parent():
        """
        Return the first parent that is not a ``ParenExpr``.
        """
        return If(
            Entity.parent.is_a(ParenExpr),
            Entity.parent.first_no_paren_parent(),
            Entity.parent
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
        res = Var(Entity.check_sem_internal())

        # Crude sanity check: since for the moment the Try blocks in
        # check_sem_internal will catch any exception happening in subcalls to
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
    name = Field(type=T.ModuleRefId)

    @langkit_property(public=True)
    def referenced_unit():
        """
        Return the unit that this import statements designates. Load it if
        needed.
        """
        return Self.internal_fetch_referenced_unit(Self.name.text)

    @langkit_property(return_type=T.SemanticResult.array)
    def check_correctness_pre():
        return If(
            Self.referenced_unit.root.is_null,
            Entity.error(S("cannot find ").concat(Self.name.text)).singleton,
            No(T.SemanticResult.array)
        )

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

    @langkit_property()
    def has_annotation(name=T.Symbol):
        """
        Return whether this node has an annotation with name ``name``.
        """
        return Not(Self.get_annotation(name).is_null)

    @langkit_property()
    def get_annotation(name=T.Symbol):
        """
        Return the annotation with name ``name``.
        """
        return Self.decl_annotations.find(
            lambda ann: ann.name.symbol == name
        )


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
        Self.name.image,
        public=True,
        doc="""
        Return the full name of this decl, as it should be seen by users/shown
        in diagnostics.
        """,
    )

    decl_type_name = AbstractProperty(
        type=T.String,
        doc="""
        Return the name of the declaration type, as it should be seen by
        users/shown in diagnostics.
        """
    )

    @langkit_property(return_type=T.FullDecl.entity)
    def full_decl():
        return Entity.parent.match(
            lambda fd=T.FullDecl: fd,
            lambda gd=T.GenericDecl: gd.full_decl,
            lambda _: PropertyError(T.FullDecl.entity, "should not happen")

        )

    quoted_name = Property(S("`").concat(Self.full_name).concat(S("`")))

    env_spec = EnvSpec(add_to_env_kv(Entity.name, Self))

    @langkit_property(return_type=T.Decl)
    def assert_bare():
        return If(
            Entity.info.rebindings == No(T.EnvRebindings),
            Entity.node,
            PropertyError(T.Decl, "Type decl has rebindings but shouldn't")
        )

    @langkit_property(public=True)
    def as_bare_decl():
        """
        Get this declaration without rebindings information.
        """
        # This is used by lkt_toolbox as a simple solution to filter out
        # rebindings information for the prelude declarations. TODO: improve it
        # in order to properly show rebindings information for prelude
        # declarations (i.e. do not show sloc nor unit name).
        return Entity.node.as_bare_entity

    @langkit_property()
    def call_scope():
        """
        If this is a declaration that can be called (either a type
        instantiation or a function call), return the scope in which
        parameters for the call are defined.
        """
        return EmptyEnv

    @langkit_property()
    def called_decl_type():
        """
        Get the type of the expression resulting of calling this decl (type
        itself it the decl is a type, return type if the decl is a
        function).
        """
        return Entity.match(
            lambda t=TypeDecl: t,
            lambda f=FunDecl: f.return_type.designated_type,
            lambda _: PropertyError(T.TypeDecl.entity, "Should not happen")
        )

    @langkit_property()
    def is_generic_fun_decl():
        """
        Return whether this declaration is a generic function declaration.
        """
        return Entity.cast(T.GenericDecl)._.decl.is_a(T.FunDecl)


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

    @langkit_property()
    def get_array_type(elt_type=T.TypeDecl):
        """
        Return an Array instantiated from type ``elt_type``.
        """
        return Self.array_gen_type.get_instantiated_type(
            [elt_type]
        ).as_entity

    @langkit_property(return_type=T.TypeDecl.entity)
    def expected_type():
        return Entity.parent.match(
            lambda fun_decl=T.FunDecl: fun_decl.return_type.designated_type,
            lambda lbd=T.LambdaExpr:
            lbd.expected_type.cast_or_raise(FunctionType)
            .return_type.as_bare_entity,

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

                If(Entity == bin_op.left, bin_op.right, bin_op.left)
                .expr_type_impl(No(T.TypeDecl.entity))
                .result_type._.root_type,

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

            lambda err_on_null=T.ErrorOnNull: err_on_null.expected_type,
            lambda paren_expr=T.ParenExpr: paren_expr.expected_type,

            lambda p=T.Param: p.call_expr.match_params().find(
                lambda pm: pm.actual == p
            ).then(lambda pm: pm.formal.formal_type),

            lambda val_decl=T.BaseValDecl: val_decl.get_type(
                no_inference=True
            ),

            lambda expr_list=T.ExprList: expr_list.parent.match(
                lambda array=T.ArrayLiteral:
                array.first_elt_context_free_type.then(
                    # The first context-free type is the expected type of all
                    # other elements in the array literal.
                    lambda t: t,
                    # When no element has a context-free type, infer the array
                    # type from the array literal's expected type (i.e. from
                    # the context).
                    default_val=array.expected_type.array_element_type
                ),
                lambda _: No(T.TypeDecl.entity)
            ),

            lambda block_expr=T.BlockExpr: block_expr.expected_type,

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
        return S("<not implemented>")

    @langkit_property(return_type=SemanticResult)
    def expr_type_impl(expected_type=T.TypeDecl.entity):
        """
        Implementation for ``Expr.expr_type``. This is the core of the current
        type system for Lkt. The default implementation has the following
        behavior:

        - If there is a context free type for this expression *and* an expected
          type, then check that they match, if they don't, return an error
          ``SemanticResult``.

        - If there is an expected type but no context-free type, check that the
          expected type is valid by calling the ``expected_type_predicate``
          property and return the expected type, otherwise fail.

        - If there is only a context free type but no expected type, return
          the context-free type.

        - If there is none, raise a ``PropertyError``.

        Implementing typing for new constructs should be simple:

        - If the construct has a definite type regardless of the context (for
          example, a variable reference), then just override
          ``expr_context_free_type``.

        - If the construct has no definite type but imposes some constraints on
          the typing - for example, a string literal will require its argument
          to be either a string or a symbol - override
          ``expected_type_predicate`` and ``invalid_expected_type_error_name``.

        - If the construct imposes some constraints on its sub expressions, you
          must encode them in ``Expr.expected_type``.

        - If the construct need rules that have not been planned by the above,
          you can still override ``expr_type_impl``. This should not be
          necessary though, and should only be done following a discussion with
          Lkt devs, and altering this comment afterwards.

        NOTE: In the absence of a solver based more clever type inference
        system (for now at least), we have to make a decision about the
        direction types commonly flow in Lkt. What follows from the previous
        description is that type commonly flow upwards, from parent nodes to
        leaves.
        """
        cf_type = Var(Entity.expr_context_free_type)

        return Cond(
            # We have both a context free type and an expected type: check that
            # they match.
            And(Not(expected_type.is_null), Not(cf_type.is_null)),
            If(
                cf_type.matches(expected_type),
                SemanticResult.new(node=Self, result_type=expected_type),

                # If they don't match, emit an error
                Self.type_mismatch_error(expected_type, cf_type)
            ),

            # We only have an expected type: run `expected_type_predicate` on
            # it.
            Not(expected_type.is_null),
            If(
                Entity.expected_type_predicate(expected_type),
                SemanticResult.new(result_type=expected_type, node=Self),

                #  If it returns false, return an error
                Self.expected_type_error(
                    expected_type,
                    got=Self.invalid_expected_type_error_name
                )
            ),

            # We don't have an expected type: Check that there is a context
            # free type and return it.
            Not(cf_type.is_null),
            SemanticResult.new(result_type=cf_type, node=Self),

            # We have neither: emit an error
            Entity.error(S("ambiguous type for expression"))
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
    decl_type_name = Property(S("lexer declaration"))

    @langkit_property(return_type=T.env_assoc.array, memoized=True)
    def builtin_decls():
        return Array(['newline', 'indent', 'dedent']).map(
            lambda sym:
            new_env_assoc(
                key=sym,
                val=SyntheticLexerDecl.new(sym=sym),
                dest_env=direct_env(Self.children_env)
            ),
        )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env(),
        add_to_env(Self.builtin_decls())
    )


class LexerFamilyDecl(Decl):
    """
    Declaration of a token family.
    """
    syn_name = Field(type=T.DefId)
    rules = Field(type=T.FullDecl.list)
    decl_type_name = Property(S("lexer family declaration"))


class LexerCaseRule(LKNode):
    """
    Lexer construct to introduce a conditional lexing action.
    """
    expr = Field(type=T.GrammarExpr)
    alts = Field(type=T.BaseLexerCaseRuleAlt.list)

    # For the moment exempt resolution for children of LexerCaseRule, because
    # even though we want to name-res them eventually, it's not a priority, and
    # it might require a rethinking of this part of the DSL.
    check_correctness_pre = Property(Self.children.map(
        lambda c: SemanticResult.new(node=c, exempt_analysis=True)
    ))


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

    decl_type_name = Property(S("grammar declaration"))

    @langkit_property(public=True)
    def lexer():
        """
        Return the lexer that is associated to this grammar.
        """
        return (Entity.full_decl.get_annotation('with_lexer')
                .params.params.at(0).value.as_entity.check_referenced_decl)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env()
    )


@abstract
class BaseGrammarRuleDecl(Decl):
    """
    Base class for grammar rules inside of grammars/lexers.
    """
    expr = AbstractField(type=T.GrammarExpr)


class GrammarRuleDecl(BaseGrammarRuleDecl):
    """
    Declaration of a grammar rule inside of a grammar.
    """
    syn_name = Field(type=T.DefId)
    expr = Field(type=T.GrammarExpr)
    decl_type_name = Property(S("grammar rule declaration"))


@synthetic
class SyntheticLexerDecl(BaseGrammarRuleDecl):
    """
    """
    sym = UserField(T.Symbol, public=False)
    name = Property(Self.sym)
    syn_name = NullField()
    expr = NullField()
    decl_type_name = Property(S("synthetic lexer declaration"))


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

        param_if_name = Var(Entity.param_if_param_name)
        is_annot_param = Var(
            Not(param_if_name.is_null)
            & Not(param_if_name.parents
                  .find(lambda n: n.is_a(DeclAnnotation)).is_null)
        )

        return If(
            is_annot_param,
            No(T.SemanticResult),
            Entity.scope.get_first(
                Self.symbol, lookup=If(
                    Entity.dot_expr_if_suffix.is_null,
                    LK.recursive,
                    LK.flat
                )
            ).cast(T.Decl).then(
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

    param_if_param_name = Property(
        Entity.parent.cast(T.Param).then(
            lambda p: If(Entity == p.name, p, No(T.Param.entity))
        )
    )

    @langkit_property()
    def scope():
        # The scope of this referenced id is either:
        return Entity.dot_expr_if_suffix.then(
            # The scope of it's prefix if it is the suffix in a dotted name
            lambda de: de.prefix.designated_scope,
        )._or(Entity.param_if_param_name.then(
            # The scope of it's called declaration if it's a parameter name in
            # a CallExpr.
            lambda p: p.call_expr.called_decl.call_scope
        ))._or(Self.parent.cast(TokenRef).then(
            # The scope of the grammar's lexer, if this is a RefId inside a
            # TokenRef.
            lambda _:
            Entity.parents.find(lambda n: n.is_a(GrammarDecl))
            .cast(GrammarDecl).lexer.children_env
        ))._or(
            # It's regular environment in other cases
            Entity.children_env
        )

    @langkit_property()
    def ref_not_found_error():
        return Self.error(
            S("Cannot find entity `")
            .concat(Self.text).concat(S("` in this scope")),
        )


class ModuleRefId(RefId):
    """
    Id referencing a langkit module.
    """
    # TODO: Being able to cross-reference modules seems like a good ability,
    # but for the moment they don't fit in the framework, because they're not
    # decls.
    referenced_decl = Property(No(SemanticResult))
    expr_type = Property(No(SemanticResult))


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


class TokenRef(GrammarExpr):
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
    env_spec = EnvSpec(
        add_env(),

        # Reference the base type scope, through which will be made
        # visible things such as inherited methods & fields.
        reference(
            Entity.owning_type._.node.cast(T.LKNode).singleton,
            through=T.TypeDecl.type_base_scope,
            kind=RefKind.transitive
        )
    )

    @langkit_property()
    def owning_type():
        """
        Return the type to which this decl block belongs, or null.
        """
        return Entity.parents.find(
            lambda p: p.is_a(T.TypeDecl)
        ).cast(T.TypeDecl)


@abstract
class TypeDecl(Decl):
    """
    Abstract base class for type declarations.
    """

    @langkit_property()
    def concrete_matches(other=T.TypeDecl.entity):
        """
        Return whether ``self`` matches ``other`` for non generic types.
        """
        return (Entity == other) | Entity.is_subtype(other)

    @langkit_property(return_type=T.Bool)
    def matches(other=T.TypeDecl.entity):
        """
        Return whether ``self`` matches ``other``.
        """
        return Entity.cast(T.InstantiatedGenericType).then(
            lambda e: other.cast(T.InstantiatedGenericType).then(

                # Both types are generic instantiations: Handle covariance
                # annotations, by manually checking that the type matches, and
                # that the actuals used for instantiation match, taking into
                # account covariance annotations.
                lambda o:
                # First, check that the instantiated generic type matches
                e.inner_type_decl.as_bare_entity.matches(
                    o.inner_type_decl.as_bare_entity
                )
                # Second, check that the actuals matches, taking into account
                # potential covariance annotations.
                & e.actuals.all(lambda i, actual_1: Let(
                    lambda
                    formal=e.generic_decl.generic_formal_decls.at(i),
                    actual_2=o.actuals.at(i):
                    If(
                        formal.has_annotation('out'),

                        # We have an out annotation on the formal. It means
                        # that this formal is covariant, and so we should use
                        # the type matching algorithm on it.
                        actual_1.as_bare_entity.matches(
                            actual_2.as_bare_entity
                        ),

                        # We have a non covariant formal: just check for basic
                        # equality.
                        actual_1 == actual_2
                    )
                )),
                # Concrete types cases
                default_val=Entity.concrete_matches(other)
            ), default_val=Entity.concrete_matches(other)
        )

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
        No(T.ComponentDecl.entity.array),
        doc="""Return the list of fields for this type"""
    )

    traits = AbstractField(T.TypeRef.list, doc="Traits for this type")

    @langkit_property(public=True, return_type=T.TypeDecl.entity.array)
    def implemented_traits():
        """
        Traits implemented by this type.
        """
        return Entity.traits.map(lambda t: t.designated_type).concat(
            Entity.base_types.mapcat(
                lambda bt: bt.implemented_traits
            )
        )

    base_type = AbstractField(T.TypeRef)

    @langkit_property(memoized=True, return_type=T.TypeDecl.entity.array,
                      public=True)
    def base_types():
        """
        Return the chain of base types for this type, if applicable, else null.
        """
        return Entity.base_type._.designated_type.then(
            lambda b: b.singleton.concat(b.base_types)
        )

    @langkit_property(memoized=True, return_type=T.TypeDecl.entity,
                      public=True)
    def root_type():
        """
        Return the root type of this type, if applicable, else null.
        """
        return Entity.base_type._.designated_type.then(
            lambda b: b.root_type
        )._or(Entity)

    @langkit_property(public=True)
    def is_subtype(potential_base=T.TypeDecl.entity):
        """
        Return whether ``self`` is a subtype of ``potential_base``.
        """
        ignore(potential_base)
        return False

    @langkit_property()
    def type_base_scope():
        """
        Return the base scope for this type, containing everything that is
        implicitly visible in the scope of definition of this type.
        """
        return (
            # Take operations from traits
            Entity.implemented_traits.map(
                lambda td: td.type_scope
            )
            # Plus operations from the base type
            .concat(
                # Wrap in Try in case we cannot resolve the base type scope
                Try(Entity.base_type._.designated_type.type_scope,
                    EmptyEnv).singleton
            )
            # And group them together
            .env_group()
        )

    call_scope = Property(Entity.type_scope)

    is_generic = Property(
        Entity.is_a(T.InstantiatedGenericType), public=True,
        doc="Returns whether this type is an instantiated generic type."
    )

    @langkit_property()
    def array_element_type():
        """
        Return the element type if Entity is an array, or No(TypeDecl.entity)
        if it is not an array type.
        """
        # As the generic Array class has only one type parameter,
        # ``get_actuals.at(0)`` is the element type.
        return Entity.cast(T.InstantiatedGenericType).then(
            lambda array: If(
                array.get_inner_type == Entity.array_type.assert_bare,
                array.get_actuals.at(0),
                No(TypeDecl.entity)
            )
        )


@synthetic
class FunctionType(TypeDecl):
    """
    Function type.
    """
    args = UserField(T.TypeDecl.array, public=False)
    return_type = UserField(T.TypeDecl, public=False)
    syn_name = NullField()
    traits = NullField()
    base_type = NullField()

    full_name = Property(
        S("(").concat(
            S(", ").join(Self.args.map(lambda t: t.full_name))
        ).concat(S(") -> ")).concat(Self.return_type.full_name)
    )

    type_scope = Property(EmptyEnv)

    decl_type_name = Property(S("function type"))


class GenericFormalTypeDecl(TypeDecl):
    """
    Declaration of a generic formal type in a generic declaration.
    """
    syn_name = Field(T.TypeDefId)
    traits = NullField()
    base_type = NullField()
    type_scope = Property(EmptyEnv)

    generic_decl = Property(
        Entity.parent.parent.parent.cast_or_raise(T.GenericDecl)
    )

    decl_type_name = Property(S("generic formal type declaration"))


@abstract
class NamedTypeDecl(TypeDecl):
    """
    Explicit named type declaration.
    """
    decls = AbstractField(type=DeclBlock)

    type_scope = Property(Entity._.decls.children_env)

    fields = Property(Entity.decls.filtermap(
        lambda d: d.decl.cast_or_raise(T.ComponentDecl),
        lambda d: d.decl.is_a(T.ComponentDecl)
    ))

    @langkit_property(kind=AbstractKind.abstract,
                      return_type=T.Bool)
    def is_authorized_decl(decl=T.Decl.entity):
        """
        Return true whether ``decl`` declaration is authorized in this named
        type declaration.
        """
        pass

    @langkit_property()
    def check_correctness_pre():
        return Entity.decls.filtermap(
            lambda d: d.error(
                d.decl.decl_type_name
                .concat(S(" forbidden in "))
                .concat(Entity.decl_type_name)
            ),
            lambda d: Not(Entity.is_authorized_decl(d.decl))
        )


class GenericParamAssoc(Struct):
    """
    Generic parameter association. Associates a generic formal type to an
    actual type.
    """
    formal = UserField(T.GenericFormalTypeDecl)
    actual = UserField(T.TypeDecl)


class InferInstantiation(Struct):
    """
    Result of an instantiation inference. The result will either be:

    * An array of generic param associations, if the inference succeeded.
    * An error, if the inference failed.
    """
    result = UserField(
        T.GenericParamAssoc.array, default_value=No(T.GenericParamAssoc.array)
    )
    error = UserField(T.SemanticResult, default_value=No(T.SemanticResult))


class GenericDecl(Decl):
    """
    Generic entity declaration.
    """
    annotations = Annotations(rebindable=True)

    generic_formal_decls = Field(type=T.FullDecl.list)
    decl = Field(type=T.Decl)
    name = Property(Self.decl.name)
    syn_name = NullField()

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env()
    )

    generic_formals = Property(Entity.generic_formal_decls.map(
        lambda gfd: gfd.decl.cast(T.GenericFormalTypeDecl)
    ))

    decl_type_name = Property(
        S("generic ").concat(Entity.decl.decl_type_name)
    )

    @langkit_property(return_type=T.Decl.entity)
    def instantiate(actuals=T.TypeDecl.array):
        """
        Instantiate this generic decl with the given actuals.
        """
        fd = Var(Self.decl.cast(FunDecl))
        return Cond(
            Entity.decl.is_a(TypeDecl),
            Entity.get_instantiated_type(actuals).cast(T.Decl).as_bare_entity,

            Entity.decl.is_a(FunDecl),
            FunDecl.entity.new(
                node=fd,
                info=T.entity_info.new(
                    md=No(T.Metadata),
                    from_rebound=False,
                    rebindings=Entity.info.rebindings.append_rebinding(
                        Self.children_env,
                        Entity.get_inst_env(actuals)
                    )
                )
            ).cast(T.Decl.entity),

            PropertyError(T.Decl.entity, "should not happen")
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
    traits = NullField()
    base_type = NullField()
    name = Property(Self.inner_type_decl.name)

    full_name = Property(
        Self.name.image.concat(S('[')).concat(
            S(", ").join(Self.actuals.map(lambda t: t.full_name))
        ).concat(S(']'))
    )

    generic_decl = Property(
        Entity.inner_type_decl.parent.as_entity.cast_or_raise(T.GenericDecl)
    )

    decl_type_name = Property(S("instantiated generic type"))

    @langkit_property(public=True)
    def get_inner_type():
        """
        Return the generic type that ``self`` instantiates.
        """
        return Entity.inner_type_decl

    @langkit_property(public=True)
    def get_actuals():
        """
        Return the declaration of types that were passed as generic actuals to
        create ``self``.
        """
        return Entity.actuals.map(lambda a: a.as_entity)

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


class TraitDecl(NamedTypeDecl):
    """
    Trait declaration. For the moment, a Trait can just be used to group
    behavior for built-in types. It's not usable as a type-bound since we don't
    have generics, and you cannot implement one either.

    The reason they're added is to lay down the basics of what we want the Lkt
    type system to be.

    TODO: Traits are *not* types. They're treated as such in the grammar for
    convenience for now, but it's probably not a good idea. Migrate away from
    this.
    """

    syn_name = Field(type=T.DefId)
    traits = NullField()
    base_type = NullField()

    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("trait declaration"))

    @langkit_property()
    def is_authorized_decl(decl=T.Decl.entity):
        # FunDecl and generic FunDecl are valid declarations
        return decl.is_a(T.FunDecl) | decl.is_generic_fun_decl


class EnumTypeDecl(NamedTypeDecl):
    """
    Enum type declaration.
    """

    syn_name = Field(type=T.DefId)
    traits = Field(type=T.TypeRef.list)
    base_type = NullField()
    literals = Field(type=T.EnumLitDecl.list)
    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("enum declaration"))

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        handle_children(),
        # Add enum literals to the DeclBlock env
        add_to_env(Entity.literals.map(lambda lit: new_env_assoc(
                key=lit.name,
                val=lit.node,
                dest_env=direct_env(Self.decls.children_env),
        ))),

        # If the enum is marked as @open, add enum literals to the enum type's
        # containing env.
        add_to_env(If(
            Entity.full_decl.has_annotation('open'),
            Entity.literals.map(lambda lit: new_env_assoc(
                key=lit.name,
                val=lit.node,
                dest_env=direct_env(Self.node_env),
            )),
            No(T.env_assoc.array)
        ))
    )

    @langkit_property()
    def is_authorized_decl(decl=T.Decl.entity):
        # No declaration allowed in an enum type
        ignore(decl)
        return False


class StructDecl(NamedTypeDecl):
    """
    Declaration for a LK struct.
    """
    syn_name = Field(type=T.DefId)
    traits = Field(type=T.TypeRef.list)
    base_type = NullField()
    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("struct declaration"))

    @langkit_property()
    def is_authorized_decl(decl=T.Decl.entity):
        return decl.is_a(T.FunDecl, T.FieldDecl)


@abstract
class BasicClassDecl(NamedTypeDecl):
    """
    Common ancestor for declarations of regular classes and enum classes.
    """
    syn_name = Field(type=T.DefId)
    base_type = Field(type=T.TypeRef)
    traits = Field(type=T.TypeRef.list)


class ClassDecl(BasicClassDecl):
    """
    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.
    """
    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("class declaration"))

    @langkit_property()
    def is_subtype(potential_base=T.TypeDecl.entity):
        return Not(
            Entity.base_types.find(lambda bt: bt == potential_base).is_null
        )

    @langkit_property()
    def is_authorized_decl(decl=T.Decl.entity):
        # FunDecl, FieldDecl, and generic FunDecl declarations are valid
        return decl.is_a(T.FunDecl, T.FieldDecl) | decl.is_generic_fun_decl


class EnumClassAltDecl(TypeDecl):
    """
    Alternative for an enum class decl.
    """
    syn_name = Field(T.DefId)
    traits = NullField()
    base_type = NullField()

    parent_type = Property(Entity.parent.parent.cast_or_raise(T.TypeDecl))

    type_scope = Property(Entity.parent_type.type_scope)

    # Empty env spec: alts are added as part of EnumClassDecl's env_spec
    env_spec = EnvSpec()

    decl_type_name = Property(S("enum class alt declaration"))


class EnumClassCase(LKNode):
    """
    Case branch for an enum class declaration.
    """
    decls = Field(T.EnumClassAltDecl)


class EnumClassDecl(BasicClassDecl):
    """
    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.
    """
    branches = Field(type=T.EnumClassCase.list)
    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("enum class declaration"))

    alts = Property(Entity.branches.mapcat(
        lambda branch: branch.decls.map(lambda d: d)
    ))

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        handle_children(),
        add_to_env(Entity.alts.map(lambda alt: new_env_assoc(
            key=alt.name,
            val=alt.node,
            dest_env=direct_env(Self.decls.children_env),
        )))
    )

    @langkit_property()
    def is_authorized_decl(decl=T.Decl.entity):
        return decl.is_a(T.FunDecl)


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
                      return_type=T.TypeDecl.entity,
                      public=True)
    def get_type(no_inference=(T.Bool, False)):
        """
        Get the type of this value declaration.
        """
        ignore(no_inference)
        pass


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

    decl_type_name = Property(S("self declaration"))


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

    decl_type_name = Property(S("node declaration"))


@abstract
class UserValDecl(BaseValDecl):
    """
    Class for user declared val declarations (not synthetic).
    """
    syn_name = Field(type=T.DefId)


class FunDecl(UserValDecl):
    """
    Function declaration.
    """
    args = Field(type=T.FunArgDecl.list)
    return_type = Field(type=T.TypeRef)
    body = Field(type=T.Expr)

    decl_type_name = Property(S("function declaration"))

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

    call_scope = Property(Entity.children_env)

    @langkit_property()
    def is_property():
        """
        Returns whether this function decl defines a property or not.

        A property is a function that has the @property annotation and has no
        parameters. As a result of this annotation, it will be callable without
        parens.
        """
        return Entity.full_decl.has_annotation('property')

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        return If(
            Entity.is_property,

            Entity.return_type.designated_type,

            Self.function_type(
                # NOTE: For methods, we don't add the owning type. We consider
                # that for the moment the only kind of references to method's
                # function types are with the self argument bound (ie. via dot
                # notation).
                #
                # At some stage we might want to create a notation for a "free
                # method", where `self` is not bound, but that doesn't exist
                # yet.
                Entity.args.map(
                    lambda a:
                    a.decl_type.designated_type.assert_bare.cast(T.TypeDecl)
                ),
                Entity.return_type.designated_type.assert_bare.cast(T.TypeDecl)
            ).as_bare_entity,
        )


class EnumLitDecl(UserValDecl):
    """
    Enum literal declaration.
    """
    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        return (
            Entity.parents.find(lambda t: t.is_a(T.EnumTypeDecl))
            .cast_or_raise(T.TypeDecl)
        )

    # Empty env spec: enum lits are added as part of EnumTypeDecl's env_spec
    env_spec = EnvSpec()

    decl_type_name = Property(S("enum literal declaration"))


@abstract
class ExplicitlyTypedDecl(UserValDecl):
    """
    Subset of user declared value declarations for values that have a type that
    can be syntactically annotated by the user.
    """
    decl_type = AbstractField(type=T.TypeRef)

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        ignore(no_inference)
        return Entity.decl_type.designated_type


@abstract
class ComponentDecl(ExplicitlyTypedDecl):
    """
    Subset of explicitly typed declarations for value declarations that:

    1. Have an optional default value.
    2. Are part of a bigger declaration that can be referred to via a call
       expression (either a type or a function).
    """
    default_val = AbstractField(type=T.Expr)

    @langkit_property()
    def to_formal_param():
        """
        Return a ``FormalParam`` structure from this component decl.
        """
        return FormalParam.new(formal_name=Self.name,
                               formal_type=Entity.get_type,
                               default_value=Entity.default_val)


class FunArgDecl(ComponentDecl):
    """
    Function argument declaration.
    """
    decl_type = Field(type=T.TypeRef)
    default_val = Field(type=T.Expr)
    decl_type_name = Property(S("fun arg declaration"))


class LambdaArgDecl(ComponentDecl):
    """
    Function argument declaration.
    """
    decl_type = Field(type=T.TypeRef)
    default_val = Field(type=T.Expr)

    decl_type_name = Property(S("lambda arg declaration"))

    owning_lambda = Property(
        Entity.parents.find(lambda p: p.is_a(LambdaExpr)).cast(T.LambdaExpr)
    )

    @langkit_property(memoized=True)
    def index():
        """
        Return the index of this argument in the containing lambda.
        """
        return Entity.parent.cast_or_raise(T.LambdaArgDecl.list).filtermap(
            lambda i, arg: i,
            lambda arg: (arg == Entity)
        ).at(0)

    @langkit_property()
    def get_type(no_inference=(T.Bool, False)):
        return If(
            # If no inference, return the explicitly declared type - that might
            # be null.
            no_inference,
            Entity.decl_type.designated_type,

            # If inference: return the infered type. NOTE: we might still need
            # to fall back on the explicit type in some cases? This might
            # return an erroneous result in case of erroneous file, but will be
            # correct when the file is correct.
            Entity.owning_lambda.expected_type
            .cast_or_raise(FunctionType).then(
                lambda ft: ft.args.at(Entity.index).as_entity
            )
        )


class FieldDecl(ComponentDecl):
    """
    Field declaration.
    """
    decl_type = Field(type=T.TypeRef)
    default_val = Field(type=T.Expr)
    decl_type_name = Property(S("field declaration"))


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

    @langkit_property(return_type=T.SemanticResult.array)
    def check_correctness_pre():

        d = Var(Entity.type_name.referenced_decl)

        return d.result_ref.then(
            lambda d: d.match(
                # The type ref references a type decl: return no error
                lambda _=T.TypeDecl: No(T.SemanticResult.array),

                # Not a type decl: return an error that the type reference is
                # invalid.
                lambda _: [Entity.error(S("Invalid type reference"))]
            )
        )


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
                lambda p: p.designated_type.node
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
                a.designated_type.node
            ),
            Entity.return_type.designated_type.node
        ).as_bare_entity


class ArrayLiteral(Expr):
    """
    Literal for an array value.
    """
    exprs = Field(type=T.Expr.list)

    @langkit_property()
    def expected_type_predicate(expected_type=T.TypeDecl.entity):
        # Check that ``expected_type`` is an array type, and that all
        # expressions in this array literal are compatible with the element
        # type.
        return expected_type.then(
            lambda t: t.array_element_type.then(
                lambda elt_type: Self.all_elements_match(elt_type)
            )
        )

    @langkit_property()
    def invalid_expected_type_error_name():
        return S("an array")

    @langkit_property()
    def all_elements_match(elt_type=T.TypeDecl.entity):
        """
        Check that all elements of this array literal match the ``elt_type``
        type.
        """
        return Self.exprs.all(lambda e: e.expected_type_predicate(elt_type))

    @langkit_property()
    def first_elt_context_free_type():
        """
        Return the first context-free type for the exprs list, or null if there
        is none.
        """
        # Get all non-null context-free type of exprs list
        return Self.exprs.filtermap(
            lambda e: e.as_entity.expr_context_free_type,
            lambda e: Not(e.as_entity.expr_context_free_type.is_null)
        ).then(
            # Return the first one if it exists
            lambda l: l.at(0)
        )

    @langkit_property()
    def expr_context_free_type():
        elt_type = Var(Entity.first_elt_context_free_type)

        # If we have an element context-free type and that all elements matches
        # it, we have a context-free array type.
        return If(
            And(
                Not(elt_type.is_null),
                Self.all_elements_match(elt_type)
            ),

            Entity.get_array_type(elt_type.assert_bare.cast(T.TypeDecl)),

            No(T.TypeDecl.entity)
        )


class NotExpr(Expr):
    """
    Boolean negation expression.
    """
    expr = Field(type=T.Expr)


class ExcludesNull(LKNode):
    """
    Whether the containing cast expression will raise on null cast result or
    not.
    """
    enum_node = True
    qualifier = True


class CastExpr(Expr):
    """
    Cast expression.
    """
    expr = Field(type=T.Expr)
    excludes_null = Field(type=T.ExcludesNull)
    dest_type = Field(type=T.TypeRef)

    expr_context_free_type = Property(Entity.dest_type.designated_type)

    @langkit_property(return_type=T.SemanticResult.array)
    def check_correctness_post():
        expr_type = Var(Entity.expr.check_expr_type)
        dest_type = Var(Entity.expr_context_free_type)
        return If(
            Or(expr_type.base_types.contains(dest_type),
               dest_type.base_types.contains(expr_type)),

            No(T.SemanticResult.array),

            Entity.error(
                S("Invalid cast: only up/down casting allowed")
            ).singleton
        )


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

    # Forward the type of the inner expression
    expr_context_free_type = Property(Entity.expr.expr_context_free_type)


class FormalParam(Struct):
    """
    Represent all the information of a formal parameter. Note that formal_name
    can (and will) be null for formals of function types.
    """
    formal_name = UserField(type=T.Symbol)
    formal_type = UserField(type=T.TypeDecl.entity)
    default_value = UserField(type=T.Expr.entity)


class CallExpr(Expr):
    """
    Call expression.
    """
    name = Field(type=T.Expr)
    args = Field(type=T.Param.list)

    @langkit_property(return_type=T.SemanticResult.array)
    def check_correctness_pre():

        rd = Var(Try(Entity.name.referenced_decl,
                     SemanticResult.new(has_error=True)))

        et = Var(Try(
            Entity.name.expr_type, SemanticResult.new(has_error=True)
        ))

        return If(
            rd.has_error | et.has_error,
            No(T.SemanticResult.array),

            # First, check for generic instantiation errors. Before this, we
            # cannot call ``Entity.called_decl``, because it assumes that
            # generic instantiation errors have been checked beforehand.
            rd.result_ref.cast(T.GenericDecl).then(
                lambda generic_decl: Entity.infer_actuals(
                    generic_decl,
                    Entity.expected_type
                ).then(lambda iir: iir.error.then(lambda e: e.singleton)),

            )

            # Handle errors for when we have a call that we can't statically
            # resolve to a declaration, so we need to rely on the object_type,
            # and the object type is non callable.
            ._or(
                If(
                    Entity.called_decl.is_null
                    & Entity.called_object_type.is_null,

                    Entity.name.error(
                        S("Object of type `")
                        .concat(et.result_type.full_name)
                        .concat(S("` is not callable"))
                    ).singleton,

                    No(T.SemanticResult.array)
                ),
            )
            # Lastly, handle formals <-> actuals matching errors
            ._or(
                Entity.match_params().filter(lambda pm: Not(pm.has_matched))
                .mapcat(
                    lambda pm: If(
                        pm.formal.is_null,

                        Array([
                            pm.actual.error(If(
                                rd.is_null,

                                S("Unmatched actual"),

                                S("Unmatched actual in call to `")
                                .concat(Entity.called_decl.full_name)
                                .concat(S("`"))
                            )),

                            # If there is an unmatched actual that is named,
                            # then exempt the resolution of the name, because
                            # it's going to fail.
                            pm.actual.name._.exempt]),

                        Self.error(
                            If(
                                rd.is_null,

                                S("No value for parameter"),

                                S("No value for parameter `")
                                .concat(pm.formal.formal_name.image)
                                .concat(S("` in call to `"))
                                .concat(Entity.called_decl.full_name)
                                .concat(S("`"))

                            )
                        ).singleton
                    )
                )
            )
            # In case of PropertyError, check for RaiseExpr
            ._or(
                If(
                    And(
                        rd.result_ref == Entity.property_error_type,
                        Not(Entity.first_no_paren_parent.is_a(T.RaiseExpr))
                    ),

                    Self.error(
                        S("cannot call ")
                        .concat(Entity.property_error_type.full_name)
                        .concat(S(" outside of a raise expression"))
                    ).singleton,

                    No(T.SemanticResult.array)
                )
            )
        )

    @langkit_property(return_type=T.InferInstantiation)
    def infer_actuals_impl(
        generic_decl=T.GenericDecl.entity,
        gen_type=T.TypeDecl.entity,
        expected_type=T.TypeDecl.entity
    ):
        """
        Recursive implementation method for ``infer_actuals``. Recursively
        traverse the expected_type and the generic type, accumulating
        associations from generic formal types to generic actuals in the
        process.

        Return a single error result on any encountered errors. This means that
        we stop at the first error. NOTE: At the moment, the generated error
        has no message, instead just containing an error flag, deferring the
        error message creation to ``infer_actuals``.
        """
        return gen_type.match(
            # First case: we have a generic formal on the gen type side: in
            # that case, return the assoc between the formal and expected_type.
            lambda gen_form=GenericFormalTypeDecl: If(
                gen_form.generic_decl.node == generic_decl.node,
                InferInstantiation.new(
                    result=GenericParamAssoc.new(
                        formal=gen_form.node,
                        actual=expected_type.node
                    ).singleton,
                ),
                # This really should never happen: any generic formal
                # referenced in gen_type should be from the generic_decl.
                PropertyError(InferInstantiation, "should not happen")
            ),

            # Second case: we have an instantiated generic type on the gen type
            # side: in that case, we ensure that we also have an instantiated
            # generic type on the expected_type side, that the generic matches,
            # and we then recurse on the actuals.
            lambda inst=InstantiatedGenericType:
            expected_type.cast(InstantiatedGenericType).then(
                lambda etype: Cond(
                    # Ensure that the instantiated generic types are the same,
                    # return an error if they're not.
                    (inst.inner_type_decl != etype.inner_type_decl),
                    InferInstantiation.new(
                        error=SemanticResult.new(has_error=True)
                    ),

                    # If they're the same, iterate on the actuals of both
                    # types, and call ``infer_actuals_impl`` recursively,
                    # searching for generic formals on the gen type side.
                    inst.actuals.map(
                        lambda i, actual: Entity.infer_actuals_impl(
                            generic_decl,
                            actual.as_bare_entity,
                            etype.actuals.at(i).as_bare_entity
                        )
                    ).then(
                        # If the result of any subresult is an error, return it
                        lambda subresults:
                        subresults.find(lambda sr: Not(sr.error.is_null)).then(
                            lambda sr: sr,
                            # Else, concatenate the subresults and return that
                            default_val=InferInstantiation.new(
                                result=subresults.mapcat(lambda sr: sr.result)
                            )
                        ),
                    ),
                ),

                # If the expected_type is not a generic, error
                default_val=InferInstantiation.new(
                    error=SemanticResult.new(has_error=True)
                ),
            ),

            # General case: the gen type is neither a generic formal nor an
            # instantiated generic type: we have nothing to do, return an empty
            # result.
            lambda _: No(InferInstantiation)
        )

    @langkit_property(return_type=T.InferInstantiation)
    def infer_actuals(
        generic_decl=T.GenericDecl.entity,
        expected_type=T.TypeDecl.entity
    ):
        """
        Infer the actuals for ``generic_decl``, given an ``expected_type`` for
        the call of the declaration. Return an error result if incorrect.
        """
        unsorted_actuals = Var(Entity.infer_actuals_impl(
            generic_decl,
            generic_decl.decl.called_decl_type, expected_type
        ))

        return Cond(
            # Inference returned an error without message -> emit a general
            # unification error.
            Not(unsorted_actuals.error.is_null)
            & unsorted_actuals.error.error_message.is_null,
            InferInstantiation.new(error=Self.error(
                S("Cannot instantiate callee: cannot unify `")
                .concat(generic_decl.decl.called_decl_type.full_name)
                .concat(S("` with `")).concat(expected_type.full_name)
                .concat(S("`"))
            )),

            # We don't have all the actuals needed to infer -> return an error
            unsorted_actuals.result.length
            < generic_decl.generic_formals.length,
            InferInstantiation.new(
                error=Self.error(
                    S("Not enough information to infer "
                      "instantiation of generic callee")
                ),
            ),

            # Check that we don't have conflicting actuals for formals
            unsorted_actuals.result.map(
                lambda a: unsorted_actuals.result.find(
                    lambda a2:
                    (a.formal == a2.formal) & (a.actual != a2.actual)
                ).then(lambda a2: InferInstantiation.new(
                    error=Self.error(
                        S("Instantiation error: "
                          "conflicting actual types for formal ")
                        .concat(a.formal.quoted_name).concat(S(": "))
                        .concat(a.actual.quoted_name).concat(S(" and "))
                        .concat(a2.actual.quoted_name)
                    )
                ))
            ).at(0)

            # At this stage, either everything is fine or inference returned
            # with an error and an error message (which never happens in the
            # current implementation) -> return the result.
            ._or(unsorted_actuals)
        )

    @langkit_property(return_type=T.TypeDecl.array)
    def check_infer_actuals(
        generic_decl=T.GenericDecl.entity,
        expected_type=T.TypeDecl.entity
    ):
        """
        Infer the actuals for ``generic_decl``, when this decl
        is being called by this CallExpr, given an ``expected_type`` for the
        call. Raise on error.
        """
        unsorted_actuals = Var(
            Entity.infer_actuals(generic_decl, expected_type)
        )

        # Sort the assocs, and return only the actuals, so that they can be
        # used for an instantiation.
        return If(
            unsorted_actuals.error.is_null,
            generic_decl.generic_formals.map(
                lambda gen_form: unsorted_actuals.result.find(
                    lambda a: a.formal == gen_form.node
                ).actual
            ),
            PropertyError(T.TypeDecl.array, "Generic inference error")
        )

    @langkit_property(public=True)
    def called_object_type():
        """
        Return the type of the called object.
        """
        exp_type = Var(Entity.name.check_expr_type)
        return exp_type.match(
            lambda fun_type=T.FunctionType: fun_type,
            lambda t: t.get_fun('__call__')._.get_type,
        )

    @langkit_property(public=True)
    def called_decl():
        """
        Return the declaration that is called by this call expression, if there
        is one that can be statically determined.
        """

        # We use this to resolve formals rather than just the type of the
        # ``name`` of the callexpr, for two language features:
        #
        # * Generic functions: We need to statically resolve them in order to
        #   allow inference in the implicit instantiation.
        #
        # * Named parameters: Since function types don't contain parameter
        #   names, if we want to allow named parameters we need to statically
        #   resolve the target of the call when possible.

        refd_decl = Var(If(

            # If the name of this callexpr is a callexpr itself, there cannot
            # be a statically known referenced decl, since whatever is
            # referenced is the result.
            Not(Entity.name.is_a(CallExpr)),

            Entity.name.referenced_decl.then(
                lambda sr: sr.result_ref
            ).then(lambda decl: If(
                decl.cast(T.FunDecl)._.is_property, No(T.Decl.entity), decl
            )),

            No(T.Decl.entity)
        ))

        # Implement special resolution for calls to objects via __call__.
        # NOTE: We do this resolution here because in case where the object is
        # statically resolvable, the __call__ method might have keyword
        # arguments, see comment above.
        called_decl = Var(
            refd_decl.then(lambda refd_decl: refd_decl.match(

                # Don't try to look for __call__ on function decls
                lambda _=FunDecl: refd_decl,

                lambda v=BaseValDecl:
                v._.get_type._.get_fun('__call__')
                .then(lambda a: a, default_val=refd_decl),

                lambda _: refd_decl,
            ))
        )

        return called_decl.cast(T.GenericDecl).then(
            # If the called decl is a generic decl, then we need to instantiate
            # it: Infer the actuals of the generic decl being called..
            lambda gd: Entity.check_infer_actuals(gd, Entity.expected_type)
            .then(
                # And instantiate the generic declaration with those inferred
                # actuals.
                lambda actuals: gd.instantiate(actuals)
            ),
            # If not a generic decl, fall back on the initially found decl
            default_val=called_decl
        )._.match(
            # Here, if we found a declaration but it's not one of the
            # statically callable declarations (FunDecl or TypeDecl), we
            # return nothing, even though we might be able to statically
            # determine a declaration, like in the example below::
            #
            #    val a: (Int) -> Int = null
            #    val b: Int = a(12)
            #
            # That's because in language semantic terms, the actual called
            # function is determined dynamically in the above case.
            #
            # TODO: Maybe introduce a base class for statically callable
            # declarations, so we don't have to do this ugly match.
            lambda t=TypeDecl: t,
            lambda f=FunDecl: f,
            lambda _: No(T.Decl.entity)
        )

    @langkit_property()
    def expr_context_free_type():
        return Entity.called_decl.then(
            # First case, we have a statically resolvable called decl: Use it
            # to determine the context free type of this expression.
            # TODO: it's not clear we actually need this branch: we could
            # determine the type from the type of name in all cases?
            lambda rd: rd.match(
                lambda fd=T.FunDecl: fd.return_type.designated_type,
                lambda td=T.TypeDecl: td,
                lambda _:
                PropertyError(T.TypeDecl.entity, "should not happen"),
            )
        )._or(
            # Second case, we don't have a statically resolvable called decl:
            # just rely on the type of the ``name``.
            Entity.name.expr_context_free_type.then(
                lambda t: t.match(
                    lambda ft=FunctionType: ft.return_type.as_entity,
                    lambda t=TypeDecl:
                    t.get_fun('__call__')._.return_type.designated_type,
                )
            )
        )

    @langkit_property(return_type=T.FormalParam.array)
    def formals():
        """
        Return an array of ``FormalParam`` corresponding to the formals of the
        called object.
        """
        return Entity.called_decl.then(
            # Case where the called decl is known statically
            lambda cd: cd.match(
                lambda fd=T.FunDecl: fd.args.map(
                    lambda p: p.cast_or_raise(T.ComponentDecl).to_formal_param,
                ),
                lambda td=T.TypeDecl:
                td.fields.map(lambda c: c.to_formal_param),
                lambda _:
                PropertyError(T.FormalParam.array, "Should not happen")
            ),

            # Case where there is no statically known declaration, so we base
            # the resolution of formals on the callable type of the name.
            default_val=Entity.called_object_type.match(
                lambda ft=T.FunctionType: ft.args.map(
                    lambda a: FormalParam.new(
                        formal_name=No(T.Symbol),
                        formal_type=a.as_entity,
                        default_value=No(T.Expr.entity)
                    )
                ),
                lambda _:
                PropertyError(T.FormalParam.array, "Should not happen")
            )
        )

    @langkit_property()
    def static_match_params(formals=T.FormalParam.array,
                            actuals=T.Param.entity.array):
        """
        Static method. Returns an array of ParamMatch structures, matching the
        actual parameters of a call to the formal parameters of the
        declaration. This will work for types constructors and function calls.

        This will also contain entries for unmmatched actuals and formals.
        """
        actual_matches = Var(actuals.map(lambda i, a: If(
            a.name.is_null,
            formals.at(i).then(
                lambda f: ParamMatch.new(has_matched=True, actual=a, formal=f),
                default_val=ParamMatch.new(has_matched=False, actual=a)
            ),
            formals.find(lambda f: f.formal_name == a.name.symbol).then(
                lambda f: ParamMatch.new(has_matched=True, actual=a, formal=f),
                default_val=ParamMatch.new(has_matched=False, actual=a)
            )
        )))

        # Create param matches for every formal that is unmatched
        formal_misses = Var(formals.filter(
            lambda formal:
            formal.default_value.is_null
            & actual_matches.find(
                lambda pm: pm.formal == formal
            ).is_null
        ).map(lambda formal: ParamMatch.new(has_matched=False, formal=formal)))

        return actual_matches.concat(formal_misses)

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

    expr_context_free_type = Property(
        Entity.instantiated_decl.match(
            lambda t=T.TypeDecl: t,
            lambda f=T.FunDecl: f.get_type(),
            lambda _: PropertyError(T.TypeDecl.entity, "")
        )
    )

    @langkit_property(public=True)
    def instantiated_decl():
        """
        Get the type designated by this instantiation.
        """
        # Get the non instantiated generic declaration
        generic_decl = Var(
            Entity.name.check_referenced_decl.cast_or_raise(T.GenericDecl)
        )

        # Then instantiate it with the given types as generic actuals
        return generic_decl.instantiate(
            Entity.args.map(
                lambda p: p.designated_type.assert_bare
                .cast_or_raise(T.TypeDecl)
            )
        )

    referenced_decl = Property(
        SemanticResult.new(result_ref=Entity.instantiated_decl, node=Self)
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
    return_type = Field(type=T.TypeRef)
    body = Field(type=T.Expr)

    @langkit_property()
    def expected_type_predicate(expected_type=T.TypeDecl.entity):
        return expected_type.is_a(FunctionType)

    invalid_expected_type_error_name = Property(S("a function literal"))

    @langkit_property()
    def is_annotated():
        """
        Returns whether this lambda is type annotated or not. NOTE: will return
        incorrect results if the lambda expr is incorrect, ie. partially
        annotated.
        """
        return Not(Entity.return_type.is_null)

    expr_context_free_type = Property(If(
        # If the lambda has annotations, create a context free type from them
        Entity.is_annotated,
        Self.function_type(
            Entity.params.map(
                lambda p:
                p.decl_type.designated_type.assert_bare.cast(T.TypeDecl)
            ),
            Entity.return_type.designated_type.assert_bare.cast(T.TypeDecl)
        ).as_bare_entity,
        # Else, no context free type
        No(T.TypeDecl.entity)
    ))

    @langkit_property(return_type=T.SemanticResult.array)
    def check_correctness_pre():
        # Check that either all types have type annotations, or no type have
        # type annotations, to simplify our job later in terms of inference.
        typs = Var(Entity.params.map(
            lambda p: Not(p.decl_type.is_null)
        ))

        return Cond(
            # Either:
            Not(Or(
                # No types have annotations
                Not(typs.any(lambda t: t)),
                # All types have annotations
                typs.all(lambda t: t)
            )),
            # If that's not the case, then error
            Self.params.error(
                S("Invalid type annotations for lambda parameters.\n"
                  "Annotate either all or no params")
            ).singleton,

            No(T.SemanticResult.array)
        )

    env_spec = EnvSpec(add_env())


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

    @langkit_property(return_type=T.SemanticResult.array)
    def check_correctness_pre():
        except_expr_type = Var(Entity.except_expr.expr_type.result_type)
        pe_type = Var(Entity.property_error_type)
        return If(
            except_expr_type == pe_type,

            No(T.SemanticResult.array),

            Self.except_expr.error(
                S("raised expression needs to be of type `").concat(
                    pe_type.full_name
                ).concat(S("`, got `")).concat(
                    except_expr_type.full_name
                ).concat(S("`"))
            ).singleton
        )


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


class MatchValDecl(ExplicitlyTypedDecl):
    """
    Value declaration in a match branch.
    """
    decl_type = Field(type=T.TypeRef)
    decl_type_name = Property(S("match value declaration"))


class BinOp(Expr):
    """
    Binary operator expression.
    """
    left = Field(type=T.Expr)
    op = Field(type=T.Op)
    right = Field(type=T.Expr)

    @langkit_property()
    def expr_context_free_type():
        return Cond(
            # If op is a relational operator, the cf type is bool
            Self.op.is_a(Op.alt_and, Op.alt_or, Op.alt_lt, Op.alt_gt,
                         Op.alt_lte, Op.alt_gte, Op.alt_eq),

            Self.bool_type,

            # Else, no idea for the moment. NOTE: we can probably improve that
            # by forwarding the type of operands.
            No(T.TypeDecl.entity)
        )


class ValDecl(ExplicitlyTypedDecl):
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

    decl_type_name = Property(S("value declaration"))


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

    invalid_expected_type_error_name = Property(S("a string literal"))


class CharLit(Lit):
    """
    Character literal expression.
    """
    token_node = True

    @langkit_property()
    def expected_type_predicate(expected_type=T.TypeDecl.entity):
        return expected_type == Self.char_type

    invalid_expected_type_error_name = Property(S("a character literal"))


class NumLit(Lit):
    """
    Number literal expression.
    """
    token_node = True

    @langkit_property()
    def expected_type_predicate(expected_type=T.TypeDecl.entity):
        return Or(expected_type == Self.int_type,
                  expected_type == Self.bigint_type)

    invalid_expected_type_error_name = Property(S("a number literal"))


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

    import_stmt=Import("import", ModuleRefId(Lex.Identifier)),
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

    lexer_rule=GOr(G.lexer_family_decl, G.decl, G.lexer_case_rule),

    lexer_family_decl=FullDecl(
        G.doc,
        List(G.decl_annotation, empty_valid=True),
        LexerFamilyDecl(
            Lex.Identifier(match_text="family"), G.def_id, "{",
            List(G.decl, empty_valid=False),
            "}"
        )
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
            "is", List(G.ref_id, sep="|", empty_valid=False),
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

    grammar_token=TokenRef(
        "@", G.ref_id, Opt("(", G.token_literal, ")")
    ),

    type_decl=Or(
        StructDecl(
            "struct", G.def_id, Opt("implements", G.type_list),
            "{", G.decl_block, "}"
        ),

        EnumClassDecl(
            "enum", "class", G.def_id,
            Opt(":", G.type_ref),
            Opt("implements", G.type_list),
            "{",
            List(EnumClassCase(
                "case", List(EnumClassAltDecl(G.def_id), sep=",")
            ), empty_valid=True),
            G.decl_block,
            "}"
        ),

        ClassDecl(
            "class", G.def_id, Opt(":", G.type_ref),
            Opt("implements", G.type_list),
            "{",
            G.decl_block,
            "}"
        ),

        EnumTypeDecl(
            "enum", G.def_id,
            Opt("implements", G.type_list),
            "{",
            "case", List(G.enum_lit_decl, sep=","),
            G.decl_block,
            "}"
        ),

        TraitDecl("trait", G.def_id, "{", G.decl_block, "}"),

    ),

    generic_decl=GenericDecl(
        "generic", "[", List(G.generic_formal_type, sep=","), "]", G.bare_decl
    ),

    generic_formal_type=FullDecl(
        G.doc,
        List(G.decl_annotation, empty_valid=True),
        GenericFormalTypeDecl(G.def_id)
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
        G.type_ref,
        Opt("=", G.expr)
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
        FunctionTypeRef(
            "(", List(G.type_ref, empty_valid=True, sep=","), ")",
            "->", G.type_ref
        )
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
        Isa(G.primary, "is", List(G.type_ref, sep="|", empty_valid=False)),
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
    char_lit=CharLit(Lex.Char),

    if_expr=IfExpr(
        "if", G.expr, "then", G.expr,
        List(ElsifBranch("elif", G.expr, "then", G.expr), empty_valid=True),
        "else", G.expr
    ),

    raise_expr=RaiseExpr("raise", G.expr),
    try_expr=TryExpr("try", G.expr, Opt("else", G.expr)),

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
        CastExpr(
            G.basic_expr, ".",
            Lex.Identifier(match_text="as"),
            ExcludesNull("!"),
            "[", G.type_ref, "]"
        ),
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
        G.char_lit,
        G.array_literal,
    ),

    basic_name=GOr(
        DotExpr(G.basic_name, ".", G.ref_id),
        G.ref_id
    ),


    lambda_expr=LambdaExpr("(", G.lambda_arg_list, ")",
                           Opt(":", G.type_ref), "=>", cut(), G.expr),

    null_lit=NullLit("null"),

    param=Param(Opt(G.ref_id, "="), G.expr),
    params=List(G.param, sep=",", empty_valid=True),

    decl_annotation_params=Opt(DeclAnnotationParams("(", G.params, ")")),
    decl_annotation=DeclAnnotation("@", G.id, G.decl_annotation_params),

)
