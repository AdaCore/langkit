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
    ASTNode, AbstractField, Annotations, Field, LogicVar, LookupKind as LK,
    NullField, Struct, T, UserField, abstract, synthetic
)
from langkit.envs import (
    EnvSpec, add_env, add_to_env, add_to_env_kv, do, handle_children,
    reference,
)
from langkit.expressions import (
    AbstractKind, AbstractProperty, ArrayLiteral as Array, CharacterLiteral,
    Cond, DynamicLexicalEnv, DynamicVariable, Entity, If, LogicFalse,
    LogicTrue, NPropagate, No, Not, Or, Predicate, Property, PropertyError,
    Self, String as S, Var, direct_env, ignore, langkit_property,
    lazy_field, new_env_assoc
)
from langkit.expressions.logic import Bind
from langkit.parsers import (
    Cut, Grammar, List, ListSepExtra, Opt, Or as GOr, Predicate as GPredicate
)


from language.lexer import lkt_lexer as Lex


current_param = DynamicVariable("current_name", T.Param.entity, doc="""
    Call parameter used to find the correspoding FunArgDecl during type
    resolution of function calls.
    """)

in_logic_call = DynamicVariable("in_logic_call", T.Bool, doc="""
    Wether or not we are currently solving a LogicPropage or a LogicPredicate.
    """)

origin = DynamicVariable("origin", T.LktNode.entity, doc="""
    Origin node of the request.
    """)


class ParamMatch(Struct):
    """
    Helper data structure to implement parameter matching.
    """
    has_matched = UserField(type=T.Bool)
    actual = UserField(type=T.Param.entity, default_value=No(T.Param.entity))
    formal = UserField(type=T.FormalParam, default_value=No(T.FormalParam))


@abstract
class LktNode(ASTNode):
    """
    Root node class for lkt AST nodes.
    """

    @langkit_property(return_type=T.Bool, external=True, public=True,
                      uses_entity_info=False, uses_envs=False)
    def set_solver_debug_mode(enable=T.Bool):
        """
        Enable or disable the solver traces for debugging purposes.
        """
        pass

    @langkit_property(return_type=T.Decl.entity)
    def root_get(entity_name=T.Symbol):
        return Self.unit.root.node_env.get_first(
            entity_name).cast_or_raise(T.Decl)

    @langkit_property(return_type=T.NamedTypeDecl.entity, memoized=True)
    def get_builtin_type(entity_name=T.Symbol):
        return Self.root_get(entity_name).cast_or_raise(T.NamedTypeDecl)

    @langkit_property(return_type=T.GenericDecl.entity)
    def get_builtin_gen_decl(entity_name=T.Symbol):
        return Self.root_get(entity_name).cast_or_raise(T.GenericDecl)

    basic_trait_gen = Property(
        Self.get_builtin_gen_decl('BasicTrait'), public=True,
        doc="Unit method. Return the ``BasicTrait`` builtin generic trait."
    )

    basic_trait = Property(
        Self.basic_trait_gen.decl.cast(T.TraitDecl), public=True,
        doc="Unit method. Return the ``BasicTrait`` builtin trait."
    )

    node_gen_trait = Property(
        Self.get_builtin_gen_decl('Node'), public=True,
        doc="Unit method. Return the ``Node`` builtin generic trait."
    )

    node_trait = Property(
        Self.node_gen_trait.decl.cast(T.TraitDecl), public=True,
        doc="Unit method. Return the ``Node`` builtin trait."
    )

    indexable_gen_trait = Property(
        Self.get_builtin_gen_decl('Indexable'), public=True,
        doc="Unit method. Return the ``Node`` builtin generic trait."
    )

    indexable_trait = Property(
        Self.indexable_gen_trait.decl.cast(T.TraitDecl), public=True,
        doc="Unit method. Return the ``Node`` builtin trait."
    )

    token_node_trait = Property(
        Self.get_builtin_type('TokenNode'), public=True,
        doc="Unit method. Return the ``TokenNode`` builtin trait."
    )

    error_node_trait = Property(
        Self.get_builtin_type('ErrorNode'), public=True,
        doc="Unit method. Return the ``ErrorNode`` builtin trait."
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

    entity_gen_type = Property(
        Self.get_builtin_gen_decl('Entity'), public=True,
        doc="Unit method. Return the logicvar builtin type."
    )

    entity_type = Property(
        Self.entity_gen_type.decl.cast(T.NamedTypeDecl), public=True,
        doc="Unit method. Return the logicvar builtin type."
    )

    logicvar_type = Property(
        Self.get_builtin_type('LogicVar'), public=True,
        doc="Unit method. Return the logicvar builtin type."
    )

    equation_type = Property(
        Self.get_builtin_type('Equation'), public=True,
        doc="Unit method. Return the logicvar builtin type."
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

    node_builder_gen_type = Property(
        Self.get_builtin_gen_decl('NodeBuilder'),
        public=True,
        doc="Unit method. Return the NodeBuilder builtin generic type."
    )

    node_builder_type = Property(
        Self.node_builder_gen_type.decl.cast(T.NamedTypeDecl),
        public=True,
        doc="Unit method. Return the NodeBuilder builtin type."
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

    @langkit_property(return_type=T.TypeDecl.entity, memoized=True)
    def any_type():
        """
        Create a AnyTypeDecl.
        """
        return T.AnyTypeDecl.new().as_entity

    @langkit_property(external=True, uses_entity_info=False, uses_envs=False,
                      return_type=T.AnalysisUnit)
    def internal_fetch_referenced_unit(name=T.String):
        """
        Return the unit that this unit name designates. Load it if needed.
        """
        pass

    @langkit_property(public=True)
    def topmost_invalid_decl():
        """
        Return the topmost (from ``Self`` to the root node) FullDecl annotated
        with ``@invalid``, null otherwise.
        """
        return Self.parents().filter(
            lambda p:
            p.cast(FullDecl).then(lambda fd: fd.has_annotation("invalid"))
        ).at(-1)

    @langkit_property(public=True, return_type=T.SolverDiagnostic.array)
    def nameres_diagnostics():
        """
        If name resolution on this lkt compilation unit fails, this returns all
        the diagnostics that were produced while resolving it.
        """
        return Entity.solve_equation.diagnostics

    @langkit_property(return_type=T.SolverResult, memoized=True,
                      call_memoizable=True)
    def solve_equation():
        """
        Solve the equation created by this node. This should be used on entry
        points only.
        """
        return Entity.xref_equation.solve_with_diagnostics

    xref_entry_point = Property(False, public=True, doc="""
        Designates entities that are entry point for the xref solving
        infrastructure. If this returns true, then nameres_diagnostics can be
        called on it.
        """)

    @langkit_property(public=True, return_type=T.SolverResult,
                      memoized=True, call_memoizable=True)
    def solve_enclosing_context():
        """
        Finds the nearest parent that is an xref_entry_point and solve its
        equation.
        """
        enclosing_context = Var(
            Entity.parents.find(lambda p: p.xref_entry_point)
        )
        return enclosing_context._.solve_equation

    @langkit_property(return_type=T.Equation,
                      kind=AbstractKind.abstract_runtime_check)
    def xref_equation():
        """
        Base property for constructing equations that will resolve names and
        types when resolved for every sub expression.
        """
        pass

    @langkit_property(return_type=T.Bool)
    def expected_type_entry_point():
        return Entity.match(
            lambda _=T.CastExpr: True,
            lambda _=T.Isa: True,
            lambda _=T.MatchExpr: True,
            lambda _=T.BaseDotExpr: True,
            lambda _=T.CallExpr: True,
            lambda _=T.LogicPredicate: True,
            lambda _=T.LogicPropagate: True,
            lambda p=T.ParenExpr: p.parent.expected_type_entry_point,
            lambda p=T.LambdaExpr: p.xref_entry_point,
            lambda _: False
        )

    @langkit_property(return_type=T.Bool, memoized=True, call_memoizable=True)
    def solve_expected_types():
        return Entity.expected_type_equation.solve

    @langkit_property(return_type=T.Equation,
                      kind=AbstractKind.abstract_runtime_check)
    def expected_type_equation():
        """
        Creates an equation that wil resolve expected types for children nodes.
        """
        pass

    @langkit_property(return_type=T.Bool)
    def generic_type_entry_point():
        return Entity.is_a(T.CallExpr, T.LogicPropagate, T.LogicPredicate)

    @langkit_property(return_type=T.Bool, memoized=True, call_memoizable=True)
    def solve_generic_types():
        return Entity.generic_type_equation.solve

    @langkit_property(return_type=T.Equation,
                      kind=AbstractKind.abstract_runtime_check)
    def generic_type_equation():
        """
        Creates an equation that will resolve generic types for children nodes.
        """
        pass

    @langkit_property(return_type=T.Bool,
                      predicate_error="unmatched arguments")
    def unmatched_argument(callee_type=T.TypeDecl.entity):
        """
        Predicate used to emit an error when an argument of a CallExpr could
        not be matched with a formal.
        """
        return (
            Not(Entity.is_null)
            | callee_type.cast(T.FunctionType)._.should_ignore_constructor_arg
        )

    @langkit_property(return_type=T.FunctionType.entity, memoized=True)
    def function_type_helper(args_types=T.TypeDecl.entity.array,
                             return_type=T.TypeDecl.entity,
                             origin=T.Decl.entity):
        """
        Helper function to create a memoized FunctionType.
        """
        return If(
            args_types.contains(No(T.TypeDecl.entity)) | return_type.is_null,
            No(T.FunctionType.entity),
            FunctionType.new(
                args=args_types,
                return_type=return_type,
                origin=origin
            ).as_entity
        )

    @langkit_property(return_type=T.LktNode.entity)
    def shed_rebindings():
        """
        Return this same entity but with its rebindings shed according to
        its children lexical environment.
        """
        return T.LktNode.entity.new(
            node=Self,
            info=Self.children_env.shed_rebindings(Entity.info)
        )


class LangkitRoot(LktNode):
    """
    For the moment, root node of a lkt compilation unit.
    """
    imports = Field(type=T.Import.list)
    decls = Field(type=T.FullDecl.list)

    @langkit_property(external=True,
                      uses_entity_info=False,
                      uses_envs=False,
                      return_type=T.AnalysisUnit)
    def fetch_prelude():
        """
        External property that will fetch the prelude unit, containing
        predefined types and values.
        """
        pass

    @langkit_property(return_type=T.LexicalEnv)
    def internal_env():
        """
        Get the hidden environment in the prelude containing a default
        declaration of the Metadata type, for when it is not defined by the
        specification.
        """
        return (
            origin.bind(
                No(T.LktNode.entity),
                Self.children_env().get_first("__internal")
                    ._.cast(T.Decl).defined_scope
            )
        )

    env_spec = EnvSpec(
        do(Self.fetch_prelude),
        handle_children(),
        reference(
            Self.cast(T.LktNode).singleton,
            T.LangkitRoot.internal_env,
            cond=(Self.unit == Self.fetch_prelude)
        )
    )


class Import(LktNode):
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

    env_spec = EnvSpec(
        do(Self.referenced_unit)
    )


class FullDecl(LktNode):
    """
    Container for an lkt declaration. Contains the decl node plus the
    documentation and annotations.
    """
    doc = Field(type=T.StringLit)
    decl_annotations = Field(type=T.DeclAnnotation.list)
    decl = Field(type=T.Decl)

    @langkit_property(public=True)
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
class Decl(LktNode):
    """
    Base class for declarations. Encompasses regular declarations as well as
    special declarations such as grammars, grammar rules, etc.
    """
    annotations = Annotations(custom_short_image=True)

    syn_name = AbstractField(type=T.DefId)
    name = Property(Self.syn_name._.symbol, public=True, doc="""
        Return the symbol corresponding to the name of this declaration.
    """)

    @langkit_property(return_type=T.String, external=True, public=True,
                      uses_entity_info=True, uses_envs=False)
    def custom_image():
        """
        Return the image string using entity information.
        """
        pass

    @langkit_property(return_type=T.String, public=True)
    def full_name():
        """
        Return the full name of this decl, as it should be seen by users/shown
        in diagnostics.
        """
        return Entity.name.image.concat(
            Entity.parent.cast(T.GenericDecl).then(
                lambda gd:
                    S("[").concat(
                        S(", ").join(
                            gd.generic_formals.map(
                                lambda gf: gd.decl.node_env
                                              .get_first(gf.name)
                                              .cast(T.Decl)
                                              .full_name
                            )
                        )
                    ).concat(S("]")),
            )
        )

    decl_type_name = AbstractProperty(
        public=True,
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
            lambda _: No(T.FullDecl.entity)
        )

    @langkit_property(return_type=T.Bool)
    def implements_node():
        """
        Checks if this decl implements the Node trait
        TODO: rework this.
        """
        return Entity.cast(T.ClassDecl).then(
            lambda cd: Not(cd.traits.find(
                lambda t:
                    t.cast(T.GenericTypeRef).then(
                        lambda gtr:
                        Self.children_env.get_first(
                            gtr.type_name.cast(T.RefId)._.symbol
                        ).cast(T.GenericDecl).decl
                    ) == Self.node_trait
            ).is_null),
            default_val=False,
        )

    env_spec = EnvSpec(
        add_to_env(
            Cond(
                # If the type explicitly implements the ``Node`` trait, or it
                # is annotated with "@root_node", consider it the root node of
                # the language specification.
                (Entity.full_decl._.has_annotation("root_node")
                 | Entity.implements_node) & Not(Entity.name == "RootNode__"),
                [
                    new_env_assoc(key="RootNode__", value=Self),
                    new_env_assoc(key=Entity.name, value=Self)
                ],
                Entity.full_decl._.has_annotation("metadata")
                & Not(Entity.name == "Metadata"),
                [
                    new_env_assoc(key="Metadata", value=Self),
                    new_env_assoc(key=Entity.name, value=Self)
                ],
                [new_env_assoc(key=Entity.name, value=Self)]
            )
        )
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

    @langkit_property(return_type=T.Bool,
                      predicate_error="$Self is not a type")
    def is_type_decl():
        return Self.is_a(T.TypeDecl)

    @langkit_property(return_type=T.Bool, predicate_error="undefined entity")
    def is_defined():
        return Not(Self.is_null)

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope():
        """
        Return the lexical environment defined by the declaration (ie. fields
        of a StructDecl).
        """
        return No(T.LexicalEnv)

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope_as_entity():
        """
        Return the lexical environment defined by the declaration as if it was
        contained inside an entity.

        Entity creates a special case for the ASTList class and Node trait:

        .. code::

            @builtin generic[T]
            class ASTList : ....... Indexable[T], Iterator[T]

        If the type ASTList is contained inside the Entity type
        (eg. Entity[FooNode.list]), then the properties inherited from its
        traits need to return entities. When this property is called on
        ASTList, we rebind T to ``Entity[T]`` and get the defined environments
        of the newly rebound entities instead.
        """
        return Entity.defined_scope

    @langkit_property(return_type=T.FunctionType.entity,
                      dynamic_vars=[(in_logic_call, False)])
    def infer_function_type(expected_call=T.FunctionType.entity):
        """
        Infer the type of the function from the expected_call if Entity is a
        generic declaration.
        This iterates through the generic formals of the decl to find all types
        that try to replace it and find their common_ancestor.
        """
        origin = Var(
            Entity.cast(T.FunctionType)._.origin._or(Entity)
        )
        generic_func = Var(
            Entity.cast(T.FunctionType)._or(Entity._.function_type)
        )
        return Cond(
            expected_call.is_null | origin.is_null,
            No(T.FunctionType.entity),
            Not(origin.is_generic),
            If(in_logic_call,
               origin.logic_function_type,
               origin.function_type),
            origin.instantiate_generic_decl(
                origin.parent.cast_or_raise(T.GenericDecl).generic_formals
                      .map(
                        lambda gf:
                        expected_call
                        .find_types_that_replace_ty(gf, generic_func)
                        .then(
                            lambda arr:
                            arr.at(0)._.imprecise_common_ancestor_list(arr)
                               .then(lambda t: If(t.matching_type(gf),
                                                  No(T.TypeDecl.entity), t))
                        )
                      )
                ).then(lambda rd: If(in_logic_call, rd.logic_function_type,
                                     rd.function_type))
        )

    @langkit_property(return_type=T.FunctionType.entity)
    def function_type():
        """
        Build and return a FunctionType corresponding to the current FunDecl.
        """
        return If(
            Entity.is_null,
            No(T.FunctionType.entity),
            Entity.match(
                lambda ft=T.FunctionType: ft,
                lambda td=T.StructDecl: td.function_type_aux,
                lambda td=T.ClassDecl: td.function_type_aux,
                lambda td=T.FunDecl: td.function_type_aux,
                lambda td=T.SynthFunDecl: td.function_type_aux,
                lambda td=T.GenericDecl: td.decl.function_type,
                lambda _: No(T.FunctionType.entity)
            )
        )

    @langkit_property(return_type=T.FunctionType.entity, memoized=True)
    def logic_function_type():
        """
        Build and return a FunctionType corresponding to the current FunDecl
        with an extra LogicVar at the beginning. Moreover, if the function is
        a dynamic combiner, set its argument to an array of logic variables.
        """
        return Entity.match(
            lambda td=T.FunDecl:
            td.function_type_aux.then(
                lambda ft: If(
                    td.is_dynamic_combiner,
                    T.FunctionType.new(
                        args=Array([
                            Entity.logicvar_type.make_array_type
                        ]),
                        return_type=ft.return_type,
                        origin=ft.origin,
                    ).as_entity,
                    T.FunctionType.new(
                        args=Array([
                            Entity.logicvar_type.cast(T.TypeDecl)
                        ]).concat(ft.args),
                        return_type=ft.return_type,
                        origin=ft.origin,
                    ).as_entity
                )
            ),
            lambda _: No(T.FunctionType.entity)
        )

    @langkit_property(return_type=T.TypeDecl.entity, public=True,
                      memoized=True, call_memoizable=True)
    def get_type():
        """
        Return the type of the Decl.
        """
        return Entity._.match(
            lambda td=T.TypeDecl: td,
            lambda fd=T.FunDecl:
                If(fd.full_decl.has_annotation("property"),
                   fd.return_type.referenced_decl,
                   fd.function_type),
            lambda eld=T.EnumLitDecl: eld.parent_type,
            lambda vd=T.ValDecl:
                Cond(
                    Not(vd.decl_type.is_null),
                    vd.decl_type.referenced_decl,
                    vd.solve_enclosing_context.success,
                    vd.expr.get_type,
                    No(T.TypeDecl.entity),
                ),
            lambda mvd=T.MatchValDecl:
                If(
                    mvd.decl_type.is_null,
                    mvd.match_expr.get_type,
                    mvd.match_expr.get_type
                                  .get_cast_type(mvd.decl_type.referenced_decl)
                ),
            lambda lad=T.LambdaArgDecl:
                If(lad.decl_type.is_null,
                   lad.parent.parent.cast(T.LambdaExpr).then(
                       lambda le:
                       If(le.solve_generic_types,
                          lad.type_var.get_value.cast(T.TypeDecl),
                          No(T.TypeDecl.entity))
                   ),
                   lad.decl_type.referenced_decl),
            lambda etd=T.ExplicitlyTypedDecl: etd.decl_type.referenced_decl,
            lambda gd=T.GenericDecl: gd.decl.get_type,
            lambda sd=T.SelfDecl:
                Entity.entity_type
                        .instantiate_generic_decl(sd.owning_type.singleton)
                        .cast_or_raise(T.TypeDecl.entity),
            lambda nd=T.NodeDecl: nd.owning_type,
            lambda _: No(T.TypeDecl.entity)
        )

    @langkit_property(return_type=T.TypeDecl.entity, public=True)
    def get_cast_type(cast_to=T.TypeDecl.entity):
        """
        If we are casting an entity (Self) to something that is not an entity,
        make it an entity.
        """
        return If(
            Not(cast_to.is_null | Entity.is_null)
            & (Self == Self.entity_type.node)
            & (cast_to.node != Self.entity_type.node),
            Self.entity_type.instantiate_generic_decl(cast_to.singleton),
            cast_to
        ).cast(T.TypeDecl)

    @langkit_property(return_type=T.TypeDecl.entity, public=True)
    def get_keep_type(keep_type=T.TypeDecl.entity):
        """
        Return the type of Entity when we only keep elements of type keep_type.
        If we are casting an entity (Self) to something that is not an entity,
        make it an entity.
        """
        return If(
            Not(keep_type.is_null | Entity.is_null) & (
                (Self == Self.entity_type.node)
                | (Self == Self.array_type.node)
                & (
                    Entity.cast(T.TypeDecl).get_array_content_type.node
                    == Self.entity_type.node
                )
            )
            & (keep_type.node != Self.entity_type.node),
            Self.entity_type.instantiate_generic_decl(keep_type.singleton),
            keep_type
        ).cast(T.TypeDecl)._.make_array_type

    @langkit_property(return_type=T.TypeDecl.entity, public=True)
    def get_suffix_type(prefix_type=T.TypeDecl.entity):
        """
        If we are accessing a ParseField of an entity, then that field's type
        also needs to be an entity.
        """
        suffix_type = Var(Entity._.get_type)
        return Cond(
            Not(prefix_type.is_null) & Not(Entity.is_null)
            & (prefix_type.node == Self.entity_type.node)
            & Entity.full_decl._.has_annotation("parse_field"),
            Self.entity_type.instantiate_generic_decl(suffix_type.singleton),
            # If we are accessing a lazy field through a type, or a @property
            # annotated function, consider it as a reference to a property-like
            # field.
            # This is necessary for type dynamic_lexical_env(Type.lazy_field).
            Not(Entity.is_null) & prefix_type.is_null & Self.is_a(T.FieldDecl)
            & Entity.full_decl.has_annotation("lazy"),
            Entity.cast(T.FieldDecl).lazy_field_function_type,
            Not(Entity.is_null) & prefix_type.is_null & Self.is_a(T.FunDecl)
            & Entity.full_decl.has_annotation("property"),
            Entity.function_type,
            suffix_type
        ).cast(T.TypeDecl)

    @langkit_property(return_type=T.Decl.entity)
    def type_var_suffix_ref(current_name=T.RefId.entity):
        """
        Return the declaration corresponding to current_name's name inside
        when Self is used as an expression:
        .. code::

            property().name
                       ^^^^

        If property() returns an Enum value, we should not be able to access
        the enum fields.
        """
        return Entity._.match(
            lambda enum=T.EnumTypeDecl:
                origin.bind(
                    Entity,
                    enum.basic_trait_from_self.defined_scope
                        .get_first(current_name.symbol, lookup=LK.flat)
                        .cast(T.Decl)._.subdecl_if_generic
                ),
            lambda _:
                origin.bind(
                    Entity,
                    Entity.defined_scope
                    .get_first(current_name.symbol, lookup=LK.flat)
                    .cast(T.Decl)._.subdecl_if_generic
                )
        )

    @langkit_property(return_type=T.Decl.entity)
    def ref_var_suffix_ref(type_var=T.TypeDecl.entity,
                           current_name=T.RefId.entity):
        """
        Return the declaration corresponding to current_name's name inside
        when Self is used as a declaration.
        """
        return Entity._.match(
            lambda enum=T.EnumTypeDecl:
                origin.bind(
                    No(T.TypeDecl.entity),
                    enum.defined_scope
                        .get_first(current_name.symbol, lookup=LK.flat)
                        .cast(T.Decl),
                ),
            lambda enum=T.EnumLitDecl:
                origin.bind(
                    No(T.TypeDecl.entity),
                    enum.defined_scope
                        .get_first(current_name.symbol, lookup=LK.flat)
                        .cast(T.Decl)._.subdecl_if_generic,
                ),
            lambda td=T.TypeDecl:
                origin.bind(
                    No(T.TypeDecl.entity),
                    Array([td.defined_scope, td.node_builder_scope])
                    .env_group()
                    .get(current_name.symbol, lookup=LK.flat)
                    .find(
                        lambda d:
                        d.is_a(T.TypeDecl, T.FunDecl)
                        | d.cast(T.FieldDecl)
                            ._.full_decl.has_annotation("lazy")
                    )
                    .cast(T.Decl)._.subdecl_if_generic,
                ),
            lambda _=T.MatchValDecl:
                # In the default case of MatchExprs, it is not possible to call
                # get_type on the MatchValDecl, so we use its type given in the
                # argument.
                type_var.then(
                    lambda t:
                    origin.bind(
                        t,
                        t.defined_scope
                        .get_first(current_name.symbol, lookup=LK.flat)
                        .cast(T.Decl)._.subdecl_if_generic,
                    )
                ),
            lambda mvd=T.LambdaArgDecl:
                mvd.decl_type
                .then(lambda _: mvd.get_type, default_val=type_var)
                .then(
                    lambda t:
                    origin.bind(
                        t,
                        t.defined_scope
                        .get_first(current_name.symbol, lookup=LK.flat)
                        .cast(T.Decl)._.subdecl_if_generic,
                    )
                ),
            lambda _:
                If(Entity.xref_entry_point
                   & type_var.is_null, Entity, type_var).then(
                    lambda t:
                    origin.bind(
                        t,
                        t.defined_scope
                        .get_first(current_name.symbol, lookup=LK.flat)
                        .cast(T.Decl)._.subdecl_if_generic
                    )
                )
        )

    @langkit_property(return_type=T.FormalParam.array, memoized=True)
    def formals(is_logic=(T.Bool, False)):
        """
        Return an array of FormalParam corresponding to the called function's
        formals.
        """
        return Entity.then(
            lambda c: c.match(
                lambda fd=T.FunDecl:
                Cond(
                    Not(is_logic),
                    fd.args.map(lambda p: p.to_formal_param),
                    fd.is_dynamic_combiner,
                    FormalParam.new(
                        formal_name=fd.args.at(0).name,
                        formal_type=Entity.logicvar_type.make_array_type,
                        has_default_value=False,
                        decl=fd.args.at(0)
                    ).singleton,
                    FormalParam.new(
                        formal_name="var",
                        formal_type=Entity.logicvar_type,
                        has_default_value=False,
                        decl=T.SynthArgDecl.new().as_entity
                    ).singleton.concat(
                        fd.args.map(lambda p: p.to_formal_param))
                ),
                lambda lfd=T.SynthFunDecl: lfd.args,
                lambda cd=T.ClassDecl:
                    cd.constructor_fields.map(lambda d: d.to_formal_param),
                lambda td=T.NamedTypeDecl:
                    td.decls.filter(lambda d: d.decl.is_a(T.FieldDecl))
                            .map(
                                lambda d:
                                d.decl.cast(T.FieldDecl).to_formal_param
                            ),
                # TODO: Other things can be called
                lambda _: No(T.FormalParam.array)
            )
        )

    @langkit_property(return_type=T.Decl.entity)
    def subdecl_if_generic():
        """
        Return the subdeclaration if Self is a GenericDecl, otherwise return
        itself.
        """
        return Entity.match(
            lambda gc=T.GenericDecl: gc.decl,
            lambda _: Entity
        )

    @langkit_property(return_type=T.Bool, public=True,
                      predicate_error="$Self is not a generic declaration")
    def is_generic():
        """
        Returns wether the Decl is generic.
        """
        return (
            Entity.is_null | Entity.parent.is_a(T.GenericDecl)
            | Entity.cast(T.FunctionType).then(
                lambda ft: ft.origin._.is_generic,
                default_val=False
            )
        )

    @langkit_property(return_type=T.Bool, public=True)
    def return_type_is_instantiated():
        """
        Return True if the return type of this function is instantiated.
        """
        return Entity.cast(T.FunctionType)._.return_type.is_instantiated()

    @langkit_property(return_type=T.Bool, public=True)
    def is_instantiated():
        """
        Return True if Self is an instantiated declaration, meaning that it
        does not use any of its declared generic types.
        """
        return Cond(
            Entity.is_null,
            True,
            Entity.is_a(T.GenericFormalTypeDecl),
            False,
            Entity.parent.cast(T.GenericDecl).then(
                lambda gd:
                gd.generic_formals.all(
                    lambda gf:
                    Not(gd.children_env.get_first(gf.name)
                                       .cast_or_raise(T.TypeDecl)
                                       .matching_type(gf))
                ),
                default_val=True
            )
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="Incorrect number of type_parameter")
    def has_correct_type_param_number(nb_types=T.Int):
        """
        Checks that the parent GenericDecl has nb_types type formals.
        """
        return Entity.is_null | Entity.parent.match(
            lambda gd=T.GenericDecl:
                gd.generic_formal_decls.length == nb_types,
            lambda _: True
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="could not determine callee type")
    def could_infer(generic_type=T.FunctionType.entity):
        """
        Predicate used to verify if we were able to find the type of the
        callee.
        """
        return (
            generic_type.is_null | Not(Entity.is_null)
        )

    @langkit_property(return_type=T.Decl.entity, memoized=True)
    def instantiate_generic_decl(type_params=(T.TypeDecl.entity.array)):
        """
        Create a DynEnvWrapper to instantiate a DynamicEnvironment to use as
        rebindings when creating a new Entity from the current type.
        """
        return If(
            Entity.is_null | type_params.any(lambda t: t.is_null),
            No(T.TypeDecl.entity),
            DynEnvWrapper.new(
                names=Entity.parent.cast(T.GenericDecl).generic_formals_names,
                types=type_params
            ).then(
                lambda dew:
                Entity.get_rebinded_decl(dew.dynenvwrapper_instantiation_env)
            )
        )

    @langkit_property(return_type=T.Decl.entity)
    def get_rebinded_decl(rebindings_env=T.LexicalEnv):
        """
        Create a new Entity from Self using rebindings_env as the new
        environment to handle generics.
        """
        shed_current = Var(Self.parent.node_env.shed_rebindings(Entity.info))
        return If(
            Entity.is_null | Not(Entity.parent.is_a(T.GenericDecl)),
            No(T.Decl.entity),
            T.Decl.entity.new(
                node=Self.parent.cast(T.GenericDecl)._.decl,
                info=T.entity_info.new(
                    md=No(T.Metadata),
                    from_rebound=False,
                    rebindings=shed_current.rebindings.append_rebinding(
                        Self.node_env,
                        rebindings_env
                    )
                )
            )
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="$Self is not a dynamic variable")
    def is_dynvar():
        return Entity.is_null | Entity.is_a(T.DynVarDecl)


@abstract
class Expr(LktNode):
    """
    Base class for expressions. Encompasses regular expressions as well as
    special expressions (grammar expressions, etc).
    """
    expected_type_var = UserField(type=T.LogicVar, public=False)
    actual_type_var = UserField(type=T.LogicVar, public=False)

    generic_func_type_var = UserField(type=T.LogicVar, public=False)

    @langkit_property(return_type=T.Bool)
    def xref_entry_point():
        return Self.parent.match(
            lambda _=T.CastExpr: True,
            lambda _=T.Isa: True,
            lambda _=T.MatchExpr: True,
            lambda _=T.LambdaExpr: Entity.has_context_free_type,
            lambda p=T.BaseDotExpr: (p.prefix == Self),
            lambda p=T.Param:
                (p.value == Self) & Entity.has_context_free_type
                & Not(p.parent.parent.parent.is_a(DeclAnnotation)),
            lambda p=T.CallExpr:
                (p.name == Self) & Entity.has_context_free_type
                & Not(Entity.is_a(T.RefId, T.BaseDotExpr)),
            lambda _: False
        )

    @langkit_property(return_type=T.Equation,
                      kind=AbstractKind.abstract_runtime_check)
    def xlogic_equation():
        """
        Build an equation to solve type and name resolution for logic
        expressions.
        """
        pass

    @langkit_property(return_type=T.Equation,
                      kind=AbstractKind.abstract_runtime_check)
    def xtype_equation():
        """
        Build an equation to solve type and name resolution for type
        referencement.
        """
        pass

    @langkit_property(return_type=T.TypeDecl.entity, public=True,
                      memoized=True, call_memoizable=True)
    def get_type():
        """
        Return the type of this expression.
        """
        return Entity.solve_enclosing_context.then(
            lambda _: Entity.actual_type_var.get_value.cast(T.TypeDecl.entity)
        )

    @langkit_property(return_type=T.TypeDecl.entity, public=True,
                      memoized=True, call_memoizable=True)
    def get_generic_type():
        """
        Return the expected type of this expression.
        """
        return If(
            Entity.parents.find(lambda p: p.generic_type_entry_point)
                          ._.solve_generic_types,
            Entity.generic_func_type_var.get_value.cast(T.TypeDecl.entity),
            No(T.TypeDecl.entity)
        )

    @langkit_property(return_type=T.TypeDecl.entity, public=True,
                      memoized=True, call_memoizable=True)
    def get_expected_type():
        """
        Return the expected type of this expression.
        """
        return If(
            Entity.parents(with_self=False)
            .find(lambda p: p.expected_type_entry_point)
            ._.solve_expected_types,
            Entity.expected_type_var.get_value.cast(T.TypeDecl.entity),
            No(T.TypeDecl.entity)
        )

    @langkit_property(return_type=T.Decl.entity, public=True,
                      memoized=True, call_memoizable=True)
    def referenced_decl():
        """
        Return the declaration referenced by this expression, if applicable,
        else null.

        The property is memoized in order to avoid use the value inside logic
        variables on every redundent call, causing faulty behavior when used
        with rebindings.
        TODO: Do like LAL to avoid memoization for more safety.
        """
        return No(T.Decl.entity)

    @langkit_property(return_type=T.Bool)
    def has_context_free_type():
        """
        Returns True if the expression's actual type can be determined without
        using its expected type.
        """
        return True

    @langkit_property(return_type=T.RefId.entity)
    def get_rightmost_refid():
        """
        Return the right-most RefId of an expression (i.e. the expression
        itself if it already is a RefId or the suffix if it is a DotExpr).
        """
        return Entity.match(
            lambda ri=T.RefId: ri,
            lambda de=T.BaseDotExpr: de.suffix,
            lambda _: No(T.RefId.entity),
        )

    @langkit_property(return_type=T.Equation,
                      kind=AbstractKind.abstract_runtime_check)
    def expected_type_equation():
        pass

    @langkit_property(return_type=T.Equation,
                      dynamic_vars=[(in_logic_call, False)])
    def call_generic_type_equation(name=T.Expr.entity,
                                   args=T.Param.list.entity):
        """
        Build an equation for solving the generic types of children the
        children of call nodes (CallExpr, LogicPropage and LogicPredicate).
        """
        return (
            If(name.xref_entry_point,
               Bind(name.generic_func_type_var, name.get_type),
               name.generic_type_equation)
            & args.logic_all(
                lambda actual:
                # Set the generic type of the argument
                current_param.bind(
                    actual,
                    # Propagate the optionnaly instanciated function type's
                    # argument type.
                    If(name.is_a(T.BaseDotExpr, T.RefId),
                       Bind(
                           actual.value.generic_func_type_var,
                           name.generic_func_type_var,
                           conv_prop=TypeDecl.match_param_get_type,
                       ),
                       LogicTrue())
                )
            )
        )

    @langkit_property(return_type=T.Equation)
    def call_expected_type_equation(name=T.Expr.entity,
                                    args=T.Param.list.entity):
        """
        Compute the expected type of name and expressions in args.
        """
        return (
            # Create a function type returning Any, and with all context free
            # argument types. This is to avoid circular dependency when trying
            # to infer a function type from its argument when the arguments
            # themselves depend on the infered function type.
            # Example: ``f(1, null)`` => (Int, Any) -> Any.
            Bind(
                name.expected_type_var,
                Entity.any_type.create_function_type(
                    args.map(
                        lambda a:
                        If(a.value.xref_entry_point,
                            a.value.get_type,
                            Entity.any_type)
                    )
                )
            )
            & args.filter(lambda p: p.value.xref_entry_point)
                  .logic_all(
                      lambda p: Bind(p.value.expected_type_var,
                                     No(T.TypeDecl.entity))
                  )
        )

    @langkit_property(return_type=T.ParamMatch.array, memoized=True)
    def match_params(formals=T.FormalParam.array, args=T.Param.list.entity):
        """
        Match a function's formals with the arguments of the CallExpr.
        """
        return formals.map(
            lambda i, f:
            # Arg at i if it does not have a name
            If(
                args.at(i)._.name.is_null,
                ParamMatch.new(
                    has_matched=True,
                    actual=args.at(i),
                    formal=f
                ),
                No(T.ParamMatch)
            )._or(
                # Find an argument that has the corresponding name
                args.find(lambda p: p.name._.symbol == f.formal_name)
                .then(
                    lambda arg: ParamMatch.new(
                        has_matched=True,
                        actual=arg,
                        formal=f
                    )
                )
            )._or(
                # No argument was found
                ParamMatch.new(
                    has_matched=Not(f.has_default_value),
                    formal=f
                )
            )
        )

    @langkit_property(return_type=T.Equation,
                      dynamic_vars=[(in_logic_call, False)])
    def xref_call_args_equation(name=T.Expr.entity, args=T.Param.list.entity):
        """
        Build an equation for name and type resolution of calls.
        """
        return args.logic_all(
            lambda actual:
            actual.value.xref_equation
            # Set the expected type of the argument
            & current_param.bind(
                actual,
                Bind(
                    actual.value.expected_type_var,
                    name.actual_type_var,
                    conv_prop=TypeDecl.match_param_get_type,
                )
            )
            # Bind the name of the argument, if any
            & If(
                actual.name.is_null,
                LogicTrue(),
                current_param.bind(
                    actual,
                    Bind(
                        actual.name.ref_var,
                        name.actual_type_var,
                        conv_prop=TypeDecl.match_param_get_decl,
                    )
                )
                & Predicate(
                    LktNode.unmatched_argument,
                    actual.name.ref_var,
                    name.actual_type_var,
                    error_location=actual.name.node
                )
            )
            & If(in_logic_call,
                 Predicate(
                    TypeDecl.matching_logic_type,
                    actual.value.expected_type_var,
                    actual.value.actual_type_var,
                    error_location=actual.value.node
                 ),
                 Predicate(
                    TypeDecl.matching_type,
                    actual.value.expected_type_var,
                    actual.value.actual_type_var,
                    error_location=actual.value.node
                 ))
        )

    @langkit_property(return_type=T.Equation,
                      dynamic_vars=[(in_logic_call, False)])
    def xref_call_equation(name=T.Expr.entity, args=T.Param.list.entity):
        """
        Build an equation to solve type and name resolution for calling
        ``name`` with ``args`` as the arguments.
        CallExprs, LogicPredicates and LogicPropagate are all calls to a given
        callee, but their only common ancestor is Expr, so it is necessary to
        build the equation here.
        """
        return (
            Entity.xref_call_args_equation(name, args)
            # If the name is not an identifier, bind the generic function type
            # to the actual type (The function type should already be
            # instanciated in that case).
            & If(name.is_a(T.BaseDotExpr, T.RefId),
                 LogicTrue(),
                 Bind(name.generic_func_type_var,
                      name.actual_type_var))
            & Predicate(
                TypeDecl.is_valid_call,
                name.actual_type_var,
                Entity.match(
                    lambda c=T.CallExpr: c.args,
                    lambda c=T.LogicPropagate: c.call.args,
                    lambda c=T.LogicPredicate: c.args,
                    lambda _: No(T.Param.list.entity)
                ),
                error_location=Self
            )
            # Verify that the callee is callable
            & Predicate(TypeDecl.is_callable,
                        name.actual_type_var,
                        error_location=name.node)
            & Predicate(Decl.could_infer,
                        name.actual_type_var,
                        name.get_generic_type.cast(T.FunctionType),
                        error_location=name.node)
        )


class LexerDecl(Decl):
    """
    Declaration of a language's lexer.
    """
    syn_name = Field(type=T.DefId)
    rules = Field(type=T.LktNode.list)
    decl_type_name = Property(S("lexer declaration"))

    @langkit_property(return_type=T.env_assoc.array, memoized=True)
    def builtin_decls():
        return Array(['newline', 'indent', 'dedent']).map(
            lambda sym:
            new_env_assoc(
                key=sym,
                value=SyntheticLexerDecl.new(sym=sym),
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


class LexerCaseRule(LktNode):
    """
    Lexer construct to introduce a conditional lexing action.
    """
    expr = Field(type=T.GrammarExpr)
    alts = Field(type=T.BaseLexerCaseRuleAlt.list)


@abstract
class BaseLexerCaseRuleAlt(LktNode):
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


class LexerCaseRuleSend(LktNode):
    """
    Lexer construction used by case alternatives to represent the token to send
    if that alternative was chosen.
    """
    sent = Field(type=T.RefId)
    match_size = Field(type=T.NumLit)


class GrammarDecl(Decl):
    """
    Declaration of a language's grammar.
    """
    syn_name = Field(type=T.DefId)
    rules = Field(type=T.FullDecl.list)

    decl_type_name = Property(S("grammar declaration"))

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


@abstract
class BaseDotExpr(Expr):
    """
    Base class for regular dotted expressions and null-conditional ones.
    """
    prefix = Field(type=T.Expr)
    suffix = Field(type=T.RefId)

    referenced_decl = Property(Entity.suffix.referenced_decl)

    has_context_free_type = Property(
        Entity.prefix.has_context_free_type
        & Not(Entity.suffix.is_being_called)
    )

    @langkit_property(return_type=T.Decl.entity, memoized=True)
    def first_var_in_prefix_env():
        """
        Bind dest's logic variables to the correct declaration depending on
        its prefix(Self).
        """
        return Entity.prefix.match(
            lambda rid=T.RefId:
            If(Entity.is_call_to_super,
               # Look for the function with the same name in the scope of
               # the parent class of self/node and bind it to the suffix's
               # ref_var.
               origin.bind(
                   Entity.parents.find(lambda p: p.is_a(T.FunDecl)),
                   Entity.prefix.get_type.get_super_of_parent
               ),
               rid.referenced_decl._.ref_var_suffix_ref(
                   rid.get_type, Entity.suffix
               )),
            lambda de=T.BaseDotExpr:
                de.referenced_decl._.ref_var_suffix_ref(de.get_type,
                                                        Entity.suffix),
            lambda expr:
                expr.get_type._.type_var_suffix_ref(Entity.suffix)
        )

    @langkit_property(return_type=T.Equation)
    def xtype_equation():
        return (
            Bind(Entity.suffix.ref_var, Entity.first_var_in_prefix_env)
            & Predicate(
                Decl.is_defined,
                Entity.suffix.ref_var,
                error_location=Self.suffix
            )
        )

    @langkit_property(return_type=T.Bool)
    def is_call_to_super():
        """
        Return True if this DotExpr is a reference to super (meaning it matches
        the patterns ``self.super`` or ``node.super``).
        """
        self_decl = Var(Self.node_env.get_first("self").cast(T.SelfDecl))
        node_decl = Var(Self.node_env.get_first("node").cast(T.NodeDecl))
        return (
            Not(self_decl.is_null | node_decl.is_null)
            & (
                (Self.suffix._.symbol == "super")
                & Entity.prefix.cast(T.RefId).first_var_in_env.then(
                    lambda first_var:
                        (first_var == self_decl) | (first_var == node_decl)
                )
            )
        )

    @langkit_property(return_type=T.Equation)
    def generic_type_equation():
        return (
            # Assign the generic type
            Bind(Entity.suffix.generic_func_type_var,
                 Entity.first_var_in_prefix_env._.function_type)
            & Bind(Entity.generic_func_type_var,
                   Entity.suffix.generic_func_type_var)
        )

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        return Bind(Entity.prefix.expected_type_var, No(T.TypeDecl.entity))

    @langkit_property(return_type=T.Equation,
                      dynamic_vars=[(in_logic_call, False)])
    def xref_typing_equation():
        return If(
            Entity.suffix.is_null,
            # The suffix can be null when the tree is incomplete or no
            # suffix was found.
            Bind(Entity.actual_type_var, No(T.TypeDecl.entity)),
            # When there is a suffix:
            Bind(Entity.suffix.ref_var, Entity.first_var_in_prefix_env)
            & Predicate(
                Decl.is_defined,
                Entity.suffix.ref_var,
                error_location=Self.suffix
            )
            & If(
                Entity.suffix.is_being_called,
                Bind(
                    Entity.actual_type_var,
                    Entity.suffix.get_generic_type._.infer_function_type(
                        Entity.get_expected_type.cast(T.FunctionType)
                    )
                ),
                # When the expression is not being called, simply get the
                # of the field. This also keeps parse_fields as Entity if
                # necessary.
                Bind(
                    Entity.suffix.actual_type_var,
                    Entity.first_var_in_prefix_env._.get_suffix_type(
                        Entity.prefix.get_type
                    )
                )
            )
            # DotExpr.actual <- B.actual
            & Bind(Entity.actual_type_var, Entity.suffix.actual_type_var)
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        # When the DotExpr is used as a callee, we need to know its
        # non-instantiated type in order to infer the real type of the function
        # we are calling. Its actual type will be determined in
        # xref_typing_equation.
        return (
            Entity.xref_typing_equation
        )

    @langkit_property(return_type=T.Equation)
    def xlogic_equation():
        return (
            in_logic_call.bind(True, Entity.xref_typing_equation)
        )


class DotExpr(BaseDotExpr):
    """
    Dotted expression.
    """
    pass


class NullCondDottedName(BaseDotExpr):
    """
    Null conditional dotted expression.
    """
    pass


class Id(Expr):
    """
    Identifier.
    """
    token_node = True
    annotations = Annotations(custom_short_image=True)

    @langkit_property(return_type=T.String, external=True, public=True,
                      uses_entity_info=True, uses_envs=False)
    def custom_image():
        """
        Returns the image of this RefId using entity information.
        """
        pass

    @langkit_property(
        external=True,
        uses_envs=False,
        uses_entity_info=False,
        return_type=T.Bool,
    )
    def is_type_name():
        """
        Return whether this identifier refers to a type name.
        """
        pass


class DefId(Id):
    """
    Defining identifier.
    """
    pass


class RefId(Id):
    """
    Reference identifier.
    """
    ref_var = UserField(type=T.LogicVar, public=False)

    @langkit_property(return_type=T.LktNode)
    def from_node():
        """
        Find the limiting node to search in the environment to avoid variables
        that reference themselves or future variables.
        """
        return Self.parents.find(
            lambda p:
            p.parent.is_a(T.BlockExpr, T.MatchBranch, T.LambdaExpr, T.FunDecl)
            | p.parent._.parent.is_a(T.BlockExpr)
        )

    @langkit_property(return_type=T.Decl.entity, memoized=True)
    def first_var_in_env():
        """
        Get the first declaration found for this RefId. This first tries to get
        variables declared before Self.from_node, if no variable was found,
        find a type or function anywhere in the node environment.
        """
        return Entity.node_env.get(
            Entity.symbol, from_node=Self.from_node
        ).find(
            lambda d: Not(Entity.parents.contains(d))
        ).cast(T.BaseValDecl)._or(
            Entity.node_env
            .get_first(Entity.symbol)
            ._.cast(T.Decl).subdecl_if_generic.then(
                lambda d:
                If(d.is_a(T.TypeDecl, T.FunDecl), d, No(T.Decl.entity))
            )
        )

    @langkit_property(return_type=T.Bool)
    def is_being_called():
        """
        Return True if this RefId is used to refer to a function being called.
        """
        return (
            Entity.parent.is_a(T.CallExpr, T.LogicPropagate, T.LogicPredicate)
            | Entity.parent.cast(T.BaseDotExpr).then(
                lambda de:
                (de.suffix == Entity)
                & de.parent.is_a(
                    T.CallExpr, T.LogicPropagate, T.LogicPredicate
                ),
                default_val=False
            )
        )

    @langkit_property(return_type=T.Decl.entity, public=True,
                      memoized=True, call_memoizable=True)
    def referenced_decl():
        return If(Entity.solve_enclosing_context.success,
                  Self.ref_var.get_value.cast_or_raise(T.Decl),
                  No(T.Decl.entity))

    @langkit_property(return_type=T.Equation)
    def xtype_equation():
        first_var = Var(
            Entity.node_env.get_first(Entity.symbol)
                           .cast_or_raise(T.Decl)._.subdecl_if_generic
        )
        return (
            Bind(Entity.ref_var, first_var)
            & If(
                first_var.is_null,
                Predicate(Decl.is_defined, Entity.ref_var,
                          error_location=Self),
                Predicate(Decl.is_type_decl, Entity.ref_var,
                          error_location=Self)
                & Bind(Entity.actual_type_var, No(T.TypeDecl.entity))
            )
        )

    @langkit_property(return_type=T.Equation)
    def generic_type_equation():
        return (
            # Assign the generic type
            Bind(Entity.generic_func_type_var,
                 Entity.first_var_in_env._.function_type)
        )

    @langkit_property(return_type=T.Equation)
    def bind_actual_type_equation(first_var=T.Decl.entity):
        """
        Bind the corresponding type of first_var to the RefId.
        """
        return If(
            Entity.is_being_called & Not(first_var.function_type.is_null),
            # If the ref_id is being called, get the function_type of the Decl
            first_var.then(
                lambda td: If(
                    first_var.is_generic,
                    in_logic_call.bind(False, Bind(
                        Entity.actual_type_var,
                        Entity.get_generic_type.infer_function_type(
                            Entity.get_expected_type.cast(T.FunctionType)
                        )
                    ))
                    & Bind(
                        Entity.generic_func_type_var,
                        Entity.ref_var,
                        conv_prop=TypeDecl.function_type
                    ),
                    Bind(Entity.actual_type_var, td.function_type)
                    & Bind(Entity.generic_func_type_var,
                           Entity.actual_type_var)
                ),
                default_val=LogicFalse()
            ),
            first_var.match(
                lambda _: Bind(Entity.actual_type_var, first_var.match(
                        lambda _=T.TypeDecl: No(T.TypeDecl.entity),
                        lambda etd=T.BaseValDecl: etd.get_type,
                        lambda gd=T.GenericDecl: gd.decl,
                        lambda _:
                        PropertyError(T.TypeDecl.entity, "Unreachable")
                    ))
            )
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        first_var = Var(Entity.first_var_in_env)
        return (
            Bind(Entity.ref_var, first_var)
            & If(first_var.is_null,
                 Bind(Entity.actual_type_var, No(T.TypeDecl.entity))
                 & Predicate(
                     Decl.is_defined,
                     Entity.ref_var,
                     error_location=Self
                 ),
                 Entity.bind_actual_type_equation(first_var))
        )

    # Logic expressions

    @langkit_property(return_type=T.Equation)
    def xlogic_equation():
        return (
            # When used inside LogicExprs, RefIds can only be ``true`` or
            # ``false``. As they are enum values, looking inside the Bool type
            # environment is enough and will avoid using other declarations.
            Bind(Entity.ref_var,
                 Entity.bool_type.children_env.get_first(Entity.symbol))
            & Bind(Entity.actual_type_var, Entity.bool_type)
            & Predicate(Decl.is_defined, Entity.ref_var, error_location=Self)
        )


class ModuleRefId(Id):
    """
    Id referencing a langkit module.
    """
    # TODO: Being able to cross-reference modules seems like a good ability,
    # but for the moment they don't fit in the framework, because they're not
    # decls.


class TokenLit(GrammarExpr):
    """
    Grammar expression for a token literal.
    """
    token_node = True

    @langkit_property(public=True, return_type=T.DecodedStringValue,
                      external=True, uses_envs=False, uses_entity_info=False)
    def denoted_value():
        """
        Return the content of the given token literal node.
        """
        pass


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

    @langkit_property(public=True, return_type=T.DecodedStringValue,
                      external=True, uses_envs=False, uses_entity_info=False)
    def denoted_value():
        """
        Return the content of the given token pattern literal node.
        """
        pass


class TokenPatternConcat(GrammarExpr):
    """
    Grammar expression for the concatenation of two patterns.
    """
    left = Field(type=GrammarExpr)
    right = Field(type=TokenPatternLit)


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
    Grammar ``Or`` expression (disjunctive choice between several grammar
    options).
    """
    sub_exprs = Field(type=T.GrammarExpr.list.list)


class GrammarOpt(GrammarExpr):
    """
    Grammar expression for an optional parsing result.
    """
    expr = Field(type=T.GrammarExpr)


class GrammarOptError(GrammarExpr):
    """
    Grammar expression for an optional parsing result. Missing result creates
    an error, but parsing continues.
    """
    expr = Field(type=T.GrammarExpr)


class GrammarStopCut(GrammarExpr):
    """
    Grammar expression for a StopCut.
    """
    expr = Field(type=T.GrammarExpr)


class GrammarOptGroup(GrammarExpr):
    """
    Grammar expression for a group of optional parsing results.
    """
    expr = Field(type=T.GrammarExpr.list)


class GrammarOptErrorGroup(GrammarExpr):
    """
    Grammar expression for a group of optional parsing results. Failure to
    parse an optional result creates an error, but parsing continues.
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


class GrammarListSep(LktNode):
    """
    Specification for the separator of a list parser.
    """
    token = Field(type=T.GrammarExpr)
    extra = Field(type=T.Id)


class GrammarList(GrammarExpr):
    """
    Grammar expression to parse lists of results. Results can be separated by a
    separator. List can be empty ('*') or not ('+').
    """
    list_type = Field(type=T.TypeRef)
    kind = Field(type=T.ListKind)
    expr = Field(type=T.GrammarExpr)
    sep = Field(type=T.GrammarListSep)


class ListKind(LktNode):
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


@synthetic
class DynEnvWrapper(LktNode):
    """
    Synthetic node to instantiate a DynamicEnvironment for generics.
    """

    names = UserField(T.Symbol.array, public=False)
    types = UserField(T.TypeDecl.entity.array, public=False)

    @lazy_field(return_type=T.LexicalEnv)
    def dynenvwrapper_instantiation_env():
        """
        Instantiate the corresponding LexicalEnv containing the declarations
        with their corresponding name.
        """
        return DynamicLexicalEnv(
            assocs_getter=DynEnvWrapper.instantiation_bindings,
            # TODO: (eng/libadalang/langkit#871) The use of the resolver below
            # is a workaround the fact that we cannot systematically shed
            # rebindings of entries retrieved from a lexical env, as it would
            # break assumptions done in libadalang.
            assoc_resolver=LktNode.shed_rebindings,
            transitive_parent=False
        )

    @langkit_property(return_type=T.inner_env_assoc.array)
    def instantiation_bindings():
        return Entity.names.map(
            lambda i, name:
            T.inner_env_assoc.new(
                key=name,
                value=Entity.types.at(i).node,
                rebindings=Entity.types.at(i).info.rebindings,
                metadata=No(T.Metadata)
            )
        )


@abstract
class TypeDecl(Decl):
    """
    Abstract base class for type declarations.
    """

    @langkit_property(memoized=True)
    def self_decl():
        return SelfDecl.new()

    @langkit_property(memoized=True)
    def node_decl():
        return NodeDecl.new()

    traits = AbstractField(T.TypeRef.list, doc="Traits for this type")

    syn_base_type = AbstractField(T.TypeRef)

    @langkit_property(return_type=T.TypeRef.entity, public=True)
    def base_type():
        """
        Return the base type for this node, if any.
        """
        return Entity.syn_base_type

    @langkit_property(return_type=T.TypeDecl.entity, public=True)
    def base_type_if_entity():
        """
        Return the base type for this node, if any.
        """
        return If(
            Self == Self.entity_type.node,
            Entity.get_entity_node_type,
            Entity
        ).base_type._.referenced_decl

    @langkit_property(return_type=T.Bool,
                      predicate_error="operands of %and and %or must be "
                                      "equations, got $Self")
    def is_equation():
        """
        Predicate used to verify that operands of equation logic operators are
        of equation type.
        """
        return (
            Entity.is_null | (Entity == Self.equation_type)
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="operands of binary logic operators "
                                      "must be of boolean type, got $Self")
    def is_bool():
        """
        Predicate used to verify that operands of boolean logic operators are
        of boolean type.
        """
        return (
            Entity.is_null | (Entity == Self.bool_type)
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="expression must be a string or an "
                                      "array, got $Self")
    def is_string_or_array_type():
        return (
            Entity.is_null | (Entity == Entity.string_type)
            | (Self == Entity.array_type.node)
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="expression must be of type Int or "
                                      "BigInt, got $Self")
    def is_int_type():
        return (
            Entity.is_null | (Entity == Entity.int_type)
            | (Entity == Entity.bigint_type)
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="expression must be of type Int, BigInt "
                                      "or Node, got $Self")
    def is_int_or_node():
        return (
            Entity.is_null
            | Entity.is_int_type
            | Entity.node_env.get_first("RootNode__")
                             ._.cast(T.TypeDecl).is_subtype_or_eq(Entity)
        )

    @langkit_property(return_type=T.TypeDecl.entity.array)
    def base_types(include_self=(T.Bool, False)):
        """
        Return an array containing all subclasses of the type.
        """
        return If(
            include_self,
            Entity.singleton,
            No(T.TypeDecl.entity.array)
        ).concat(
            Entity.base_type.then(
                lambda bt:
                bt.referenced_decl.then(
                    lambda b: b.singleton.concat(b.base_types)
                )
            )
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def get_entity_node_type():
        """
        Get the type parameter used to rebind the Entity type declaration if
        Self is the enity type.
        """
        return Cond(
            Entity.is_null,
            No(T.TypeDecl.entity),
            Self == Entity.entity_type.node,
            Entity.parent.cast(T.GenericDecl)._.children_env.get_first(
                Entity.entity_gen_type.generic_formal_decls.at(0).decl.name
            ).cast(T.TypeDecl).then(
                lambda td: If(
                    td.is_generic,
                    td,
                    td.as_bare_decl.cast_or_raise(T.TypeDecl)
                )
            ),
            Entity
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="$rhs is not a subtype of $Self")
    def is_subtype_or_eq(rhs=T.TypeDecl.entity, allow_entity=(T.Bool, False)):
        """
        Return True if rhs is equal to or is a subtype of Self.
        If allow_entity is True, Entity and/or rhs are entities, get the node
        type.
        """
        return If(
            allow_entity & Not(Entity.is_null | rhs.is_null),
            Entity.get_entity_node_type.is_subtype_or_eq(
                rhs.get_entity_node_type
            ),
            Entity.is_null | rhs.is_null
            | (Entity == rhs) | rhs.base_types.contains(Entity)
        )

    @langkit_property(return_type=T.Bool)
    def is_subtype(other=T.TypeDecl.entity):
        """
        Return true if Self is a subtype of other.
        """
        ignore(other)
        return False

    @langkit_property(return_type=T.TypeDecl.entity)
    def common_ancestor_helper(other_types=T.TypeDecl.entity.array, idx=T.Int,
                               imprecise=T.Bool):
        return If(
            idx == other_types.length - 1,
            Entity.common_ancestor(other_types.at(idx), imprecise),
            other_types.at(idx).then(
                lambda t:
                t.common_ancestor(
                    Entity.common_ancestor_helper(other_types, idx + 1,
                                                  imprecise),
                    imprecise
                ),
            )
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def imprecise_common_ancestor_list(other_types=T.TypeDecl.entity.array):
        """
        Return the nearest common ancestor of Self and other_types, ignoring
        any type that does not share a common ancestor with the rest of the
        types.
        If one of the type is Entity, the result will also be wrapped by
        Entity.
        """
        no_entity_other_types = Var(
            other_types.map(lambda t: t._.get_entity_node_type)
        )
        return Cond(
            other_types.length == 0,
            Entity,
            Entity.is_null,
            no_entity_other_types.find(lambda t: Not(t.is_null))
            ._.imprecise_common_ancestor_list(no_entity_other_types),
            Entity.get_entity_node_type.common_ancestor_helper(
                no_entity_other_types, 0, True
            )
        ).then(
            lambda t:
            If((t.node != t.entity_type.node) & ((Self == t.entity_type.node)
               | other_types.any(lambda ot: ot._.node == t.entity_type.node)),
               t.entity_type.instantiate_generic_decl(t.singleton)
                            .cast(T.TypeDecl),
               t)
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def common_ancestor(other=T.TypeDecl.entity, imprecise=(T.Bool, False)):
        """
        Return the nearest common ancestor between Self and other.
        If imprecise is True, return either type that is non-null.
        If both types are the Entity type, use their wrapped type instead.
        """
        return Cond(
            (Self == Entity.entity_type.node)
            & (other.node == Entity.entity_type.node),
            Entity.get_entity_node_type.common_ancestor(
                other.get_entity_node_type
            ),
            Entity == other,
            Entity,
            No(T.TypeDecl.entity)
        ).then(
            lambda ca: ca,
            default_val=If(
                imprecise,
                If(Entity.is_null, other, Entity),
                No(T.TypeDecl.entity)
            )
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="expected $Self, got $other")
    def commutative_matching_type(other=T.TypeDecl.entity,
                                  allow_common_ancestor=(T.Bool, False)):
        """
        Return true if Self and other are matching type that can be permutated.
        """
        return (
            Entity.is_null | other.is_null
            | Entity.matching_type(other)
            | other.matching_type(Entity)
            | (
                allow_common_ancestor
                & Not(Entity.common_ancestor(other).is_null)
            )
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="could not determine type")
    def could_determine_type():
        """
        Return true if Self and other are matching types.
        """
        return Not(Self.is_null)

    @langkit_property(return_type=T.Bool,
                      predicate_error="expected $Self, got $other")
    def matching_generic_types(other=T.TypeDecl.entity):
        """
        Return true if Self and other are matcing generic types.
        """
        return Entity.parent.cast(T.GenericDecl).then(
            lambda lhs_gen:
            other.parent.cast(T.GenericDecl).then(
                lambda rhs_gen:
                (lhs_gen.decl.node == rhs_gen.decl.node)
                & lhs_gen.generic_formal_decls.all(
                    lambda f:
                    lhs_gen.children_env
                    .get_first(f.decl.name)
                    .cast_or_raise(T.TypeDecl)
                    .matching_type(
                        rhs_gen.children_env.get_first(f.decl.name)
                                            .cast_or_raise(T.TypeDecl)
                    )
                ),
                default_val=False
            ),
            default_val=Self == other.node
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="expected $Self, got $other")
    def matching_type(other=T.TypeDecl.entity):
        """
        Return true if Self and other are matching types.
        """
        return If(
            Entity.is_a(T.AnyTypeDecl) | other.is_a(T.AnyTypeDecl),
            True,
            # If either self or other is null, consider that it is not
            # necessary to test if types match, as there should be an other
            # predicate failing prior to this one.
            Entity.is_null | other.is_null | (
                Entity.match(
                    lambda ft=T.FunctionType:
                        other.cast(T.FunctionType).then(
                            lambda rhs:
                            (ft.args.length == rhs.args.length)
                            & ft.args.all(
                                lambda idx, a:
                                a.matching_type(rhs.args.at(idx))
                            )
                            & ft.return_type
                                .matching_type(rhs.return_type),
                            default_val=False
                        ),
                    lambda _:
                        (Entity == other) | other.is_subtype(Entity)
                        | Entity.matching_generic_types(other)
                )
            )
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="expected $Self, got $other")
    def matching_logic_type(other=T.TypeDecl.entity):
        """
        Return True if the types match or we are expecting an Entity and get a
        LogicVar.
        """
        return (
            Entity.is_null | other.is_null
            | (
                (Self == Entity.entity_type.node)
                & other.matching_type(Self.logicvar_type)
            )
            | Entity.matching_type(other)
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="$Self is not an array")
    def is_of_array_type():
        """
        Predicate used to verify that a type is an array (implements the
        Indexable trait).
        """
        return (
            Entity.is_null
            | (origin.bind(
                Entity,
                Entity.defined_scope.get_first("__call__")
                                    .cast(T.FunDecl)._.owning_type
            ) == Self.indexable_trait.node)
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="$Self is not callable")
    def is_callable():
        """
        Predicate used to emit an error when the type of an expression is not
        callable.
        """
        return Entity.is_null | Entity.is_a(T.FunctionType)

    @langkit_property(return_type=T.Decl.entity,
                      dynamic_vars=[current_param, (in_logic_call, False)])
    def match_param_get_type():
        """
        Return the formal type corresponding to current_param's declaration.
        """
        ft = Var(Entity.cast(T.FunctionType))
        origin = Var(ft._.origin)
        return If(
            origin.is_null,
            No(T.Decl.entity),
            current_param.parent.parent.then(
                lambda ce:
                ce.cast(T.Expr)
                .match_params(
                    origin.formals(in_logic_call),
                    ce.match(
                        lambda c=T.CallExpr: c.args,
                        lambda c=T.LogicPropagate: c.call.args,
                        lambda c=T.LogicPredicate: c.args,
                        lambda _: No(T.Param.list.entity)
                    )
                )
            )
            .find(lambda pm: pm.actual == current_param)
            ._.formal
            .formal_type
        )

    @langkit_property(return_type=T.Decl.entity,
                      dynamic_vars=[current_param, (in_logic_call, False)])
    def match_param_get_decl():
        """
        Return the formal declaration corresponding to current_param.
        """
        ft = Var(Entity.cast(T.FunctionType))
        origin = Var(ft._.origin)
        return If(
            origin.is_null,
            No(T.Decl.entity),
            current_param.parent.parent.cast_or_raise(T.CallExpr).then(
                lambda ce: ce.match_params(origin.formals(in_logic_call),
                                           ce.args)
            ).find(lambda pm: pm.actual == current_param)._.formal.decl
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def lambda_param_get_type(current_largdecl=T.LambdaArgDecl.entity):
        """
        Return the type declaration corresponding to current_largdecl's formal
        type.
        """
        param_idx = Var(
            current_largdecl.parent.parent.cast(T.LambdaExpr).params
            .map(lambda idx, p: If(p == current_largdecl, idx, -1))
            .filter(lambda i: i != -1)
            .at(0)
        )
        return Entity.cast(T.FunctionType).then(
            lambda ft: ft.args.at(param_idx)
        )

    @langkit_property(return_type=T.Bool,
                      dynamic_vars=[(in_logic_call, False)],
                      predicate_error="invalid call, expected $Self")
    def is_valid_call(args=T.Param.list.entity):
        """
        Check wether the call to Self is valid and all formals are paired or
        have a default value.
        """
        matched = Var(
            Entity.cast(T.FunctionType)
            .then(
                lambda ft:
                args.parent.cast(T.Expr).match_params(
                    ft.origin.then(
                        lambda origin: origin.formals(in_logic_call)
                    ),
                    args
                )
            )
        )
        return Entity.cast(T.FunctionType).then(
            lambda ft:
            (
                (matched == No(T.ParamMatch.array))
                & (args.length == ft.args.length)
            )
            # Verify that all arguments of the CallExpr have been matched
            # with a formal.
            | (
                args.all(
                    lambda p: Not(
                            p.name.is_null
                            & matched.find(lambda m: m.actual == p).is_null
                    )
                )
                # Verify that all formals have been matched with an argument
                # of the CallExpr, and that their types match.
                & matched.all(
                    lambda p:
                        Not(p.actual.is_null) | p.formal.has_default_value
                )
            ),
            default_val=True
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def get_return_type():
        """
        Return the return type of the FunctionType.
        """
        return Entity.cast(T.FunctionType)._.return_type

    @langkit_property(return_type=T.TypeDecl.entity)
    def create_function_type(args=T.TypeDecl.entity.array):
        """
        Create a FunctionType, using Self as the return type and args for the
        arguments.
        """
        return (
            Entity.singleton.concat(args)
            .find(lambda t: Not(t.is_null))
            .then(
                lambda t:
                t.function_type_helper(
                    args,
                    Entity.then(lambda e: e, default_val=t.any_type),
                    No(T.Decl.entity)
                )
            )
        )

    @langkit_property(return_type=T.TypeDecl.entity, memoized=True)
    def make_array_type():
        """
        Create a rebinded Array type declaration, using Self as the type
        parameter.
        """
        return (
            Entity._.array_type.instantiate_generic_decl(Entity.singleton)
                               .cast(T.TypeDecl)
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def get_array_content_type():
        """
        Get the type parameter used to rebind the Array type declaration.
        """
        return Cond(
            Entity.is_null,
            No(T.TypeDecl.entity),
            Entity.is_of_array_type,
            # The Indexable[T] trait declares a function "__call__"
            # representing the array indexing. Find that function and get its
            # return type.
            origin.bind(
                Entity,
                Entity.defined_scope.get_first("__call__")
                                    .cast_or_raise(T.FunDecl)
                                    .return_type.referenced_decl
            ),
            No(T.TypeDecl.entity)
        )

    @langkit_property(return_type=T.Decl.entity, dynamic_vars=[origin])
    def get_super_of_parent():
        return (
            Entity._.base_type_if_entity._.defined_scope
                    .get_first(origin.cast(T.FunDecl).name, lookup=LK.flat)
                    .cast(T.Decl)._.subdecl_if_generic
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def basic_trait_from_self():
        """
        Return a rebinded version of BasicTrait, in order to allow for all
        types to have builtins (do, singleton...) in their environment.
        """
        return (
            Entity.basic_trait.instantiate_generic_decl(Entity.singleton)
                              .cast(T.TypeDecl)
        )

    @langkit_property(return_type=T.TypeDecl.entity.array)
    def find_types_that_replace_ty(ty=T.TypeDecl.entity,
                                   origin=T.TypeDecl.entity):
        """
        Traverse ``Entity`` and ``origin`` simultaneously and list all
        all types found in origin that would replace ty in Entity.

        .. code::

            ((Entity[T], Array[Entity[T]]) => T).find_types_that_replace_ty(
                (T),
                ((Entity[A], Array[Entity[B[C]]]) => D)
            ) ==> [A, B[C], D]

        """
        return If(
            (origin == ty) & Not(Entity.is_a(T.AnyTypeDecl)),
            # If origin is equal to ty, it means that Entity is replacing ty
            Entity.singleton,
            # Go through each type of the function
            Entity.cast(T.FunctionType).then(
                lambda ft:
                ft.args.concat(ft.return_type.singleton).then(
                    lambda lhs_types:
                    origin.cast(T.FunctionType).then(
                        lambda oft:
                        oft.args.concat(oft.return_type.singleton).mapcat(
                            lambda idx, ot:
                            lhs_types.at(idx)
                                     ._.find_types_that_replace_ty(ty, ot)
                        )
                    )
                ),
                # Go through each GenericFormalTypeDecl
                default_val=Entity.parent.cast(T.GenericDecl).then(
                    lambda gd:
                    origin.parent.cast(T.GenericDecl).then(
                        lambda ogd:
                        gd.instantiated_generic_formals.mapcat(
                            lambda i, t:
                            t.find_types_that_replace_ty(
                                ty, ogd.instantiated_generic_formals.at(i)
                            )
                        )
                    ),
                    default_val=No(T.TypeDecl.entity.array)
                )
            )
        )

    @langkit_property(return_type=T.LexicalEnv, memoized=True)
    def node_builder_scope():
        """
        Create an environment with the builder() function associated to the
        node type parameter. If the type is not a class, return an empty
        environment.
        """
        return No(T.LexicalEnv)

    @langkit_property(return_type=T.TypeDecl.entity)
    def as_node_builder_type():
        """
        Return the corresponding NodeBuilder type for Entity. If Entity is not
        a class (hence not a Node), return Entity as is.
        """
        return Entity.cast(T.ClassDecl).then(
            lambda _:
            Entity.node_builder_type
            .instantiate_generic_decl(Entity.singleton),
            default_val=Entity
        ).cast(T.TypeDecl)


@synthetic
class AnyTypeDecl(TypeDecl):
    """
    Internal type to represent a type that can be matched with anything.
    """
    traits = NullField()
    syn_name = NullField()
    syn_base_type = NullField()

    full_name = Property(S("[Any]"))
    decl_type_name = Property(S("Any Type"))


@synthetic
class FunctionType(TypeDecl):
    """
    Function type.
    """
    args = UserField(T.TypeDecl.entity.array, public=False)
    return_type = UserField(T.TypeDecl.entity, public=False)

    origin = UserField(T.Decl.entity, public=False)

    syn_name = NullField()
    traits = NullField()
    syn_base_type = NullField()

    full_name = Property(
        S("(").concat(
            S(", ").join(Entity.args.map(lambda t: t.full_name))
        ).concat(S(") -> ")).concat(Entity.return_type.full_name)
    )

    decl_type_name = Property(S("function type"))

    defined_scope = Property(No(T.LexicalEnv))

    @langkit_property(return_type=T.Bool)
    def should_ignore_constructor_arg():
        """
        Return True if the arguments should be ignored.
        """
        return (
            Entity._.origin._.full_decl
                           ._.has_annotation("ignore_constructor_arg")
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="Propagate functions "
                                      "must return entities")
    def returns_entity():
        return (
            Entity.is_null
            | (Entity.return_type.node == Entity.entity_type.node)
        )

    @langkit_property(return_type=T.Bool,
                      predicate_error="Predicate functions "
                                      "must return booleans")
    def returns_bool():
        return (
            Entity.is_null | Entity.return_type.matching_type(Entity.bool_type)
        )


class ClassQualifier(LktNode):
    """
    Whether this generic formal type must be a class.
    """
    enum_node = True
    qualifier = True


class GenericFormalTypeDecl(TypeDecl):
    """
    Declaration of a generic formal type in a generic declaration.
    """
    has_class = Field(T.ClassQualifier)
    syn_name = Field(T.TypeDefId)
    traits = NullField()
    syn_base_type = NullField()

    @langkit_property()
    def decl_type_name():
        return If(
            Self.has_class.as_bool,
            S("generic formal class type declaration"),
            S("generic formal type declaration"),
        )


@abstract
class NamedTypeDecl(TypeDecl):
    """
    Explicit named type declaration.
    """
    decls = AbstractField(type=DeclBlock)

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope():
        return Entity.decls.children_env.then(
            lambda le:
            If(Entity == origin,
               Array([le,
                      Entity.basic_trait_from_self.defined_scope]).env_group(),
               le)
        )


class GenericFormalDeclList(FullDecl.list):
    """
    Comma-separated list of generic formal types.
    """
    pass


class GenericDecl(Decl):
    """
    Generic entity declaration.
    """
    annotations = Annotations(rebindable=True)

    generic_formal_decls = Field(type=T.GenericFormalDeclList)
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

    generic_formals_names = Property(Entity.generic_formal_decls.map(
        lambda gfd: gfd.decl.cast(T.GenericFormalTypeDecl).name
    ))

    decl_type_name = Property(
        S("generic ").concat(Entity.decl.decl_type_name)
    )

    instantiated_generic_formals = Property(Entity.generic_formal_decls.map(
        lambda gfd:
            Entity.children_env.get_first(gfd.decl.name).cast(T.TypeDecl)
    ))


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
    syn_base_type = NullField()

    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("trait declaration"))

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope_as_entity():
        return If(
            Self == Self.node_trait.node,
            Entity.as_bare_decl.instantiate_generic_decl(
                Entity.entity_type
                .as_bare_decl
                .instantiate_generic_decl(
                    Entity.node_env.get_first("T").cast(T.TypeDecl).singleton
                ).cast(T.TypeDecl).singleton
            ).defined_scope,
            Entity.super()
        )


class EnumTypeDecl(NamedTypeDecl):
    """
    Enum type declaration.
    """

    syn_name = Field(type=T.DefId)
    traits = Field(type=T.TypeRef.list)
    syn_base_type = NullField()
    literals = Field(type=T.EnumLitDecl.list)
    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("enum declaration"))

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        handle_children(),
        # Add enum literals to the DeclBlock env
        add_to_env(Entity.literals.map(lambda lit: new_env_assoc(
                key=lit.name,
                value=lit.node,
                dest_env=direct_env(Self.decls.children_env),
        ))),

        # If the enum is marked as @open, add enum literals to the enum type's
        # containing env.
        add_to_env(If(
            Entity.full_decl.has_annotation('open'),
            Entity.literals.map(lambda lit: new_env_assoc(
                key=lit.name,
                value=lit.node,
                dest_env=direct_env(Self.node_env),
            )),
            No(T.env_assoc.array)
        ))
    )


class StructDecl(NamedTypeDecl):
    """
    Declaration for a LK struct.
    """
    syn_name = Field(type=T.DefId)
    traits = Field(type=T.TypeRef.list)
    syn_base_type = NullField()
    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("struct declaration"))

    @langkit_property(return_type=T.FunctionType.entity)
    def function_type_aux():
        """
        Build and return a FunctionType corresponding to the current FunDecl.
        """
        return Entity.function_type_helper(
            Entity.decls.filter(lambda d: d.decl.is_a(T.FieldDecl))
                        .map(
                            lambda a:
                            a.decl.cast(T.FieldDecl).decl_type.referenced_decl
                        ),
            Entity, Entity
        )

    @langkit_property(return_type=T.LexicalEnv)
    def entity_scope():
        """
        If Self is the Entity struct, add the scope defined by the type
        parameter.
        """
        return If(
            Self == Self.entity_type.node,
            Entity.parent.cast(T.GenericDecl)._.children_env.get_first(
                Entity.entity_gen_type.generic_formal_decls.at(0).decl.name
            ).cast(T.TypeDecl).then(
                lambda t:
                origin.bind(No(T.TypeDecl.entity), t.defined_scope_as_entity)
            ),
            No(T.LexicalEnv)
        )

    @langkit_property(return_type=T.LexicalEnv, memoized=True)
    def update_func_env():
        """
        Return a LexicalEnv containing a synthetic declaration of the
        ``update`` function for the StructDecl.
        """
        return DynEnvWrapper.new(
            names=["update"],
            types=T.SynthFunDecl.new(
                args=Entity.decls
                .filter(lambda d: d.decl.is_a(T.FieldDecl))
                .map(
                    lambda a:
                    FormalParam.new(
                        formal_name=a.decl.name,
                        formal_type=a.decl.cast(T.FieldDecl).get_type,
                        has_default_value=True,
                        decl=T.SynthArgDecl.new().as_entity
                    ),
                ),
                return_type=Entity
            ).as_bare_entity.function_type_aux.cast(T.TypeDecl).singleton
        ).then(lambda dew: dew.dynenvwrapper_instantiation_env)

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope():
        return Entity.decls.children_env.singleton.concat(
            Entity.traits.map(
                lambda t: t.referenced_decl._.defined_scope
            )
        ).concat([
            Entity.entity_scope,
            Entity.update_func_env
        ]).env_group().then(
            lambda le:
            If(Entity == origin,
               Array([le,
                      Entity.basic_trait_from_self.defined_scope]).env_group(),
               le)
        )


@abstract
class BasicClassDecl(NamedTypeDecl):
    """
    Common ancestor for declarations of regular classes and enum classes.
    """
    syn_name = Field(type=T.DefId)
    syn_base_type = Field(type=T.TypeRef)
    traits = Field(type=T.TypeRef.list)

    @langkit_property(return_type=T.Bool)
    def is_subtype(other=T.TypeDecl.entity):
        return Not(Entity.base_types.find(lambda b: b == other).is_null)

    @langkit_property(return_type=T.LexicalEnv, memoized=True,
                      call_memoizable=True, dynamic_vars=[origin])
    def defined_scope():
        return Array([
            Entity.decls.children_env,
            Entity.base_type._.referenced_decl.cast(T.NamedTypeDecl)
                                              ._.defined_scope
        ]).concat(
            Entity.traits.map(
                lambda t: t.referenced_decl._.defined_scope
            )
        ).env_group().then(
            lambda le:
            If(Entity == origin,
               Array([le,
                      Entity.basic_trait_from_self.defined_scope]).env_group(),
               le)
        )

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope_as_entity():
        entity = If(
            Self == Self.astlist_type.node,
            Entity.as_bare_decl.instantiate_generic_decl(
                Entity.entity_type
                .as_bare_decl
                .instantiate_generic_decl(
                    Entity.node_env.get_first("T").cast(T.TypeDecl).singleton
                ).cast(T.TypeDecl).singleton,
            ).cast(T.BasicClassDecl),
            Entity
        )
        return Array([
            entity.decls.children_env,
            entity.base_type._.referenced_decl.cast(T.NamedTypeDecl)
                                              ._.defined_scope_as_entity
        ]).concat(
            entity.traits.map(
                lambda t: t.referenced_decl._.defined_scope_as_entity
            )
        ).env_group().then(
            lambda le:
            If(Entity == origin,
               Array([le,
                      entity.basic_trait_from_self.defined_scope]).env_group(),
               le)
        )

    @langkit_property(return_type=T.TypeDecl.entity)
    def common_ancestor(other=T.TypeDecl.entity, imprecise=(T.Bool, False)):
        other_bases = Var(other._.base_types(True))
        return If(
            other.is_null,
            No(T.TypeDecl.entity),
            Entity._.base_types(True).find(lambda b: other_bases.contains(b))
        ).then(
            lambda ca: ca,
            default_val=If(imprecise, If(Entity.is_null, other, Entity),
                           No(T.TypeDecl.entity))
        )


class ClassDecl(BasicClassDecl):
    """
    Declaration for a LK class. This only cover node classes for the moment,
    but might be extended to support regular classes in the future.
    """
    decls = Field(type=DeclBlock)

    decl_type_name = Property(S("class declaration"))

    @langkit_property(return_type=T.FieldDecl.entity.array)
    def constructor_fields():
        """
        Return a list of all fields that are necessaru in the constructor of
        the class.
        """
        return Entity.base_types(True).mapcat(
            lambda base:
            base.cast(T.ClassDecl).decls.filtermap(
                lambda d: d.decl.cast(T.FieldDecl),
                lambda d:
                d.decl.is_a(T.FieldDecl)
                & If(
                    d.has_annotation("parse_field"),
                    Not(d.has_annotation("abstract"))
                    & Not(d.has_annotation("null_field")),
                    True
                )
            )
        )

    @langkit_property(return_type=T.LexicalEnv, memoized=True)
    def node_builder_scope():
        return If(
            Entity.full_decl.has_annotation("synthetic"),
            DynEnvWrapper.new(
                names=["builder"],
                types=T.SynthFunDecl.new(
                    args=Entity.constructor_fields.map(
                        lambda a:
                        FormalParam.new(
                            formal_name=a.name,
                            formal_type=a.get_type.as_node_builder_type,
                            has_default_value=False,
                            decl=T.SynthArgDecl.new().as_entity
                        )
                    ),
                    return_type=Entity.as_node_builder_type
                ).as_bare_entity
                .function_type_aux
                .cast(T.TypeDecl).singleton
            ).then(lambda dew: dew.dynenvwrapper_instantiation_env),
            No(T.LexicalEnv)
        )

    @langkit_property(return_type=T.FunctionType.entity)
    def function_type_aux():
        """
        Build and return a FunctionType corresponding to the constructor of
        this current ClassDecl.
        """
        return Entity.function_type_helper(
            Entity.constructor_fields.map(
                lambda f: f.decl_type.referenced_decl
            ),
            Entity, Entity
        )


class EnvSpecDecl(Decl):
    """
    Env spec declaration.

    Each node type can have one or no env spec. Env specs contains only a list
    of env actions.
    """
    syn_name = Field(type=T.DefId)
    actions = Field(type=T.CallExpr.list)

    owning_type = Property(
        Self.parents.find(lambda t: t.is_a(T.TypeDecl)).cast(T.TypeDecl)
    )

    env_spec = EnvSpec(
        add_env(),
        # Add node & self if there is an owning type
        add_to_env(Self.owning_type.then(
            lambda ot: [new_env_assoc("self", ot.self_decl),
                        new_env_assoc("node", ot.node_decl)]
        )),
    )

    decl_type_name = Property(S("env spec declaration"))

    xref_entry_point = Property(True)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return Entity.actions.logic_all(
            lambda action:
                action.xref_equation
                & Bind(action.expected_type_var,
                       Entity.get_builtin_type("EnvAction"))
                & Predicate(
                    TypeDecl.matching_type,
                    action.expected_type_var,
                    action.actual_type_var,
                    error_location=action.node
                )
        )


class EnumClassAltDecl(TypeDecl):
    """
    Alternative for an enum class decl.
    """
    syn_name = Field(T.DefId)
    traits = NullField()
    syn_base_type = NullField()

    # Empty env spec: alts are added as part of EnumClassDecl's env_spec
    env_spec = EnvSpec()

    decl_type_name = Property(S("enum class alt declaration"))

    @langkit_property(return_type=T.Bool)
    def is_subtype(other=T.TypeDecl.entity):
        return Entity.parent_type.is_subtype(other)

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope():
        return Entity.parent_type.defined_scope

    @langkit_property(return_type=T.TypeDecl.entity)
    def parent_type():
        """
        Return the parent EnumTypeDecl.
        """
        return (
            Entity.parent.parent.parent.parent.cast_or_raise(T.TypeDecl.entity)
        )

    @langkit_property(return_type=T.TypeDecl.entity.array)
    def base_types(include_self=(T.Bool, False)):
        """
        Return an array containing all subclasses of the type.
        """
        return If(
            include_self,
            Entity.cast(T.TypeDecl).singleton,
            No(T.TypeDecl.entity.array)
        ).concat(Entity.parent_type.base_types(True))


class EnumClassCase(LktNode):
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
        add_to_env(
            Entity.alts.map(
                lambda alt:
                new_env_assoc(
                    key=alt.name,
                    value=alt.node,
                    dest_env=direct_env(Self.decls.children_env),
                )
            ).concat(
                If(Entity.full_decl.has_annotation("qualifier"),
                   [new_env_assoc(
                       "as_bool",
                       Entity.function_type_helper(
                           Array([], T.TypeDecl.entity),
                           Self.bool_type,
                           No(T.Decl.entity)
                       ).node,
                       dest_env=direct_env(Self.decls.children_env)
                   )],
                   No(T.env_assoc.array))
            )
        )
    )


@abstract
class BaseValDecl(Decl):
    """
    Abstract class for named values declarations, such as arguments, local
    value bindings, fields, etc.
    """

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope():
        return Entity.get_type.then(lambda t: origin.bind(t, t.defined_scope))


@synthetic
class SelfDecl(BaseValDecl):
    """
    Synthetic declaration for the implicit "self" variable available in
    properties.
    """

    syn_name = NullField()
    name = Property('self')

    decl_type_name = Property(S("self declaration"))

    owning_type = Property(
        Entity.parents.find(lambda t: t.is_a(T.TypeDecl)).cast(T.TypeDecl)
    )


@synthetic
class NodeDecl(BaseValDecl):
    """
    Synthetic declaration for the implicit "node" variable available in
    properties.
    """

    syn_name = NullField()
    name = Property('node')

    decl_type_name = Property(S("node declaration"))

    owning_type = Property(
        Entity.parents.find(lambda t: t.is_a(T.TypeDecl)).cast(T.TypeDecl)
    )


@abstract
class UserValDecl(BaseValDecl):
    """
    Class for user declared val declarations (not synthetic).
    """

    xref_entry_point = Property(True)


class FunDecl(UserValDecl):
    """
    Function declaration.
    """
    syn_name = Field(type=T.DefId)
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

    @langkit_property(return_type=T.Bool, public=True)
    def is_dynamic_combiner():
        """
        When this property is used as a a combinder inside an NPropagate
        equation, return wether it expects a dynamic number of arguments.
        """
        return Entity.args.at(0)._.get_type.node == Entity.array_type.node

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return If(
            Entity.body.is_null,
            LogicTrue(),
            Entity.body.xref_equation
            & Bind(Entity.body.expected_type_var,
                   Entity.return_type.referenced_decl)
            & Predicate(
                TypeDecl.matching_type,
                Entity.body.expected_type_var,
                Entity.body.actual_type_var,
                error_location=Self.body
            )
        )

    @langkit_property(return_type=T.FunctionType.entity)
    def function_type_aux():
        """
        Build and return a FunctionType corresponding to the current FunDecl.
        """
        return Entity.function_type_helper(
            Entity.args.map(lambda a: a.get_type),
            Entity.return_type.referenced_decl,
            Entity
        )


class EnumLitDecl(UserValDecl):
    """
    Enum literal declaration.
    """
    syn_name = Field(type=T.DefId)

    # Empty env spec: enum lits are added as part of EnumTypeDecl's env_spec
    env_spec = EnvSpec()

    decl_type_name = Property(S("enum literal declaration"))

    xref_entry_point = Property(False)

    @langkit_property(return_type=T.LexicalEnv, dynamic_vars=[origin])
    def defined_scope():
        return Entity.parent_type.basic_trait_from_self.defined_scope

    @langkit_property(return_type=T.TypeDecl.entity)
    def parent_type():
        """
        Return the parent EnumTypeDecl.
        """
        return Entity.parent.parent.cast_or_raise(T.TypeDecl.entity)


@abstract
class ExplicitlyTypedDecl(UserValDecl):
    """
    Subset of user declared value declarations for values that have a type that
    can be syntactically annotated by the user.
    """
    decl_type = AbstractField(type=T.TypeRef)


@abstract
class ComponentDecl(ExplicitlyTypedDecl):
    """
    Subset of explicitly typed declarations for value declarations that:

    1. Have an optional default value.
    2. Are part of a bigger declaration that can be referred to via a call
       expression (either a type or a function).
    """
    default_val = AbstractField(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return If(
            Entity.default_val.is_null,
            LogicTrue(),
            Entity.default_val.xref_equation
            & Bind(Entity.default_val.expected_type_var,
                   Entity.decl_type._.referenced_decl)
            & Predicate(
                TypeDecl.matching_type,
                Entity.default_val.expected_type_var,
                Entity.default_val.actual_type_var,
                error_location=Self.default_val
            )
        )

    @langkit_property(return_type=T.FormalParam)
    def to_formal_param():
        """
        Create a FormalParam from the current ComponentDecl.
        """
        return FormalParam.new(
            formal_name=Entity.syn_name.symbol,
            formal_type=Entity.decl_type.referenced_decl,
            has_default_value=(
                Not(Entity.default_val.is_null)
                | Entity.full_decl._.has_annotation("nullable")
            ),
            decl=Entity
        )


class FunArgDecl(ComponentDecl):
    """
    Function argument declaration.
    """
    decl_annotations = Field(type=T.DeclAnnotation.list)
    syn_name = Field(type=T.DefId)
    decl_type = Field(type=T.TypeRef)
    default_val = Field(type=T.Expr)
    decl_type_name = Property(S("fun arg declaration"))


class LambdaArgDecl(ComponentDecl):
    """
    Function argument declaration.
    """
    syn_name = Field(type=T.DefId)
    decl_type = Field(type=T.TypeRef)
    default_val = Field(type=T.Expr)

    type_var = UserField(type=T.LogicVar, public=False)

    decl_type_name = Property(S("lambda arg declaration"))


class FieldDecl(ComponentDecl):
    """
    Field declaration.
    """
    syn_name = Field(type=T.DefId)
    decl_type = Field(type=T.TypeRef)
    default_val = Field(type=T.Expr)

    decl_type_name = Property(S("field declaration"))

    owning_type = Property(
        Self.parents.find(lambda t: t.is_a(T.TypeDecl)).cast(T.TypeDecl)
    )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name, Self),
        add_env(),
        add_to_env(
            If(Entity.full_decl.has_annotation("lazy"),
               Self.owning_type.then(
                   lambda ot: [new_env_assoc("node", ot.node_decl)]
               ),
               No(T.env_assoc.array))
        )
    )

    @langkit_property(return_type=T.FunctionType.entity)
    def lazy_field_function_type():
        """
        Lazy fields can be seen as memoized properties that do not take
        arguments. Return a function type corresponding to that supposed
        property.
        """
        return Entity.function_type_helper(
            No(T.TypeDecl.entity.array),
            Entity.get_type,
            Entity
        )


@abstract
class TypeRef(LktNode):
    """
    Base class for a reference to a type.
    """

    xref_entry_point = Property(
        Not(Self.parent.is_a(T.TypeRef)
            | Self.parent.parent.is_a(T.TypeRef)
            | Self.parent.parent.is_a(T.GenericInstantiation)),
        doc="""
        Designates entities that are entry point for the xref solving
        infrastructure. If this returns true, then nameres_diagnostics can be
        called on it.
        """
    )

    type_var = UserField(type=T.LogicVar, public=False)

    @langkit_property(return_type=T.TypeDecl.entity, public=True,
                      memoized=True, call_memoizable=True)
    def referenced_decl():
        """
        Returns the referenced type declaration.
        """
        return If(Entity.solve_enclosing_context.success,
                  Self.type_var.get_value.cast_or_raise(T.TypeDecl),
                  No(T.TypeDecl.entity))


class SimpleTypeRef(TypeRef):
    """
    Simple reference to a type.
    """
    type_name = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.type_name.xtype_equation
            & Entity.type_name.match(
                lambda ri=T.RefId: Bind(Entity.type_var, ri.ref_var),
                lambda de=T.BaseDotExpr:
                    Bind(Entity.type_var, de.suffix.ref_var),
                lambda _: PropertyError(T.Equation, "Unreachable")
            )
        )


class DefaultListTypeRef(TypeRef):
    """
    "list" type reference in parsers.
    """

    token_node = True


class GenericTypeRef(TypeRef):
    """
    Reference to a generic type.
    """
    type_name = Field(type=T.Expr)
    params = Field(type=T.TypeRef.list)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.type_name.xtype_equation
            & Entity.params.logic_all(lambda p: p.xref_equation)
            & Entity.type_name.get_rightmost_refid.then(
                lambda ri:
                    NPropagate(
                        Entity.type_var,
                        Decl.instantiate_generic_decl,
                        ri.ref_var.singleton.concat(Entity.params.map(
                            lambda p: p.type_var
                        ))
                    ) & Predicate(
                        Decl.is_generic,
                        ri.ref_var,
                        error_location=ri.node
                    ) & Predicate(
                        Decl.has_correct_type_param_number,
                        ri.ref_var,
                        Entity.params.length,
                        error_location=Self.params
                    ),
                default_val=LogicFalse()
            )
        )


class FunctionTypeRef(TypeRef):
    """
    Reference to a function type.
    """
    args_types = Field(T.TypeRef.list)
    return_type = Field(T.TypeRef)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.args_types.logic_all(lambda a: a.xref_equation)
            & Entity.return_type.xref_equation
            & NPropagate(
                Entity.type_var,
                TypeDecl.create_function_type,
                Entity.return_type.type_var.singleton.concat(
                    Entity.args_types.map(lambda tr: tr.type_var)
                )
            )
        )


class ArrayLiteral(Expr):
    """
    Literal for an array value.
    """
    exprs = Field(type=T.Expr.list)
    element_type = Field(type=T.TypeRef)

    expected_exprs_type_var = UserField(type=T.LogicVar, public=False)

    actual_element_type = UserField(type=T.LogicVar, public=False)

    has_context_free_type = Property(
        Entity.exprs.any(lambda e: e.has_context_free_type)
    )

    @langkit_property(return_type=T.Equation)
    def expected_exprs_type_equation():
        logic_vars = Var(
            Entity.exprs
            .filter(lambda expr: expr.has_context_free_type)
            .map(lambda expr: expr.actual_type_var)
        )
        # Compute the expected type for the elements using the context free
        # elements or the expected type from the context when no elements
        # are context free.
        return Cond(
            logic_vars.length != 0,
            NPropagate(
                Entity.expected_exprs_type_var,
                TypeDecl.imprecise_common_ancestor_list,
                logic_vars
            ),
            Entity.xref_entry_point,
            Bind(
                Entity.expected_exprs_type_var,
                Entity.get_expected_type.get_array_content_type
            ),
            Bind(
                Entity.expected_exprs_type_var,
                Entity.expected_type_var,
                conv_prop=TypeDecl.get_array_content_type
            )
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expected_exprs_type_equation
            & If(
                Entity.element_type.is_null,
                If(Entity.exprs.length == 0,
                   Bind(Entity.actual_element_type, No(T.TypeDecl.entity))
                   & Predicate(TypeDecl.could_determine_type,
                               Entity.actual_element_type,
                               error_location=Self),
                   NPropagate(
                       Entity.actual_element_type,
                       TypeDecl.imprecise_common_ancestor_list,
                       Entity.exprs.map(lambda expr: expr.actual_type_var)
                   )),
                Bind(Entity.actual_element_type,
                     Entity.element_type.referenced_decl)
            )
            # Set the type of the array
            & Bind(
                Entity.actual_type_var,
                Entity.actual_element_type,
                conv_prop=TypeDecl.make_array_type
            )
            & Entity.exprs.logic_all(
                lambda expr:
                # Type the elements
                expr.xref_equation
                # Set the expected types of elements
                & Bind(
                    expr.expected_type_var,
                    Entity.expected_exprs_type_var
                )
                # Verify that the types of elements all match
                & Predicate(
                    TypeDecl.matching_type,
                    expr.expected_type_var,
                    expr.actual_type_var,
                    error_location=expr.node
                )
            )
        )


class NotExpr(Expr):
    """
    Boolean negation expression.
    """
    expr = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expr.xref_equation
            & Bind(Entity.actual_type_var, Entity.bool_type)
            & Bind(Entity.expr.expected_type_var, Entity.bool_type)
            & Predicate(
                TypeDecl.matching_type,
                Entity.expr.expected_type_var,
                Entity.expr.actual_type_var,
                error_location=Self
            )
        )


class ExcludesNull(LktNode):
    """
    Whether the containing cast expression will raise on null cast result or
    not.
    """
    enum_node = True
    qualifier = True


class KeepExpr(Expr):
    """
    Keep expression.
    """
    expr = Field(type=T.Expr)
    keep_type = Field(type=T.TypeRef)

    array_element_type = UserField(type=T.LogicVar, public=False)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expr.xref_equation
            & Entity.keep_type.xref_equation
            & NPropagate(
                Entity.actual_type_var,
                TypeDecl.get_keep_type,
                Entity.expr.actual_type_var,
                Entity.keep_type.type_var
            )
            & Bind(
                Entity.expr.expected_type_var,
                No(T.TypeDecl.entity)
            )
            & Predicate(
                TypeDecl.is_of_array_type,
                Entity.expr.actual_type_var,
                error_location=Self.expr
            )
            & Bind(
                Entity.array_element_type,
                Entity.expr.actual_type_var,
                conv_prop=TypeDecl.get_array_content_type
            )
            & Predicate(
                TypeDecl.is_subtype_or_eq,
                Entity.array_element_type,
                Entity.keep_type.type_var,
                True,
                error_location=Self.keep_type
            )
        )


class CastExpr(Expr):
    """
    Cast expression.
    """
    expr = Field(type=T.Expr)
    excludes_null = Field(type=T.ExcludesNull)
    dest_type = Field(type=T.TypeRef)

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        return Bind(Entity.expr.expected_type_var, No(T.TypeDecl.entity))

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.dest_type.xref_equation
            & Bind(
                Entity.actual_type_var,
                Entity.expr.get_type.get_cast_type(
                    Entity.dest_type.referenced_decl
                )
            )
        )


class IsaList(TypeRef.list):
    """
    Pipe-separated list of type references.

    This is used to represent the accepted types in an ``Isa`` expression.
    """
    pass


class Isa(Expr):
    """
    Isa expression.
    """
    expr = Field(type=T.Expr)
    dest_type = Field(type=T.IsaList)

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        return Bind(Entity.expr.expected_type_var, No(T.TypeDecl.entity))

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return Bind(Entity.actual_type_var, Entity.bool_type)

    has_context_free_type = Property(True)


class AnyOfList(Expr.list):
    """
    Pipe-separated list of expressions.

    This is used to represent the "values" operand of an ``AnyOf`` expression.
    """
    pass


class AnyOf(Expr):
    """
    "Any of" expression.
    """
    expr = Field(type=T.Expr)
    values = Field(type=T.AnyOfList)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expr.xref_equation
            & Bind(Entity.actual_type_var, Entity.bool_type)
            & NPropagate(
                Entity.expr.expected_type_var,
                TypeDecl.imprecise_common_ancestor_list,
                Entity.expr.singleton.concat(Entity.values.as_array).filtermap(
                    lambda e: e.actual_type_var,
                    lambda e: e.has_context_free_type,
                )
            )
            & Entity.values.logic_all(
                lambda v:
                    v.xref_equation
                    & Bind(v.expected_type_var, Entity.expr.actual_type_var)
                    & Predicate(
                        TypeDecl.commutative_matching_type,
                        v.expected_type_var,
                        v.actual_type_var,
                        error_location=v.node
                    )
            )
        )

    has_context_free_type = Property(True)


class DeclAnnotation(LktNode):
    """
    Compile time annotation attached to a declaration.
    """
    name = Field(type=T.Id)
    params = Field(type=T.DeclAnnotationParams)

    xref_entry_point = Property(
        Entity.params.then(lambda p: p.params.length != 0)
    )

    @langkit_property(return_type=T.Equation)
    def with_dynvars_equation():
        """
        Build an equation for solving type and name resolution of the
        'with_dynvars' annotation.
        """
        return Entity.params._.params.logic_all(
            lambda p:
                p.xref_equation
                & Cond(
                    Not(p.name.is_null),
                    Predicate(
                        Decl.is_dynvar,
                        p.name.ref_var,
                        error_location=p.node
                    ),
                    p.value.is_a(T.RefId),
                    Predicate(
                        Decl.is_dynvar,
                        p.value.cast(T.RefId).ref_var,
                        error_location=p.node
                    ),
                    LogicTrue()
                )
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        """
        Only create an equation for annotations that really require typing and
        name resolution (such as @with_dynvars).
        """
        return Cond(
            Entity.name.symbol == "with_dynvars",
            Entity.with_dynvars_equation,
            LogicTrue()
        )


class DeclAnnotationParams(LktNode):
    """
    List of arguments for an annotation with a call syntax. This intermediate
    node is necessary in order to determine after parsing whether there is no
    param list, or if the list is empty.
    """
    params = Field(type=T.Param.list)


class Param(LktNode):
    """
    Parameter for function calls or for annotations.
    """
    name = Field(type=T.RefId)
    value = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        # Param's exprs expected type can only be obtained once the function
        # type has been infered. This must be done separately from the callee's
        # expected type.
        return If(
            Entity.value.has_context_free_type,
            Bind(Entity.value.expected_type_var, No(T.TypeDecl.entity)),
            LogicTrue()
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.value.xref_equation
            & Entity.name.then(
                lambda n: n.xref_equation,
                default_val=LogicTrue()
            )
        )


class ParenExpr(Expr):
    """
    Parenthesized expression.
    """
    expr = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        return Bind(Entity.expr.expected_type_var, Entity.get_expected_type)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expr.xref_equation
            & Bind(Entity.actual_type_var, Entity.expr.actual_type_var)
            & If(Entity.xref_entry_point,
                 Bind(Entity.expr.expected_type_var, Entity.get_expected_type),
                 Bind(Entity.expr.expected_type_var, Entity.expected_type_var))
        )

    has_context_free_type = Property(Entity.expr.has_context_free_type)


class FormalParam(Struct):
    """
    Represent all the information of a formal parameter. Note that formal_name
    can (and will) be null for formals of function types.
    """
    formal_name = UserField(type=T.Symbol)
    formal_type = UserField(type=T.TypeDecl.entity)
    has_default_value = UserField(type=T.Bool)
    accept_logical_var = UserField(type=T.Bool, default_value=False)
    decl = UserField(type=T.Decl.entity)


@abstract
class BaseCallExpr(Expr):
    """
    Base class for expressions that are syntactically call-like.
    """
    name = Field(type=T.Expr)
    args = Field(type=T.Param.list)

    @langkit_property(return_type=T.Equation)
    def generic_type_equation():
        return Entity.call_generic_type_equation(Entity.name, Entity.args)

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        return Entity.call_expected_type_equation(Entity.name, Entity.args)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.name.xref_equation
            # Verify that all arguments match with formals are valid
            & Entity.xref_call_equation(Entity.name, Entity.args)
            & Bind(Entity.actual_type_var, Entity.name.actual_type_var,
                   conv_prop=FunctionType.get_return_type)
        )

    @langkit_property(return_type=T.Equation)
    def xlogic_unknown():
        """
        Build an equation that emits a diagnostic for when the name of the name
        tries to call an unknown logic function.
        """
        return (
            Bind(Entity.name.actual_type_var, No(T.TypeDecl.entity))
            & Predicate(
                Decl.is_defined,
                Entity.name.actual_type_var,
                error_location=Self.name
            )
            & Entity.args.logic_all(
                lambda a:
                a.xref_equation
                & Bind(a.value.expected_type_var, No(T.TypeDecl.entity))
            )
            & Bind(Entity.name.actual_type_var, No(T.TypeDecl.entity))
        )

    @langkit_property(return_type=T.Equation)
    def xlogic_any_all():
        return (
            Bind(Entity.actual_type_var, Entity.equation_type)
            & Bind(Entity.name.actual_type_var, No(T.TypeDecl.entity))
            & Entity.args.logic_all(
                lambda a:
                a.xref_equation
                & Bind(a.value.expected_type_var, Entity.equation_type)
                & Predicate(
                    TypeDecl.matching_type,
                    a.value.expected_type_var,
                    a.value.actual_type_var,
                    error_location=a.node
                )
            )
        )

    @langkit_property(return_type=T.Equation)
    def xlogic_equation():
        """
        Called when a CallExpr is used inside a LogicExpr.
        """
        return Entity.name.cast(T.RefId).then(
            lambda ri:
            Cond(
                (ri.symbol == "all") | (ri.symbol == "any"),
                Entity.xlogic_any_all,
                ri.symbol == "domain",
                Entity.xref_equation,
                Entity.xlogic_unknown
            ),
            default_val=Entity.xlogic_unknown
        )


@abstract
class LogicCallExpr(BaseCallExpr):
    """
    Base class for logic call expresions, of the form::

        name%(args)
    """
    pass


class CallExpr(BaseCallExpr):
    """
    Call expression.
    """
    pass


class SubscriptExpr(Expr):
    """
    Array subscript expression.
    """
    prefix = Field(type=T.Expr)
    index = Field(type=T.Expr)

    has_context_free_type = Property(Entity.prefix.has_context_free_type)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.prefix.xref_equation
            & Entity.index.xref_equation
            & Bind(
                Entity.actual_type_var,
                Entity.prefix.actual_type_var,
                conv_prop=TypeDecl.get_array_content_type
            )
            # The index should be an integer
            & Bind(
                Entity.index.expected_type_var,
                Entity.int_type
            )
            & Cond(
                Not(Entity.xref_entry_point),
                Bind(
                    Entity.prefix.expected_type_var,
                    Entity.expected_type_var,
                    conv_prop=TypeDecl.make_array_type
                ),
                Entity.prefix.has_context_free_type,
                Bind(
                    Entity.prefix.expected_type_var,
                    No(T.TypeDecl.entity)
                ),
                Bind(
                    Entity.prefix.expected_type_var,
                    Entity.get_expected_type._.make_array_type
                ),
            )
            & Predicate(
                TypeDecl.is_int_type,
                Entity.index.actual_type_var,
                error_location=Self.index
            )
            & Predicate(
                TypeDecl.is_of_array_type,
                Entity.prefix.actual_type_var,
                error_location=Self.prefix
            )
        )


class NullCondSubscriptExpr(SubscriptExpr):
    """
    Null conditional subscript expression.
    """
    pass


class GenericInstantiation(Expr):
    """
    Generic instantiation.
    """
    name = Field(type=T.Expr)
    args = Field(type=T.TypeRef.list)

    rebinded_var = UserField(type=T.LogicVar, public=False)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            # The name cannot be an entry point as we need to run predicates on
            # its referenced declaration.
            Entity.name.xref_equation
            # TODO (eng/libadalang/langkit#796): Capturing call-site values is
            # not yet possible, so we need to include the equations of the type
            # references in order to rebind the declaration referenced by
            # self.name.
            & Entity.args.logic_all(lambda a: a.xref_equation)
            & Entity.name.get_rightmost_refid.then(
                lambda ri:
                NPropagate(
                    Entity.rebinded_var,
                    Decl.instantiate_generic_decl,
                    ri.ref_var.singleton.concat(
                        Entity.args.map(lambda a: a.type_var)
                    )
                )
                & Predicate(
                    Decl.is_generic,
                    ri.ref_var,
                    error_location=ri.node
                ) & Predicate(
                    Decl.has_correct_type_param_number,
                    ri.ref_var,
                    Entity.args.length,
                    error_location=Self.args
                ),
                default_val=Bind(Entity.rebinded_var, No(T.TypeDecl.entity))
            ) & Bind(
                Entity.actual_type_var,
                Entity.rebinded_var,
                conv_prop=Decl.function_type
            )
        )


class ErrorOnNull(Expr):
    """
    Expression that throws an error if LHS is null.
    """
    expr = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expr.xref_equation
            & Bind(Entity.actual_type_var, Entity.expr.actual_type_var)
            & If(Entity.xref_entry_point,
                 Bind(Entity.expr.expected_type_var, Entity.get_expected_type),
                 Bind(Entity.expr.expected_type_var, Entity.expected_type_var))
        )

    has_context_free_type = Property(Entity.expr.has_context_free_type)


class LambdaExpr(Expr):
    """
    Lambda expression.
    """
    params = Field(type=T.LambdaArgDecl.list)
    return_type = Field(type=T.TypeRef)
    body = Field(type=T.Expr)

    env_spec = EnvSpec(add_env())

    has_context_free_type = Property(Entity.body.has_context_free_type)

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        return If(
            Entity.body.xref_entry_point,
            Bind(Entity.body.expected_type_var, No(T.TypeDecl.entity)),
            LogicFalse()
        )

    @langkit_property(return_type=T.Equation)
    def generic_type_equation():
        lambda_type = Var(
            Self.parents.find(lambda p: p.is_a(T.CallExpr)).then(
                lambda ce:
                If(ce.cast(T.CallExpr).args.at(0).then(lambda a: a <= Self),
                    # The lambda is used as an argument, use the generic type
                    Entity.get_generic_type,
                    # The lambda is used as callee, get the expected call
                    Entity.get_expected_type),
                # If there is no CallExpr, there might be an other type of call
                # node, but then the LambdaExpr will only be in its arguments.
                default_val=Entity.get_generic_type
            )
        )
        return Entity.params.logic_all(
            lambda p:
            If(p.decl_type.is_null & Not(lambda_type.is_null),
               Bind(p.type_var, lambda_type.lambda_param_get_type(p)),
               LogicTrue())
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        arg_type_vars = Var(Entity.params.map(lambda p: p.type_var))
        return (
            Cond(
                Not(Entity.return_type.is_null),
                # Lambda have never been used with an explicit type, this is
                # easy to implement, do it later.
                LogicTrue(),
                # If the type of the body can be determined without context, do
                # not include the equation.
                Entity.has_context_free_type,
                Bind(
                    Entity.actual_type_var,
                    Entity.body.get_type._.create_function_type(
                        Entity.params.map(lambda a: a.get_type)
                    )
                ),
                Entity.body.xref_equation
                & NPropagate(
                    Entity.actual_type_var, TypeDecl.create_function_type,
                    Entity.body.actual_type_var.singleton.concat(arg_type_vars)
                )
            )
            & If(
                Entity.has_context_free_type,
                LogicTrue(),
                # If the non instantiated type of the lambda returns a
                # generic type, do not set that type to the expected body type
                # as it should determine itself. Instead, set it to
                # No(TypeDecl) to avoid a cycling dependency where the lambda's
                # type would try to use the body return type to find its own
                # instantiated type when the body type requires an expected
                # type to determine his type.
                Bind(Entity.body.expected_type_var, No(T.TypeDecl.entity))
                | (
                    Bind(Entity.body.expected_type_var,
                         Entity.get_expected_type._.get_return_type)
                    & Predicate(Decl.return_type_is_instantiated,
                                Entity.generic_func_type_var)
                )
            )
        )


class TryExpr(Expr):
    """
    Try expression.
    """
    try_expr = Field(type=T.Expr)
    or_expr = Field(type=T.Expr)

    expected_expr_type_var = UserField(type=LogicVar, public=False)

    has_context_free_type = Property(False)

    @langkit_property(return_type=T.Expr.entity.array)
    def exprs():
        """
        Return an array containing all expressions.
        """
        return If(
            Entity.or_expr.is_null,
            Entity.try_expr.singleton,
            Array([Entity.try_expr, Entity.or_expr]),
        )

    @langkit_property(return_type=T.Equation)
    def expected_exprs_type_equation():
        """
        Find the expected type for all branches by computing the common
        ancestor of the type of all context free expressions, or the expected
        type of the TryExpr if no expression is context free.
        """
        logic_vars = Var(
            Entity.exprs
            .filter(lambda e: e.has_context_free_type)
            .map(lambda e: e.actual_type_var)
            .concat(Entity.expected_type_var.singleton)
            ._or(Entity.expected_type_var.singleton)
        )
        return (
            NPropagate(
                Entity.expected_expr_type_var,
                TypeDecl.imprecise_common_ancestor_list,
                logic_vars,
            ) & Entity.exprs.logic_all(
                lambda e:
                Bind(e.expected_type_var, Entity.expected_expr_type_var)
            )
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.try_expr.xref_equation
            & Entity.expected_exprs_type_equation
            & Entity.exprs.logic_all(
                lambda e:
                    e.xref_equation
                    & Predicate(
                        TypeDecl.commutative_matching_type,
                        e.expected_type_var, e.actual_type_var,
                        error_location=e.node
                    )
            )
            & NPropagate(
                Entity.actual_type_var,
                TypeDecl.imprecise_common_ancestor_list,
                Entity.exprs.map(lambda e: e.actual_type_var)
            )
        )

    has_context_free_type = Property(False)


class RaiseExpr(Expr):
    """
    Raise expression.
    """
    dest_type = Field(type=T.TypeRef)
    except_expr = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.except_expr.xref_equation
            & If(
                Entity.dest_type.is_null,
                Bind(Entity.actual_type_var, Entity.get_expected_type),
                Bind(Entity.actual_type_var, Entity.dest_type.referenced_decl),
            )
            & Bind(Entity.except_expr.expected_type_var, No(T.TypeDecl.entity))
        )

    has_context_free_type = Property(Not(Entity.dest_type.is_null))


class IfExpr(Expr):
    """
    If expression.
    """
    cond_expr = Field(type=T.Expr)
    then_expr = Field(type=T.Expr)
    alternatives = Field(type=T.ElsifBranch.list)
    else_expr = Field(type=T.Expr)

    expected_branch_type_var = UserField(type=LogicVar, public=False)

    has_context_free_type = Property(
        Entity.branch_exprs.any(lambda e: e.has_context_free_type)
    )

    @langkit_property(return_type=T.Expr.entity.array)
    def branch_exprs():
        """
        Return an array containing the expression of all branches.
        """
        return Array([Entity.then_expr, Entity.else_expr]).concat(
            Entity.alternatives.map(lambda alt: alt.then_expr)
        )

    @langkit_property(return_type=T.Equation)
    def expected_branch_type_equation():
        """
        Find the expected type for all branches by computing the common
        ancestor of the type of all context free expressions, or the expected
        type of the IfExpr if no expression is context free.
        """
        logic_vars = Var(
            Entity.branch_exprs
            .filter(lambda expr: expr.has_context_free_type)
            .map(lambda expr: expr.actual_type_var)
            ._or(Entity.expected_type_var.singleton)
        )
        return (
            NPropagate(
                Entity.expected_branch_type_var,
                TypeDecl.imprecise_common_ancestor_list,
                logic_vars,
            ) & Entity.branch_exprs.logic_all(
                lambda e:
                Bind(e.expected_type_var, Entity.expected_branch_type_var)
            )
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.cond_branches_equation
            & Entity.expected_branch_type_equation
            # Check the type of the branches
            & Entity.branch_exprs.logic_all(
                lambda e:
                e.xref_equation
                & Predicate(
                    TypeDecl.matching_type,
                    e.expected_type_var,
                    e.actual_type_var,
                    error_location=e.node
                )
            )
            # Set the type of the IfExpr
            & NPropagate(
                Entity.actual_type_var,
                TypeDecl.imprecise_common_ancestor_list,
                Entity.branch_exprs.map(lambda e: e.actual_type_var)
            )
        )

    @langkit_property(return_type=T.Equation)
    def cond_branches_equation():
        """
        Create an equation verifying that all conditions are valid.
        """
        return (
            Entity.cond_expr.xref_equation
            & Bind(Entity.cond_expr.expected_type_var, Entity.bool_type)
            & Predicate(
                TypeDecl.matching_type,
                Entity.cond_expr.expected_type_var,
                Entity.cond_expr.actual_type_var,
                error_location=Self.cond_expr
            )
            & Entity.alternatives.logic_all(
                lambda alt:
                alt.cond_expr.xref_equation
                & Bind(alt.cond_expr.expected_type_var, Entity.bool_type)
                & Predicate(
                    TypeDecl.commutative_matching_type,
                    alt.cond_expr.expected_type_var,
                    alt.cond_expr.actual_type_var,
                    error_location=alt.cond_expr.node
                )
            )
        )


class ElsifBranch(LktNode):
    """
    Elsif branch of an if expression.
    """
    cond_expr = Field(type=T.Expr)
    then_expr = Field(type=T.Expr)


class BlockDeclList(LktNode.list):
    """
    Semicolon-separated list of declarations.

    This is used to represent declarations in a block expression.
    """
    pass


class BlockExpr(Expr):
    """
    Block expression.
    """
    val_defs = Field(type=T.BlockDeclList)
    expr = Field(type=T.Expr)

    env_spec = EnvSpec(add_env())

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expr.xref_equation
            & Bind(Entity.actual_type_var, Entity.expr.actual_type_var)
            & Bind(Entity.expected_type_var, Entity.expr.expected_type_var)
        )

    has_context_free_type = Property(Entity.expr.has_context_free_type)


class MatchExpr(Expr):
    """
    Binary operator expression.
    """
    match_expr = Field(type=T.Expr)
    branches = Field(type=T.MatchBranch.list)

    expected_branch_type_var = UserField(type=T.LogicVar, public=False)

    has_context_free_type = Property(
        Entity.branch_exprs.any(lambda e: e.has_context_free_type)
    )

    @langkit_property(return_type=T.Expr.entity.array)
    def branch_exprs():
        """
        Return an array containing the expression of all branches.
        """
        return Entity.branches.map(lambda b: b.expr)

    @langkit_property(return_type=T.Equation)
    def expected_branch_type_equation():
        """
        Find the expected type for all branches by computing the common
        ancestor of the type of all context free expressions, or the expected
        type of the MatchExpr if no expression is context free.
        """
        logic_vars = Var(
            Entity.branch_exprs
            .filter(lambda expr: expr.has_context_free_type)
            .map(lambda expr: expr.actual_type_var)
            ._or(Entity.expected_type_var.singleton)
        )
        return (
            NPropagate(
                Entity.expected_branch_type_var,
                TypeDecl.imprecise_common_ancestor_list,
                logic_vars
            ) & Entity.branches.logic_all(
                lambda b:
                Bind(b.expr.expected_type_var, Entity.expected_branch_type_var)
            )
        )

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expected_branch_type_equation
            & Entity.branches.logic_all(
                lambda mb:
                    mb.expr.xref_equation
                    & Predicate(
                        TypeDecl.matching_type,
                        mb.expr.expected_type_var,
                        mb.expr.actual_type_var,
                        error_location=mb.expr.node
                    )
            ) & NPropagate(
                Entity.actual_type_var,
                TypeDecl.imprecise_common_ancestor_list,
                Entity.branch_exprs.map(lambda e: e.actual_type_var)
            )

        )


class MatchBranch(LktNode):
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
    syn_name = Field(type=T.DefId)
    decl_type = Field(type=T.TypeRef)
    decl_type_name = Property(S("match value declaration"))

    xref_entry_point = Property(False)

    match_expr = Property(
        Entity.parents
        .find(lambda p: p.is_a(MatchExpr))
        .cast(T.MatchExpr).match_expr
    )


class BinOp(Expr):
    """
    Binary operator expression.
    """
    left = Field(type=T.Expr)
    op = Field(type=T.Op)
    right = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.left.xref_equation & Entity.right.xref_equation
            & Bind(Entity.left.expected_type_var, Entity.right.actual_type_var)
            & Bind(Entity.right.expected_type_var, Entity.left.actual_type_var)
            & Predicate(
                TypeDecl.commutative_matching_type,
                Entity.right.expected_type_var,
                Entity.right.actual_type_var,
                # When dealing with order operators or the "or?" operator, we
                # must allow common ancestors. This is unnecessary for "==" and
                # "!=", because if one of the type is not a subtype of the
                # other, the operands will always be different.
                Entity.op.is_order_op | Entity.op.is_a(Op.alt_or_int),
                error_location=Self.right
            )
            & Cond(
                # "and" "or"
                Entity.op.is_bool_op,
                Bind(Entity.actual_type_var, Entity.bool_type)
                & Predicate(
                    TypeDecl.is_bool,
                    Entity.left.actual_type_var,
                    error_location=Self.left
                ),
                # "%and" "%or"
                Entity.op.is_equation_op,
                Bind(Entity.actual_type_var, Entity.equation_type)
                & Predicate(
                    TypeDecl.is_equation,
                    Entity.left.actual_type_var,
                    error_location=Self.left
                ),
                # ">" ">=" "<" "<="
                Entity.op.is_order_op,
                Bind(Entity.actual_type_var, Entity.bool_type)
                & Predicate(
                    TypeDecl.is_int_or_node,
                    Entity.left.actual_type_var,
                    error_location=Self.left
                )
                & Predicate(
                    TypeDecl.is_int_or_node,
                    Entity.right.actual_type_var,
                    error_location=Self.right
                ),
                # " +" "-" "*" "/"
                Entity.op.is_arith_op,
                Bind(Entity.actual_type_var, Entity.left.actual_type_var)
                & Predicate(
                    TypeDecl.is_int_type,
                    Entity.left.actual_type_var,
                    error_location=Self.left
                )
                & Predicate(
                    TypeDecl.is_int_type,
                    Entity.right.actual_type_var,
                    error_location=Self.right
                ),
                # "&"
                Entity.op.is_a(Op.alt_amp),
                Bind(Entity.actual_type_var, Entity.left.actual_type_var)
                & Predicate(
                    TypeDecl.is_string_or_array_type,
                    Entity.left.actual_type_var,
                    error_location=Self.left
                )
                & Predicate(
                    TypeDecl.is_string_or_array_type,
                    Entity.right.actual_type_var,
                    error_location=Self.right
                ),
                # "or?"
                Entity.op.is_a(Op.alt_or_int),
                NPropagate(
                    Entity.actual_type_var,
                    TypeDecl.imprecise_common_ancestor_list,
                    [Entity.left.actual_type_var, Entity.right.actual_type_var]
                ),
                # " ==" "!="
                Bind(Entity.actual_type_var, Entity.bool_type)
            )
        )

    @langkit_property(return_type=T.Bool)
    def has_context_free_type():
        return (
            Entity.left.has_context_free_type
            | Entity.right.has_context_free_type
        )


class UnOp(Expr):
    """
    Unary operator expression.
    """
    op = Field(type=T.Op)
    expr = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expr.xref_equation
            & Bind(Entity.actual_type_var, Entity.expr.actual_type_var)
            & Predicate(
                TypeDecl.is_int_type,
                Entity.actual_type_var,
                error_location=Self.expr
            )
        )


class DynVarDecl(ExplicitlyTypedDecl):
    """
    Dynamic variable declaration.
    """
    syn_name = Field(type=T.DefId)
    decl_type = Field(type=T.TypeRef)

    decl_type_name = Property(S("dynamic variable declaration"))

    xref_entry_point = Property(False)


class ValDecl(ExplicitlyTypedDecl):
    """
    Value declaration.
    """
    syn_name = Field(type=T.DefId)
    decl_type = Field(type=T.TypeRef)
    expr = Field(type=T.Expr)

    decl_type_name = Property(S("value declaration"))

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return If(
            Entity.decl_type.is_null,
            Entity.expr.xref_equation
            & Bind(Entity.expr.expected_type_var, No(T.TypeDecl.entity)),
            Entity.expr.xref_equation
            & Bind(Entity.expr.expected_type_var,
                   Entity.decl_type.referenced_decl)
            & If(
                Entity.decl_type.referenced_decl.is_null,
                LogicFalse(),
                Predicate(
                    TypeDecl.matching_type,
                    Entity.expr.expected_type_var,
                    Entity.expr.actual_type_var,
                    error_location=Self.expr
                )
            )
        ) & Predicate(
            TypeDecl.could_determine_type,
            Entity.expr.actual_type_var,
            error_location=Self.syn_name
        )


class Op(LktNode):
    """
    Operator in a binary operator expression.
    """
    enum_node = True

    alternatives = [
        "and", "or", "or_int", "logic_and", "logic_or", "plus", "minus", "eq",
        "ne", "mult", "div", "lt", "gt", "lte", "gte", "amp",
    ]

    @langkit_property(return_type=T.Bool)
    def is_equation_op():
        return Self.is_a(Op.alt_logic_and, Op.alt_logic_or)

    @langkit_property(return_type=T.Bool)
    def is_bool_op():
        return Self.is_a(Op.alt_and, Op.alt_or)

    @langkit_property(return_type=T.Bool)
    def is_arith_op():
        return Self.is_a(
            Op.alt_plus, Op.alt_minus, Op.alt_mult, Op.alt_div,
        )

    @langkit_property(return_type=T.Bool)
    def is_order_op():
        return Self.is_a(Op.alt_lt, Op.alt_lte, Op.alt_gt, Op.alt_gte)


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
    dest_type = Field(type=T.TypeRef)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return If(
            Entity.dest_type.is_null,
            (
                If(Entity.xref_entry_point,
                   Bind(Entity.actual_type_var, Entity.get_expected_type),
                   Bind(Entity.actual_type_var, Entity.expected_type_var))
                # Since the type of NullLit is not context free when it does
                # not have an explicit type, cycling dependencies can happen
                # when it is trying to get its type from an other expresssion
                # (e.g: null == null), making the topological sort fail. This
                # disjunction allows to break the cycle and the predicate below
                # will always emit a diagnostic.
                | Bind(Entity.actual_type_var, No(T.TypeDecl.entity))
            )
            & Predicate(
                TypeDecl.could_determine_type,
                Entity.actual_type_var,
                error_location=Self
            ),
            Bind(Entity.actual_type_var, Entity.dest_type.referenced_decl)
        )

    @langkit_property(return_type=T.Bool)
    def has_context_free_type():
        return Not(Entity.dest_type.is_null)


class DecodedStringValue(Struct):
    """
    Result for ``StringLit.p_denoted_value``.

    If that property is successful, set ``has_error`` to false and ``value`` to
    the decoded string value. Otherwise, set ``has_error`` to true and
    ``error_sloc`` and ``error_message`` to give information about the decoding
    failure.
    """
    value = UserField(type=T.String, default_value=No(T.String))
    has_error = UserField(T.Bool, default_value=False)
    error_sloc = UserField(
        T.SourceLocation, default_value=No(T.SourceLocation)
    )
    error_message = UserField(T.String, default_value=No(T.String))


@abstract
class StringLit(Lit):
    """
    Base node type for string literals.
    """
    @langkit_property(
        public=True,
        return_type=T.DecodedStringValue,
        kind=AbstractKind.abstract,
    )
    def denoted_value():
        """
        Return the content of the given string literal node.
        """
        pass

    @langkit_property(
        public=True, return_type=T.Bool, kind=AbstractKind.abstract
    )
    def is_prefixed_string():
        """
        Return whether this string is prefixed or not.
        """
        pass

    @langkit_property(
        public=True, return_type=T.Character, kind=AbstractKind.abstract
    )
    def prefix():
        """
        Return the prefix of this string, or the null character if there is no
        prefix.
        """
        pass

    @langkit_property(return_type=T.Bool, public=True)
    def is_regexp_literal():
        """
        Return whether this string literal is actually a regexp literal, by
        checking that this string is prefixed by 'p'.
        """
        return Self.prefix == CharacterLiteral('p')

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return If(
            Entity.is_prefixed_string
            & (Entity.prefix == CharacterLiteral("s")),
            Bind(Entity.actual_type_var, Self.symbol_type),
            Bind(Entity.actual_type_var, Self.string_type)
        )


class BlockStringLine(LktNode):
    """
    A single line in a block string literal.
    """
    token_node = True


class BlockStringLit(StringLit):
    """
    String literal expression, made of multiple line strings.

    The denoted string value is the concatenation of all line string items.
    Each line string item must be either:

    * The empty string designator (``|"``), to denote an empty line (``\\n``).

    * ``|" <content>``, to designate a non-empty line. The space before
      ``<content>`` is mandatory, and is not included in the denoted string
      value. ``<content>`` can be anything that appear in a regular string
      literal: escape sequences are interpreted the same way.
    """
    lines = Field(type=T.BlockStringLine.list)

    @langkit_property(external=True, uses_envs=False, uses_entity_info=False)
    def denoted_value():
        pass

    @langkit_property()
    def is_prefixed_string():
        return False

    @langkit_property()
    def prefix():
        return CharacterLiteral("\x00")


class SingleLineStringLit(StringLit):
    """
    Single line string literal expression.

    Note that in order to reduce the size of the node type hierarchy, we define
    only one node (StringLit) for all our string literals (only regular strings
    and pattern string literals at the moment). This will also make it easy to
    add new string prefixes in the future.
    """
    token_node = True

    @langkit_property(external=True, uses_envs=False, uses_entity_info=False)
    def denoted_value():
        pass

    @langkit_property(external=True, uses_envs=False, uses_entity_info=False)
    def is_prefixed_string():
        pass

    @langkit_property(external=True, uses_envs=False, uses_entity_info=False)
    def prefix():
        pass


class PatternSingleLineStringLit(SingleLineStringLit):
    """
    Pattern single line string literal expression.
    """
    pass


class DecodedCharValue(Struct):
    """
    Result for ``CharLit.p_denoted_value``.

    If that property is successful, set ``has_error`` to false and ``value`` to
    the decoded character value. Otherwise, set ``has_error`` to true and
    ``error_sloc`` and ``error_message`` to give information about the decoding
    failure.
    """
    value = UserField(type=T.Character, default_value=No(T.String))
    has_error = UserField(T.Bool, default_value=False)
    error_sloc = UserField(
        T.SourceLocation, default_value=No(T.SourceLocation)
    )
    error_message = UserField(T.String, default_value=No(T.String))


class CharLit(Lit):
    """
    Character literal expression.
    """
    token_node = True

    @langkit_property(public=True, return_type=T.DecodedCharValue,
                      external=True, uses_envs=False, uses_entity_info=False)
    def denoted_value():
        """
        Return the content of the given character literal node.
        """
        pass

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return Bind(Entity.actual_type_var, Self.char_type)


class NumLit(Lit):
    """
    Number literal expression.
    """
    token_node = True

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return Bind(Entity.actual_type_var, Self.int_type)


class BigNumLit(Lit):
    """
    Big number literal expression.
    """
    token_node = True

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return Bind(Entity.actual_type_var, Self.bigint_type)


class VarBind(LktNode):
    """
    Dynamic var bind expression.
    """
    name = Field(type=T.RefId)
    expr = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.name.xref_equation
            & Entity.expr.xref_equation
            & Bind(Entity.expr.expected_type_var, Entity.name.actual_type_var)
            & Bind(Entity.name.expected_type_var, No(T.TypeDecl.entity))
            & Predicate(
                TypeDecl.matching_type,
                Entity.expr.expected_type_var, Entity.expr.actual_type_var,
                error_location=Self.expr
            )
        )


@synthetic
class SynthArgDecl(Decl):
    """
    Logic argument function declaration.
    """
    syn_name = NullField()

    full_name = Property(S("[Synthetic argument]"))

    decl_type_name = Property(S("function argument declaration"))


@synthetic
class SynthFunDecl(Decl):
    """
    Logic function declaration.
    """
    syn_name = NullField()
    args = UserField(type=T.FormalParam.array, public=False)
    return_type = UserField(type=T.TypeDecl.entity, public=False)

    @langkit_property(return_type=T.FunctionType.entity)
    def function_type_aux():
        return Entity.function_type_helper(
            Entity.args.map(lambda a: a.formal_type),
            Entity.return_type,
            Entity
        )

    decl_type_name = Property(S("function declaration"))


class LogicExpr(Expr):
    """
    Class for logic expressions (any ``basic_expr`` starting with %).
    """
    expr = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.expr.xlogic_equation
            & Bind(Entity.expr.expected_type_var, Entity.bool_type)
            & Bind(Entity.actual_type_var, Entity.equation_type)
        )


class LogicUnify(Expr):
    """
    Class for "unify" equations.
    """
    lhs = Field(type=T.Expr)
    rhs = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.lhs.xref_equation
            & Bind(Entity.lhs.expected_type_var, Entity.logicvar_type)
            & Predicate(
                TypeDecl.matching_type,
                Entity.lhs.expected_type_var,
                Entity.lhs.actual_type_var,
                error_location=Self.lhs
            )
            & Entity.rhs.xref_equation
            & Bind(Entity.rhs.expected_type_var, Entity.logicvar_type)
            & Predicate(
                TypeDecl.matching_type,
                Entity.rhs.expected_type_var,
                Entity.rhs.actual_type_var,
                error_location=Self.rhs
            )
            & Bind(Entity.actual_type_var, Entity.equation_type)
        )


class LogicAssign(Expr):
    """
    Class for "assign to logic var" equations.
    """
    dest_var = Field(type=T.Expr)
    value = Field(type=T.Expr)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.dest_var.xref_equation
            & Bind(Entity.dest_var.expected_type_var, Entity.logicvar_type)
            & Predicate(
                TypeDecl.matching_type,
                Entity.dest_var.expected_type_var,
                Entity.dest_var.actual_type_var,
                error_location=Self.dest_var
            )
            & Entity.value.xref_equation
            & Bind(
                Entity.value.expected_type_var,
                Entity.entity_type
                    .instantiate_generic_decl(
                        Entity.node_env.get_first("RootNode__")
                            .cast(T.TypeDecl).singleton
                    )
            )
            & Predicate(
                TypeDecl.matching_type,
                Entity.value.expected_type_var,
                Entity.value.actual_type_var,
                error_location=Self.value
            )
            & Bind(Entity.actual_type_var, Entity.equation_type)
        )


class LogicPropagateCall(LogicCallExpr):
    """
    Class for the call inside "propagate" equations.
    """
    pass


class LogicPropagate(Expr):
    """
    Class for "propagate" equations.
    """
    dest_var = Field(type=T.Expr)
    call = Field(type=T.LogicPropagateCall)

    @langkit_property(return_type=T.Equation)
    def generic_type_equation():
        return in_logic_call.bind(
            True,
            Entity.call_generic_type_equation(Entity.call.name,
                                              Entity.call.args)
        )

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        return Entity.call_expected_type_equation(Entity.call.name,
                                                  Entity.call.args)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.dest_var.xref_equation
            & Bind(Entity.dest_var.expected_type_var, Entity.logicvar_type)
            & Predicate(
                TypeDecl.matching_type,
                Entity.dest_var.expected_type_var,
                Entity.dest_var.actual_type_var,
                error_location=Self.dest_var
            )
            & Entity.call.name.xlogic_equation
            & in_logic_call.bind(
                True,
                Entity.xref_call_equation(Entity.call.name, Entity.call.args)
            )
            & Predicate(
                FunctionType.returns_entity, Entity.call.name.actual_type_var,
                error_location=Self.call.name
            )
            & Bind(Entity.actual_type_var, Entity.equation_type)
        )


class LogicPredicate(LogicCallExpr):
    """
    Class for "predicate" equations.
    """
    pass

    @langkit_property(return_type=T.Equation)
    def generic_type_equation():
        return in_logic_call.bind(
            True,
            Entity.call_generic_type_equation(Entity.name, Entity.args)
        )

    @langkit_property(return_type=T.Equation)
    def expected_type_equation():
        return Entity.call_expected_type_equation(Entity.name, Entity.args)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return (
            Entity.name.xlogic_equation
            & in_logic_call.bind(
                True, Entity.xref_call_equation(Entity.name, Entity.args)
            )
            & Predicate(
                FunctionType.returns_bool, Entity.name.actual_type_var,
                error_location=Self.name
            )
            & Bind(Entity.actual_type_var, Entity.equation_type)
        )


lkt_grammar = Grammar('main_rule', with_unparsers=True)
G = lkt_grammar
lkt_grammar.add_rules(
    main_rule=LangkitRoot(
        G.imports, G.decls, Lex.Termination
    ),
    id=Id(Lex.Identifier),
    ref_id=RefId(Lex.Identifier),
    type_ref_id=GPredicate(G.ref_id, RefId.is_type_name),
    def_id=DefId(Lex.Identifier),

    doc=Opt(G.string_lit),

    import_stmt=Import("import", ModuleRefId(Lex.Identifier)),
    imports=List(G.import_stmt, empty_valid=True),

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
        G.token_pattern,
        G.grammar_cut,
        G.grammar_skip,
        G.grammar_null,
        G.grammar_token,
        G.grammar_stopcut,
        G.parse_node_expr,
        G.grammar_opt,
        G.grammar_opt_error,
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

    grammar_opt_error=GOr(
        GrammarOptError("!", G.grammar_expr),
        GrammarOptErrorGroup(
            "!", "(", List(G.grammar_expr, empty_valid=True), ")"
        ),
    ),

    grammar_cut=GrammarCut("/"),
    grammar_stopcut=GrammarStopCut(
        Lex.Identifier("stop_cut"), "(", G.grammar_expr, ")"
    ),

    grammar_or_expr=GrammarOrExpr(
        "or", "(",
        List(List(G.grammar_expr), sep="|", extra=ListSepExtra.allow_leading),
        ")"
    ),

    grammar_discard_expr=GrammarDiscard(
        "discard", "(", G.grammar_expr, ")"
    ),

    token_literal=TokenLit(Lex.String),

    token_no_case_literal=TokenNoCaseLit(
        Lex.Identifier(match_text="no_case"), "(", G.token_literal, ")"
    ),

    token_pattern=GOr(
        TokenPatternConcat(G.token_pattern, "&", G.token_pattern_literal),
        G.token_pattern_literal,
    ),
    token_pattern_literal=TokenPatternLit(Lex.PString),

    parse_node_expr=ParseNodeExpr(
        G.type_ref, "(", List(G.grammar_expr, empty_valid=True), ")"
    ),

    grammar_rule_ref=GrammarRuleRef(G.ref_id),

    grammar_list_expr=GrammarList(
        # Match either "list" (type inference will determine the list type) or
        # a specific list type.
        GOr(DefaultListTypeRef(Lex.Identifier(match_text="list")), G.type_ref),

        GOr(ListKind.alt_one("+"), ListKind.alt_zero("*")),
        "(",

        # Main list expr
        G.grammar_implicit_pick | G.grammar_expr,

        # Separator
        Opt(",", G.grammar_list_sep), ")",
    ),

    grammar_list_sep=GrammarListSep(G.grammar_expr, Opt(",", G.id)),

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
        "generic",
        "[",
        List(G.generic_formal_type, sep=",", list_cls=GenericFormalDeclList),
        "]",
        G.bare_decl,
    ),

    generic_formal_type=FullDecl(
        G.doc,
        List(G.decl_annotation, empty_valid=True),
        GenericFormalTypeDecl(ClassQualifier("class"), G.def_id),
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

    fun_arg_decl=FunArgDecl(
        List(G.decl_annotation, empty_valid=True),
        G.def_id,
        ":", G.type_ref,
        Opt("=", G.expr),
    ),

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
        G.env_spec_decl,
        G.grammar_rule,
        G.dynvar_decl,
    ),

    decl=FullDecl(
        G.doc, List(G.decl_annotation, empty_valid=True), G.bare_decl
    ),

    type_expr=GOr(
        DotExpr(G.type_expr, ".", Cut(), G.type_ref_id),
        G.type_ref_id,
    ),

    type_ref=GOr(
        GenericTypeRef(G.type_expr, "[", G.type_list, "]"),
        SimpleTypeRef(G.type_expr),
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

    dynvar_decl=DynVarDecl("dynvar", G.def_id, ":", G.type_ref),

    var_bind=VarBind("bind", G.ref_id, "=", G.expr),

    env_spec_action=CallExpr(RefId(Lex.Identifier), "(", G.params, ")"),

    env_spec_decl=EnvSpecDecl(
        DefId(Lex.Identifier(match_text="env_spec")),
        "{",
        List(G.env_spec_action, empty_valid=True),
        "}",
    ),

    block=BlockExpr(
        "{",
        # TODO: Add discard/ignore in the list
        List(
            GOr(G.val_decl, G.var_bind),
            empty_valid=False,
            sep=";",
            list_cls=BlockDeclList,
        ),
        ";",
        G.expr,
        "}"
    ),

    expr=GOr(
        BinOp(
            G.expr,
            GOr(
                Op.alt_or_int("or", "?"),
                Op.alt_or("or"),
                Op.alt_and("and"),
                Op.alt_logic_and("%", "and"),
                Op.alt_logic_or("%", "or"),
            ),
            G.rel,
        ),
        G.rel,
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
                        Op.alt_eq("=="),
                        Op.alt_ne("!=")), G.arith_1),
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
              GOr(Op.alt_mult("*"), Op.alt_div("/")),
              G.arith_3),
        G.arith_3,
    ),

    arith_3=GOr(
        UnOp(GOr(Op.alt_plus("+"), Op.alt_minus("-")), G.isa_or_primary),
        G.isa_or_primary,
    ),

    isa_or_primary=GOr(
        Isa(
            G.primary,
            "is",
            List(G.type_ref, sep="|", empty_valid=False, list_cls=IsaList),
        ),
        AnyOf(
            G.primary,
            "in",
            List(G.primary, sep="|", empty_valid=False, list_cls=AnyOfList),
        ),
        LogicUnify(G.isa_or_primary, "<->", G.primary),
        LogicPropagate(
            G.isa_or_primary, "<-", G.logic_propagate_call
        ),
        LogicAssign(G.isa_or_primary, "<-", G.primary),
        G.primary
    ),

    logic_propagate_call=LogicPropagateCall(
        G.callable_ref, "%", "(", G.params, ")"
    ),

    primary=GOr(
        G.lambda_expr,
        G.if_expr,
        G.raise_expr,
        G.try_expr,
        G.basic_expr,
    ),

    match_expr=MatchExpr(
        "match", G.expr, "{",
        List(MatchBranch("case",
                         MatchValDecl(G.def_id, Opt(":", G.type_ref)),
                         "=>", G.expr)),
        "}"
    ),

    num_lit=NumLit(Lex.Number),
    big_num_lit=BigNumLit(Lex.BigNumber),
    string_lit=GOr(
        SingleLineStringLit(Lex.String),
        PatternSingleLineStringLit(Lex.PString),
        G.block_string_lit,
    ),
    block_string_lit=BlockStringLit(
        List(BlockStringLine(Lex.BlockStringLine), empty_valid=False)
    ),
    char_lit=CharLit(Lex.Char),

    if_expr=IfExpr(
        "if", G.expr, "then", G.expr,
        List(ElsifBranch("elif", G.expr, "then", G.expr), empty_valid=True),
        "else", G.expr
    ),

    raise_expr=RaiseExpr("raise", Opt("[", G.type_ref, "]"), G.expr),
    try_expr=TryExpr("try", G.expr, Opt("else", G.expr)),

    array_literal=ArrayLiteral(
        "[",
        List(G.expr, sep=",", empty_valid=True),
        "]",
        Opt(":", G.type_ref),
    ),

    callable_ref=GOr(
        DotExpr(G.callable_ref, ".", G.ref_id),
        G.ref_id,
    ),

    basic_expr=GOr(
        CallExpr(G.basic_expr, "(", G.params, ")"),
        GenericInstantiation(G.basic_expr, "[", G.type_list, "]"),
        SubscriptExpr(G.basic_expr, "[", G.expr, "]"),
        NullCondSubscriptExpr(G.basic_expr, "?", "[", G.expr, "]"),
        ErrorOnNull(G.basic_expr, "!"),
        KeepExpr(
            G.basic_expr, ".",
            Lex.Identifier(match_text="keep"),
            "[", G.type_ref, "]"
        ),
        CastExpr(
            G.basic_expr, ".",
            Lex.Identifier(match_text="as"),
            ExcludesNull("!"),
            "[", G.type_ref, "]"
        ),
        LogicPredicate(G.basic_expr, "%", "(", G.params, ")"),
        DotExpr(G.basic_expr, ".", Cut(), G.ref_id),
        NullCondDottedName(G.basic_expr, "?", ".", G.ref_id),

        LogicExpr("%", CallExpr(G.ref_id, "(", G.params, ")")),
        LogicExpr("%", G.ref_id),
        G.term
    ),

    term=GOr(
        ParenExpr("(", G.expr, ")"),
        G.match_expr,
        G.null_lit,
        G.ref_id,
        G.block,
        G.num_lit,
        G.big_num_lit,
        G.string_lit,
        G.char_lit,
        G.array_literal,
    ),

    basic_name=GOr(
        DotExpr(G.basic_name, ".", Cut(), G.ref_id),
        G.ref_id
    ),


    lambda_expr=LambdaExpr("(", G.lambda_arg_list, ")",
                           Opt(":", G.type_ref), "=>", Cut(), G.expr),

    null_lit=NullLit("null", Opt("[", G.type_ref, "]")),

    param=Param(Opt(G.ref_id, "="), G.expr),
    params=List(G.param, sep=",", empty_valid=True),

    decl_annotation_params=Opt(DeclAnnotationParams("(", G.params, ")")),
    decl_annotation=DeclAnnotation("@", G.id, G.decl_annotation_params),

)
