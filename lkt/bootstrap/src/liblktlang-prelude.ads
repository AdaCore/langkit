package Liblktlang.Prelude is
   Content : constant String :=
      "# In this file, the RootNode__ type designates the root node type from the" & Character'Val (10)
       & "# Langkit spec importing this prelude. See the ``Decl`` environment spec for" & Character'Val (10)
       & "# more details." & Character'Val (10)
       & Character'Val (10)
       & "|"" Trait implicitly implemented by all types" & Character'Val (10)
       & "generic[T]" & Character'Val (10)
       & "trait BasicTrait {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun is_null(): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun singleton(): Array[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    generic[U]" & Character'Val (10)
       & "    fun do(do_fn: (T)->U, default_val: U = null[U]): U" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun as_entity(): Entity[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun as_bare_entity(): Entity[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun to_builder(): NodeBuilder[T]" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct Int {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun as_big_int(): BigInt" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct BigInt {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun as_int(): Int" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct Symbol {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun image(): String" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct Regexp {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "@open" & Character'Val (10)
       & "enum Bool {" & Character'Val (10)
       & "    case false, true" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "trait Sized {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun length(): Int" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[T]" & Character'Val (10)
       & "trait Indexable {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun __call__(index: Int): T" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[T]" & Character'Val (10)
       & "trait Iterable {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    generic[U]" & Character'Val (10)
       & "    fun map(map_fn: (T)->U): Array[U]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    generic[U]" & Character'Val (10)
       & "    fun imap(map_fn: (T, Int)->U): Array[U]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    generic[U]" & Character'Val (10)
       & "    fun mapcat(map_fn: (T)->Array[U]): Array[U]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    generic[U]" & Character'Val (10)
       & "    fun imapcat(map_fn: (T, Int)->Array[U]): Array[U]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun filter(filter_fn: (T)->Bool): Array[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun ifilter(filter_fn: (T, Int)->Bool): Array[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    generic[U]" & Character'Val (10)
       & "    fun filtermap(map_fn: (T)->U, filter_fn: (T)->Bool): Array[U]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    generic[U]" & Character'Val (10)
       & "    fun ifiltermap(map_fn: (T, Int)->U, filter_fn: (T, Int)->Bool): Array[U]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun find(filter_fn: (T)->Bool): T" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun take_while(pred_fn: (T)->Bool): Array[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun itake_while(pred_fn: (T, Int)->Bool): Array[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun empty(): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun contains(elt: T): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun all(logic_fn: (T)->Bool): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun iall(logic_fn: (T, Int)->Bool): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun any(logic_fn: (T)->Bool): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun iany(logic_fn: (T, Int)->Bool): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun logic_all(logic_fn: (T)->Equation): Equation" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun ilogic_all(logic_fn: (T, Int)->Equation): Equation" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun logic_any(logic_fn: (T)->Equation): Equation" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun ilogic_any(logic_fn: (T, Int)->Equation): Equation" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[T]" & Character'Val (10)
       & "struct Array implements Sized, Indexable[T], Iterable[T] {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun to_iterator(): Iterator[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun unique(): Array[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun env_group(with_md: Metadata = null[Metadata]): LexicalEnv" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[T]" & Character'Val (10)
       & "struct Iterator {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[T]" & Character'Val (10)
       & "trait Node {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun parent(): T" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun node_env(): LexicalEnv" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun children_env(): LexicalEnv" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun unit(): AnalysisUnit" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun parents(with_self: Bool = true): Array[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun children(): Array[T]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun text(): String" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun ple_root(): RootNode__" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun can_reach(from_node: RootNode__): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun token_start(): Token" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun token_end(): Token" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun child_index(): Int" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun previous_sibling(): Entity[RootNode__]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun next_sibling(): Entity[RootNode__]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun is_ghost(): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun full_sloc_image(): String" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[class T]" & Character'Val (10)
       & "class ASTList: RootNode__ implements Sized, Indexable[T], Iterable[T] {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun as_array(): Array[T]" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[class T]" & Character'Val (10)
       & "class NodeBuilder {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun build(parent: RootNode__): T" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct Char {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct String implements Sized, Indexable[Char], Iterable[Char] {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun to_symbol(): Symbol" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun join(strings: Array[String]): String" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct LogicVar {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun get_value(): Entity[RootNode__]" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct LogicContext {" & Character'Val (10)
       & "    ref_node: Entity[RootNode__]" & Character'Val (10)
       & "    decl_node: Entity[RootNode__]" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "dynvar logic_context: LogicContext" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct Equation {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun solve(): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun solve_with_diagnostics(): SolverResult" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct PropertyError {" & Character'Val (10)
       & "    exception_message: String = ""PropertyError exception""" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct PreconditionFailure {" & Character'Val (10)
       & "    exception_message: String = ""PreconditionFailure exception""" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "@ignore_constructor_arg" & Character'Val (10)
       & "struct RefCategories {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "enum LookupKind {" & Character'Val (10)
       & "    case recursive, flat, minimal" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct LexicalEnv {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun get(" & Character'Val (10)
       & "        symbol: Symbol," & Character'Val (10)
       & "        from: RootNode__ = null[RootNode__]," & Character'Val (10)
       & "        lookup: LookupKind = LookupKind.recursive," & Character'Val (10)
       & "        categories: RefCategories = null[RefCategories]" & Character'Val (10)
       & "    ): Array[Entity[RootNode__]]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun get_first(" & Character'Val (10)
       & "        symbol: Symbol," & Character'Val (10)
       & "        from: RootNode__ = null[RootNode__]," & Character'Val (10)
       & "        lookup: LookupKind = LookupKind.recursive," & Character'Val (10)
       & "        categories: RefCategories = null[RefCategories]" & Character'Val (10)
       & "    ): Entity[RootNode__]" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun env_node(): RootNode__" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun env_orphan(): LexicalEnv" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun env_parent(): LexicalEnv" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun is_visible_from(base_env: LexicalEnv): Bool" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun shed_rebindings(info: EntityInfo): EntityInfo" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun rebind_env(r: EnvRebindings): LexicalEnv" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "enum DesignatedEnvKind {" & Character'Val (10)
       & "    case none, current_env, named_env, direct_env" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct DesignatedEnv {" & Character'Val (10)
       & "    kind: DesignatedEnvKind" & Character'Val (10)
       & "    env_name: Symbol" & Character'Val (10)
       & "    direct_env: LexicalEnv" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "enum AnalysisUnitKind {" & Character'Val (10)
       & "    case unit_specification, unit_body" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "trait AnalysisUnit {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun root(): RootNode__" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun is_referenced_from(unit: AnalysisUnit): Bool" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct EnvRebindings {" & Character'Val (10)
       & "    old_env: LexicalEnv" & Character'Val (10)
       & "    new_env: LexicalEnv" & Character'Val (10)
       & "    get_parent: EnvRebindings" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun append_rebinding(" & Character'Val (10)
       & "        old_env: LexicalEnv," & Character'Val (10)
       & "        new_env: LexicalEnv" & Character'Val (10)
       & "    ): EnvRebindings" & Character'Val (10)
       & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    fun concat_rebindings(rhs: EnvRebindings): EnvRebindings" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct EnvAssoc {" & Character'Val (10)
       & "    key: Symbol" & Character'Val (10)
       & "    value: RootNode__" & Character'Val (10)
       & "    dest_env: DesignatedEnv" & Character'Val (10)
       & "    metadata: Metadata" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct InnerEnvAssoc {" & Character'Val (10)
       & "    key: Symbol" & Character'Val (10)
       & "    value: RootNode__" & Character'Val (10)
       & "    rebindings: EnvRebindings = null[EnvRebindings]" & Character'Val (10)
       & "    metadata: Metadata = null[Metadata]" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "@open" & Character'Val (10)
       & "enum RefKind {" & Character'Val (10)
       & "    case transitive, prioritary, normal" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct EnvAction {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun set_initial_env(env: DesignatedEnv): EnvAction" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun add_env(" & Character'Val (10)
       & "    no_parent: Bool = false," & Character'Val (10)
       & "    transitive_parent: Bool = false," & Character'Val (10)
       & "    names: Array[Symbol] = null" & Character'Val (10)
       & "): EnvAction" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun add_single_to_env(mapping: EnvAssoc): EnvAction" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun add_all_to_env(mappings: Array[EnvAssoc]): EnvAction" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun add_to_env_kv(" & Character'Val (10)
       & "    key: Symbol," & Character'Val (10)
       & "    value: RootNode__," & Character'Val (10)
       & "    dest_env: DesignatedEnv = null," & Character'Val (10)
       & "    metadata: Metadata = null" & Character'Val (10)
       & "): EnvAction" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[T]" & Character'Val (10)
       & "fun do(expr: T): EnvAction" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun handle_children(): EnvAction" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun reference(" & Character'Val (10)
       & "    nodes: Array[RootNode__]," & Character'Val (10)
       & "    through: ()->LexicalEnv," & Character'Val (10)
       & "    kind: RefKind = normal," & Character'Val (10)
       & "    dest_env: LexicalEnv = null," & Character'Val (10)
       & "    cond: Bool = true," & Character'Val (10)
       & "    category: String = null," & Character'Val (10)
       & "    shed_corresponding_rebindings: Bool = false" & Character'Val (10)
       & "): EnvAction" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct EntityInfo {" & Character'Val (10)
       & "    |"" The metadata associated to the AST node" & Character'Val (10)
       & "    md: Metadata" & Character'Val (10)
       & "    rebindings: EnvRebindings" & Character'Val (10)
       & "    from_rebound: Bool" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "generic[N]" & Character'Val (10)
       & "struct Entity {" & Character'Val (10)
       & "    node: N" & Character'Val (10)
       & "    info: EntityInfo" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "trait TokenNode {" & Character'Val (10)
       & "    @builtin" & Character'Val (10)
       & "    @property" & Character'Val (10)
       & "    fun symbol(): Symbol" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct SolverDiagnostic {" & Character'Val (10)
       & "    template: String" & Character'Val (10)
       & "    args: Array[Entity[RootNode__]]" & Character'Val (10)
       & "    location: RootNode__" & Character'Val (10)
       & "    contexts: Array[LogicContext]" & Character'Val (10)
       & "    round: Int" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct SolverResult {" & Character'Val (10)
       & "    success: Bool" & Character'Val (10)
       & "    diagnostics: Array[SolverDiagnostic] = null[Array[SolverDiagnostic]]" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct Token {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct SourceLocation {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "struct __internal {" & Character'Val (10)
       & "    |"" Placeholder type when the langkit specification does not have a type" & Character'Val (10)
       & "    |"" annotated with ``@metadata``." & Character'Val (10)
       & "    |""" & Character'Val (10)
       & "    |"" This is hidden in its own scope and later has a reference in the" & Character'Val (10)
       & "    |"" global lexical environemnt so that if no type is designated as metadata," & Character'Val (10)
       & "    |"" this types serves as fallback." & Character'Val (10)
       & "    @metadata" & Character'Val (10)
       & "    struct __EmptyMetadata {" & Character'Val (10)
       & "    }" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "trait ErrorNode {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "struct Address {" & Character'Val (10)
       & "}" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun dynamic_lexical_env(" & Character'Val (10)
       & "    assocs_getter: ()->Array[InnerEnvAssoc]," & Character'Val (10)
       & "    assoc_resolver: ()->Entity[RootNode__] = null," & Character'Val (10)
       & "    transitive_parent: Bool = true" & Character'Val (10)
       & "): LexicalEnv" & Character'Val (10)
       & Character'Val (10)
       & "@builtin" & Character'Val (10)
       & "fun domain(var: LogicVar, values: Array[Entity[RootNode__]]): Equation" & Character'Val (10)
       & Character'Val (10)
       & "dynvar error_location: RootNode__" & Character'Val (10)

   ;
end Liblktlang.Prelude;