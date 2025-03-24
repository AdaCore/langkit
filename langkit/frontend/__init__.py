"""
Package to gather the logic to lower Lkt syntax trees to Langkit internal data
structures.

Global architecture:

* In the constructor of CompileCtx, all Lkt units are loaded into the
  CompileCtx.lkt_units list. At this stage, compilation is aborted in case of
  lexing/parsing error.

* During the "lower_lkt" pass:

  * The "langkit.frontend.lexer.create_lexer" function instantiates the
    langkit.lexer.Lexer class and populates it.

  * The "langkit.frontend.grammar.create_grammar" function instantiates the
    langkit.parsers.Grammar class and populates it.

  * The "langkit.frontend.types.create_types" function instantiates all
    CompiledType instances mentionned in the language spec.

The last step is the most complex one: type declarations refer to each other,
and this step includes the lowering of property expressions to abstract
expressions. The lowering of types goes as follows:

* [ROOT_SCOPE_CREATION] The first step looks at all top-level declarations
  (lexers, grammars and types) and registers them by name in the root scope.

* [TYPES_LOWERING] We then iterate on all types and lower them. All type
  declarations in the language spec are lowered in sequence (arbitrary order),
  except base classes, which are lowered before classes that they derive from.
  This step creates the actual ``CompiledType``/``GenericInterface`` instances,
  but defers the lowering of type members
  (``CompileCtx.deferred.type_members.add``).

* [DYNVAR_LOWERING] Now that all compiled types are known, another step lowers
  all dynamic variables. This must be done before lowering type members, since
  they may use dynamic variables in their signatures.

* [TYPE_MEMBERS_LOWERING] When reaching this step, all ``CompiledType``
  instances are created, or for "on-demand" types such as arrays or iterators,
  it is trivial to create one. So it is only at this point that type members
  (fields, properties) can be lowered: ``AbstractNodeData`` instances are
  created and added to their ownning compiled type
  (call to ``CompileCtx.deferred.type_members.resolve()``).

* [GENERIC_INTERFACE_MEMBERS_LOWERING] Now that all generic interfaces and
  compiled types are known, it is possible to create InterfaceMethodProfile
  instances for all the generic interface methods.

* [ENV_SPECS_LOWERING] It is the turn of env specs are lowered. Lowering them
  before expressions is necessary since the set of legal ref categories is
  determined by env specs, and constructing RefCategory expressions needs to
  know the whole set of ref categories.

* [STATIC_EXPR_LOWERING] Lowering arguments must be done after env specs
  lowering (so that the compilation of arguments default values knows about the
  set of valid ref categories). Computing property attributes also needs to 1)
  know about arguments, and 2) happen before the lowering of property bodies.
  So the lowering of arguments needs to happen in a dedicated pass.

  Likewise, lowering default valaues for fields must be done before the
  "compute types" pass runs, and that pass must run before the lowering of
  property bodies.

* [EXPR_LOWERING] Finally, the fields' and arguments' default values and the
  bodies of all properties are lowered.
"""
