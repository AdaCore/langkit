def reset():
    """
    Reset global state in Langkit.

    TODO: this is a hack to workaround another hack. At some point in the
    future, we should get rid of this global state in Langkit.
    """

    import langkit.compile_context
    import langkit.compiled_types
    import langkit.dsl
    import langkit.expressions.base
    import langkit.lexer
    import langkit.utils

    langkit.compiled_types.CompiledTypeRepo.reset()
    langkit.compiled_types.create_builtin_types()

    langkit.dsl._StructMetaclass.reset()
    langkit.dsl._ASTNodeMetaclass.reset()
    langkit.dsl._EnumMetaclass.reset()

    langkit.expressions.base.Entity.unfreeze()
    langkit.expressions.base.Self.unfreeze()

    langkit.lexer.LexerToken.reset()

    langkit.utils.reset_memoized()
    langkit.compile_context.compile_ctx = None
