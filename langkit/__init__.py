def reset():
    """
    Reset global state in Langkit.

    TODO: this is a hack to workaround another hack. At some point in the
    future, we should get rid of this global state in Langkit.
    """

    import langkit.compile_context
    import langkit.compiled_types
    import langkit.expressions.base
    import langkit.lexer
    import langkit.utils

    langkit.utils.reset_memoized()

    langkit.compiled_types.CompiledTypeRepo.reset()
    langkit.compiled_types.create_builtin_types()

    langkit.expressions.base.Entity.unfreeze()
    langkit.expressions.base.Self.unfreeze()

    langkit.lexer.LexerToken.reset()

    langkit.compile_context.compile_ctx = None
