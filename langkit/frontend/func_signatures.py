from __future__ import annotations

from langkit.compile_context import CompileCtx
from langkit.diagnostics import check_source_language, error
from langkit.frontend.utils import lkt_context

import liblktlang as L


class FunctionParamSpec:
    """
    Specification for a function parameter.
    """

    def __init__(
        self,
        name: str,
        optional: bool = False,
        keyword_only: bool = False,
    ):
        """
        :param optional: Whether passing an argument for this parameter is
            optional.
        """
        self.name = name
        self.optional = optional
        self.keyword_only = keyword_only


class FunctionSignature:
    """
    Specification of required/accepted parameters for a function.
    """

    def __init__(
        self,
        *param_specs: FunctionParamSpec,
        positional_variadic: bool = False,
    ):
        """
        Create a function signature.

        :param param_specs: List of parameter specifications.
        :param positional_variadic: Whether this function accepts an arbitrary
            number of positional arguments in addition to the ones described by
            ``param_specs``.
        """
        self.param_specs = list(param_specs)
        self.positional_variadic = positional_variadic

        self.positionals = [
            spec for spec in self.param_specs if not spec.keyword_only
        ]
        """Subset of parameters that can be passed as positional arguments."""

        self.by_name: dict[str, FunctionParamSpec] = {}
        for spec in self.param_specs:
            assert spec.name not in self.by_name
            self.by_name[spec.name] = spec

    def match(
        self, ctx: CompileCtx, call: L.CallExpr
    ) -> tuple[dict[str, L.Expr], list[L.Expr]]:
        """
        Match call arguments against this signature. If successful, return the
        parsed arguments. Abort with a user error otherwise.

        The result is a dict that maps parameter names to syntactically passed
        arguments, and a list of remaining arguments if
        ``self.positional_variadic`` is true.
        """
        # Match results
        args: dict[str, L.Expr] = {}
        vargs: list[L.Expr] = []

        # Index in "self.positionals" for the next expected positional
        # argument.
        next_positional = 0

        for arg in call.f_args:
            if arg.f_name:
                # This is a keyword argument. Make sure it is known and that it
                # was not passed already.
                name = arg.f_name.text
                with lkt_context(arg.f_name):
                    check_source_language(
                        name in self.by_name, "unknown argument"
                    )
                    check_source_language(
                        name not in args, "this argument is already passed"
                    )
                args[name] = arg.f_value

            else:
                # This is a positional argument: look for the corresponding
                # parameter spec. First update "next_positional" in case they
                # were passed by keyword.
                while (
                    next_positional < len(self.positionals)
                    and self.positionals[next_positional].name in args
                ):
                    next_positional += 1

                if next_positional < len(self.positionals):
                    args[self.positionals[next_positional].name] = arg.f_value
                else:
                    with lkt_context(arg):
                        check_source_language(
                            self.positional_variadic,
                            f"at most {next_positional} positional argument(s)"
                            f" expected, got {next_positional + 1}",
                        )
                    vargs.append(arg.f_value)

        # Check that all required arguments were passed
        missing = {
            name for name, spec in self.by_name.items() if not spec.optional
        } - set(args)
        if missing:
            loc_node = (
                call.f_name.f_suffix
                if isinstance(call.f_name, L.DotExpr)
                else call
            )
            with lkt_context(loc_node):
                error(f"argument '{list(missing)[0]}' is missing")

        return args, vargs


add_env_signature = FunctionSignature(
    FunctionParamSpec("no_parent", optional=True, keyword_only=True),
    FunctionParamSpec("transitive_parent", optional=True, keyword_only=True),
    FunctionParamSpec("names", optional=True, keyword_only=True),
)
"""
Signature for the "add_env" env action.
"""

add_to_env_kv_signature = FunctionSignature(
    FunctionParamSpec("key"),
    FunctionParamSpec("value"),
    FunctionParamSpec("dest_env", optional=True, keyword_only=True),
    FunctionParamSpec("metadata", optional=True, keyword_only=True),
    FunctionParamSpec("resolver", optional=True, keyword_only=True),
)
"""
Signature for the "add_to_env_kv" env action.
"""

add_single_to_env_signature = FunctionSignature(
    FunctionParamSpec("mapping"),
    FunctionParamSpec("resolver", optional=True, keyword_only=True),
)
"""
Signature for the "add_single_to_env" env action.
"""

add_all_to_env_signature = FunctionSignature(
    FunctionParamSpec("mappings"),
    FunctionParamSpec("resolver", optional=True, keyword_only=True),
)
"""
Signature for the "add_all_to_env" env action.
"""

append_rebinding_signature = FunctionSignature(
    FunctionParamSpec("old_env"),
    FunctionParamSpec("new_env"),
)
"""
Signature for ".append_rebinding".
"""

collection_iter_signature = FunctionSignature(FunctionParamSpec("expr"))
"""
Common signature for collection iteration expressions.
"""

concat_rebindings_signature = FunctionSignature(
    FunctionParamSpec("rebindings"),
)
"""
Signature for ".concat_rebindings".
"""

contains_signature = FunctionSignature(FunctionParamSpec("value"))
"""
Signature for ".contains".
"""

do_signature = FunctionSignature(
    FunctionParamSpec("expr"),
    FunctionParamSpec("default_val", optional=True, keyword_only=True),
)
"""
Signature for ".do".
"""

do_env_signature = FunctionSignature(FunctionParamSpec("expr"))
"""
Signature for the "do" env action.
"""

domain_signature = FunctionSignature(
    FunctionParamSpec("var"), FunctionParamSpec("domain")
)
"""
Signature for "%domain".
"""

dynamic_lexical_env_signature = FunctionSignature(
    FunctionParamSpec("assocs"),
    FunctionParamSpec("assoc_resolver", optional=True, keyword_only=True),
    FunctionParamSpec("transitive_parent", optional=True, keyword_only=True),
)
"""
Signature for the "dynamic_lexical_env" builtin function.
"""

empty_signature: FunctionSignature = FunctionSignature()
"""
Signature for a function that takes no argument.
"""

env_group_signature = FunctionSignature(
    FunctionParamSpec("with_md", optional=True, keyword_only=True),
)
"""
Signature for ".env_group".
"""

eq_signature = FunctionSignature(
    FunctionParamSpec("to"),
    FunctionParamSpec("from"),
    FunctionParamSpec("conv_prop", optional=True, keyword_only=True),
)
"""
Signature for "%eq".
"""

exception_signature = FunctionSignature(
    FunctionParamSpec("exception_message", optional=True),
)

filtermap_signature = FunctionSignature(
    FunctionParamSpec("expr"), FunctionParamSpec("filter")
)
"""
Signature for ".[i]filtermap".
"""

get_signature = FunctionSignature(
    FunctionParamSpec("symbol"),
    FunctionParamSpec("lookup", optional=True, keyword_only=True),
    FunctionParamSpec("from", optional=True, keyword_only=True),
    FunctionParamSpec("categories", optional=True, keyword_only=True),
)
"""
Signature for ".get"/".get_first".
"""

is_visible_from_signature = FunctionSignature(FunctionParamSpec("unit"))
"""
Signature for ".is_visible_from".
"""

join_signature = FunctionSignature(FunctionParamSpec("strings"))
"""
Signature for ".join".
"""

logic_all_any_signature = FunctionSignature(positional_variadic=True)
"""
Signature for "%all" and for "%any".
"""

predicate_signature = FunctionSignature(
    FunctionParamSpec("pred_prop"),
    FunctionParamSpec("node"),
    positional_variadic=True,
)
"""
Signature for "%predicate".
"""

propagate_signature = FunctionSignature(
    FunctionParamSpec("dest"),
    FunctionParamSpec("comb_prop"),
    positional_variadic=True,
)
"""
Signature for "%propagate".
"""

rebind_env_signature = FunctionSignature(FunctionParamSpec("env"))
"""
Signature for ".rebind_env".
"""

reference_signature = FunctionSignature(
    FunctionParamSpec("nodes"),
    FunctionParamSpec("resolver"),
    FunctionParamSpec("kind", optional=True, keyword_only=True),
    FunctionParamSpec("dest_env", optional=True, keyword_only=True),
    FunctionParamSpec("cond", optional=True, keyword_only=True),
    FunctionParamSpec("category", optional=True, keyword_only=True),
    FunctionParamSpec(
        "shed_corresponding_rebindings", optional=True, keyword_only=True
    ),
)
"""
Signature for the "reference" env action.
"""

set_initial_env_signature = FunctionSignature(FunctionParamSpec("env"))
"""
Signature for the "set_initial_env" env action.
"""

shed_rebindings_signature = FunctionSignature(FunctionParamSpec("entity_info"))
"""
Signature for ".shed_rebindings".
"""
