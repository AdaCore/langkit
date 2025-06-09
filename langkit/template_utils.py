from __future__ import annotations

import os.path
import sys
from typing import Any

import mako.exceptions
from mako.lookup import TemplateLookup

from langkit.common import (
    ada_block_with_parens,
    ada_enum_type_decl,
    ada_pipe_list,
    ascii_repr,
    bytes_repr,
    text_repr,
)
from langkit.diagnostics import DiagnosticError
from langkit.names import Name


# Root directory for Langkit's builtin templates
builtin_template_dir = os.path.join(
    os.path.dirname(os.path.realpath(__file__)), "templates"
)


def create_template_lookup(directories: list[str]) -> TemplateLookup:
    """
    Wrapper to create a TemplateLookup instance.

    Automatically add Langkit's builtin templates and activate strict behavior
    for undefined variables.
    """
    return TemplateLookup(
        directories=[builtin_template_dir] + directories, strict_undefined=True
    )


class Renderer:

    def __init__(
        self,
        template_lookup: TemplateLookup | None = None,
        template_env: dict[str, Any] | None = None,
        **kwargs: Any,
    ):
        self.template_lookup = template_lookup or create_template_lookup([])
        self.env = dict(template_env or {})
        self.env.update(kwargs)
        self.env.update(
            {
                "ada_block_with_parens": ada_block_with_parens,
                "ada_enum_type_decl": ada_enum_type_decl,
                "ada_pipe_list": ada_pipe_list,
                "ascii_repr": ascii_repr,
                "bytes_repr": bytes_repr,
                "text_repr": text_repr,
                "Name": Name,
            }
        )

    def update(self, env: dict[str, Any]) -> Renderer:
        return Renderer(self.template_lookup, self.env, **env)

    def render(
        self,
        template_name: str,
        env: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> str:
        env = dict(env or {})
        env.update(kwargs)
        return self.update(env)._render(template_name)

    def _render(self, template_name: str) -> str:
        template_filename = f"{template_name}.mako"
        try:
            template = self.template_lookup.get_template(template_filename)
            return template.render(**self.env)
        except DiagnosticError:  # no-code-coverage
            # In the case of DiagnosticErrors, we don't want to show the
            # traceback.
            raise
        except Exception:  # no-code-coverage
            sys.stderr.write(
                "Mako exception:\n{}\n".format(
                    mako.exceptions.text_error_template().render()
                )
            )
            raise
