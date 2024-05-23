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


class Renderer:

    def __init__(
        self,
        template_env: dict[str, Any] | None = None,
        **kwargs: Any,
    ):
        self.env = dict(template_env or {})
        self.env.update(kwargs)
        self.env.update({
            'ada_block_with_parens': ada_block_with_parens,
            'ada_enum_type_decl': ada_enum_type_decl,
            'ada_pipe_list': ada_pipe_list,
            'ascii_repr': ascii_repr,
            'bytes_repr': bytes_repr,
            'text_repr': text_repr,
            'Name': Name,
        })

    def update(self, env: dict[str, Any]) -> Renderer:
        return Renderer(self.env, **env)

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
        try:
            return mako_template(template_name).render(**self.env)
        except DiagnosticError:  # no-code-coverage
            # In the case of DiagnosticErrors, we don't want to show the
            # traceback.
            raise
        except Exception:  # no-code-coverage
            sys.stderr.write('Mako exception:\n{}\n'.format(
                mako.exceptions.text_error_template().render())
            )
            raise


_template_dirs: list[str] = []
_template_lookup: mako.utils.TemplateLookup | None = None


def add_template_dir(path: str) -> None:
    global _template_lookup
    _template_dirs.append(path)
    _template_lookup = TemplateLookup(directories=_template_dirs,
                                      strict_undefined=True)


add_template_dir(os.path.join(os.path.dirname(os.path.realpath(__file__)),
                              'templates'))


def mako_template(file_name: str) -> mako.template.Template:
    assert _template_lookup is not None
    return _template_lookup.get_template("{}.mako".format(file_name))
