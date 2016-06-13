from __future__ import absolute_import

import sys

import mako.exceptions

from langkit import documentation, names
from langkit.common import string_repr, get_type, null_constant
from langkit.diagnostics import DiagnosticError


class TemplateEnvironment(dict):
    """
    Environment that gathers names for template processing.
    Names are associated to values with the attribute syntax.
    """

    def __setattr__(self, name, value):
        self[name] = value

    def __getattr__(self, name):
        return self[name]


class Renderer(object):

    def __init__(self, template_env=None, **kwargs):
        self.env = TemplateEnvironment(template_env or {})
        self.env.update(kwargs)
        # "self" is a reserved name in Mako, so our variables cannot use it.
        # TODO??? don't use "_self" at all in templates. Use more specific
        # names instead.

    def update(self, env):
        return Renderer(self.env, **env)

    def extend(self, **kwargs):
        return Renderer(self.env, **kwargs)

    def render(self, template_name, env=None, **kwargs):
        env = env or {}
        return self.update(
            TemplateEnvironment(
                env.items() + kwargs.items()))._render(template_name)

    def _render(self, template_name):
        try:
            return mako_template(template_name).render(**self.env)
        except DiagnosticError:
            # In the case of DiagnosticErrors, we don't want to show the
            # traceback.
            raise
        except Exception:
            sys.stderr.write('Mako exception:\n{}\n'.format(
                mako.exceptions.text_error_template().render())
            )
            raise


template_lookup = None
":type: mako.utils.TemplateLookup"


def mako_template(file_name):
    return template_lookup.get_template("{}.mako".format(file_name))


common_renderer = Renderer({
    'string_repr':      string_repr,
    'get_type':         get_type,
    'null_constant':    null_constant,
    'Name':             names.Name,

    'ada_doc':          documentation.ada_doc,
    'c_doc':            documentation.c_doc,
    'py_doc':           documentation.py_doc,
    'ada_c_doc':        documentation.ada_c_doc,
})
