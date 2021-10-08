import os.path
import sys

import mako.exceptions
from mako.lookup import TemplateLookup

from langkit.common import ascii_repr
from langkit.diagnostics import DiagnosticError
from langkit.names import Name


class Renderer:

    def __init__(self, template_env=None, **kwargs):
        self.env = dict(template_env or {})
        self.env.update(kwargs)
        self.env.update({
            'ascii_repr': ascii_repr,
            'Name': Name,
        })

    def update(self, env):
        return Renderer(self.env, **env)

    def render(self, template_name, env=None, **kwargs):
        env = dict(env or {})
        env.update(kwargs)
        return self.update(env)._render(template_name)

    def _render(self, template_name):
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


_template_dirs = []
_template_lookup = None
":type: mako.utils.TemplateLookup"


def add_template_dir(path):
    global _template_lookup
    _template_dirs.append(path)
    _template_lookup = TemplateLookup(directories=_template_dirs,
                                      strict_undefined=True)


add_template_dir(os.path.join(os.path.dirname(os.path.realpath(__file__)),
                              'templates'))


def mako_template(file_name):
    return _template_lookup.get_template("{}.mako".format(file_name))
