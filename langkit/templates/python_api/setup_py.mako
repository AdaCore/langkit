## vim: filetype=makopython

from setuptools import setup, find_packages

<% name = ctx.python_api_settings.module_name %>

setup(
    name=${repr(ctx.lib_name.camel)},
    version='0.1',
    packages=[${repr(name)}],
    package_data={
        ${repr(ctx.python_api_settings.module_name)}:
            ['*.{}'.format(ext) for ext in ('dll', 'so', 'dylib')],
    },
    zip_safe=False,
)
