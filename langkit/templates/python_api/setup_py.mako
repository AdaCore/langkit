## vim: filetype=makopython

<%
    name = ctx.python_api_settings.module_name
    if ctx.version:
        version = ctx.version
        if ctx.build_date:
            version += "-" + ctx.build_date
    else:
        version = "0.1"
%>

from setuptools import setup, find_packages


setup(
    name=${repr(ctx.lib_name.camel)},
    version=${repr(version)},
    packages=[${repr(name)}],
    package_data={
        ${repr(ctx.python_api_settings.module_name)}:
            ['*.{}'.format(ext) for ext in ('dll', 'so', 'so.*', 'dylib')]
            + ["py.typed"],
    },
    zip_safe=False,
)
