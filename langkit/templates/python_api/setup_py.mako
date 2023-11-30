## vim: filetype=makopython

<% pyapi = ctx.python_api_settings %>


from setuptools import setup, find_packages


setup(
    name=${repr(ctx.lib_name.camel)},
    version=${repr(pyapi.pep440_version)},
    packages=[${repr(pyapi.module_name)}],
    package_data={
        ${repr(pyapi.module_name)}:
            ['*.{}'.format(ext) for ext in ('dll', 'so', 'so.*', 'dylib')]
            + ["py.typed"],
    },
    zip_safe=False,
)
