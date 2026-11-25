## vim: filetype=makotoml

<%
    pyapi = ctx.python_api_settings
    cfg = ctx.config.library.python_api


    def to_toml(value):
        match value:
            case str():
                result = '"'
                for c in value:
                    c_ord = ord(c)
                    if c in "\"\\":
                        result += "\\" + c
                    elif c_ord in range(0x20, 0x7f):
                        result += c
                    elif c_ord <= 0xffff:
                        result += f"\\u{c_ord:04x}"
                    else:
                        result += f"\\U{c_ord:08x}"
                return result + '"'
            case list():
                return "[" + ", ".join(to_toml(item) for item in value) + "]"
            case dict():
                return (
                    "{"
                    + ", ".join(
                        f"{to_toml(k)} = {to_toml(v)}"
                        for k, v in value.items()
                    )
                    + "}"
                )
            case _:
                raise ValueError(f"unhandled value: {value!r}")


    def people_to_json(p):
        result = {}
        if p.name:
            result["name"] = p.name
        if p.email:
            result["email"] = p.email
        return result


    def people_list(key, pl):
        return (
            f"{key} = {to_toml([people_to_json(p) for p in pl])}"
            if pl
            else ""
        )
%>

[build-system]
requires = ["setuptools"]
build-backend = "setuptools.build_meta"

[project]
name = ${repr(ctx.lib_name.camel)}
version = ${repr(pyapi.pep440_version)}
${people_list("authors", cfg.authors)}
${people_list("maintainers", cfg.maintainers)}
% if cfg.description:
description = ${to_toml(cfg.description)}
% endif
% if readme_basename:
readme = ${to_toml(readme_basename)}
% endif
% if cfg.license:
license = ${to_toml(cfg.license)}
% endif
% if cfg.keywords:
keywords = ${to_toml(cfg.keywords)}
% endif
% if cfg.urls:
urls = ${to_toml(cfg.urls)}
% endif
% if cfg.requires_python:
requires-python = ${to_toml(cfg.requires_python)}
% endif

[tool.setuptools]
packages = [${repr(pyapi.module_name)}]

[tool.setuptools.package-data]
<%
    patterns = (
        ['*.{}'.format(ext) for ext in ('dll', 'so', 'so.*', 'dylib')]
        + ["py.typed"]
    )
%>
${repr(pyapi.module_name)} = [
    % for p in patterns:
    ${repr(p)},
    % endfor
]
