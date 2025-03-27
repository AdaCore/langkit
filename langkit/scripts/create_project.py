import argparse
import os

import langkit.names as names


parser = argparse.ArgumentParser(
    description="Generate a skeleton project for a Langkit-based language"
    " engine"
)
parser.add_argument(
    "language", help="Name of the target language (camel case)"
)


def generate(lang_name: str) -> None:
    lname = names.Name(lang_name)

    dir_name = lname.lower
    templates = {
        "langkit.yaml": yaml_template,
        os.path.join(dir_name, "tokens.lkt"): tokens_template,
        os.path.join(dir_name, "parser.lkt"): parser_template,
        os.path.join(dir_name, "nodes.lkt"): nodes_template,
    }
    template_args = {
        "lang_name": lname.camel_with_underscores,
        "lang_name_lower": lname.lower,
        "lexer": f"{lname.lower}_lexer",
        "grammar": f"{lname.lower}_grammar",
        "root_node": f"{lname.camel}Node",
    }

    # Create the directory and all the initial files
    if not os.path.exists(dir_name):
        os.mkdir(dir_name)
    for filename, template in templates.items():
        with open(filename, "w") as f:
            f.write(template.format(**template_args))


yaml_template = """\
lkt_spec:
  entry_point: {lang_name_lower}/nodes.lkt
  source_dirs:
    - {lang_name_lower}/

library:
  language_name: {lang_name}
"""

tokens_template = """\
lexer {lexer} {{
    Example <- "example"
}}
"""

parser_template = """\
@with_lexer({lexer})
grammar {grammar} {{
    @main_rule main_rule <- ExampleNode("example")
}}
"""

nodes_template = """\
import parser
import tokens


|" Root node class for {lang_name} nodes.
class {root_node} implements Node[{root_node}] {{
}}

|" Example node.
class ExampleNode: {root_node} implements TokenNode {{
}}
"""


def main(argv: list[str] | None = None) -> None:
    generate(parser.parse_args(argv).language)


if __name__ == "__main__":
    main()
