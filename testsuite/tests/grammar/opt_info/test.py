"""
Test that the is_optional property on parse fields works as expected.
"""


def main(ctx):
    for n in ctx.node_types:
        if n.lkt_name not in {
            "ParserTest",
            "SynthParent",
            "NullFieldParent",
            "OptionalByAnnotation",
        }:
            continue
        for field in n.get_parse_fields():
            print(
                "Field {} is {}".format(
                    field.qualname,
                    "optional" if field.nullable else "not optional",
                )
            )
