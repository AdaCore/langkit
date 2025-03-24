"""
Check precise types for parse fields.
"""


def main(ctx):
    for n in ctx.node_types:
        fields = {f.original_name: f for f in n.get_fields()}
        for _, f in sorted(fields.items()):
            print(f"== Doc for {f.qualname} ==")
            print(f.doc)
            print("")


print('Done')
