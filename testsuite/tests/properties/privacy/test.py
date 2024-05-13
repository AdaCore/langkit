fmt_privacy = {
    None: 'default',
    True: 'public',
    False: 'private',
}


def main(ctx):
    if ctx is None:
        print("Compilation failed")
    else:
        for node in ctx.astnode_types:
            if node.kwless_raw_name.camel in ("AbstractNode", "ConcreteNode"):
                for name, fld in sorted(node._fields.items()):
                    if name == "prop":
                        print(
                            "  {}: {}".format(
                                fld.qualname,
                                fmt_privacy[fld.original_is_public]
                            )
                        )
    print("")
