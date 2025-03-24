"""
Check that precise types for fields of synthetized nodes account for types
coming from synthetization properties.
"""


def main(ctx):
    nodes = {n.dsl_name: n for n in ctx.node_types}

    for node_name in ['SynthNode', 'AbstractHolder', 'AbstractManyHolder']:
        node = nodes[node_name]
        fields = {f.original_name: f for f in node.get_fields()}
        f = fields['f']

        if not f.type.is_list_type:
            print('Precise types for {}:'.format(f.qualname))
            for t in f.precise_types.minimal_matched_types:
                print('  * {}'.format(t.dsl_name))
        else:
            print('Precise elements types for {}:'.format(f.qualname))
            for t in f.precise_element_types.minimal_matched_types:
                print('  * {}'.format(t.dsl_name))
