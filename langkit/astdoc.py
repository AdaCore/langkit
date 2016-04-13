from __future__ import absolute_import

from langkit import compiled_types, documentation


def write_astdoc(context, file):
    """
    Generate a synthetic text documentation about AST nodes and types.

    :param CompileContext context: Compile contxt from which types to document
        are retreived.
    :param file file: Output file for the documentation.
    """
    i = 0
    for enum_type in context.enum_types:
        i += 1
        print >> file, 'enum {}:'.format(enum_type.name().camel)
        doc = enum_type.doc()
        if doc:
            print >> file, documentation.format_text(doc, 4)
        print >> file, '    {}'.format(
            ' '.join(enum_type.alternatives)
        )
        print >> file, ''

    if i > 0:
        print >> file, ''

    i = 0
    for typ in context.astnode_types:
        if i > 0:
            print >> file, ''
        i += 1

        # If this is not ASTNode, get the parent class
        bases = list(typ.get_inheritance_chain())
        base = bases[-2] if len(bases) > 1 else None
        abs_fields = list(typ.get_abstract_fields())

        print >> file, '{}node {}{}{}'.format(
            'abstract ' if typ.abstract else '',
            typ.name().camel,
            '({})'.format(base.name().camel) if base else '',
            ':' if abs_fields else ''
        )
        doc = typ.doc()
        if doc:
            print >> file, documentation.format_text(doc, 4)
            print >> file, ''

        for abs_field in abs_fields:
            prefixes = []
            if abs_field.is_private:
                prefixes.append('private')
            prefixes.append('field'
                            if isinstance(abs_field, compiled_types.Field) else
                            'property')

            inherit_note = (
                '' if abs_field.ast_node == typ else
                ' [inherited from {}]'.format(
                    abs_field.ast_node.name().camel
                )
            )

            print >> file, '    {} {}: {}{}'.format(
                ' '.join(prefixes),
                abs_field.name.lower,
                abs_field.type.name().camel,
                inherit_note
            )
            doc = abs_field.doc()
            if doc:
                print >> file, documentation.format_text(doc, 8)
