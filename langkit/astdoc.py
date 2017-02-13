from __future__ import absolute_import

from langkit import compiled_types, documentation


def escape(text):
    for char, entity in (
        ('<', '&lt;'), ('>', '&gt;'), ('&', '&amp;')
    ):
        text = text.replace(char, entity)
    return text


def format_doc(entity):
    doc = entity.doc()
    if doc:
        doc = doc.strip().replace('\n', '<br/>')
        doc = '<p class="doc">{}</p>'.format(doc)
    else:
        doc = '<p class="disabled">No documentation</p>'.format(doc)
    return doc


def astnode_ref(node):
    return '<a href="#{name}" class="ref-link">{name}</a>'.format(
        name=node.name().camel
    )


def field_ref(field):
    return '<a href="#{node}-{field}" class="ref-link">{node}</a>'.format(
        node=field.struct.name().camel,
        field=field.name.lower
    )


def write_astdoc(context, file):
    """
    Generate a synthetic HTML documentation about AST nodes and types.

    :param context: Compile context to provide the AST nodes to document.
    :type context: langkit.compile_context.CompileCtx

    :param file file: Output file for the documentation.
    """
    print >> file, '''<html>
<head>
    <title>{lang_name} - AST documentation</title>
    <style type="text/css">{css}</style>
</head>
<body>
    <h1>{lang_name} - AST documentation</h1>'''.format(
        lang_name=context.lang_name.camel,
        css='''
html { background-color: rgb(8, 8, 8); color: rgb(248, 248, 242); }
.kw  { color: rgb(255, 95, 135); font-weight: bold; }
.priv  { color: rgb(255, 130, 20); font-weight: bold; }
.def { color: rgb(138, 226, 52); font-weight: bold; }
.ref { color: rgb(102, 217, 239); }
.ref-link { color: rgb(102, 217, 239); text-decoration: underline; }
.disabled { color: rgb(117, 113, 94); font-style: italic;}
'''
    )

    if context.enum_types:
        print >> file, '<h2>Enumeration types</h2>'

        for enum_type in context.enum_types:
            print >> file, 'enum {}:'.format(enum_type.name().camel)
            doc = enum_type.doc()
            if doc:
                print >> file, documentation.format_text(doc, 4)
            print >> file, '    {}'.format(
                ' '.join(enum_type.alternatives)
            )
            print >> file, ''

    print >> file, '<h2>AST node types</h2>'

    print >> file, '<dl>'
    for typ in context.astnode_types:
        base = typ.base()
        abs_fields = list(typ.get_abstract_fields())

        # Do not document internal fields unless everything is public
        if not context.library_fields_all_public:
            abs_fields = [f for f in abs_fields if not f.is_internal]

        descr = []
        if typ.abstract:
            descr.append('<span class="kw">abstract</span>')
        descr.append('<span class="kw">root</span>'
                     if base == compiled_types.ASTNode else
                     astnode_ref(base))
        print >> file, (
            '<dt id="{name}">'
            '<span class="kw">node</span>'
            ' <span class="def">{name}</span>'
            ' : {descr}</dt>'.format(
                name=typ.name().camel,
                descr=' '.join(descr)
            )
        )
        print >> file, '<dd>{}'.format(format_doc(typ))

        print >> file, '<dl>'
        for abs_field in abs_fields:
            prefixes = []
            if abs_field.is_private:
                prefixes.append('<span class="priv">private</span>')
            prefixes.append('<span class="kw">{}</span>'.format(
                'field'
                if isinstance(abs_field, compiled_types.Field) else
                'property'
            ))

            inherit_note = (
                '' if abs_field.struct == typ else
                ' [inherited from {}]'.format(field_ref(abs_field))
            )

            print >> file, (
                '<dt>{prefixes}'
                ' <span class="def" id="{node}-{field}">{field}</span>'
                ' : {type}{inherit_note}</dt>'.format(
                    prefixes=' '.join(prefixes),
                    node=typ.name().camel,
                    field=abs_field.name.lower,
                    type=(
                        astnode_ref(abs_field.type)
                        if abs_field.type in context.astnode_types else
                        abs_field.type.name().camel
                    ),
                    inherit_note=inherit_note
                )
            )
            # Don't repeat the documentation for inheritted fields
            if abs_field.struct == typ:
                print >> file, '<dd>{}</dd>'.format(format_doc(abs_field))

        print >> file, '</dl>'

        print >> file, '</dd>'

    print >> file, '</dl>'

    print >> file, '</body></html>'
