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


def print_struct(context, file, struct):
    is_astnode = struct.is_ast_node()
    base = struct.base() if is_astnode else None
    fields = list(struct.get_abstract_fields())

    # Do not document internal fields unless everything is public
    if not context.library_fields_all_public:
        fields = [f for f in fields if not f.is_internal]

    descr = []
    if is_astnode:
        kind = 'node'
        if struct.abstract:
            descr.append('<span class="kw">abstract</span>')
        descr.append('<span class="kw">root</span>'
                     if base == compiled_types.ASTNode else
                     astnode_ref(base))
    else:
        kind = 'struct'
    print >> file, (
        '<dt id="{name}">'
        '<span class="kw">{kind}</span>'
        ' <span class="def">{name}</span>'
        '{descr}</dt>'.format(
            name=struct.name().camel,
            kind=kind,
            descr=' : {}'.format(' '.join(descr)) if descr else ''
        )
    )
    print >> file, '<dd>{}'.format(format_doc(struct))

    print >> file, '<dl>'
    for f in fields:
        print_field(context, file, struct, f)

    print >> file, '</dl>'
    print >> file, '</dd>'


def print_field(context, file, struct, field):
    prefixes = []
    if field.is_private:
        prefixes.append('<span class="priv">private</span>')
    prefixes.append('<span class="kw">{}</span>'.format(
        'field'
        if isinstance(field, compiled_types.Field) else
        'property'
    ))

    inherit_note = (
        '' if field.struct == struct else
        ' [inherited from {}]'.format(field_ref(field))
    )

    print >> file, (
        '<dt>{prefixes}'
        ' <span class="def" id="{node}-{field}">{field}</span>'
        ' : {type}{inherit_note}</dt>'.format(
            prefixes=' '.join(prefixes),
            node=struct.name().camel,
            field=field.name.lower,
            type=(
                astnode_ref(field.type)
                if field.type in context.astnode_types else
                field.type.name().camel
            ),
            inherit_note=inherit_note
        )
    )
    # Don't repeat the documentation for inheritted fields
    if field.struct == struct:
        print >> file, '<dd>{}</dd>'.format(format_doc(field))


def astnode_ref(node):
    return '<a href="#{name}" class="ref-link">{name}</a>'.format(
        name=node.name().camel
    )


def field_ref(field):
    return '<a href="#{node}-{field}" class="ref-link">{node}</a>'.format(
        node=field.struct.name().camel,
        field=field.name.lower
    )


ASTDOC_CSS = """
html {
    background-color: rgb(8, 8, 8);
    color: rgb(248, 248, 242);
    font-family: sans-serif;
}
.kw  { color: rgb(255, 95, 135); font-weight: bold; }
.priv  { color: rgb(255, 130, 20); font-weight: bold; }
.def { color: rgb(138, 226, 52); font-weight: bold; }
.ref { color: rgb(102, 217, 239); }
.ref-link { color: rgb(102, 217, 239); text-decoration: underline; }
.disabled { color: rgb(117, 113, 94); font-style: italic;}
dt { font-family: monospace; }
""".strip()


ASTDOC_HTML = """<html>
<head>
    <title>{lang_name} - AST documentation</title>
    <style type="text/css">{css}</style>
</head>
<body>
    <h1>{lang_name} - AST documentation</h1>
"""


def write_astdoc(context, file):
    """
    Generate a synthetic HTML documentation about AST nodes and types.

    :param context: Compile context to provide the AST nodes to document.
    :type context: langkit.compile_context.CompileCtx

    :param file file: Output file for the documentation.
    """
    print >> file, ASTDOC_HTML.format(
        lang_name=context.lang_name.camel,
        css=ASTDOC_CSS
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

    if context.struct_types:
        print >> file, '<h2>Structure types</h2>'

        print >> file, '<dl>'
        for struct_type in context.struct_types:
            print_struct(context, file, struct_type)
        print >> file, '</dl>'

    print >> file, '<h2>AST node types</h2>'

    print >> file, '<dl>'
    for typ in context.astnode_types:
        print_struct(context, file, typ)
    print >> file, '</dl>'
    print >> file, '</body></html>'
