<% rules = tm_settings.all_rules %>
{
    "scopeName": "source.${ctx.config.library.language_name.lower}",
    "patterns": [
        %for i, r in enumerate(rules):
        { "include": "#${r.identifier}" }${',' if i < len(rules) - 1 else ''}
        %endfor
    ],
    "repository": {
        %for i, r in enumerate(rules):
        "${r.identifier}": ${r.render()}${',' if i < len(rules) - 1 else ''}
        %endfor
    }
}
