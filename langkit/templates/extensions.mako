## vim: filetype=makoada

## Helper def to include an extension iff it exists on disk. It doesn't
## directly call compilectx.ext, because it is useful to be able to call it
## from the outside, to change the shape of the surrounding code depending on
## the existence of the extension.
<%def name="include_extension(ext)">
   % if ext:
      <%include file='${ext}'/>
   % endif
</%def>

## Helper to generate a list of Ada WITH clauses.
##
## `packages` is a list of tuples: (package name, use clause?). We generate one
## WITH clause for each package name, and the corresponding USE clause if the
## second tuple member is true.
<%def name="with_clauses(clauses)">
   % for pkg, clauses_info in sorted(clauses.items()):
      ${'private' if clauses_info.is_private else ''} with ${pkg};
      % if clauses_info.use_clause:
         use ${pkg};
      % endif
   % endfor
</%def>
