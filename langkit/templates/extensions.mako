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
## second tuple member is true. For convenience, None items and duplicate
## packages are allowed (and filtered out) in the top-level list.
<%def name="with_clauses(packages)">
   % for pkg, use_clause, is_private in sorted(set(packages) - {None}):
      % if pkg:
         ${'private' if is_private else ''} with ${pkg};
         % if use_clause:
            use ${pkg};
         % endif
      % endif
   % endfor
</%def>
