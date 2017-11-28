## vim: filetype=makoada

<%def name="start_scope(scope)">
   ${gdb_scope_start()}
</%def>

<%def name="finalize_scope(scope)">
   ${gdb_end()}
   % if scope and scope.has_refcounted_vars():
      ${scope.finalizer_name};
   % endif
</%def>
