## vim: filetype=makoada

<%def name="start_scope(scope)">
   ${gdb_helper('scope-start')}
</%def>

<%def name="finalize_scope(scope)">
   ${gdb_helper('end')}
   % if scope.has_refcounted_vars():
      ${scope.finalizer_name};
   % endif
</%def>
