## vim: filetype=makoada

<%def name="finalize_scope(scope)">
   % if scope.has_refcounted_vars():
      ${scope.finalizer_name};
   % endif
</%def>
