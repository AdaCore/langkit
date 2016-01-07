## vim: filetype=makoada

<%def name="decl(cls)">

type ${cls.c_type(capi).name}_Ptr is
   access ${cls.name().camel_with_underscores};

</%def>
