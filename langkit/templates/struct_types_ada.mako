## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<%def name="public_incomplete_decl(cls)">

   type ${cls.name()};
   ${ada_doc(cls, 3)}

</%def>

<%def name="public_decl(cls)">

   <%
      fields = cls.get_fields(include_inherited=False)
      ext = ctx.ext("nodes", cls.name(), "components")
   %>

   type ${cls.name()} is record
      % for f in fields:
      ${f.name} : aliased ${decl_type(f.type)}
         := ${f.type.nullexpr()};
       ${ada_doc(f, 6)}
      % endfor
      ${exts.include_extension(ext)}
      Is_Null : Boolean := True;
   end record
     with Convention => C;
   ${cls.nullexpr()} : constant ${cls.name()} := (others => <>);
</%def>


<%def name="body(cls)">

   % for prop in cls.get_properties(include_inherited=False):
   ${prop.prop_def}
   % endfor

</%def>
