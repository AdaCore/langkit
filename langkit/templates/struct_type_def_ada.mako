## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<%
   fields = cls.get_fields(include_inherited=False)
   ext = ctx.ext("nodes", cls.name(), "components")
%>

% if not private_part:
   type ${cls.name()} is record
   % if fields or ext:
      % for f in fields:
      ${f.name} : aliased ${decl_type(f.type)}
         := ${f.type.nullexpr()};
       ${ada_doc(f, 6)}
      % endfor
      ${exts.include_extension(ext)}
   % else:
      null;
   % endif
   end record
     with Convention => C;

   ${cls.nullexpr()} : constant ${cls.name()};
% else:
   ${cls.nullexpr()} : constant ${cls.name()} := (others => <>);
% endif
