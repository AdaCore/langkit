## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<%def name="public_incomplete_decl(cls)">

   type ${cls.name};
   ${ada_doc(cls, 3)}

</%def>

<%def name="public_decl(cls)">

   <%
      fields = cls.get_fields(include_inherited=False)
      ext = ctx.ext("nodes", cls.name, "components")
      extensions = exts.include_extension(ext)
   %>

   type ${cls.name} is record

      % if fields or extensions:
         % for f in fields:
            ${f.name} : aliased ${f.type.storage_type_name}
               := ${f.type.nullexpr};
             ${ada_doc(f, 6)}
            ${extensions}
         % endfor
      % else:
         null;
      % endif
   end record
     with Convention => C;
   ${cls.nullexpr} : constant ${cls.name} :=
   % if fields or extensions:
   (others => <>);
   % else:
   (null record);
   % endif

   % if cls.is_refcounted:
      procedure Inc_Ref (R : ${cls.name});
      procedure Dec_Ref (R : in out ${cls.name});
   % endif

   % if cls.is_entity_type:
      function Create
         (El : ${cls.el_type.name}; Info : Entity_Info)
          return ${cls.name};
   % endif
</%def>


<%def name="body(cls)">

   <% fields = cls.get_fields(include_inherited=False) %>

   % if cls.is_refcounted:

      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : ${cls.name}) is
      begin
         % for f in fields:
            % if f.type.is_refcounted:
               Inc_Ref (R.${f.name});
            % endif
         % endfor
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out ${cls.name}) is
      begin
         % for f in fields:
            % if f.type.is_refcounted:
               Dec_Ref (R.${f.name});
            % endif
         % endfor
      end Dec_Ref;

   % endif

   % if cls.is_entity_type:

      ------------
      -- Create --
      ------------

      function Create
         (El : ${cls.el_type.name}; Info : Entity_Info)
          return ${cls.name} is
       begin
         Inc_Ref (Info.Rebindings);
         return (El => El, Info => Info);
       end Create;

   % endif

</%def>
