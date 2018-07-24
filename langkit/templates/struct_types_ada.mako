## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<%def name="incomplete_decl(cls)">

   type ${cls.name};
   ${ada_doc(cls, 3)}

</%def>

<%def name="decl(cls)">

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

   % if cls.has_equivalent_function:
      function Equivalent (L, R : ${cls.name}) return Boolean;
   % endif

   ${decl_hash(cls)}

   % if ctx.properties_logging:
      function Trace_Image (R : ${cls.name}) return String;
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
         if El = null then
            return ${cls.nullexpr};
         end if;
         return (El => El, Info => Info);
       end Create;

   % endif

   % if cls.has_equivalent_function:

      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : ${cls.name}) return Boolean is
      begin
         return ${(' and then '.join(
            ('Equivalent (L.{}, R.{})'
             if f.type.has_equivalent_function else
             'L.{} = R.{}').format(f.name, f.name)
            for f in cls.get_fields()
         ))};
      end Equivalent;

   % endif

   ${body_hash(cls)}

   % if ctx.properties_logging:
      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (R : ${cls.name}) return String is
      begin
         % if cls.is_entity_type:
            return Trace_Image (Entity'(El   => ${root_node_type_name} (R.El),
                                        Info => R.Info));
         % else:
            return
              ("("
               % if not cls.get_fields():
                  & "null record"
               % else:
                  % for i, f in enumerate (cls.get_fields()):
                     % if i > 0:
                        & ", "
                     % endif
                     & "${f.name} => " & Trace_Image (R.${f.name})
                  % endfor
               % endif
               & ")");
         % endif
      end Trace_Image;
   % endif

</%def>

<%def name="decl_hash(cls)">
   % if cls.requires_hash_function:
      function Hash (R : ${cls.name}) return Hash_Type;
   % endif
</%def>

<%def name="body_hash(cls)">
   % if cls.requires_hash_function:

      ----------
      -- Hash --
      ----------

      function Hash (R : ${cls.name}) return Hash_Type is
      begin
         <%
            fields = cls.get_fields()

            def field_hash(f):
               return 'Hash (R.{})'.format(f.name)
         %>
         % if len(fields) == 0:
            return Initial_Hash;
         % elif len(fields) == 1:
            return ${field_hash(fields[0])};
         % elif len(fields) == 2:
            return Combine
              (${field_hash(fields[0])}, ${field_hash(fields[1])});
         % else:
            return Combine ((${', '.join(field_hash(f) for f in fields)}));
         % endif
      end Hash;
   % endif
</%def>
