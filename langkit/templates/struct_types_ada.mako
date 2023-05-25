## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<%def name="create_prototype(cls)">
   <%
      args = []
      for f in cls.get_fields():
         arg_name = f.name
         arg_type = str(f.public_type.api_name)
         if f.public_type.is_entity_type:
            arg_type += "'Class"
         args.append(f"{arg_name} : {arg_type}")
   %>
   function Create_${cls.api_name}
   % if args:
     (${'; '.join(args)})
   % endif
     return ${cls.api_name}
</%def>

<%def name="accessor_prototype(cls, f)">
   function ${f.name}
     (Self : ${cls.api_name})
      return ${f.public_type.api_name}${(
         "'Class" if f.public_type.is_entity_type else '')}
</%def>

<%def name="public_api_decl(cls)">
   type ${cls.api_name} is private;
   ${ada_doc(cls, 3)}

   % for f in cls.get_fields():
      ${accessor_prototype(cls, f)};
      ${ada_doc(f, 6)}
   % endfor

   ${create_prototype(cls)};
</%def>

<%def name="public_api_private_decl(cls)">
   type ${cls.public_record_type} is limited record
      % for f in cls.get_fields():
      Internal_${f.name} : ${(f.public_type.api_access_name
                              if f.public_type.public_requires_boxing else
                              f.public_type.api_name)};
      % endfor

      Refcount : Positive;
   end record;

   function Refcount (Self : ${cls.public_record_type}) return Positive;
   procedure Set_Refcount
     (Self : in out ${cls.public_record_type}; Count : Positive);
   procedure Release (Self : in out ${cls.public_record_type})
      % if not cls.contains_boxed_fields:
         is null
      % endif
   ;

   package Boxed_${cls.api_name} is new Langkit_Support.Boxes
     (${cls.public_record_type}, Refcount, Set_Refcount, Release);

   type ${cls.api_name} is new Boxed_${cls.api_name}.Reference;

</%def>

<%def name="ada_api_converters_decl(cls)">
   % if cls.to_public_converter_required:
      function ${cls.to_public_converter}
        (Value : ${cls.name}) return ${cls.api_name};
   % endif

   % if cls.to_internal_converter_required:
      function ${cls.to_internal_converter}
        (Value : ${cls.api_name}) return ${cls.name};
   % endif
</%def>

<%def name="public_api_body(cls)">

   % for f in cls.get_fields():
      ${accessor_prototype(cls, f)} is
         Record_Ref : constant Boxed_${cls.api_name}.Element_Access :=
            Internal_Access (Self);
      begin
         % if f.type.is_big_integer_type:
            return Result : Big_Integer do
               Result.Set (Record_Ref.Internal_${f.name});
            end return;
         % else:
            return Record_Ref.Internal_${f.name}
               % if f.type.public_requires_boxing:
                  .all
               % endif
            ;
         % endif
      end;
   % endfor

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : ${cls.public_record_type}) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out ${cls.public_record_type}; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

   % if cls.contains_boxed_fields:
      -------------
      -- Release --
      -------------

      procedure Release (Self : in out ${cls.public_record_type}) is
         <% boxed_fields = [f for f in cls.get_fields()
                            if f.type.public_requires_boxing] %>
      begin
         % for f in boxed_fields:
            Free (Self.Internal_${f.name});
         % endfor
      end Release;
   % endif

   % if cls.to_public_converter_required:
      function ${cls.to_public_converter}
        (Value : ${cls.name}) return ${cls.api_name}
      is
         Result : constant ${cls.api_name} :=
            ${cls.api_name} (Boxed_${cls.api_name}.Create_Element);
         Record_Ref : constant Boxed_${cls.api_name}.Element_Access :=
            Internal_Access (Result);
      begin
         % for f in cls.get_fields():
            <%
               dest_expr = 'Record_Ref.Internal_{}'.format(f.name)
               convert_expr = f.type.to_public_expr('Value.{}'.format(f.name))
            %>
            % if f.type.is_big_integer_type:
               ${dest_expr}.Set (${convert_expr});
            % elif f.type.is_string_type or f.type.is_array_type:
               ${dest_expr} := new ${f.type.api_name}'(${convert_expr});
            % else:
               ${dest_expr} := ${convert_expr};
            % endif
         % endfor
         return Result;
      end;
   % endif

   % if cls.to_internal_converter_required:
      function ${cls.to_internal_converter}
        (Value : ${cls.api_name}) return ${cls.name}
      is
         Record_Ref : constant Boxed_${cls.api_name}.Element_Access :=
            Internal_Access (Value);
         Result     : ${cls.name}${(
             f" := {cls.nullexpr};"
             if cls.is_empty else
             ";"
         )}
      begin
         % for f in cls.get_fields():
            <%
               dest_expr = 'Result.{}'.format(f.name)
               convert_expr = f.type.to_internal_expr(
                  'Record_Ref.Internal_{}{}'.format(
                     f.name, '.all' if f.type.public_requires_boxing else ''
                  )
               )
            %>
            ${dest_expr} := ${convert_expr};
         % endfor
         return Result;
      end;
   % endif

   ${create_prototype(cls)} is
      Result     : constant ${cls.api_name} :=
         ${cls.api_name} (Boxed_${cls.api_name}.Create_Element);
      Record_Def : constant Boxed_${cls.api_name}.Element_Access :=
         Internal_Access (Result);
   begin
      % for f in cls.get_fields():
         <% field_expr = 'Record_Def.Internal_{}'.format(f.name) %>
         % if f.type.is_big_integer_type:
            ${field_expr}.Set (${f.name});
         % elif f.type.is_array_type or f.type.is_string_type:
            ${field_expr} := new ${f.type.api_name}'(${f.name});
         % elif f.type.is_entity_type or f.type.is_ast_node:
            ${field_expr} := ${f.name}.As_${f.public_type.api_name};
         % else:
            ${field_expr} := ${f.name};
         % endif
      % endfor
      return Result;
   end;
</%def>

<%def name="incomplete_decl(cls)">
   % if not cls.is_predeclared:
      type ${cls.name};
      ${ada_doc(cls, 3)}
   % endif
</%def>

<%def name="decl(cls, incomplete_nullexpr=True)">

   % if not cls.is_predeclared:
      <%
         fields = cls.get_fields(include_inherited=False)
         ext = ctx.ext("nodes", cls.name, "components")
         extensions = exts.include_extension(ext)
      %>
      type ${cls.name} is record

         % if fields or extensions:
            % for f in fields:
               ${f.name} : aliased ${f.type.storage_type_name};
               ${ada_doc(f, 12)}
               ${extensions}
            % endfor
         % else:
            Dummy : Character;
         % endif
      end record
        with Convention => C;
      % if incomplete_nullexpr:
      ${cls.nullexpr} : constant ${cls.name};
      % endif
   % endif

   % if cls.is_refcounted:
      procedure Inc_Ref (R : ${cls.name});
      procedure Dec_Ref (R : in out ${cls.name});
   % endif

   % if cls.is_entity_type:
      function ${cls.constructor_name}
        (Node : ${cls.element_type.name}; Info : ${T.entity_info.name})
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

<%def name="nullexpr_decl(cls)">
    % if not cls.is_predeclared:
      <%
         fields = cls.get_fields(include_inherited=False)
         ext = ctx.ext("nodes", cls.name, "components")
         extensions = exts.include_extension(ext)
      %>

      ${cls.nullexpr} : constant ${cls.name} :=
      % if fields or extensions:
      (
            % for f in fields:
               ${f.name} => ${f.type.nullexpr}${", " if not loop.last else ""}
            % endfor
      );
      % else:
      (Dummy => Character'Val (0));
      % endif
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

      function ${cls.constructor_name}
        (Node : ${cls.element_type.name}; Info : ${T.entity_info.name})
         return ${cls.name} is
      begin
         if Node = null then
            return ${cls.nullexpr};
         end if;
         return (Node => Node, Info => Info);
      end;

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

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : ${cls.name}) return String is
         pragma Warnings (On, "referenced");
      begin
         % if cls.is_entity_type:
            return Image (Entity'(Node => R.Node, Info => R.Info));
         % else:
            return
              ("("
               % if cls.is_empty:
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

      pragma Warnings (Off, "referenced");
      function Hash (R : ${cls.name}) return Hash_Type is
         pragma Warnings (On, "referenced");
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
