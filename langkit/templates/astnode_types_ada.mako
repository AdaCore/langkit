## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />


<%def name="public_incomplete_decl(cls)">

   type ${cls.name()}_Type;
   type ${cls.name()} is access all ${cls.name()}_Type'Class;
   ${ada_doc(cls, 3)}

</%def>


<%def name="public_decl(cls)">

   <%
      type_name = "{}_Type".format(cls.name())
      base_name = cls.base().name()
   %>

   --
   --  ${cls.name()}
   --

   type ${type_name} is ${"abstract" if cls.abstract else "" }
      new ${base_name}_Type with private;

   % if not cls.abstract:

      overriding
      function Kind (Node : access ${type_name}) return ${root_node_kind_name};
      overriding
      function Kind_Name (Node : access ${type_name}) return String;
      overriding
      function Image (Node : access ${type_name}) return String;


      overriding
      function Child_Count (Node : access ${type_name}) return Natural;
      overriding
      procedure Get_Child (Node  : access ${type_name};
                           Index : Natural;
                           Exists : out Boolean;
                           Result : out ${root_node_type_name});

      overriding
      procedure Print (Node  : access ${type_name};
                       Level : Natural := 0);
      overriding
      procedure Validate (Node   : access ${type_name};
                          Parent : ${root_node_type_name} := null);

      overriding
      function Lookup_Children (Node : access ${type_name};
                                Sloc : Source_Location;
                                Snap : Boolean := False)
        return ${root_node_type_name};

      % if cls.env_spec:
      overriding function Do_Env_Actions
        (Self       : access ${type_name};
         Parent_Env : in out AST_Envs.Lexical_Env) return AST_Envs.Lexical_Env;
      % endif

      overriding procedure Destroy
        (Node : access ${cls.name()}_Type);
   % endif

   ## Attribute getters

   % for field in cls.get_fields(include_inherited=False):
       function ${field.name}
         (Node : ${cls.name()}) return ${decl_type(field.type)};
       ${ada_doc(field, 6)}
   % endfor

   % for prop in cls.get_properties(include_inherited=False):
   ${prop.prop_decl}
   % endfor

</%def>


<%def name="private_decl(cls)">

   <%
      type_name = "{}_Type".format(cls.name())
      base_name = cls.base().name()
      fields = cls.get_fields(include_inherited=False)
      ext = ctx.ext("nodes", cls.name(), "components")
   %>

   type ${type_name} is ${"abstract" if cls.abstract else "" }
      new ${base_name}_Type with

   % if fields or ext:
      record
       % for f in fields:
            ${f.name} : aliased ${decl_type(f.type)}
               := ${f.type.nullexpr()};
            ${ada_doc(f, 12)}
       % endfor
         ${exts.include_extension(ext)}
      end record;
   % else:
      null record;
   % endif

   % if not cls.abstract:
      package ${cls.name()}_Alloc is
        new Tagged_Alloc (${type_name});
   % endif

</%def>


<%def name="body(cls)">

   --
   --  Primitives for ${cls.name()}
   --

   <%
   # Keep a list of ASTNode fields
   astnode_fields = cls.get_parse_fields(lambda f: is_ast_node(f.type))

   # Keep a list of fields that are annotated with repr
   repr_fields = cls.get_parse_fields(lambda f: f.repr)

   # Shortcut for ${cls.name()}_Type
   type_name = '{}_Type'.format(cls.name())
   %>

   % if not cls.abstract:

      ----------
      -- Kind --
      ----------

      overriding
      function Kind
        (Node : access ${type_name})
         return ${root_node_kind_name}
      is
         pragma Unreferenced (Node);
      begin
         return ${cls.name()}_Kind;
      end Kind;

      ---------------
      -- Kind_Name --
      ---------------

      overriding
      function Kind_Name (Node : access ${type_name}) return String is
         pragma Unreferenced (Node);
      begin
         return "${cls.repr_name()}";
      end Kind_Name;

      -----------
      -- Image --
      -----------

      overriding
      function Image (Node : access ${type_name}) return String is
         Result : Unbounded_String;
      begin
         Append (Result, Kind_Name (Node));
         Append (Result, '[');
         Append (Result, Image (Sloc_Range (${root_node_type_name} (Node))));
         Append (Result, "](");

         % for i, field in enumerate(repr_fields):
             % if i > 0:
                 Append (Result, ", ");
             % endif

             % if field.type.is_ptr:
                 if Node.${field.name} /= null then
             % endif

             % if is_ast_node(field.type):
                Append (Result,
                        Image (${root_node_type_name} (Node.${field.name})));
             % else:
                Append (Result, Image (Node.${field.name}));
             % endif

             % if field.type.is_ptr:
                 else
                    Append (Result, "None");
                 end if;
             % endif
         % endfor

         Append (Result, ')');
         return To_String (Result);
      end Image;

      -----------------
      -- Child_Count --
      -----------------

      overriding
      function Child_Count (Node : access ${type_name}) return Natural is
         pragma Unreferenced (Node);
      begin
         return ${len(astnode_fields)};
      end Child_Count;

      ---------------
      -- Get_Child --
      ---------------

      overriding
      procedure Get_Child (Node   : access ${type_name};
                           Index  : Natural;
                           Exists : out Boolean;
                           Result : out ${root_node_type_name}) is
         ## Some ASTnodes have no ASTNode child: avoid the "unused parameter"
         ## compilation warning for them.
         % if not astnode_fields:
             pragma Unreferenced (Node);
             pragma Unreferenced (Result);
         % endif
      begin
         case Index is
             % for i, field in enumerate(astnode_fields):
                 when ${i} =>
                     Result := ${root_node_type_name} (Node.${field.name});
                     Exists := True;
             % endfor
             when others =>
                Exists := False;
                Result := null;
         end case;
      end Get_Child;

      -----------
      -- Print --
      -----------

      overriding
      procedure Print (Node  : access ${type_name};
                       Level : Natural := 0)
      is
         Nod : constant ${root_node_type_name} :=
            ${root_node_type_name} (Node);
      begin
         Put_Line
           (Level, Kind_Name (Nod) & "[" & Image (Sloc_Range (Nod)) & "]");

         % for i, field in enumerate(repr_fields):
            % if field.type.is_ptr:
               if Node.${field.name} /= null
                  and then not Is_Empty_List (Node.${field.name})
               then
                  Put_Line (Level + 1, "${field._name.lower}:");
                  Node.${field.name}.Print (Level + 2);
               end if;
            % else:
               Put_Line (Level + 1, "${field._name.lower}: "
                         & Image (Node.${field.name}));
            % endif
         % endfor

      end Print;

      --------------
      -- Validate --
      --------------

      overriding
      procedure Validate (Node   : access ${type_name};
                          Parent : ${root_node_type_name} := null)
      is
      begin
         if Node.Parent /= Parent then
            raise Program_Error;
         end if;

         % for field in astnode_fields:
            if Node.${field.name} /= null then
               Node.${field.name}.Validate (${root_node_type_name} (Node));
            end if;
         % endfor
      end Validate;

      overriding procedure Destroy
        (Node : access ${cls.name()}_Type)
      is
      begin
         ## When no extension is registered, we don't need to recurse on the
         ## tree at all.
         ## TODO: this is wrong: leaf nodes can have extensions even if their
         ## parents don't.
         if Langkit_Support.Extensions.Has_Extensions then
            Node.Free_Extensions;
            % for i, field in enumerate(astnode_fields):
               if Node.${field.name} /= null then
                  Destroy (Node.${field.name});
               end if;
            % endfor
         end if;
      end Destroy;

      ---------------------
      -- Lookup_Children --
      ---------------------

      overriding
      function Lookup_Children (Node : access ${type_name};
                                Sloc : Source_Location;
                                Snap : Boolean := False)
        return ${root_node_type_name}
      is
         ## For this implementation helper (i.e. internal primitive), we can
         ## assume that all lookups fall into this node's sloc range.

         Nod : constant ${root_node_type_name} :=
            ${root_node_type_name} (Node);
         pragma Assert (Compare (Sloc_Range (Nod, Snap), Sloc) = Inside);

         Child : ${root_node_type_name};
         Pos   : Relative_Position;

         ## Some ASTnodes have no ASTNode child: avoid the "unused parameter"
         ## compilation warning for them.
         % if not astnode_fields:
             pragma Unreferenced (Child);
             pragma Unreferenced (Pos);
         % endif

      begin
         ## Look for a child node that contains Sloc (i.e. return the most
         ## precise result).

         % for i, field in enumerate(astnode_fields):
            ## Note that we assume here that child nodes are ordered so
            ## that the first one has a sloc range that is before the
            ## sloc range of the second child node, etc.

            if Node.${field.name} /= null then
               Lookup_Relative (${root_node_type_name} (Node.${field.name}),
                                Sloc, Pos, Child, Snap);
               case Pos is
                  when Before =>
                      ## If this is the first node, Sloc is before it, so
                      ## we can stop here.  Otherwise, Sloc is between the
                      ## previous child node and the next one...  so we can
                      ## stop here, too.
                      return Nod;

                  when Inside =>
                      return Child;

                  when After =>
                      ## Sloc is after the current child node, so see with
                      ## the next one.
                      null;
               end case;
            end if;
         % endfor

         ## If we reach this point, we found no children that covers Sloc,
         ## but Node still covers it (see the assertion).

         return Nod;
      end Lookup_Children;

      % if cls.env_spec:
      --------------------
      -- Do_Env_Actions --
      --------------------

      overriding function Do_Env_Actions
        (Self : access ${type_name};
         Parent_Env : in out AST_Envs.Lexical_Env) return AST_Envs.Lexical_Env
      is
         use AST_Envs;
         use AST_Envs.Lexical_Env_Vectors;
         Ret : Lexical_Env := null;

         Initial_Env : Lexical_Env := Parent_Env;
      begin

         % if cls.env_spec._initial_env:
            Initial_Env := ${cls.env_spec.initial_env};
         % endif

         % if cls.env_spec._add_to_env:

            ## If we have an _add_to_env specification, we generate code to
            ## add elements to the lexical environment.
            declare

               ## We assume the existence of a P_Name property on the result
               ## of the key expression. TODO: Add check for this in
               ## EnvSpec.compute.
               ## Ultimately, _add_to_env will be able to recognize other
               ## return types for the key expression, and handle them
               ## appropriately.
               T : Token :=
                 ${cls.env_spec.add_to_env_key}.P_Name;
            begin

               ## Add a new entry to the lexical env, for which the key is
               ## the symbol for retrieved token, and the value is the
               ## result of the expression for the value.
               Add (Initial_Env, Symbol_Type (T.Text),
                    ${root_node_type_name}
                      (${cls.env_spec.add_to_env_val.strip()}));
            end;
         % endif

         % if cls.env_spec._add_env:
            Ret := AST_Envs.Create (Initial_Env);
            Self.Parent_Env := Ret;
         % endif

         return Ret;
      end Do_Env_Actions;
      % endif
   % endif

   ## Body of attribute getters

   % for field in cls.get_fields(include_inherited=False):
      function ${field.name}
        (Node : ${cls.name()}) return ${decl_type(field.type)}
      is
      begin
         return ${decl_type(field.type)}
           (${type_name} (Node.all).${field.name});
      end ${field.name};
   % endfor

   % for prop in cls.get_properties(include_inherited=False):
   ${prop.prop_def}
   % endfor

</%def>
