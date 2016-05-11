## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />
<%namespace name="prop_helpers" file="properties/helpers.mako" />


<%def name="public_incomplete_decl(cls)">

   type ${cls.name()}_Type;
   type ${cls.name()} is access all ${cls.name()}_Type'Class;
   ${ada_doc(cls, 3)}

</%def>


<%def name="field_decl(field)">
   <% type_name = "{}_Type".format(field.struct.name()) %>
   function ${field.name}
     (Node : access ${type_name}) return ${field.type.name()};
   ${ada_doc(field, 6)}
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

      overriding procedure Destroy
        (Node : access ${cls.name()}_Type);
   % endif

   ## Public field getters

   % for field in cls.get_fields(include_inherited=False, \
                                 predicate=library_public_field):
      ${field_decl(field)}
   % endfor

   % for prop in cls.get_properties(include_inherited=False, \
                                    predicate=library_public_field):
      ${prop.prop_decl}
   % endfor

</%def>

<%def name="node_fields(cls, emit_null=True)">
   <%
      fields = cls.get_fields(include_inherited=False,
                              predicate=lambda f: f.should_emit)
      ext = ctx.ext("nodes", cls.name(), "components")
   %>
   % if fields or ext:
       % for f in fields:
            ${f.name} : aliased ${f.type.storage_type_name()}
               := ${f.type.storage_nullexpr()};
            ${ada_doc(f, 12)}
       % endfor
       ${exts.include_extension(ext)}
   % elif emit_null:
      null;
   % endif
</%def>

<%def name="private_decl(cls)">

   <%
      type_name = "{}_Type".format(cls.name())
      base_name = cls.base().name()
   %>

   type ${type_name} is ${"abstract" if cls.abstract else "" }
      new ${base_name}_Type with record
      ${node_fields(cls)}
   end record;

   ## Private field getters

   <% library_private_field = lambda f: not library_public_field(f) %>

   % for field in cls.get_fields(include_inherited=False, \
                                 predicate=library_private_field):
      ${field_decl(field)}
   % endfor

   % for prop in cls.get_properties(include_inherited=False, \
                                    predicate=library_private_field):
      ${prop.prop_decl}
   % endfor

   % if not cls.is_env_spec_inherited:

      overriding
      function Do_Env_Actions
        (Self        : access ${type_name};
         Current_Env : in out AST_Envs.Lexical_Env)
         return AST_Envs.Lexical_Env;

      % if cls.env_spec._add_env:
         overriding
         function Node_Env
           (Node : access ${type_name})
            return AST_Envs.Lexical_Env
         is (Node.Self_Env.Parent);
      % endif

   % endif

   % if not cls.abstract:
      overriding
      function Lookup_Children (Node : access ${type_name};
                                Sloc : Source_Location;
                                Snap : Boolean := False)
        return ${root_node_type_name};

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
         return ${cls.ada_kind_name()};
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
             % elif is_token_type(field.type):
                Append (Result, Image (Token (Node, Node.${field.name})));
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

            % elif is_token_type(field.type):
               Put_Line (Level + 1, "${field._name.lower}: "
                         & Image (Token (Node, Node.${field.name})));

            % else:
               Put_Line (Level + 1, "${field._name.lower}: "
                         & Image (Node.${field.name}));
            % endif
         % endfor

      end Print;

      -------------
      -- Destroy --
      -------------

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

   % endif

   % if not cls.is_env_spec_inherited:
   --------------------
   -- Do_Env_Actions --
   --------------------

   overriding
   function Do_Env_Actions
     (Self        : access ${type_name};
      Current_Env : in out AST_Envs.Lexical_Env) return AST_Envs.Lexical_Env
   is
      use AST_Envs;
      use AST_Envs.Lexical_Env_Vectors;

      Ret         : Lexical_Env := null;
      Initial_Env : Lexical_Env := Current_Env;

      <%def name="add_to_env(key, val)">
         ## Add a new entry to the lexical env, for which the key is
         ## the symbol for retrieved token, and the value is the
         ## result of the expression for the value.
         Add (Initial_Env, ${key}, ${val});
      </%def>
   begin

      % if cls.env_spec.initial_env:
         Initial_Env := ${cls.env_spec.initial_env_expr};
      % endif

      % if cls.env_spec.is_adding_to_env:
         ## If we have an _add_to_env specification, we generate code to
         ## add elements to the lexical environment.

         % if is_array_type(cls.env_spec.add_to_env_key.type):
            ## If the supplied expression for the key is an array, we add
            ## a (kn, v) pair for every kn it contains. V stays the same for
            ## every element.
            declare
               Names : ${cls.env_spec.add_to_env_key.type.name()} :=
                  ${cls.env_spec.add_to_env_key_expr};
            begin
               for El of Names.Items loop
                  ${add_to_env("El", cls.env_spec.add_to_env_value_expr)}
               end loop;
               Dec_Ref (Names);
            end;

         % else:
            ## Else, just add (key, val) pair once
            ${add_to_env(cls.env_spec.add_to_env_key_expr,
                         cls.env_spec.add_to_env_value_expr)}
         % endif
      % endif

      % if cls.env_spec._add_env:
         Ret := AST_Envs.Create (Initial_Env, Self, True);
         Register_Destroyable (Self.Unit, Ret);
         Self.Self_Env := Ret;
      % endif

      return Ret;
   end Do_Env_Actions;
   % endif

   ## Body of attribute getters

   % for field in cls.get_fields(include_inherited=False):
      function ${field.name}
        (Node : access ${type_name}) return ${field.type.name()}
      is
      begin
         return ${field.type.extract_from_storage_expr(
                      node_expr='Node',
                      base_expr='Node.{}'.format(field.name)
                  )};
      end ${field.name};
   % endfor

   ## Generate logic/predicate binders for the properties who require it. Note
   ## that we need to generate them before the properties bodies, because
   ## they'll be used in the bodies.

   --------------------
   --  Logic helpers --
   --------------------

   % for prop in cls.get_properties(include_inherited=False):
   ${prop_helpers.generate_logic_binder(prop)}
   ${prop_helpers.generate_logic_predicate(prop)}
   % endfor

   ## Generate the bodies of properties
   % for prop in cls.get_properties(include_inherited=False):
   ${prop.prop_def}
   % endfor

</%def>
