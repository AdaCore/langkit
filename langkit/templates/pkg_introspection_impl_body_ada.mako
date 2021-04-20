## vim: filetype=makoada

<%def name="return_program_error()">
   pragma Warnings (Off, "value not in range of type");
   return (raise Program_Error);
   pragma Warnings (On, "value not in range of type");
</%def>

package body ${ada_lib_name}.Introspection_Implementation is

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Internal_Value) return Boolean is
   begin
      return Self.Boolean_Value;
   end As_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Internal_Value) return Integer is
   begin
      return Self.Integer_Value;
   end As_Integer;

   ------------------
   -- As_Character --
   ------------------

   function As_Character (Self : Internal_Value) return Character_Type is
   begin
      return Self.Character_Value;
   end As_Character;

   -------------
   -- As_Node --
   -------------

   function As_Node (Self : Internal_Value) return ${T.entity.name} is
   begin
      return Self.Node_Value;
   end As_Node;

   % for enum_type in ctx.enum_types:
      function As_${enum_type.introspection_prefix}
        (Self : Internal_Value) return ${enum_type.api_name} is
      begin
         return Self.${enum_type.introspection_kind};
      end As_${enum_type.introspection_prefix};

   % endfor

   --  Now we can emit descriptor tables

   ----------------------
   -- Struct_Type_Desc --
   ----------------------

   function Struct_Type_Desc
     (Kind : Struct_Value_Kind) return Struct_Type_Descriptor_Access
   is
   begin
      % if ctx.sorted_public_structs:
         case Kind is
         % for t in ctx.sorted_public_structs:
            when ${t.introspection_kind} =>
               return Desc_For_${t.name}'Access;
         % endfor
         end case;

      % else:
         pragma Unreferenced (Kind);
         return (raise Program_Error);
      % endif
   end Struct_Type_Desc;

   -----------------------
   -- Struct_Field_Name --
   -----------------------

   function Struct_Field_Name (Field : Struct_Field_Reference) return Text_Type
   is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return To_Text (Struct_Field_Descriptors (Field).Name);
      pragma Warnings (On, "value not in range of subtype");
   end Struct_Field_Name;

   -----------------------
   -- Struct_Field_Type --
   -----------------------

   function Struct_Field_Type
     (Field : Struct_Field_Reference) return Type_Constraint is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Struct_Field_Descriptors (Field).Field_Type;
      pragma Warnings (On, "value not in range of subtype");
   end Struct_Field_Type;

   -------------------
   -- Struct_Fields --
   -------------------

   pragma Warnings (Off, "referenced");
   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array
   is
      pragma Warnings (On, "referenced");
   begin
      % if ctx.sorted_public_structs:
         declare
            Desc : Struct_Type_Descriptor renames Struct_Type_Desc (Kind).all;
         begin
            return Result : Struct_Field_Reference_Array (Desc.Fields'Range) do
               for I in Result'Range loop
                  Result (I) := Desc.Fields (I).Reference;
               end loop;
            end return;
         end;
      % else:
         return (raise Program_Error);
      % endif
   end Struct_Fields;

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type is
   begin
      return To_Text (To_String (Node_Type_Descriptors (Id).DSL_Name));
   end DSL_Name;

   ---------------------
   -- Lookup_DSL_Name --
   ---------------------

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id is
      use Node_Type_Id_Maps;

      Position : constant Cursor :=
         DSL_Name_To_Node_Type.Find (To_Unbounded_String (Image (Name)));
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return None;
      end if;
   end Lookup_DSL_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Node_Type_Descriptors (Id).Is_Abstract;
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return ${T.node_kind} is
      Desc : Node_Type_Descriptor renames Node_Type_Descriptors (Id).all;
   begin
      if Desc.Is_Abstract then
         raise Bad_Type_Error with "trying to get kind for abstract node";
      end if;
      return Desc.Kind;
   end Kind_For;

   --------------------
   -- First_Kind_For --
   --------------------

   function First_Kind_For (Id : Node_Type_Id) return ${T.node_kind} is

      --  Look for the leftmost leaf derivation of an abstract node. Langkit
      --  disallows abstract nodes with no concrete derivation, so each time we
      --  see an an abstract node, we know there are concrete derivations down
      --  the tree.
      --
      --  Note that we have to stop at the first concrete node we see because
      --  of the way we sort kinds: the kind of concrete root comes before the
      --  kinds of all its derivations.

      Cur : Node_Type_Id := Id;
   begin
      loop
         declare
            Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cur).all;
         begin
            exit when not Desc.Is_Abstract or else Desc.Derivations'Length = 0;
            Cur := Desc.Derivations (Desc.Derivations'First);
         end;
      end loop;
      return Kind_For (Cur);
   end First_Kind_For;

   -------------------
   -- Last_Kind_For --
   -------------------

   function Last_Kind_For (Id : Node_Type_Id) return ${T.node_kind} is

      --  Look for the rightmost leaf derivation. Langkit disallows abstract
      --  nodes with no concrete derivation, so we know that the result is
      --  concrete.

      Cur : Node_Type_Id := Id;
   begin
      loop
         declare
            Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cur).all;
         begin
            exit when Desc.Derivations'Length = 0;
            Cur := Desc.Derivations (Desc.Derivations'Last);
         end;
      end loop;
      return Kind_For (Cur);
   end Last_Kind_For;

   -----------------
   -- Id_For_Kind --
   -----------------

   function Id_For_Kind (Kind : ${T.node_kind}) return Node_Type_Id is
   begin
      return Kind_To_Id (Kind);
   end Id_For_Kind;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Id = ${T.root_node.introspection_name};
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      if Is_Root_Node (Id) then
         raise Bad_Type_Error with "trying to get base type of root node";
      end if;
      return Node_Type_Descriptors (Id).Base_Type;
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Node_Type_Descriptors (Id).Derivations;
   end Derived_Types;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean is
      Cursor : Any_Node_Type_Id := Id;
   begin
      while Cursor /= None loop
         if Cursor = Parent then
            return True;
         end if;

         Cursor := Node_Type_Descriptors (Cursor).Base_Type;
      end loop;
      return False;
   end Is_Derived_From;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name (Member : Member_Reference) return Text_Type is
   begin
      case Member is
         when Struct_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Struct_Field_Name (Member);
            pragma Warnings (On, "value not in range of type");

         when Syntax_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Syntax_Field_Name (Member);
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Property_Name (Member);
      end case;
   end Member_Name;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type (Member : Member_Reference) return Type_Constraint is
   begin
      case Member is
         when Struct_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Struct_Field_Type (Member);
            pragma Warnings (On, "value not in range of type");

         when Syntax_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return (Kind      => Node_Value,
                    Node_Type => Syntax_Field_Type (Member));
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Property_Return_Type (Member);
      end case;
   end Member_Type;

   --------------------------
   -- Lookup_Member_Struct --
   --------------------------

   function Lookup_Member_Struct
     (Kind : Struct_Value_Kind;
      Name : Text_Type) return Any_Member_Reference
   is
      pragma Warnings (Off, "value not in range of type");
      Desc : Struct_Type_Descriptor renames Struct_Type_Desc (Kind).all;
      pragma Warnings (On, "value not in range of type");
   begin
      for F of Desc.Fields loop
         if To_Text (F.Name) = Name then
            return F.Reference;
         end if;
      end loop;

      return None;
   end Lookup_Member_Struct;

   ------------------------
   -- Lookup_Member_Node --
   ------------------------

   function Lookup_Member_Node
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference
   is
      Cursor : Any_Node_Type_Id := Id;
   begin
      --  Go through the derivation chain for Id and look for any field or
      --  property whose name matches Name.

      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cursor).all;
         begin
            for F of Node_Desc.Fields loop
               pragma Warnings (Off, "value not in range of type");
               if Syntax_Field_Name (F.Field) = Name then
                  return F.Field;
               end if;
               pragma Warnings (On, "value not in range of type");
            end loop;

            for P of Node_Desc.Properties loop
               if Property_Name (P) = Name then
                  return P;
               end if;
            end loop;

            Cursor := Node_Desc.Base_Type;
         end;
      end loop;
      return None;
   end Lookup_Member_Node;

   -----------------------
   -- Syntax_Field_Name --
   -----------------------

   function Syntax_Field_Name (Field : Syntax_Field_Reference) return Text_Type
   is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return To_Text (Syntax_Field_Descriptors (Field).Name);
      pragma Warnings (On, "value not in range of subtype");
   end Syntax_Field_Name;

   -----------------------
   -- Syntax_Field_Type --
   -----------------------

   function Syntax_Field_Type
     (Field : Syntax_Field_Reference) return Node_Type_Id is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Syntax_Field_Descriptors (Field).Field_Type;
      pragma Warnings (On, "value not in range of subtype");
   end Syntax_Field_Type;

   -----------------------
   -- Eval_Syntax_Field --
   -----------------------

   function Eval_Syntax_Field
     (Node  : ${T.root_node.name};
      Field : Syntax_Field_Reference) return ${T.root_node.name}
   is
      Kind : constant ${T.node_kind} := Node.Kind;
   begin
      <%
         def get_actions(astnode, node_expr):
            fields = astnode.get_parse_fields(
               predicate=lambda f: not f.overriding,
               include_inherited=False)
            result = []

            if fields:
               result.append('case Field is')
               for f in fields:
                  result.append('when {} => return {} ({});'
                                .format(f.introspection_enum_literal,
                                        f.name,
                                        node_expr))
               result.append('when others => null;')
               result.append('end case;')

            return '\n'.join(result)
      %>
      ${ctx.generate_actions_for_hierarchy('Node', 'Kind', get_actions)}

      ## If we haven't matched the requested field on Node, report an error
      return (raise Bad_Type_Error with "no such field on this node");
   end Eval_Syntax_Field;

   -----------
   -- Index --
   -----------

   function Index
     (Kind : ${T.node_kind}; Field : Syntax_Field_Reference) return Positive is
   begin
      % if ctx.sorted_parse_fields:
         <%
            concrete_astnodes = [n for n in ctx.astnode_types
                                 if not n.abstract]
            def enum_literal(f):
               return (f.overriding or f).introspection_enum_literal
         %>
         case Kind is
            % for n in concrete_astnodes:
               when ${n.ada_kind_name} =>
               return (case Field is
                       % for f in n.get_parse_fields( \
                          predicate=lambda f: not f.null \
                       ):
                       when ${enum_literal(f)} => ${f.index + 1},
                       % endfor
                       when others => raise Bad_Type_Error);
            % endfor
         end case;

      % else:
         return (raise Program_Error);
      % endif
   end Index;

   ---------------------------------------
   -- Syntax_Field_Reference_From_Index --
   ---------------------------------------

   function Syntax_Field_Reference_From_Index
     (Kind : ${T.node_kind}; Index : Positive) return Syntax_Field_Reference is
   begin
      <%
         def get_actions(astnode, node_expr):
            fields = astnode.get_parse_fields(
               predicate=lambda f: not f.abstract and not f.null,
               include_inherited=False
            )
            result = []

            # List types have no field, so just raise an error if a list kind
            # is passed.
            if astnode.is_generic_list_type:
               result.append(
                  'raise Bad_Type_Error with "List AST nodes have no field";'
               )
            elif astnode.is_list:
               pass

            # Otherwise, dispatch on the index to return the corresponding
            # field enum value.
            elif fields:
               result.append('case Index is')
               for f in fields:
                  result.append('when {} => return {};'.format(
                     f.index + 1,
                     (f.overriding or f).introspection_enum_literal
                  ))
               result.append('when others => null;')
               result.append('end case;')

            return '\n'.join(result)
      %>
      ${ctx.generate_actions_for_hierarchy(None, 'Kind', get_actions)}

      pragma Warnings (Off, "value not in range of type");
      return (raise Bad_Type_Error with "Index is out of bounds");
      pragma Warnings (On, "value not in range of type");
   end Syntax_Field_Reference_From_Index;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Kind : ${T.node_kind}) return Syntax_Field_Reference_Array is
   begin
      % if ctx.sorted_parse_fields:
         return Syntax_Fields (Id_For_Kind (Kind), Concrete_Only => True);
      % else:
         ${return_program_error()}
      % endif
   end Syntax_Fields;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Id            : Node_Type_Id;
      Concrete_Only : Boolean) return Syntax_Field_Reference_Array
   is
      Cursor : Any_Node_Type_Id := Id;

      Added_Fields : array (Syntax_Field_Reference) of Boolean :=
        (others => False);
      --  Set of field references that were added to Result

      Result : Syntax_Field_Reference_Array (1 .. Added_Fields'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin
      % if ctx.sorted_parse_fields:

         --  Go through the derivation chain for Id and collect fields. Do
         --  it in reverse order as we process base types last.
         while Cursor /= None loop
            declare
               Node_Desc : Node_Type_Descriptor renames
                  Node_Type_Descriptors (Cursor).all;
            begin
               for Field_Index in reverse Node_Desc.Fields'Range loop
                  declare
                     Field_Desc : Node_Field_Descriptor renames
                        Node_Desc.Fields (Field_Index).all;
                     Field      : Syntax_Field_Reference renames
                        Field_Desc.Field;
                  begin
                     --  Abstract fields share the same Syntax_Field_Reference
                     --  value with the corresponding concrete fields, so
                     --  collect fields only once. We process fields in reverse
                     --  order, so we know that concrete ones will be processed
                     --  before the abstract fields they override.
                     if not (Concrete_Only
                             and then Field_Desc.Is_Abstract_Or_Null)
                        and then not Added_Fields (Field)
                     then
                        Added_Fields (Field) := True;
                        Last := Last + 1;
                        Result (Last) := Field;
                     end if;
                  end;
               end loop;
               Cursor := Node_Desc.Base_Type;
            end;
         end loop;

         --  At this point, Result contains elements in the opposite order as
         --  expected, so reverse it.

         for I in 1 .. Last / 2 loop
            declare
               Other_I : constant Positive := Last - I + 1;
               Swap    : constant Syntax_Field_Reference := Result (I);
            begin
               Result (I) := Result (Other_I);
               Result (Other_I) := Swap;
            end;
         end loop;

         return Result (1 .. Last);

      % else:
         ${return_program_error()}
      % endif
   end Syntax_Fields;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array is
   begin
      return Syntax_Fields (Id, Concrete_Only => False);
   end Syntax_Fields;

   % if ctx.sorted_properties:

   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Property : Property_Reference) return Text_Type is
   begin
      return To_Text (Property_Descriptors (Property).Name);
   end Property_Name;

   --------------------------
   -- Property_Return_Type --
   --------------------------

   function Property_Return_Type
     (Property : Property_Reference) return Type_Constraint is
   begin
      return Property_Descriptors (Property).Return_Type;
   end Property_Return_Type;

   ---------------------------
   -- Check_Argument_Number --
   ---------------------------

   procedure Check_Argument_Number
     (Desc : Property_Descriptor; Argument_Number : Positive) is
   begin
      if Argument_Number not in Desc.Argument_Names'Range then
         raise Property_Error with "out-of-bounds argument number";
      end if;
   end Check_Argument_Number;

   -----------------------------
   -- Property_Argument_Types --
   -----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array is
   begin
      return Property_Descriptors (Property).Argument_Types;
   end Property_Argument_Types;

   ----------------------------
   -- Property_Argument_Name --
   ----------------------------

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type
   is
      Desc : Property_Descriptor renames Property_Descriptors (Property).all;
   begin
      Check_Argument_Number (Desc, Argument_Number);
      return To_Text
        (Property_Descriptors (Property).Argument_Names (Argument_Number).all);
   end Property_Argument_Name;

   -------------------------------------
   -- Property_Argument_Default_Value --
   -------------------------------------

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Internal_Value
   is
      Desc : Property_Descriptor renames Property_Descriptors (Property).all;
   begin
      Check_Argument_Number (Desc, Argument_Number);
      return Desc.Argument_Default_Values (Argument_Number);
   end Property_Argument_Default_Value;

   ----------------
   -- Properties --
   ----------------

   function Properties (Kind : ${T.node_kind}) return Property_Reference_Array
   is
   begin
      return Properties (Id_For_Kind (Kind));
   end Properties;

   ----------------
   -- Properties --
   ----------------

   function Properties (Id : Node_Type_Id) return Property_Reference_Array is
      Cursor : Any_Node_Type_Id := Id;

      Result : Property_Reference_Array (1 .. Property_Descriptors'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin
      --  Go through the derivation chain for Id and collect properties. Do
      --  it in reverse order as we process base types last.

      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cursor).all;
         begin
            for Prop_Desc of reverse Node_Desc.Properties loop
               Last := Last + 1;
               Result (Last) := Prop_Desc;
            end loop;
            Cursor := Node_Desc.Base_Type;
         end;
      end loop;

      --  At this point, Result contains elements in the opposite order as
      --  expected, so reverse it.

      for I in 1 .. Last / 2 loop
         declare
            Other_I : constant Positive := Last - I + 1;
            Swap    : constant Property_Reference := Result (I);
         begin
            Result (I) := Result (Other_I);
            Result (Other_I) := Swap;
         end;
      end loop;

      return Result (1 .. Last);
   end Properties;

   % endif

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : ${T.node_kind}) return Token_Kind is
      <% token_nodes = [n for n in ctx.astnode_types
                        if not n.abstract and n.is_token_node] %>
   begin
      % if ctx.generate_unparser:
         case Kind is
            % for n in token_nodes:
               when ${n.ada_kind_name} =>
                  return ${n.token_kind.ada_name};
            % endfor

            when others =>
               --  Kind is not a token node, and thus the precondition does not
               --  hold.
               return (raise Program_Error);
         end case;

      % else:
         pragma Unreferenced (Kind);
         return (raise Program_Error);
      % endif
   end Token_Node_Kind;

begin
   for D in Node_Type_Descriptors'Range loop
      DSL_Name_To_Node_Type.Insert (Node_Type_Descriptors (D).DSL_Name, D);
   end loop;
end ${ada_lib_name}.Introspection_Implementation;
