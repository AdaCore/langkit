## vim: filetype=makoada

with Ada.Unchecked_Conversion;

with ${ada_lib_name}.Common;

with ${ada_lib_name}.Public_Converters; use ${ada_lib_name}.Public_Converters;
with ${ada_lib_name}.Rewriting_Implementation;

package body ${ada_lib_name}.Rewriting is

   package Impl renames ${ada_lib_name}.Rewriting_Implementation;

   function Unwrap_RH is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Impl.Rewriting_Handle);
   function Wrap_RH is new Ada.Unchecked_Conversion
     (Impl.Rewriting_Handle, Rewriting_Handle);

   function Unwrap_Node_RH is new Ada.Unchecked_Conversion
     (Node_Rewriting_Handle, Impl.Node_Rewriting_Handle);
   function Wrap_Node_RH is new Ada.Unchecked_Conversion
     (Impl.Node_Rewriting_Handle, Node_Rewriting_Handle);

   function Unwrap_Unit_RH is new Ada.Unchecked_Conversion
     (Unit_Rewriting_Handle, Impl.Unit_Rewriting_Handle);
   function Wrap_Unit_RH is new Ada.Unchecked_Conversion
     (Impl.Unit_Rewriting_Handle, Unit_Rewriting_Handle);

   function Wrap_Apply_Result
     (Res : Impl.Apply_Result) return Apply_Result;

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array;

   function Wrap_Node_RH_Array
     (Arr : Impl.Node_Rewriting_Handle_Array)
      return Node_Rewriting_Handle_Array;
   function Unwrap_Node_RH_Array
     (Arr : Node_Rewriting_Handle_Array)
      return Impl.Node_Rewriting_Handle_Array;

   function Wrap_Apply_Result
     (Res : Impl.Apply_Result) return Apply_Result is
   begin
      if Res.Success then
         return (Success => True);
      else
         return
           (Success     => False,
            Unit        => Wrap_Unit (Res.Unit),
            Diagnostics => Res.Diagnostics);
      end if;
   end Wrap_Apply_Result;

   ------------------------
   -- Wrap_Unit_RH_Array --
   ------------------------

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array
   is
      Res : Unit_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Wrap_Unit_RH (Arr (I));
      end loop;
      return Res;
   end Wrap_Unit_RH_Array;

   ------------------------
   -- Wrap_Node_RH_Array --
   ------------------------

   function Wrap_Node_RH_Array
     (Arr : Impl.Node_Rewriting_Handle_Array)
      return Node_Rewriting_Handle_Array
   is
      Res : Node_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Wrap_Node_RH (Arr (I));
      end loop;
      return Res;
   end Wrap_Node_RH_Array;

   --------------------------
   -- Unwrap_Node_RH_Array --
   --------------------------

   function Unwrap_Node_RH_Array
     (Arr : Node_Rewriting_Handle_Array)
      return Impl.Node_Rewriting_Handle_Array
   is
      Res : Impl.Node_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Unwrap_Node_RH (Arr (I));
      end loop;
      return Res;
   end Unwrap_Node_RH_Array;

   ------------
   -- Handle --
   ------------

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Handle (Unwrap_Context (Context)));
   end Handle;

   -------------
   -- Context --
   -------------

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
   begin
      return Wrap_Context (Impl.Context (Unwrap_RH (Handle)));
   end Context;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Start_Rewriting (Unwrap_Context (Context)));
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting
     (Handle          : in out Rewriting_Handle)
   is
      Internal_Handle : Impl.Rewriting_Handle := Unwrap_RH (Handle);
   begin
      Impl.Abort_Rewriting (Internal_Handle);
      Handle := Wrap_RH (Internal_Handle);
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is
      Internal_Handle : Impl.Rewriting_Handle := Unwrap_RH (Handle);
      Res             : Impl.Apply_Result := Impl.Apply (Internal_Handle);
   begin
      Handle := Wrap_RH (Internal_Handle);
      return Wrap_Apply_Result (Res);
   end Apply;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array is
   begin
      return Wrap_Unit_RH_Array (Impl.Unit_Handles (Unwrap_RH (Handle)));
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle is
   begin
      return Wrap_Unit_RH (Impl.Handle (Unwrap_Unit (Unit)));
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit is
   begin
      return Wrap_Unit (Impl.Unit (Unwrap_Unit_RH (Handle)));
   end Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Root (Unwrap_Unit_RH (Handle)));
   end Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle) is
   begin
      Impl.Set_Root (Unwrap_Unit_RH (Handle), Unwrap_Node_RH (Root));
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type is
   begin
      return Impl.Unparse (Unwrap_Unit_RH (Handle));
   end Unparse;

   ------------
   -- Handle --
   ------------

   function Handle
     (Node : ${root_entity.api_name}'Class) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Handle (Unwrap_Node (Node)));
   end Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return ${root_entity.api_name} is
   begin
      return Wrap_Node (Impl.Node (Unwrap_Node_RH (Handle)));
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Context (Unwrap_Node_RH (Handle)));
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Impl.Unparse (Unwrap_Node_RH (Handle));
   end Unparse;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return ${T.node_kind} is
   begin
      return Impl.Kind (Unwrap_Node_RH (Handle));
   end Kind;

   -----------
   -- Image --
   -----------

   function Image (Handle : Node_Rewriting_Handle) return String is
   begin
      return Impl.Image (Unwrap_Node_RH (Handle));
   end Image;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return Impl.Tied (Unwrap_Node_RH (Handle));
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Parent (Unwrap_Node_RH (Handle)));
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      return Impl.Children_Count (Unwrap_Node_RH (Handle));
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Child (Unwrap_Node_RH (Handle), Field));
   end Child;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Fields : Struct_Member_Ref_Array) return Node_Rewriting_Handle is
   begin
      return Result : Node_Rewriting_Handle := Handle do
         for F of Fields loop
            Result := Child (Result, F);
         end loop;
      end return;
   end Child;

   --------------
   -- Children --
   --------------

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array
   is
   begin
      return Wrap_Node_RH_Array (Impl.Children (Unwrap_Node_RH (Handle)));
   end Children;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref;
      Child  : Node_Rewriting_Handle)
   is
   begin
      Impl.Set_Child (Unwrap_Node_RH (Handle), Field, Unwrap_Node_RH (Child));
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Impl.Text (Unwrap_Node_RH (Handle));
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin
      Impl.Set_Text (Unwrap_Node_RH (Handle), Text);
   end Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin
      Impl.Replace (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Node));
   end Replace;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Handles : Node_Rewriting_Handle_Array) is
   begin
      Impl.Rotate (Unwrap_Node_RH_Array (Handles));
   end Rotate;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return Impl.Is_List_Node (Unwrap_Node_RH (Handle));
   end Is_List_Node;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.First_Child (Unwrap_Node_RH (Handle)));
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Last_Child (Unwrap_Node_RH (Handle)));
   end Last_Child;

   ----------------
   -- Next_Child --
   ----------------

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Next_Child (Unwrap_Node_RH (Handle)));
   end Next_Child;

   --------------------
   -- Previous_Child --
   --------------------

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Previous_Child (Unwrap_Node_RH (Handle)));
   end Previous_Child;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      Impl.Insert_Before
        (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Sibling));
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      Impl.Insert_After
        (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Sibling));
   end Insert_After;

   ------------------
   -- Insert_First --
   ------------------

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle) is
   begin
      Impl.Insert_First
        (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Child));
   end Insert_First;

   -----------------
   -- Insert_Last --
   -----------------

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle) is
   begin
      Impl.Insert_Last
        (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Child));
   end Insert_Last;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Handle : Node_Rewriting_Handle) is
   begin
      Impl.Remove_Child (Unwrap_Node_RH (Handle));
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Clone (Unwrap_Node_RH (Handle)));
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind}) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_Node (Unwrap_RH (Handle), Kind));
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind};
      Text   : Text_Type) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH
        (Impl.Create_Token_Node (Unwrap_RH (Handle), Kind, Text));
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${T.node_kind};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_Regular_Node
        (Unwrap_RH (Handle), Kind, Unwrap_Node_RH_Array (Children)));
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_From_Template
        (Unwrap_RH (Handle),
         Template,
         Unwrap_Node_RH_Array (Arguments),
         Rule));
   end Create_From_Template;

   % for n in ctx.node_types:
      % if not n.abstract and \
            not n.is_token_node and \
            not n.is_list_type and \
            n.get_parse_fields():

         function Create_${n.entity.api_name}
           (Handle : Rewriting_Handle
            % for f in n.get_parse_fields():
               ; ${f.api_name} : Node_Rewriting_Handle
            % endfor
            ) return Node_Rewriting_Handle is
         begin
            <%
               args = ["Unwrap_RH (Handle)"]
               args += [
                  f"{f.names.codegen} => Unwrap_Node_RH ({f.api_name})"
                  for f in n.get_parse_fields()
               ]
            %>
            return Wrap_Node_RH (Impl.Create_${n.entity.api_name}
            ${ada_block_with_parens(args, 12)});
         end;

      % endif
   % endfor

end ${ada_lib_name}.Rewriting;
