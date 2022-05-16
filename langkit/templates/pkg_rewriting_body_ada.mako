## vim: filetype=makoada

with Ada.Unchecked_Conversion;

with ${ada_lib_name}.Common;

with ${ada_lib_name}.Public_Converters; use ${ada_lib_name}.Public_Converters;
with ${ada_lib_name}.Rewriting_Implementation;

package body ${ada_lib_name}.Rewriting is

   package Impl renames ${ada_lib_name}.Rewriting_Implementation;

   pragma Warnings (Off, "possible aliasing problem for type");
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
   pragma Warnings (On, "possible aliasing problem for type");

   function Wrap_Apply_Result
     (Res : Impl.Apply_Result) return Apply_Result;

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array;

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
      Index  : Positive) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Child (Unwrap_Node_RH (Handle), Index));
   end Child;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin
      Impl.Set_Child (Unwrap_Node_RH (Handle), Index, Unwrap_Node_RH (Child));
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

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle) is
   begin
      Impl.Insert_Child
        (Unwrap_Node_RH (Handle), Index, Unwrap_Node_RH (Child));
   end Insert_Child;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle) is
   begin
      Impl.Append_Child (Unwrap_Node_RH (Handle), Unwrap_Node_RH (Child));
   end Append_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) is
   begin
      Impl.Remove_Child (Unwrap_Node_RH (Handle), Index);
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

   % for n in ctx.astnode_types:
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
            return Wrap_Node_RH (Impl.Create_${n.entity.api_name}
               (Unwrap_RH (Handle),
                ${
                   ', '.join(
                     '{} => Unwrap_Node_RH ({})'.format(f.name, f.api_name)
                     for i, f in enumerate(n.get_parse_fields(), 1)
                )}));
         end;

      % endif
   % endfor

end ${ada_lib_name}.Rewriting;
