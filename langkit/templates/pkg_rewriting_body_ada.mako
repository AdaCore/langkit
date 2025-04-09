## vim: filetype=makoada

with ${ada_lib_name}.Generic_API; use ${ada_lib_name}.Generic_API;
with ${ada_lib_name}.Generic_Introspection;
use ${ada_lib_name}.Generic_Introspection;

package body ${ada_lib_name}.Rewriting is

   function "+" (Handle : Rewriting_Handle) return G.Rewriting_Handle
   is (G.Rewriting_Handle (Handle));

   function "+" (Handle : Unit_Rewriting_Handle) return G.Unit_Rewriting_Handle
   is (G.Unit_Rewriting_Handle (Handle));

   function "+" (Handle : Node_Rewriting_Handle) return G.Node_Rewriting_Handle
   is (G.Node_Rewriting_Handle (Handle));

   function "+" (Handle : G.Rewriting_Handle) return Rewriting_Handle
   is (Rewriting_Handle (Handle));

   function "+" (Handle : G.Unit_Rewriting_Handle) return Unit_Rewriting_Handle
   is (Unit_Rewriting_Handle (Handle));

   function "+" (Handle : G.Node_Rewriting_Handle) return Node_Rewriting_Handle
   is (Node_Rewriting_Handle (Handle));

   ------------
   -- Handle --
   ------------

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return +G.Handle (To_Generic_Context (Context));
   end Handle;

   -------------
   -- Context --
   -------------

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
   begin
      return From_Generic_Context (G.Context (+Handle));
   end Context;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return +G.Start_Rewriting (To_Generic_Context (Context));
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) is
      H : G.Rewriting_Handle := +Handle;
   begin
      G.Abort_Rewriting (H);
      Handle := +H;
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is
      H : G.Rewriting_Handle := +Handle;
      R : constant G.Apply_Result := G.Apply (H);
   begin
      Handle := +H;
      if R.Success then
         return (Success => True);
      else
         return (Success     => False,
                 Unit        => From_Generic_Unit (R.Unit),
                 Diagnostics => R.Diagnostics);
      end if;
   end Apply;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array
   is
      R : constant G.Unit_Rewriting_Handle_Array := G.Unit_Handles (+Handle);
   begin
      return Result : Unit_Rewriting_Handle_Array (R'Range) do
         for I in R'Range loop
            Result (I) := +R (I);
         end loop;
      end return;
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle is
   begin
      return +G.Handle (To_Generic_Unit (Unit));
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit is
   begin
      return From_Generic_Unit (G.Unit (+Handle));
   end Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return +G.Root (+Handle);
   end Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle; Root : Node_Rewriting_Handle) is
   begin
      G.Set_Root (+Handle, +Root);
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type is
   begin
      return G.Unparse (+Handle);
   end Unparse;

   ------------
   -- Handle --
   ------------

   function Handle
     (Node : ${root_entity.api_name}'Class) return Node_Rewriting_Handle is
   begin
      return +G.Handle (To_Generic_Node (Node));
   end Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return ${root_entity.api_name} is
   begin
      return From_Generic_Node (G.Node (+Handle));
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      return +G.Context (+Handle);
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return G.Unparse (+Handle);
   end Unparse;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return ${T.node_kind} is
   begin
      return From_Generic_Node_Type (G.Type_Of (+Handle));
   end Kind;

   -----------
   -- Image --
   -----------

   function Image (Handle : Node_Rewriting_Handle) return String is
   begin
      return G.Image (+Handle);
   end Image;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return G.Tied (+Handle);
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return +G.Parent (+Handle);
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      return G.Children_Count (+Handle);
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle is
   begin
      return +G.Child (+Handle, Field);
   end Child;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Fields : Struct_Member_Ref_Array) return Node_Rewriting_Handle is
   begin
      return +G.Child (+Handle, Fields);
   end Child;

   --------------
   -- Children --
   --------------

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array
   is
      R : constant G.Node_Rewriting_Handle_Array := G.Children (+Handle);
   begin
      return Result : Node_Rewriting_Handle_Array (R'Range) do
         for I in R'Range loop
            Result (I) := +R (I);
         end loop;
      end return;
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
      G.Set_Child (+Handle, Field, +Child);
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return G.Text (+Handle);
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin
      G.Set_Text (+Handle, Text);
   end Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin
      G.Replace (+Handle, +New_Node);
   end Replace;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Handles : Node_Rewriting_Handle_Array) is
      H : G.Node_Rewriting_Handle_Array (Handles'Range);
   begin
      for I in Handles'Range loop
         H (I) := +Handles (I);
      end loop;
      G.Rotate (H);
   end Rotate;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return G.Is_List_Node (+Handle);
   end Is_List_Node;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return +G.First_Child (+Handle);
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return +G.Last_Child (+Handle);
   end Last_Child;

   ----------------
   -- Next_Child --
   ----------------

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return +G.Next_Child (+Handle);
   end Next_Child;

   --------------------
   -- Previous_Child --
   --------------------

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return +G.Previous_Child (+Handle);
   end Previous_Child;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      G.Insert_Before (+Handle, +New_Sibling);
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (Handle, New_Sibling : Node_Rewriting_Handle) is
   begin
      G.Insert_After (+Handle, +New_Sibling);
   end Insert_After;

   ------------------
   -- Insert_First --
   ------------------

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle) is
   begin
      G.Insert_First (+Handle, +New_Child);
   end Insert_First;

   -----------------
   -- Insert_Last --
   -----------------

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle) is
   begin
      G.Insert_Last (+Handle, +New_Child);
   end Insert_Last;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Handle : Node_Rewriting_Handle) is
   begin
      G.Remove_Child (+Handle);
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return +G.Clone (+Handle);
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind}) return Node_Rewriting_Handle is
   begin
      return +G.Create_Node (+Handle, From_Index (Self_Id, Node_Kinds (Kind)));
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : ${T.node_kind};
      Text   : Text_Type) return Node_Rewriting_Handle is
   begin
      return +G.Create_Token_Node
        (+Handle, From_Index (Self_Id, Node_Kinds (Kind)), Text);
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : ${T.node_kind};
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle
   is
      C : G.Node_Rewriting_Handle_Array (Children'Range);
   begin
      for I in Children'Range loop
         C (I) := +Children (I);
      end loop;
      return +G.Create_Regular_Node
        (+Handle, From_Index (Self_Id, Node_Kinds (Kind)), C);
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle
   is
      A : G.Node_Rewriting_Handle_Array (Arguments'Range);
   begin
      for I in Arguments'Range loop
         A (I) := +Arguments (I);
      end loop;
      return +G.Create_From_Template
        (+Handle, Template, A, To_Generic_Grammar_Rule (Rule));
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
            return Create_Regular_Node
              (Handle,
               ${n.ada_kind_name},
               ${ada_block_with_parens(
                   [
                       f"{i} => {f.api_name}"
                       for i, f in enumerate(n.get_parse_fields(), 1)
                   ],
                   15
               )});
         end;

      % endif
   % endfor

end ${ada_lib_name}.Rewriting;
