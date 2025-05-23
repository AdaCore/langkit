
with Liblktlang.Generic_API; use Liblktlang.Generic_API;
with Liblktlang.Generic_Introspection;
use Liblktlang.Generic_Introspection;

package body Liblktlang.Rewriting is

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
     (Node : Lkt_Node'Class) return Node_Rewriting_Handle is
   begin
      return +G.Handle (To_Generic_Node (Node));
   end Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return Lkt_Node is
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

   function Kind (Handle : Node_Rewriting_Handle) return Lkt_Node_Kind_Type is
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
      Kind   : Lkt_Node_Kind_Type) return Node_Rewriting_Handle is
   begin
      return +G.Create_Node (+Handle, From_Index (Self_Id, Node_Kinds (Kind)));
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Lkt_Node_Kind_Type;
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
      Kind     : Lkt_Node_Kind_Type;
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


         function Create_Argument
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Argument,
                 (1 => F_Name,
                  2 => F_Value));
         end;


         function Create_Lexer_Case_Rule_Cond_Alt
           (Handle : Rewriting_Handle
               ; F_Cond_Exprs : Node_Rewriting_Handle
               ; F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Lexer_Case_Rule_Cond_Alt,
                 (1 => F_Cond_Exprs,
                  2 => F_Send));
         end;


         function Create_Lexer_Case_Rule_Default_Alt
           (Handle : Rewriting_Handle
               ; F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Lexer_Case_Rule_Default_Alt,
                 (1 => F_Send));
         end;


         function Create_Match_Branch
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Match_Branch,
                 (1 => F_Decl,
                  2 => F_Expr));
         end;


         function Create_Pattern_Match_Branch
           (Handle : Rewriting_Handle
               ; F_Pattern : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Pattern_Match_Branch,
                 (1 => F_Pattern,
                  2 => F_Expr));
         end;


         function Create_Grammar_Rule_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Rule_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Expr));
         end;


         function Create_Synthetic_Lexer_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Synthetic_Lexer_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Expr));
         end;


         function Create_Node_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Node_Decl,
                 (1 => F_Syn_Name));
         end;


         function Create_Self_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Self_Decl,
                 (1 => F_Syn_Name));
         end;


         function Create_Binding_Val_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Binding_Val_Decl,
                 (1 => F_Syn_Name));
         end;


         function Create_Enum_Lit_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Enum_Lit_Decl,
                 (1 => F_Syn_Name));
         end;


         function Create_Field_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Trait_Ref : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Field_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Decl_Type,
                  3 => F_Trait_Ref,
                  4 => F_Default_Val));
         end;


         function Create_Fun_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Decl_Annotations : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Fun_Param_Decl,
                 (1 => F_Decl_Annotations,
                  2 => F_Syn_Name,
                  3 => F_Decl_Type,
                  4 => F_Default_Val));
         end;


         function Create_Lambda_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Lambda_Param_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Decl_Type,
                  3 => F_Default_Val));
         end;


         function Create_Dyn_Var_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Dyn_Var_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Decl_Type));
         end;


         function Create_Match_Val_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Match_Val_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Decl_Type));
         end;


         function Create_Val_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Val_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Decl_Type,
                  3 => F_Expr));
         end;


         function Create_Fun_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Return_Type : Node_Rewriting_Handle
               ; F_Trait_Ref : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Fun_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Params,
                  3 => F_Return_Type,
                  4 => F_Trait_Ref,
                  5 => F_Body));
         end;


         function Create_Env_Spec_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Actions : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Env_Spec_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Actions));
         end;


         function Create_Generic_Decl
           (Handle : Rewriting_Handle
               ; F_Generic_Param_Decls : Node_Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Generic_Decl,
                 (1 => F_Generic_Param_Decls,
                  2 => F_Decl,
                  3 => F_Syn_Name));
         end;


         function Create_Grammar_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Rules));
         end;


         function Create_Lexer_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Lexer_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Rules));
         end;


         function Create_Lexer_Family_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Lexer_Family_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Rules));
         end;


         function Create_Synth_Fun_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Synth_Fun_Decl,
                 (1 => F_Syn_Name));
         end;


         function Create_Synth_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Synth_Param_Decl,
                 (1 => F_Syn_Name));
         end;


         function Create_Any_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Any_Type_Decl,
                 (1 => F_Traits,
                  2 => F_Syn_Name,
                  3 => F_Syn_Base_Type));
         end;


         function Create_Enum_Class_Alt_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Enum_Class_Alt_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Traits,
                  3 => F_Syn_Base_Type));
         end;


         function Create_Function_Type
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Function_Type,
                 (1 => F_Syn_Name,
                  2 => F_Traits,
                  3 => F_Syn_Base_Type));
         end;


         function Create_Generic_Param_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Has_Class : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Generic_Param_Type_Decl,
                 (1 => F_Has_Class,
                  2 => F_Syn_Name,
                  3 => F_Traits,
                  4 => F_Syn_Base_Type));
         end;


         function Create_Class_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Class_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Syn_Base_Type,
                  3 => F_Traits,
                  4 => F_Decls));
         end;


         function Create_Enum_Class_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Branches : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Enum_Class_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Syn_Base_Type,
                  3 => F_Traits,
                  4 => F_Branches,
                  5 => F_Decls));
         end;


         function Create_Enum_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Literals : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Enum_Type_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Traits,
                  3 => F_Syn_Base_Type,
                  4 => F_Literals,
                  5 => F_Decls));
         end;


         function Create_Struct_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Struct_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Traits,
                  3 => F_Syn_Base_Type,
                  4 => F_Decls));
         end;


         function Create_Trait_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Trait_Decl,
                 (1 => F_Syn_Name,
                  2 => F_Traits,
                  3 => F_Syn_Base_Type,
                  4 => F_Decls));
         end;


         function Create_Decl_Annotation
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Decl_Annotation,
                 (1 => F_Name,
                  2 => F_Args));
         end;


         function Create_Decl_Annotation_Args
           (Handle : Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Decl_Annotation_Args,
                 (1 => F_Args));
         end;


         function Create_Elsif_Branch
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Elsif_Branch,
                 (1 => F_Cond_Expr,
                  2 => F_Then_Expr));
         end;


         function Create_Enum_Class_Case
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Enum_Class_Case,
                 (1 => F_Decls));
         end;


         function Create_Any_Of
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Values : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Any_Of,
                 (1 => F_Expr,
                  2 => F_Values));
         end;


         function Create_Array_Literal
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
               ; F_Element_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Array_Literal,
                 (1 => F_Exprs,
                  2 => F_Element_Type));
         end;


         function Create_Call_Expr
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Call_Expr,
                 (1 => F_Name,
                  2 => F_Args));
         end;


         function Create_Logic_Predicate
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Logic_Predicate,
                 (1 => F_Name,
                  2 => F_Args));
         end;


         function Create_Logic_Propagate_Call
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Logic_Propagate_Call,
                 (1 => F_Name,
                  2 => F_Args));
         end;


         function Create_Bin_Op
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Bin_Op,
                 (1 => F_Left,
                  2 => F_Op,
                  3 => F_Right));
         end;


         function Create_Block_Expr
           (Handle : Rewriting_Handle
               ; F_Val_Defs : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Block_Expr,
                 (1 => F_Val_Defs,
                  2 => F_Expr));
         end;


         function Create_Cast_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Excludes_Null : Node_Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Cast_Expr,
                 (1 => F_Expr,
                  2 => F_Null_Cond,
                  3 => F_Excludes_Null,
                  4 => F_Dest_Type));
         end;


         function Create_Dot_Expr
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Dot_Expr,
                 (1 => F_Prefix,
                  2 => F_Null_Cond,
                  3 => F_Suffix));
         end;


         function Create_Error_On_Null
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Error_On_Null,
                 (1 => F_Expr));
         end;


         function Create_Generic_Instantiation
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Generic_Instantiation,
                 (1 => F_Name,
                  2 => F_Args));
         end;


         function Create_Grammar_Discard
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Discard,
                 (1 => F_Expr));
         end;


         function Create_Grammar_Dont_Skip
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Dont_Skip : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Dont_Skip,
                 (1 => F_Expr,
                  2 => F_Dont_Skip));
         end;


         function Create_Grammar_List
           (Handle : Rewriting_Handle
               ; F_List_Type : Node_Rewriting_Handle
               ; F_Kind : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Sep : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_List,
                 (1 => F_List_Type,
                  2 => F_Kind,
                  3 => F_Expr,
                  4 => F_Sep));
         end;


         function Create_Grammar_Null
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Null,
                 (1 => F_Name));
         end;


         function Create_Grammar_Opt
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Opt,
                 (1 => F_Expr));
         end;


         function Create_Grammar_Opt_Error
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Opt_Error,
                 (1 => F_Expr));
         end;


         function Create_Grammar_Opt_Error_Group
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Opt_Error_Group,
                 (1 => F_Expr));
         end;


         function Create_Grammar_Opt_Group
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Opt_Group,
                 (1 => F_Expr));
         end;


         function Create_Grammar_Or_Expr
           (Handle : Rewriting_Handle
               ; F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Or_Expr,
                 (1 => F_Sub_Exprs));
         end;


         function Create_Grammar_Pick
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Pick,
                 (1 => F_Exprs));
         end;


         function Create_Grammar_Implicit_Pick
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Implicit_Pick,
                 (1 => F_Exprs));
         end;


         function Create_Grammar_Predicate
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Prop_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Predicate,
                 (1 => F_Expr,
                  2 => F_Prop_Ref));
         end;


         function Create_Grammar_Rule_Ref
           (Handle : Rewriting_Handle
               ; F_Node_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Rule_Ref,
                 (1 => F_Node_Name));
         end;


         function Create_Grammar_Skip
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Skip,
                 (1 => F_Name));
         end;


         function Create_Grammar_Stop_Cut
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_Stop_Cut,
                 (1 => F_Expr));
         end;


         function Create_Parse_Node_Expr
           (Handle : Rewriting_Handle
               ; F_Node_Name : Node_Rewriting_Handle
               ; F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Parse_Node_Expr,
                 (1 => F_Node_Name,
                  2 => F_Sub_Exprs));
         end;


         function Create_Token_No_Case_Lit
           (Handle : Rewriting_Handle
               ; F_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Token_No_Case_Lit,
                 (1 => F_Lit));
         end;


         function Create_Token_Pattern_Concat
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Token_Pattern_Concat,
                 (1 => F_Left,
                  2 => F_Right));
         end;


         function Create_Token_Ref
           (Handle : Rewriting_Handle
               ; F_Token_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Token_Ref,
                 (1 => F_Token_Name,
                  2 => F_Expr));
         end;


         function Create_If_Expr
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
               ; F_Else_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_If_Expr,
                 (1 => F_Cond_Expr,
                  2 => F_Then_Expr,
                  3 => F_Alternatives,
                  4 => F_Else_Expr));
         end;


         function Create_Isa
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Isa,
                 (1 => F_Expr,
                  2 => F_Pattern));
         end;


         function Create_Keep_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Keep_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Keep_Expr,
                 (1 => F_Expr,
                  2 => F_Null_Cond,
                  3 => F_Keep_Type));
         end;


         function Create_Lambda_Expr
           (Handle : Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Return_Type : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Lambda_Expr,
                 (1 => F_Params,
                  2 => F_Return_Type,
                  3 => F_Body));
         end;


         function Create_Null_Lit
           (Handle : Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Null_Lit,
                 (1 => F_Dest_Type));
         end;


         function Create_Block_String_Lit
           (Handle : Rewriting_Handle
               ; F_Lines : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Block_String_Lit,
                 (1 => F_Lines));
         end;


         function Create_Logic_Assign
           (Handle : Rewriting_Handle
               ; F_Dest_Var : Node_Rewriting_Handle
               ; F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Logic_Assign,
                 (1 => F_Dest_Var,
                  2 => F_Value));
         end;


         function Create_Logic_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Logic_Expr,
                 (1 => F_Expr));
         end;


         function Create_Logic_Propagate
           (Handle : Rewriting_Handle
               ; F_Dest_Var : Node_Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Logic_Propagate,
                 (1 => F_Dest_Var,
                  2 => F_Call));
         end;


         function Create_Logic_Unify
           (Handle : Rewriting_Handle
               ; F_Lhs : Node_Rewriting_Handle
               ; F_Rhs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Logic_Unify,
                 (1 => F_Lhs,
                  2 => F_Rhs));
         end;


         function Create_Match_Expr
           (Handle : Rewriting_Handle
               ; F_Match_Expr : Node_Rewriting_Handle
               ; F_Branches : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Match_Expr,
                 (1 => F_Match_Expr,
                  2 => F_Branches));
         end;


         function Create_Not_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Not_Expr,
                 (1 => F_Expr));
         end;


         function Create_Paren_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Paren_Expr,
                 (1 => F_Expr));
         end;


         function Create_Raise_Expr
           (Handle : Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
               ; F_Except_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Raise_Expr,
                 (1 => F_Dest_Type,
                  2 => F_Except_Expr));
         end;


         function Create_Subscript_Expr
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Subscript_Expr,
                 (1 => F_Prefix,
                  2 => F_Null_Cond,
                  3 => F_Index));
         end;


         function Create_Try_Expr
           (Handle : Rewriting_Handle
               ; F_Try_Expr : Node_Rewriting_Handle
               ; F_Or_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Try_Expr,
                 (1 => F_Try_Expr,
                  2 => F_Or_Expr));
         end;


         function Create_Un_Op
           (Handle : Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Un_Op,
                 (1 => F_Op,
                  2 => F_Expr));
         end;


         function Create_Full_Decl
           (Handle : Rewriting_Handle
               ; F_Doc : Node_Rewriting_Handle
               ; F_Decl_Annotations : Node_Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Full_Decl,
                 (1 => F_Doc,
                  2 => F_Decl_Annotations,
                  3 => F_Decl));
         end;


         function Create_Grammar_List_Sep
           (Handle : Rewriting_Handle
               ; F_Token : Node_Rewriting_Handle
               ; F_Extra : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Grammar_List_Sep,
                 (1 => F_Token,
                  2 => F_Extra));
         end;


         function Create_Import
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Import,
                 (1 => F_Name));
         end;


         function Create_Langkit_Root
           (Handle : Rewriting_Handle
               ; F_Imports : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Langkit_Root,
                 (1 => F_Imports,
                  2 => F_Decls));
         end;


         function Create_Lexer_Case_Rule
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Alts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Lexer_Case_Rule,
                 (1 => F_Expr,
                  2 => F_Alts));
         end;


         function Create_Lexer_Case_Rule_Send
           (Handle : Rewriting_Handle
               ; F_Sent : Node_Rewriting_Handle
               ; F_Match_Size : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Lexer_Case_Rule_Send,
                 (1 => F_Sent,
                  2 => F_Match_Size));
         end;


         function Create_Binding_Pattern
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Sub_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Binding_Pattern,
                 (1 => F_Decl,
                  2 => F_Sub_Pattern));
         end;


         function Create_Ellipsis_Pattern
           (Handle : Rewriting_Handle
               ; F_Binding : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Ellipsis_Pattern,
                 (1 => F_Binding));
         end;


         function Create_Extended_Pattern
           (Handle : Rewriting_Handle
               ; F_Sub_Pattern : Node_Rewriting_Handle
               ; F_Details : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Extended_Pattern,
                 (1 => F_Sub_Pattern,
                  2 => F_Details));
         end;


         function Create_Filtered_Pattern
           (Handle : Rewriting_Handle
               ; F_Sub_Pattern : Node_Rewriting_Handle
               ; F_Predicate : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Filtered_Pattern,
                 (1 => F_Sub_Pattern,
                  2 => F_Predicate));
         end;


         function Create_List_Pattern
           (Handle : Rewriting_Handle
               ; F_Sub_Patterns : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_List_Pattern,
                 (1 => F_Sub_Patterns));
         end;


         function Create_Not_Pattern
           (Handle : Rewriting_Handle
               ; F_Sub_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Not_Pattern,
                 (1 => F_Sub_Pattern));
         end;


         function Create_Or_Pattern
           (Handle : Rewriting_Handle
               ; F_Left_Sub_Pattern : Node_Rewriting_Handle
               ; F_Right_Sub_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Or_Pattern,
                 (1 => F_Left_Sub_Pattern,
                  2 => F_Right_Sub_Pattern));
         end;


         function Create_Paren_Pattern
           (Handle : Rewriting_Handle
               ; F_Sub_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Paren_Pattern,
                 (1 => F_Sub_Pattern));
         end;


         function Create_Tuple_Pattern
           (Handle : Rewriting_Handle
               ; F_Sub_Patterns : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Tuple_Pattern,
                 (1 => F_Sub_Patterns));
         end;


         function Create_Type_Pattern
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Type_Pattern,
                 (1 => F_Type_Name));
         end;


         function Create_Field_Pattern_Detail
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Expected_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Field_Pattern_Detail,
                 (1 => F_Id,
                  2 => F_Expected_Value));
         end;


         function Create_Property_Pattern_Detail
           (Handle : Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
               ; F_Expected_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Property_Pattern_Detail,
                 (1 => F_Call,
                  2 => F_Expected_Value));
         end;


         function Create_Selector_Pattern_Detail
           (Handle : Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
               ; F_Sub_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Selector_Pattern_Detail,
                 (1 => F_Call,
                  2 => F_Sub_Pattern));
         end;


         function Create_Selector_Call
           (Handle : Rewriting_Handle
               ; F_Quantifier : Node_Rewriting_Handle
               ; F_Binding : Node_Rewriting_Handle
               ; F_Selector_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Selector_Call,
                 (1 => F_Quantifier,
                  2 => F_Binding,
                  3 => F_Selector_Call));
         end;


         function Create_Function_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Param_Types : Node_Rewriting_Handle
               ; F_Return_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Function_Type_Ref,
                 (1 => F_Param_Types,
                  2 => F_Return_Type));
         end;


         function Create_Generic_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Generic_Type_Ref,
                 (1 => F_Type_Name,
                  2 => F_Args));
         end;


         function Create_Simple_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Simple_Type_Ref,
                 (1 => F_Type_Name));
         end;


         function Create_Var_Bind
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Create_Regular_Node
              (Handle,
               Lkt_Var_Bind,
                 (1 => F_Name,
                  2 => F_Expr));
         end;


end Liblktlang.Rewriting;
