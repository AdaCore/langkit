
with Ada.Unchecked_Conversion;

with Liblktlang.Common;

with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;
with Liblktlang.Rewriting_Implementation;

package body Liblktlang.Rewriting is

   package Impl renames Liblktlang.Rewriting_Implementation;

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
     (Node : Lkt_Node'Class) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Handle (Unwrap_Node (Node)));
   end Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return Lkt_Node is
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

   function Kind (Handle : Node_Rewriting_Handle) return Lkt_Node_Kind_Type is
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
      Kind   : Lkt_Node_Kind_Type) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_Node (Unwrap_RH (Handle), Kind));
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Lkt_Node_Kind_Type;
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
      Kind     : Lkt_Node_Kind_Type;
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


         function Create_Argument
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Argument
              (Unwrap_RH (Handle),
               Argument_F_Name => Unwrap_Node_RH (F_Name),
               Argument_F_Value => Unwrap_Node_RH (F_Value)));
         end;


         function Create_Lexer_Case_Rule_Cond_Alt
           (Handle : Rewriting_Handle
               ; F_Cond_Exprs : Node_Rewriting_Handle
               ; F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Lexer_Case_Rule_Cond_Alt
              (Unwrap_RH (Handle),
               Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs => Unwrap_Node_RH (F_Cond_Exprs),
               Lexer_Case_Rule_Cond_Alt_F_Send => Unwrap_Node_RH (F_Send)));
         end;


         function Create_Lexer_Case_Rule_Default_Alt
           (Handle : Rewriting_Handle
               ; F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Lexer_Case_Rule_Default_Alt
              (Unwrap_RH (Handle),
               Lexer_Case_Rule_Default_Alt_F_Send => Unwrap_Node_RH (F_Send)));
         end;


         function Create_Grammar_Rule_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Rule_Decl
              (Unwrap_RH (Handle),
               Grammar_Rule_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Grammar_Rule_Decl_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Synthetic_Lexer_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Synthetic_Lexer_Decl
              (Unwrap_RH (Handle),
               Synthetic_Lexer_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Synthetic_Lexer_Decl_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Node_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Node_Decl
              (Unwrap_RH (Handle),
               Node_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name)));
         end;


         function Create_Self_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Self_Decl
              (Unwrap_RH (Handle),
               Self_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name)));
         end;


         function Create_Enum_Lit_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Enum_Lit_Decl
              (Unwrap_RH (Handle),
               Enum_Lit_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name)));
         end;


         function Create_Field_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Trait_Ref : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Field_Decl
              (Unwrap_RH (Handle),
               Field_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Field_Decl_F_Decl_Type => Unwrap_Node_RH (F_Decl_Type),
               Field_Decl_F_Trait_Ref => Unwrap_Node_RH (F_Trait_Ref),
               Field_Decl_F_Default_Val => Unwrap_Node_RH (F_Default_Val)));
         end;


         function Create_Fun_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Decl_Annotations : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Fun_Param_Decl
              (Unwrap_RH (Handle),
               Fun_Param_Decl_F_Decl_Annotations => Unwrap_Node_RH (F_Decl_Annotations),
               Fun_Param_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Fun_Param_Decl_F_Decl_Type => Unwrap_Node_RH (F_Decl_Type),
               Fun_Param_Decl_F_Default_Val => Unwrap_Node_RH (F_Default_Val)));
         end;


         function Create_Lambda_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Lambda_Param_Decl
              (Unwrap_RH (Handle),
               Lambda_Param_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Lambda_Param_Decl_F_Decl_Type => Unwrap_Node_RH (F_Decl_Type),
               Lambda_Param_Decl_F_Default_Val => Unwrap_Node_RH (F_Default_Val)));
         end;


         function Create_Dyn_Var_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Dyn_Var_Decl
              (Unwrap_RH (Handle),
               Dyn_Var_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Dyn_Var_Decl_F_Decl_Type => Unwrap_Node_RH (F_Decl_Type)));
         end;


         function Create_Match_Val_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Match_Val_Decl
              (Unwrap_RH (Handle),
               Match_Val_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Match_Val_Decl_F_Decl_Type => Unwrap_Node_RH (F_Decl_Type)));
         end;


         function Create_Val_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Val_Decl
              (Unwrap_RH (Handle),
               Val_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Val_Decl_F_Decl_Type => Unwrap_Node_RH (F_Decl_Type),
               Val_Decl_F_Expr => Unwrap_Node_RH (F_Expr)));
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
            
            return Wrap_Node_RH (Impl.Create_Fun_Decl
              (Unwrap_RH (Handle),
               Fun_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Fun_Decl_F_Params => Unwrap_Node_RH (F_Params),
               Fun_Decl_F_Return_Type => Unwrap_Node_RH (F_Return_Type),
               Fun_Decl_F_Trait_Ref => Unwrap_Node_RH (F_Trait_Ref),
               Fun_Decl_F_Body => Unwrap_Node_RH (F_Body)));
         end;


         function Create_Env_Spec_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Actions : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Env_Spec_Decl
              (Unwrap_RH (Handle),
               Env_Spec_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Env_Spec_Decl_F_Actions => Unwrap_Node_RH (F_Actions)));
         end;


         function Create_Generic_Decl
           (Handle : Rewriting_Handle
               ; F_Generic_Param_Decls : Node_Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Generic_Decl
              (Unwrap_RH (Handle),
               Generic_Decl_F_Generic_Param_Decls => Unwrap_Node_RH (F_Generic_Param_Decls),
               Generic_Decl_F_Decl => Unwrap_Node_RH (F_Decl),
               Generic_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name)));
         end;


         function Create_Grammar_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Decl
              (Unwrap_RH (Handle),
               Grammar_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Grammar_Decl_F_Rules => Unwrap_Node_RH (F_Rules)));
         end;


         function Create_Lexer_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Lexer_Decl
              (Unwrap_RH (Handle),
               Lexer_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Lexer_Decl_F_Rules => Unwrap_Node_RH (F_Rules)));
         end;


         function Create_Lexer_Family_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Lexer_Family_Decl
              (Unwrap_RH (Handle),
               Lexer_Family_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Lexer_Family_Decl_F_Rules => Unwrap_Node_RH (F_Rules)));
         end;


         function Create_Synth_Fun_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Synth_Fun_Decl
              (Unwrap_RH (Handle),
               Synth_Fun_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name)));
         end;


         function Create_Synth_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Synth_Param_Decl
              (Unwrap_RH (Handle),
               Synth_Param_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name)));
         end;


         function Create_Any_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Any_Type_Decl
              (Unwrap_RH (Handle),
               Any_Type_Decl_F_Traits => Unwrap_Node_RH (F_Traits),
               Any_Type_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Any_Type_Decl_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type)));
         end;


         function Create_Enum_Class_Alt_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Enum_Class_Alt_Decl
              (Unwrap_RH (Handle),
               Enum_Class_Alt_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Enum_Class_Alt_Decl_F_Traits => Unwrap_Node_RH (F_Traits),
               Enum_Class_Alt_Decl_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type)));
         end;


         function Create_Function_Type
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Function_Type
              (Unwrap_RH (Handle),
               Function_Type_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Function_Type_F_Traits => Unwrap_Node_RH (F_Traits),
               Function_Type_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type)));
         end;


         function Create_Generic_Param_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Has_Class : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Generic_Param_Type_Decl
              (Unwrap_RH (Handle),
               Generic_Param_Type_Decl_F_Has_Class => Unwrap_Node_RH (F_Has_Class),
               Generic_Param_Type_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Generic_Param_Type_Decl_F_Traits => Unwrap_Node_RH (F_Traits),
               Generic_Param_Type_Decl_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type)));
         end;


         function Create_Class_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Class_Decl
              (Unwrap_RH (Handle),
               Basic_Class_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Basic_Class_Decl_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type),
               Basic_Class_Decl_F_Traits => Unwrap_Node_RH (F_Traits),
               Class_Decl_F_Decls => Unwrap_Node_RH (F_Decls)));
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
            
            return Wrap_Node_RH (Impl.Create_Enum_Class_Decl
              (Unwrap_RH (Handle),
               Basic_Class_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Basic_Class_Decl_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type),
               Basic_Class_Decl_F_Traits => Unwrap_Node_RH (F_Traits),
               Enum_Class_Decl_F_Branches => Unwrap_Node_RH (F_Branches),
               Enum_Class_Decl_F_Decls => Unwrap_Node_RH (F_Decls)));
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
            
            return Wrap_Node_RH (Impl.Create_Enum_Type_Decl
              (Unwrap_RH (Handle),
               Enum_Type_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Enum_Type_Decl_F_Traits => Unwrap_Node_RH (F_Traits),
               Enum_Type_Decl_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type),
               Enum_Type_Decl_F_Literals => Unwrap_Node_RH (F_Literals),
               Enum_Type_Decl_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Struct_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Struct_Decl
              (Unwrap_RH (Handle),
               Struct_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Struct_Decl_F_Traits => Unwrap_Node_RH (F_Traits),
               Struct_Decl_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type),
               Struct_Decl_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Trait_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Trait_Decl
              (Unwrap_RH (Handle),
               Trait_Decl_F_Syn_Name => Unwrap_Node_RH (F_Syn_Name),
               Trait_Decl_F_Traits => Unwrap_Node_RH (F_Traits),
               Trait_Decl_F_Syn_Base_Type => Unwrap_Node_RH (F_Syn_Base_Type),
               Trait_Decl_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Decl_Annotation
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Decl_Annotation
              (Unwrap_RH (Handle),
               Decl_Annotation_F_Name => Unwrap_Node_RH (F_Name),
               Decl_Annotation_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Decl_Annotation_Args
           (Handle : Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Decl_Annotation_Args
              (Unwrap_RH (Handle),
               Decl_Annotation_Args_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Elsif_Branch
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Elsif_Branch
              (Unwrap_RH (Handle),
               Elsif_Branch_F_Cond_Expr => Unwrap_Node_RH (F_Cond_Expr),
               Elsif_Branch_F_Then_Expr => Unwrap_Node_RH (F_Then_Expr)));
         end;


         function Create_Enum_Class_Case
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Enum_Class_Case
              (Unwrap_RH (Handle),
               Enum_Class_Case_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Any_Of
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Values : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Any_Of
              (Unwrap_RH (Handle),
               Any_Of_F_Expr => Unwrap_Node_RH (F_Expr),
               Any_Of_F_Values => Unwrap_Node_RH (F_Values)));
         end;


         function Create_Array_Literal
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
               ; F_Element_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Array_Literal
              (Unwrap_RH (Handle),
               Array_Literal_F_Exprs => Unwrap_Node_RH (F_Exprs),
               Array_Literal_F_Element_Type => Unwrap_Node_RH (F_Element_Type)));
         end;


         function Create_Call_Expr
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Call_Expr
              (Unwrap_RH (Handle),
               Base_Call_Expr_F_Name => Unwrap_Node_RH (F_Name),
               Base_Call_Expr_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Logic_Predicate
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Logic_Predicate
              (Unwrap_RH (Handle),
               Base_Call_Expr_F_Name => Unwrap_Node_RH (F_Name),
               Base_Call_Expr_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Logic_Propagate_Call
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Logic_Propagate_Call
              (Unwrap_RH (Handle),
               Base_Call_Expr_F_Name => Unwrap_Node_RH (F_Name),
               Base_Call_Expr_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Bin_Op
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Bin_Op
              (Unwrap_RH (Handle),
               Bin_Op_F_Left => Unwrap_Node_RH (F_Left),
               Bin_Op_F_Op => Unwrap_Node_RH (F_Op),
               Bin_Op_F_Right => Unwrap_Node_RH (F_Right)));
         end;


         function Create_Block_Expr
           (Handle : Rewriting_Handle
               ; F_Val_Defs : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Block_Expr
              (Unwrap_RH (Handle),
               Block_Expr_F_Val_Defs => Unwrap_Node_RH (F_Val_Defs),
               Block_Expr_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Cast_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Excludes_Null : Node_Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Cast_Expr
              (Unwrap_RH (Handle),
               Cast_Expr_F_Expr => Unwrap_Node_RH (F_Expr),
               Cast_Expr_F_Null_Cond => Unwrap_Node_RH (F_Null_Cond),
               Cast_Expr_F_Excludes_Null => Unwrap_Node_RH (F_Excludes_Null),
               Cast_Expr_F_Dest_Type => Unwrap_Node_RH (F_Dest_Type)));
         end;


         function Create_Dot_Expr
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Dot_Expr
              (Unwrap_RH (Handle),
               Dot_Expr_F_Prefix => Unwrap_Node_RH (F_Prefix),
               Dot_Expr_F_Null_Cond => Unwrap_Node_RH (F_Null_Cond),
               Dot_Expr_F_Suffix => Unwrap_Node_RH (F_Suffix)));
         end;


         function Create_Error_On_Null
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Error_On_Null
              (Unwrap_RH (Handle),
               Error_On_Null_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Generic_Instantiation
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Generic_Instantiation
              (Unwrap_RH (Handle),
               Generic_Instantiation_F_Name => Unwrap_Node_RH (F_Name),
               Generic_Instantiation_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Grammar_Discard
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Discard
              (Unwrap_RH (Handle),
               Grammar_Discard_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Grammar_Dont_Skip
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Dont_Skip : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Dont_Skip
              (Unwrap_RH (Handle),
               Grammar_Dont_Skip_F_Expr => Unwrap_Node_RH (F_Expr),
               Grammar_Dont_Skip_F_Dont_Skip => Unwrap_Node_RH (F_Dont_Skip)));
         end;


         function Create_Grammar_List
           (Handle : Rewriting_Handle
               ; F_List_Type : Node_Rewriting_Handle
               ; F_Kind : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Sep : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_List
              (Unwrap_RH (Handle),
               Grammar_List_F_List_Type => Unwrap_Node_RH (F_List_Type),
               Grammar_List_F_Kind => Unwrap_Node_RH (F_Kind),
               Grammar_List_F_Expr => Unwrap_Node_RH (F_Expr),
               Grammar_List_F_Sep => Unwrap_Node_RH (F_Sep)));
         end;


         function Create_Grammar_Null
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Null
              (Unwrap_RH (Handle),
               Grammar_Null_F_Name => Unwrap_Node_RH (F_Name)));
         end;


         function Create_Grammar_Opt
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Opt
              (Unwrap_RH (Handle),
               Grammar_Opt_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Grammar_Opt_Error
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Opt_Error
              (Unwrap_RH (Handle),
               Grammar_Opt_Error_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Grammar_Opt_Error_Group
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Opt_Error_Group
              (Unwrap_RH (Handle),
               Grammar_Opt_Error_Group_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Grammar_Opt_Group
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Opt_Group
              (Unwrap_RH (Handle),
               Grammar_Opt_Group_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Grammar_Or_Expr
           (Handle : Rewriting_Handle
               ; F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Or_Expr
              (Unwrap_RH (Handle),
               Grammar_Or_Expr_F_Sub_Exprs => Unwrap_Node_RH (F_Sub_Exprs)));
         end;


         function Create_Grammar_Pick
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Pick
              (Unwrap_RH (Handle),
               Grammar_Pick_F_Exprs => Unwrap_Node_RH (F_Exprs)));
         end;


         function Create_Grammar_Implicit_Pick
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Implicit_Pick
              (Unwrap_RH (Handle),
               Grammar_Pick_F_Exprs => Unwrap_Node_RH (F_Exprs)));
         end;


         function Create_Grammar_Predicate
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Prop_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Predicate
              (Unwrap_RH (Handle),
               Grammar_Predicate_F_Expr => Unwrap_Node_RH (F_Expr),
               Grammar_Predicate_F_Prop_Ref => Unwrap_Node_RH (F_Prop_Ref)));
         end;


         function Create_Grammar_Rule_Ref
           (Handle : Rewriting_Handle
               ; F_Node_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Rule_Ref
              (Unwrap_RH (Handle),
               Grammar_Rule_Ref_F_Node_Name => Unwrap_Node_RH (F_Node_Name)));
         end;


         function Create_Grammar_Skip
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Skip
              (Unwrap_RH (Handle),
               Grammar_Skip_F_Name => Unwrap_Node_RH (F_Name)));
         end;


         function Create_Grammar_Stop_Cut
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_Stop_Cut
              (Unwrap_RH (Handle),
               Grammar_Stop_Cut_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Parse_Node_Expr
           (Handle : Rewriting_Handle
               ; F_Node_Name : Node_Rewriting_Handle
               ; F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Parse_Node_Expr
              (Unwrap_RH (Handle),
               Parse_Node_Expr_F_Node_Name => Unwrap_Node_RH (F_Node_Name),
               Parse_Node_Expr_F_Sub_Exprs => Unwrap_Node_RH (F_Sub_Exprs)));
         end;


         function Create_Token_No_Case_Lit
           (Handle : Rewriting_Handle
               ; F_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Token_No_Case_Lit
              (Unwrap_RH (Handle),
               Token_No_Case_Lit_F_Lit => Unwrap_Node_RH (F_Lit)));
         end;


         function Create_Token_Pattern_Concat
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Token_Pattern_Concat
              (Unwrap_RH (Handle),
               Token_Pattern_Concat_F_Left => Unwrap_Node_RH (F_Left),
               Token_Pattern_Concat_F_Right => Unwrap_Node_RH (F_Right)));
         end;


         function Create_Token_Ref
           (Handle : Rewriting_Handle
               ; F_Token_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Token_Ref
              (Unwrap_RH (Handle),
               Token_Ref_F_Token_Name => Unwrap_Node_RH (F_Token_Name),
               Token_Ref_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_If_Expr
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
               ; F_Else_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_If_Expr
              (Unwrap_RH (Handle),
               If_Expr_F_Cond_Expr => Unwrap_Node_RH (F_Cond_Expr),
               If_Expr_F_Then_Expr => Unwrap_Node_RH (F_Then_Expr),
               If_Expr_F_Alternatives => Unwrap_Node_RH (F_Alternatives),
               If_Expr_F_Else_Expr => Unwrap_Node_RH (F_Else_Expr)));
         end;


         function Create_Isa
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Isa
              (Unwrap_RH (Handle),
               Isa_F_Expr => Unwrap_Node_RH (F_Expr),
               Isa_F_Dest_Type => Unwrap_Node_RH (F_Dest_Type)));
         end;


         function Create_Keep_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Keep_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Keep_Expr
              (Unwrap_RH (Handle),
               Keep_Expr_F_Expr => Unwrap_Node_RH (F_Expr),
               Keep_Expr_F_Null_Cond => Unwrap_Node_RH (F_Null_Cond),
               Keep_Expr_F_Keep_Type => Unwrap_Node_RH (F_Keep_Type)));
         end;


         function Create_Lambda_Expr
           (Handle : Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Return_Type : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Lambda_Expr
              (Unwrap_RH (Handle),
               Lambda_Expr_F_Params => Unwrap_Node_RH (F_Params),
               Lambda_Expr_F_Return_Type => Unwrap_Node_RH (F_Return_Type),
               Lambda_Expr_F_Body => Unwrap_Node_RH (F_Body)));
         end;


         function Create_Null_Lit
           (Handle : Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Null_Lit
              (Unwrap_RH (Handle),
               Null_Lit_F_Dest_Type => Unwrap_Node_RH (F_Dest_Type)));
         end;


         function Create_Block_String_Lit
           (Handle : Rewriting_Handle
               ; F_Lines : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Block_String_Lit
              (Unwrap_RH (Handle),
               Block_String_Lit_F_Lines => Unwrap_Node_RH (F_Lines)));
         end;


         function Create_Logic_Assign
           (Handle : Rewriting_Handle
               ; F_Dest_Var : Node_Rewriting_Handle
               ; F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Logic_Assign
              (Unwrap_RH (Handle),
               Logic_Assign_F_Dest_Var => Unwrap_Node_RH (F_Dest_Var),
               Logic_Assign_F_Value => Unwrap_Node_RH (F_Value)));
         end;


         function Create_Logic_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Logic_Expr
              (Unwrap_RH (Handle),
               Logic_Expr_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Logic_Propagate
           (Handle : Rewriting_Handle
               ; F_Dest_Var : Node_Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Logic_Propagate
              (Unwrap_RH (Handle),
               Logic_Propagate_F_Dest_Var => Unwrap_Node_RH (F_Dest_Var),
               Logic_Propagate_F_Call => Unwrap_Node_RH (F_Call)));
         end;


         function Create_Logic_Unify
           (Handle : Rewriting_Handle
               ; F_Lhs : Node_Rewriting_Handle
               ; F_Rhs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Logic_Unify
              (Unwrap_RH (Handle),
               Logic_Unify_F_Lhs => Unwrap_Node_RH (F_Lhs),
               Logic_Unify_F_Rhs => Unwrap_Node_RH (F_Rhs)));
         end;


         function Create_Match_Expr
           (Handle : Rewriting_Handle
               ; F_Match_Expr : Node_Rewriting_Handle
               ; F_Branches : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Match_Expr
              (Unwrap_RH (Handle),
               Match_Expr_F_Match_Expr => Unwrap_Node_RH (F_Match_Expr),
               Match_Expr_F_Branches => Unwrap_Node_RH (F_Branches)));
         end;


         function Create_Not_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Not_Expr
              (Unwrap_RH (Handle),
               Not_Expr_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Paren_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Paren_Expr
              (Unwrap_RH (Handle),
               Paren_Expr_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Raise_Expr
           (Handle : Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
               ; F_Except_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Raise_Expr
              (Unwrap_RH (Handle),
               Raise_Expr_F_Dest_Type => Unwrap_Node_RH (F_Dest_Type),
               Raise_Expr_F_Except_Expr => Unwrap_Node_RH (F_Except_Expr)));
         end;


         function Create_Subscript_Expr
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Subscript_Expr
              (Unwrap_RH (Handle),
               Subscript_Expr_F_Prefix => Unwrap_Node_RH (F_Prefix),
               Subscript_Expr_F_Null_Cond => Unwrap_Node_RH (F_Null_Cond),
               Subscript_Expr_F_Index => Unwrap_Node_RH (F_Index)));
         end;


         function Create_Try_Expr
           (Handle : Rewriting_Handle
               ; F_Try_Expr : Node_Rewriting_Handle
               ; F_Or_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Try_Expr
              (Unwrap_RH (Handle),
               Try_Expr_F_Try_Expr => Unwrap_Node_RH (F_Try_Expr),
               Try_Expr_F_Or_Expr => Unwrap_Node_RH (F_Or_Expr)));
         end;


         function Create_Un_Op
           (Handle : Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Un_Op
              (Unwrap_RH (Handle),
               Un_Op_F_Op => Unwrap_Node_RH (F_Op),
               Un_Op_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Full_Decl
           (Handle : Rewriting_Handle
               ; F_Doc : Node_Rewriting_Handle
               ; F_Decl_Annotations : Node_Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Full_Decl
              (Unwrap_RH (Handle),
               Full_Decl_F_Doc => Unwrap_Node_RH (F_Doc),
               Full_Decl_F_Decl_Annotations => Unwrap_Node_RH (F_Decl_Annotations),
               Full_Decl_F_Decl => Unwrap_Node_RH (F_Decl)));
         end;


         function Create_Grammar_List_Sep
           (Handle : Rewriting_Handle
               ; F_Token : Node_Rewriting_Handle
               ; F_Extra : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Grammar_List_Sep
              (Unwrap_RH (Handle),
               Grammar_List_Sep_F_Token => Unwrap_Node_RH (F_Token),
               Grammar_List_Sep_F_Extra => Unwrap_Node_RH (F_Extra)));
         end;


         function Create_Import
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Import
              (Unwrap_RH (Handle),
               Import_F_Name => Unwrap_Node_RH (F_Name)));
         end;


         function Create_Langkit_Root
           (Handle : Rewriting_Handle
               ; F_Imports : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Langkit_Root
              (Unwrap_RH (Handle),
               Langkit_Root_F_Imports => Unwrap_Node_RH (F_Imports),
               Langkit_Root_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Lexer_Case_Rule
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Alts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Lexer_Case_Rule
              (Unwrap_RH (Handle),
               Lexer_Case_Rule_F_Expr => Unwrap_Node_RH (F_Expr),
               Lexer_Case_Rule_F_Alts => Unwrap_Node_RH (F_Alts)));
         end;


         function Create_Lexer_Case_Rule_Send
           (Handle : Rewriting_Handle
               ; F_Sent : Node_Rewriting_Handle
               ; F_Match_Size : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Lexer_Case_Rule_Send
              (Unwrap_RH (Handle),
               Lexer_Case_Rule_Send_F_Sent => Unwrap_Node_RH (F_Sent),
               Lexer_Case_Rule_Send_F_Match_Size => Unwrap_Node_RH (F_Match_Size)));
         end;


         function Create_Match_Branch
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Match_Branch
              (Unwrap_RH (Handle),
               Match_Branch_F_Decl => Unwrap_Node_RH (F_Decl),
               Match_Branch_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Function_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Param_Types : Node_Rewriting_Handle
               ; F_Return_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Function_Type_Ref
              (Unwrap_RH (Handle),
               Function_Type_Ref_F_Param_Types => Unwrap_Node_RH (F_Param_Types),
               Function_Type_Ref_F_Return_Type => Unwrap_Node_RH (F_Return_Type)));
         end;


         function Create_Generic_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Generic_Type_Ref
              (Unwrap_RH (Handle),
               Generic_Type_Ref_F_Type_Name => Unwrap_Node_RH (F_Type_Name),
               Generic_Type_Ref_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Simple_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Simple_Type_Ref
              (Unwrap_RH (Handle),
               Simple_Type_Ref_F_Type_Name => Unwrap_Node_RH (F_Type_Name)));
         end;


         function Create_Var_Bind
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
            return Wrap_Node_RH (Impl.Create_Var_Bind
              (Unwrap_RH (Handle),
               Var_Bind_F_Name => Unwrap_Node_RH (F_Name),
               Var_Bind_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


end Liblktlang.Rewriting;
