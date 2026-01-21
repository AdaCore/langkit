
with Ada.Exceptions; use Ada.Exceptions;

with System.Memory;

with Liblktlang_Support.Generic_API; use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Generic_API.Rewriting;
use Liblktlang_Support.Generic_API.Rewriting;

with Liblktlang.Common;      use Liblktlang.Common;
with Liblktlang.Generic_API; use Liblktlang.Generic_API;

package body Liblktlang.Rewriting_C is

   use type System.Memory.size_t;

   function From_C_Context (Context : Internal_Context) return Lk_Context
   is (Wrap_Context (Self_Id, Context));

   function To_C_Context (Context : Lk_Context) return Internal_Context
   renames Unwrap_Context;

   function From_C_Unit (Unit : Internal_Unit) return Lk_Unit
   is (Wrap_Unit (Self_Id, Unit));

   function To_C_Unit (Unit : Lk_Unit) return Internal_Unit
   renames Unwrap_Unit;

   function From_C_Node (Node : Internal_Node) return Lk_Node
   is (Wrap_Node (Self_Id, Node));

   function To_C_Node (Node : Lk_Node) return Internal_Node
   is (Unwrap_Node (Node).Node);

   function From_C_NRH_Array
     (Handles : C_Node_Array.Object_Pointer;
      Count   : Interfaces.C.int) return Node_Rewriting_Handle_Array;

   function From_C_Node_Kind (Kind : lkt_node_kind_enum) return Type_Ref
   is (To_Generic_Node_Type (Lkt_Node_Kind_Type'Enum_Val (Kind)));

   ----------------------
   -- From_C_NRH_Array --
   ----------------------

   function From_C_NRH_Array
     (Handles : C_Node_Array.Object_Pointer;
      Count   : Interfaces.C.int) return Node_Rewriting_Handle_Array
   is
      type C_Array_Type is
        array (1 .. Natural (Count)) of C_Node_Rewriting_Handle;
      Handles_Address : constant System.Address :=
        C_Node_Array.To_Address (Handles);
      C_Handles       : constant C_Array_Type
        with Import, Address => Handles_Address;
   begin
      return Ada_Handles : Node_Rewriting_Handle_Array (C_Array_Type'Range) do
         for I in C_Array_Type'Range loop
            Ada_Handles (I) := From_C_Node_Rewriting_Handle (C_Handles (I));
         end loop;
      end return;
   end From_C_NRH_Array;

   -------------------------
   -- C_Context_To_Handle --
   -------------------------

   function C_Context_To_Handle
     (Context : Internal_Context) return C_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return To_C_Rewriting_Handle (Handle (From_C_Context (Context)));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Rewriting_Handle;
   end C_Context_To_Handle;

   -------------------------
   -- C_Handle_To_Context --
   -------------------------

   function C_Handle_To_Context
     (Handle : C_Rewriting_Handle) return Internal_Context is
   begin
      Clear_Last_Exception;
      return To_C_Context (From_C_Rewriting_Handle (Handle).Context);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_Internal_Context;
   end C_Handle_To_Context;

   -----------------------
   -- C_Start_Rewriting --
   -----------------------

   function C_Start_Rewriting
     (Context : Internal_Context) return C_Rewriting_Handle is
   begin
      Clear_Last_Exception;

      --  For now, assume that all users of the rewriting API going through the
      --  C bindings will want the rewritten parts of their tree be formatted
      --  according to the language defaults.

      return To_C_Rewriting_Handle
        (Start_Rewriting
          (From_C_Context (Context),
           Default_Rewriting_Options (From_C_Context (Context).Language)));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Rewriting_Handle;
   end C_Start_Rewriting;

   -----------------------
   -- C_Abort_Rewriting --
   -----------------------

   procedure C_Abort_Rewriting (Handle : C_Rewriting_Handle) is
      H : Rewriting_Handle;
   begin
      Clear_Last_Exception;
      H := From_C_Rewriting_Handle (Handle);
      H.Abort_Rewriting;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Abort_Rewriting;

   -------------
   -- C_Apply --
   -------------

   procedure C_Apply
     (Handle : C_Rewriting_Handle;
      Result : access C_Apply_Result)
   is
      H          : Rewriting_Handle;
      Ada_Result : Apply_Result;
   begin
      Clear_Last_Exception;

      H := From_C_Rewriting_Handle (Handle);
      Ada_Result := H.Apply;
      Result.Success := (if Ada_Result.Success then 1 else 0);
      if not Ada_Result.Success then
         Result.Unit := To_C_Unit (Ada_Result.Unit);
         if Ada_Result.Diagnostics.Is_Empty then
            Result.Diagnostics_Count := 0;
            Result.Diagnostics := null;
         else
            Result.Diagnostics_Count :=
              Interfaces.C.int (Ada_Result.Diagnostics.Length);
            declare
               type Array_Type is
                 array (1 .. Natural (Result.Diagnostics_Count))
                 of lkt_diagnostic;
               Diagnostics_Address : constant System.Address :=
                 System.Memory.Alloc (Array_Type'Size / 8);
               Diagnostics         : Array_Type
                 with Import, Address => Diagnostics_Address;
            begin
               Result.Diagnostics :=
                 C_Diagnostic_Array.To_Pointer (Diagnostics_Address);
               for I in Diagnostics'Range loop
                  declare
                     D : Diagnostic renames Ada_Result.Diagnostics (I);
                  begin
                     Diagnostics (I).Sloc_Range := Wrap (D.Sloc_Range);
                     Diagnostics (I).Message :=
                       Wrap_Alloc (To_Text (D.Message));
                  end;
               end loop;
            end;
         end if;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Apply;

   -----------------------
   -- Free_Apply_Result --
   -----------------------

   procedure Free_Apply_Result (Result : access C_Apply_Result) is
      use type Interfaces.C.int;
   begin
      Clear_Last_Exception;

      if Result.Success = 0 then
         if Result.Diagnostics_Count /= 0 then
            declare
               type Array_Type is
                 array (1 .. Natural (Result.Diagnostics_Count))
                 of lkt_diagnostic;
               Diagnostics_Address : constant System.Address :=
                 (C_Diagnostic_Array.To_Address (Result.Diagnostics));
               Diagnostics         : Array_Type
                 with Import, Address => Diagnostics_Address;
            begin
               for D of Diagnostics loop
                  lkt_destroy_text
                    (D.Message'Unrestricted_Access);
               end loop;
            end;
            System.Memory.Free
              (C_Diagnostic_Array.To_Address (Result.Diagnostics));
         end if;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end Free_Apply_Result;

   --------------------
   -- C_Unit_Handles --
   --------------------

   function C_Unit_Handles
     (Handle : C_Rewriting_Handle) return C_Unit_Array.Object_Pointer is
   begin
      Clear_Last_Exception;

      declare
         H     : constant Rewriting_Handle := From_C_Rewriting_Handle (Handle);
         Units : constant Unit_Rewriting_Handle_Array := H.Unit_Handles;
         type Array_Type is
           array (Units'First .. Units'Last + 1)
           of C_Unit_Rewriting_Handle;

         Result_Address : constant System.Address :=
           System.Memory.Alloc (Array_Type'Size / 8);
         Result         : Array_Type with Import, Address => Result_Address;
      begin
         for I in Units'Range loop
            Result (I) := To_C_Unit_Rewriting_Handle (Units (I));
         end loop;
         Result (Result'Last) := No_C_Unit_Rewriting_Handle;
         return C_Unit_Array.To_Pointer (Result_Address);
      end;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end C_Unit_Handles;

   ----------------------
   -- C_Unit_To_Handle --
   ----------------------

   function C_Unit_To_Handle
     (Unit : Internal_Unit) return C_Unit_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return To_C_Unit_Rewriting_Handle (Handle (From_C_Unit (Unit)));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Unit_Rewriting_Handle;
   end C_Unit_To_Handle;

   ----------------------
   -- C_Handle_To_Unit --
   ----------------------

   function C_Handle_To_Unit
     (Handle : C_Unit_Rewriting_Handle) return Internal_Unit is
   begin
      Clear_Last_Exception;
      return To_C_Unit (From_C_Unit_Rewriting_Handle (Handle).Unit);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_Internal_Unit;
   end C_Handle_To_Unit;

   ------------
   -- C_Root --
   ------------

   function C_Root
     (Handle : C_Unit_Rewriting_Handle) return C_Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return To_C_Node_Rewriting_Handle
               (From_C_Unit_Rewriting_Handle (Handle).Root);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Root;

   ----------------
   -- C_Set_Root --
   ----------------

   procedure C_Set_Root
     (Handle : C_Unit_Rewriting_Handle;
      Root   : C_Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      From_C_Unit_Rewriting_Handle (Handle).Set_Root
        (From_C_Node_Rewriting_Handle (Root));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Set_Root;

   ---------------
   -- C_Unparse --
   ---------------

   procedure C_Unparse
     (Handle : C_Unit_Rewriting_Handle; Result : access lkt_text)
   is
      Text : Unbounded_Text_Type;
   begin
      Clear_Last_Exception;
      Text := From_C_Unit_Rewriting_Handle (Handle).Unparse;
      Result.all := Wrap_Alloc (Text);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Unparse;

   ----------------------
   -- C_Node_To_Handle --
   ----------------------

   function C_Node_To_Handle
     (Node : Internal_Node) return C_Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return To_C_Node_Rewriting_Handle (Handle (From_C_Node (Node)));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Node_To_Handle;

   ----------------------
   -- C_Handle_To_Node --
   ----------------------

   function C_Handle_To_Node
     (Handle : C_Node_Rewriting_Handle) return Internal_Node
   is
   begin
      Clear_Last_Exception;
      return To_C_Node (From_C_Node_Rewriting_Handle (Handle).Node);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_Internal_Node;
   end C_Handle_To_Node;

   -----------------------
   -- C_Node_To_Context --
   -----------------------

   function C_Node_To_Context
     (Node : C_Node_Rewriting_Handle) return C_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return
        To_C_Rewriting_Handle (From_C_Node_Rewriting_Handle (Node).Context);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Rewriting_Handle;
   end C_Node_To_Context;

   ---------------
   -- C_Unparse --
   ---------------

   procedure C_Unparse
     (Handle : C_Node_Rewriting_Handle; Result : access lkt_text) is
   begin
      Clear_Last_Exception;
      declare
         Text : constant Text_Type :=
           From_C_Node_Rewriting_Handle (Handle).Unparse;
      begin
         Result.all := Wrap_Alloc (Text);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Unparse;

   ------------
   -- C_Kind --
   ------------

   function C_Kind (Handle : C_Node_Rewriting_Handle) return lkt_node_kind_enum
   is
   begin
      Clear_Last_Exception;
      declare
         T : constant Type_Ref :=
           From_C_Node_Rewriting_Handle (Handle).Type_Of;
         K : constant Lkt_Node_Kind_Type := From_Generic_Node_Type (T);
      begin
         return lkt_node_kind_enum (K'Enum_Rep);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end C_Kind;

   -------------
   -- C_Image --
   -------------

   procedure C_Image
     (Handle : C_Node_Rewriting_Handle; Result : access lkt_text) is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type :=
           To_Text (From_C_Node_Rewriting_Handle (Handle).Image);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Image;

   ------------
   -- C_Tied --
   ------------

   function C_Tied (Handle : C_Node_Rewriting_Handle) return Interfaces.C.int
   is
   begin
      Clear_Last_Exception;
      return (if From_C_Node_Rewriting_Handle (Handle).Tied then 1 else 0);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end C_Tied;

   --------------
   -- C_Parent --
   --------------

   function C_Parent
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return To_C_Node_Rewriting_Handle
               (From_C_Node_Rewriting_Handle (Handle).Parent);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Parent;

   ----------------------
   -- C_Children_Count --
   ----------------------

   function C_Children_Count
     (Handle : C_Node_Rewriting_Handle) return Interfaces.C.int is
   begin
      Clear_Last_Exception;
      return Interfaces.C.int
               (From_C_Node_Rewriting_Handle (Handle).Children_Count);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end C_Children_Count;

   -------------
   -- C_Child --
   -------------

   function C_Child
     (Handle : C_Node_Rewriting_Handle;
      Field  : Interfaces.C.int) return C_Node_Rewriting_Handle
   is
      F : Struct_Member_Ref;
   begin
      Clear_Last_Exception;
      F := From_Index (Self_Id, Struct_Member_Index (Field));
      return To_C_Node_Rewriting_Handle
               (From_C_Node_Rewriting_Handle (Handle).Child (F));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Child;

   ----------------
   -- C_Children --
   ----------------

   procedure C_Children
     (Handle   : C_Node_Rewriting_Handle;
      Children : access C_Node_Array.Object_Pointer;
      Count    : access Interfaces.C.int) is
   begin
      Clear_Last_Exception;

      declare
         Ada_Result : constant Node_Rewriting_Handle_Array :=
           From_C_Node_Rewriting_Handle (Handle).Children;

         type Array_Type is
           array (Ada_Result'Range) of C_Node_Rewriting_Handle;
         C_Result_Address : constant System.Address :=
           System.Memory.Alloc (Array_Type'Size / 8);
         C_Result         : Array_Type
           with Import, Address => C_Result_Address;
      begin
         for I in Ada_Result'Range loop
            C_Result (I) := To_C_Node_Rewriting_Handle (Ada_Result (I));
         end loop;
         Children.all := C_Node_Array.To_Pointer (C_Result_Address);
         Count.all := C_Result'Length;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Children;

   -----------------
   -- C_Set_Child --
   -----------------

   procedure C_Set_Child
     (Handle : C_Node_Rewriting_Handle;
      Field  : Interfaces.C.int;
      Child  : C_Node_Rewriting_Handle)
   is
      F : Struct_Member_Ref;
   begin
      Clear_Last_Exception;
      F := From_Index (Self_Id, Struct_Member_Index (Field));
      From_C_Node_Rewriting_Handle (Handle).Set_Child
        (F, From_C_Node_Rewriting_Handle (Child));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Set_Child;

   ------------
   -- C_Text --
   ------------

   procedure C_Text
     (Handle : C_Node_Rewriting_Handle; Result : access lkt_text) is
   begin
      Clear_Last_Exception;
      declare
         T : constant Text_Type := From_C_Node_Rewriting_Handle (Handle).Text;
      begin
         Result.all := Wrap_Alloc (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Text;

   ----------------
   -- C_Set_Text --
   ----------------

   procedure C_Set_Text
     (Handle : C_Node_Rewriting_Handle; Text : access lkt_text)
   is
      T : constant Text_Type (1 .. Natural (Text.Length))
      with Import, Address => Text.Chars;
   begin
      Clear_Last_Exception;
      From_C_Node_Rewriting_Handle (Handle).Set_Text (T);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Set_Text;

   ---------------
   -- C_Replace --
   ---------------

   procedure C_Replace (Handle, New_Node : C_Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      From_C_Node_Rewriting_Handle (Handle).Replace
        (From_C_Node_Rewriting_Handle (New_Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Replace;

   --------------
   -- C_Rotate --
   --------------

   procedure C_Rotate
     (Handles : C_Node_Array.Object_Pointer;
      Count   : Interfaces.C.int) is
   begin
      Clear_Last_Exception;

      Rotate (From_C_NRH_Array (Handles, Count));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Rotate;

   -------------------
   -- C_First_Child --
   -------------------

   function C_First_Child
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return To_C_Node_Rewriting_Handle
               (From_C_Node_Rewriting_Handle (Handle).First_Child);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_First_Child;

   ------------------
   -- C_Last_Child --
   ------------------

   function C_Last_Child
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return To_C_Node_Rewriting_Handle
               (From_C_Node_Rewriting_Handle (Handle).Last_Child);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Last_Child;

   ------------------
   -- C_Next_Child --
   ------------------

   function C_Next_Child
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle
   is
   begin
      Clear_Last_Exception;
      return To_C_Node_Rewriting_Handle
               (From_C_Node_Rewriting_Handle (Handle).Next_Child);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Next_Child;

   ----------------------
   -- C_Previous_Child --
   ----------------------

   function C_Previous_Child
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle
   is
   begin
      Clear_Last_Exception;
      return To_C_Node_Rewriting_Handle
               (From_C_Node_Rewriting_Handle (Handle).Previous_Child);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Previous_Child;

   ---------------------
   -- C_Insert_Before --
   ---------------------

   procedure C_Insert_Before (Handle, New_Sibling : C_Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      From_C_Node_Rewriting_Handle (Handle).Insert_Before
        (From_C_Node_Rewriting_Handle (New_Sibling));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Insert_Before;

   --------------------
   -- C_Insert_After --
   --------------------

   procedure C_Insert_After (Handle, New_Sibling : C_Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      From_C_Node_Rewriting_Handle (Handle).Insert_After
        (From_C_Node_Rewriting_Handle (New_Sibling));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Insert_After;

   --------------------
   -- C_Insert_First --
   --------------------

   procedure C_Insert_First (Handle, New_Sibling : C_Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      From_C_Node_Rewriting_Handle (Handle).Insert_First
        (From_C_Node_Rewriting_Handle (New_Sibling));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Insert_First;

   -------------------
   -- C_Insert_Last --
   -------------------

   procedure C_Insert_Last (Handle, New_Sibling : C_Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      From_C_Node_Rewriting_Handle (Handle).Insert_Last
        (From_C_Node_Rewriting_Handle (New_Sibling));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Insert_Last;

   --------------------
   -- C_Remove_Child --
   --------------------

   procedure C_Remove_Child (Handle : C_Node_Rewriting_Handle) is
   begin
      Clear_Last_Exception;
      From_C_Node_Rewriting_Handle (Handle).Remove_Child;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end C_Remove_Child;

   -------------
   -- C_Clone --
   -------------

   function C_Clone
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      return To_C_Node_Rewriting_Handle
               (From_C_Node_Rewriting_Handle (Handle).Clone);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Clone;

   -------------------
   -- C_Create_Node --
   -------------------

   function C_Create_Node
     (Handle : C_Rewriting_Handle;
      Kind   : lkt_node_kind_enum) return C_Node_Rewriting_Handle
   is
      K : Type_Ref;
      R : Node_Rewriting_Handle;
   begin
      Clear_Last_Exception;
      K := From_C_Node_Kind (Kind);
      R := From_C_Rewriting_Handle (Handle).Create_Node (K);
      return To_C_Node_Rewriting_Handle (R);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Create_Node;

   -------------------------
   -- C_Create_Token_Node --
   -------------------------

   function C_Create_Token_Node
     (Handle : C_Rewriting_Handle;
      Kind   : lkt_node_kind_enum;
      Text   : access lkt_text) return C_Node_Rewriting_Handle
   is
      K : Type_Ref;
      T : constant Text_Type (1 .. Natural (Text.Length))
      with Import, Address => Text.Chars;
      R : Node_Rewriting_Handle;
   begin
      Clear_Last_Exception;
      K := From_C_Node_Kind (Kind);
      R := From_C_Rewriting_Handle (Handle).Create_Token_Node (K, T);
      return To_C_Node_Rewriting_Handle (R);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Create_Token_Node;

   ---------------------------
   -- C_Create_Regular_Node --
   ---------------------------

   function C_Create_Regular_Node
     (Handle   : C_Rewriting_Handle;
      Kind     : lkt_node_kind_enum;
      Children : C_Node_Array.Object_Pointer;
      Count    : Interfaces.C.int) return C_Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      declare
         K : constant Type_Ref := From_C_Node_Kind (Kind);
         C : constant Node_Rewriting_Handle_Array :=
           From_C_NRH_Array (Children, Count);
         R : Node_Rewriting_Handle :=
           From_C_Rewriting_Handle (Handle).Create_Regular_Node (K, C);
      begin
         return To_C_Node_Rewriting_Handle (R);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Create_Regular_Node;

   ----------------------------
   -- C_Create_From_Template --
   ----------------------------

   function C_Create_From_Template
     (Handle    : C_Rewriting_Handle;
      Template  : access lkt_text;
      Arguments : C_Node_Array.Object_Pointer;
      Count     : Interfaces.C.int;
      Rule      : lkt_grammar_rule) return C_Node_Rewriting_Handle is
   begin
      Clear_Last_Exception;
      declare
         Ada_Template : constant Text_Type (1 .. Natural (Template.Length))
         with Import, Address => Template.Chars;

         H : constant Rewriting_Handle := From_C_Rewriting_Handle (Handle);
         A : constant Node_Rewriting_Handle_Array :=
           From_C_NRH_Array (Arguments, Count);
         R : constant Grammar_Rule_Ref := To_Generic_Grammar_Rule (Rule);
      begin
         return To_C_Node_Rewriting_Handle
                  (H.Create_From_Template (Ada_Template, A, R));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return No_C_Node_Rewriting_Handle;
   end C_Create_From_Template;

end Liblktlang.Rewriting_C;
