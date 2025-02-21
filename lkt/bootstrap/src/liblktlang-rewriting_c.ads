
with Interfaces.C;

with System;
with System.Address_To_Access_Conversions;

with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;
with Liblktlang_Support.Internal.Analysis;
use Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Internal.Conversions;
use Liblktlang_Support.Internal.Conversions;

with Liblktlang.Implementation.C; use Liblktlang.Implementation.C;

--  Internal package: C bindings for the rewriting API

private package Liblktlang.Rewriting_C is

   use Support.Diagnostics, Support.Text;

   function C_Context_To_Handle
     (Context : Internal_Context) return C_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_context_to_handle";

   function C_Handle_To_Context
     (Handle : C_Rewriting_Handle) return Internal_Context
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_handle_to_context";

   function C_Start_Rewriting
     (Context : Internal_Context) return C_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_start_rewriting";

   procedure C_Abort_Rewriting (Handle : C_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_abort_rewriting";

   package C_Diagnostic_Array is new
     System.Address_To_Access_Conversions (lkt_diagnostic);

   type C_Apply_Result is record
      Success           : Interfaces.C.int;
      Unit              : Internal_Unit;
      Diagnostics_Count : Interfaces.C.int;
      Diagnostics       : C_Diagnostic_Array.Object_Pointer;
   end record
      with Convention => C;

   procedure C_Apply
     (Handle : C_Rewriting_Handle;
      Result : access C_Apply_Result)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_apply";

   procedure Free_Apply_Result (Result : access C_Apply_Result)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_free_apply_result";

   package C_Unit_Array is new
     System.Address_To_Access_Conversions (C_Unit_Rewriting_Handle);

   function C_Unit_Handles
     (Handle : C_Rewriting_Handle) return C_Unit_Array.Object_Pointer
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_handles";

   function C_Unit_To_Handle
     (Unit : Internal_Unit) return C_Unit_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_to_handle";

   function C_Handle_To_Unit
     (Handle : C_Unit_Rewriting_Handle) return Internal_Unit
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_handle_to_unit";

   function C_Root
     (Handle : C_Unit_Rewriting_Handle) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_root";

   procedure C_Set_Root
     (Handle : C_Unit_Rewriting_Handle;
      Root   : C_Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_set_root";

   procedure C_Unparse
     (Handle : C_Unit_Rewriting_Handle; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_unit_unparse";


   function C_Node_To_Handle
     (Node : Internal_Node) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_node_to_handle";

   function C_Handle_To_Node
     (Handle : C_Node_Rewriting_Handle) return Internal_Node
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_handle_to_node";

   function C_Node_To_Context
     (Node : C_Node_Rewriting_Handle) return C_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_node_to_context";

   procedure C_Unparse
     (Handle : C_Node_Rewriting_Handle; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_node_unparse";

   function C_Kind (Handle : C_Node_Rewriting_Handle) return lkt_node_kind_enum
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_kind";

   procedure C_Image
     (Handle : C_Node_Rewriting_Handle; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_node_image";

   function C_Tied (Handle : C_Node_Rewriting_Handle) return Interfaces.C.int
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_tied";

   function C_Parent
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_parent";

   function C_Children_Count
     (Handle : C_Node_Rewriting_Handle) return Interfaces.C.int
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_children_count";

   function C_Child
     (Handle : C_Node_Rewriting_Handle;
      Field  : Interfaces.C.int) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_child";

   package C_Node_Array is new
     System.Address_To_Access_Conversions (C_Node_Rewriting_Handle);

   procedure C_Children
     (Handle   : C_Node_Rewriting_Handle;
      Children : access C_Node_Array.Object_Pointer;
      Count    : access Interfaces.C.int)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_children";

   procedure C_Set_Child
     (Handle : C_Node_Rewriting_Handle;
      Field  : Interfaces.C.int;
      Child  : C_Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_set_child";

   procedure C_Text
     (Handle : C_Node_Rewriting_Handle; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_text";

   procedure C_Set_Text
     (Handle : C_Node_Rewriting_Handle; Text : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_set_text";

   procedure C_Replace (Handle, New_Node : C_Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_replace";

   procedure C_Rotate
     (Handles : C_Node_Array.Object_Pointer;
      Count   : Interfaces.C.int)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_rotate";

   function C_First_Child
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_first_child";

   function C_Last_Child
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_last_child";

   function C_Next_Child
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_next_child";

   function C_Previous_Child
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_previous_child";

   procedure C_Insert_Before (Handle, New_Sibling : C_Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_insert_before";

   procedure C_Insert_After (Handle, New_Sibling : C_Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_insert_after";

   procedure C_Insert_First (Handle, New_Sibling : C_Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_insert_first";

   procedure C_Insert_Last (Handle, New_Sibling : C_Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_insert_last";

   procedure C_Remove_Child (Handle : C_Node_Rewriting_Handle)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_remove_child";

   function C_Clone
     (Handle : C_Node_Rewriting_Handle) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_clone";

   function C_Create_Node
     (Handle : C_Rewriting_Handle;
      Kind   : lkt_node_kind_enum) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_create_node";

   function C_Create_Token_Node
     (Handle : C_Rewriting_Handle;
      Kind   : lkt_node_kind_enum;
      Text   : access lkt_text) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_create_token_node";

   function C_Create_Regular_Node
     (Handle   : C_Rewriting_Handle;
      Kind     : lkt_node_kind_enum;
      Children : C_Node_Array.Object_Pointer;
      Count    : Interfaces.C.int) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_rewriting_create_regular_node";

   function C_Create_From_Template
     (Handle    : C_Rewriting_Handle;
      Template  : access lkt_text;
      Arguments : C_Node_Array.Object_Pointer;
      Count     : Interfaces.C.int;
      Rule      : lkt_grammar_rule) return C_Node_Rewriting_Handle
      with Export        => True,
           Convention    => C,
           External_Name =>
             "lkt_rewriting_create_from_template";

end Liblktlang.Rewriting_C;
