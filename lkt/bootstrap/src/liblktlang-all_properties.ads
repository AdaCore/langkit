



pragma Warnings (Off, "referenced");
pragma Warnings (Off, "use clause for package * has no effect");

       with Liblktlang_Support.Adalog.Debug;
         use Liblktlang_Support.Adalog.Debug;
       with Liblktlang_Support.Lexical_Envs;
         use Liblktlang_Support.Lexical_Envs;
       with Liblktlang_Support.Slocs;
         use Liblktlang_Support.Slocs;
       with Liblktlang_Support.Symbols;
         use Liblktlang_Support.Symbols;
       with Liblktlang_Support.Text;
         use Liblktlang_Support.Text;
       with Liblktlang_Support.Token_Data_Handlers;
       with Liblktlang_Support.Types;
         use Liblktlang_Support.Types;
       with Liblktlang.Common;
         use Liblktlang.Common;
       with Liblktlang.Impl_0;
       with Liblktlang.Implementation;
         use Liblktlang.Implementation;
       with Liblktlang.Implementation.Extensions;
       with System;
         use System;

pragma Warnings (On, "referenced");
pragma Warnings (On, "use clause for package * has no effect");

private package Liblktlang.All_Properties is

         function Node_Env
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Implementation.Node_Env;
         function Children_Env
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Implementation.Children_Env;
         function Parent
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity
         renames Liblktlang.Implementation.Parent;
         function Parents
           
  (Node : Bare_Lkt_Node
      ; With_Self : Boolean
         := True
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Array_Access
         renames Liblktlang.Implementation.Parents;
         function Children
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Array_Access
         renames Liblktlang.Implementation.Children;
         function Token_Start
           
  (Node : Bare_Lkt_Node
  )

            return Token_Reference
         renames Liblktlang.Implementation.Token_Start;
         function Token_End
           
  (Node : Bare_Lkt_Node
  )

            return Token_Reference
         renames Liblktlang.Implementation.Token_End;
         function Child_Index
           
  (Node : Bare_Lkt_Node
  )

            return Integer
         renames Liblktlang.Implementation.Child_Index;
         function Previous_Sibling
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity
         renames Liblktlang.Implementation.Previous_Sibling;
         function Next_Sibling
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity
         renames Liblktlang.Implementation.Next_Sibling;
         function Unit
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Unit
         renames Liblktlang.Implementation.Unit;
         function Ple_Root
           
  (Node : Bare_Lkt_Node
  )

            return Bare_Lkt_Node
         renames Liblktlang.Implementation.Ple_Root;
         function Is_Ghost
           
  (Node : Bare_Lkt_Node
  )

            return Boolean
         renames Liblktlang.Implementation.Is_Ghost;
         function Text
           
  (Node : Bare_Lkt_Node
  )

            return String_Type
         renames Liblktlang.Implementation.Text;
         function Full_Sloc_Image
           
  (Node : Bare_Lkt_Node
  )

            return String_Type
         renames Liblktlang.Implementation.Full_Sloc_Image;
         function Completion_Item_Kind_To_Int
           
  (Node : Bare_Lkt_Node
      ; Kind : Completion_Item_Kind
  )

            return Integer
         renames Liblktlang.Implementation.Completion_Item_Kind_To_Int;
         function Lkt_Node_P_Set_Solver_Debug_Mode
           
  (Node : Bare_Lkt_Node
      ; Enable : Boolean
  )

            return Boolean
         renames Liblktlang.Implementation.Extensions.Lkt_Node_P_Set_Solver_Debug_Mode;
         function Lkt_Node_P_Prelude_Unit
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Unit
         renames Liblktlang.Implementation.Extensions.Lkt_Node_P_Prelude_Unit;
         function Lkt_Node_P_Prelude_Env
           
  (Node : Bare_Lkt_Node
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Lkt_Node_P_Prelude_Env;
         function Lkt_Node_P_Is_From_Prelude
           
  (Node : Bare_Lkt_Node
  )

            return Boolean
         renames Liblktlang.Impl_0.Lkt_Node_P_Is_From_Prelude;
         function Lkt_Node_P_Root_Env
           
  (Node : Bare_Lkt_Node
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Lkt_Node_P_Root_Env;
         function Lkt_Node_P_Root_Get
           
  (Node : Bare_Lkt_Node
      ; Entity_Name : Symbol_Type
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Root_Get;
         function Lkt_Node_P_Get_Builtin_Type
           
  (Node : Bare_Lkt_Node
      ; Entity_Name : Symbol_Type
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Get_Builtin_Type;
         function Lkt_Node_P_Get_Builtin_Gen_Decl
           
  (Node : Bare_Lkt_Node
      ; Entity_Name : Symbol_Type
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Get_Builtin_Gen_Decl;
         function Lkt_Node_P_Basic_Trait_Gen
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Basic_Trait_Gen;
         function Lkt_Node_P_Basic_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Trait_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Basic_Trait;
         function Lkt_Node_P_Node_Gen_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Node_Gen_Trait;
         function Lkt_Node_P_Node_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Trait_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Node_Trait;
         function Lkt_Node_P_Indexable_Gen_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Indexable_Gen_Trait;
         function Lkt_Node_P_Indexable_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Trait_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Indexable_Trait;
         function Lkt_Node_P_Token_Node_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Token_Node_Trait;
         function Lkt_Node_P_Error_Node_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Error_Node_Trait;
         function Lkt_Node_P_Char_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Char_Type;
         function Lkt_Node_P_Int_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Int_Type;
         function Lkt_Node_P_Bool_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Bool_Type;
         function Lkt_Node_P_Bigint_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Bigint_Type;
         function Lkt_Node_P_String_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_String_Type;
         function Lkt_Node_P_Symbol_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Symbol_Type;
         function Lkt_Node_P_Property_Error_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Property_Error_Type;
         function Lkt_Node_P_Regexp_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Regexp_Type;
         function Lkt_Node_P_Entity_Gen_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Entity_Gen_Type;
         function Lkt_Node_P_Entity_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Entity_Type;
         function Lkt_Node_P_Logicvar_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Logicvar_Type;
         function Lkt_Node_P_Equation_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Equation_Type;
         function Lkt_Node_P_Array_Gen_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Array_Gen_Type;
         function Lkt_Node_P_Array_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Array_Type;
         function Lkt_Node_P_Stream_Gen_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Stream_Gen_Type;
         function Lkt_Node_P_Stream_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Stream_Type;
         function Lkt_Node_P_Astlist_Gen_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Astlist_Gen_Type;
         function Lkt_Node_P_Astlist_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Astlist_Type;
         function Lkt_Node_P_Node_Builder_Gen_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Node_Builder_Gen_Type;
         function Lkt_Node_P_Node_Builder_Type
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Named_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Node_Builder_Type;
         function Lkt_Node_P_Iterator_Gen_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Iterator_Gen_Trait;
         function Lkt_Node_P_Iterator_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Trait_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Iterator_Trait;
         function Lkt_Node_P_Analysis_Unit_Gen_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Generic_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Analysis_Unit_Gen_Trait;
         function Lkt_Node_P_Analysis_Unit_Trait
           
  (Node : Bare_Lkt_Node
  )

            return Internal_Entity_Trait_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Analysis_Unit_Trait;
         function Lkt_Node_P_Get_Empty_Type_Ref_List
           
  (Node : Bare_Lkt_Node
  )

            return Bare_Synthetic_Type_Ref_List
         renames Liblktlang.Impl_0.Lkt_Node_P_Get_Empty_Type_Ref_List;
         function Lkt_Node_P_Any_Type
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Lkt_Node_P_Any_Type;
         function Lkt_Node_P_Internal_Fetch_Referenced_Unit
           
  (Node : Bare_Lkt_Node
      ; Name : String_Type
  )

            return Internal_Unit
         renames Liblktlang.Implementation.Extensions.Lkt_Node_P_Internal_Fetch_Referenced_Unit;
         function Lkt_Node_P_Self_Root
           
  (Node : Bare_Lkt_Node
  )

            return Bare_Langkit_Root
         renames Liblktlang.Impl_0.Lkt_Node_P_Self_Root;
         function Lkt_Node_P_Referenced_Module
           
  (Node : Bare_Lkt_Node
      ; Module_Name : Bare_Id
  )

            return Internal_Entity_Langkit_Root
         renames Liblktlang.Impl_0.Lkt_Node_P_Referenced_Module;
         function Lkt_Node_P_Topmost_Invalid_Decl
           
  (Node : Bare_Lkt_Node
  )

            return Bare_Lkt_Node
         renames Liblktlang.Impl_0.Lkt_Node_P_Topmost_Invalid_Decl;
         function Lkt_Node_P_Nameres_Diagnostics
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Solver_Diagnostic_Array_Access
         renames Liblktlang.Impl_0.Lkt_Node_P_Nameres_Diagnostics;
         function Lkt_Node_P_Solve_Equation
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Solver_Result
         renames Liblktlang.Impl_0.Lkt_Node_P_Solve_Equation;
         function Lkt_Node_P_Solve_Enclosing_Context
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Solver_Result
         renames Liblktlang.Impl_0.Lkt_Node_P_Solve_Enclosing_Context;
         function Lkt_Node_P_Expected_Type_Entry_Point
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Lkt_Node_P_Expected_Type_Entry_Point;
         function Lkt_Node_P_Solve_Expected_Types
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Lkt_Node_P_Solve_Expected_Types;
         function Lkt_Node_P_Generic_Type_Entry_Point
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Lkt_Node_P_Generic_Type_Entry_Point;
         function Lkt_Node_P_Solve_Generic_Types
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Lkt_Node_P_Solve_Generic_Types;
         function Lkt_Node_P_Function_Type_Helper
           
  (Node : Bare_Lkt_Node
      ; Param_Types : Internal_Entity_Type_Decl_Array_Access
      ; Return_Type : Internal_Entity_Type_Decl
      ; Origin : Internal_Entity_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Lkt_Node_P_Function_Type_Helper;
         function Lkt_Node_P_Shed_Rebindings
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity
         renames Liblktlang.Impl_0.Lkt_Node_P_Shed_Rebindings;
         function Dispatcher_Lkt_Node_P_Xref_Entry_Point
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Dispatcher_Lkt_Node_P_Xref_Entry_Point;
         function Dispatcher_Lkt_Node_P_Xref_Equation
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dispatcher_Lkt_Node_P_Xref_Equation;
         function Dispatcher_Lkt_Node_P_Expected_Type_Equation
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dispatcher_Lkt_Node_P_Expected_Type_Equation;
         function Dispatcher_Lkt_Node_P_Generic_Type_Equation
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dispatcher_Lkt_Node_P_Generic_Type_Equation;
         function Dispatcher_Lkt_Node_P_Complete
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Complete_Item_Array_Access
         renames Liblktlang.Impl_0.Dispatcher_Lkt_Node_P_Complete;
         function Lkt_Node_P_Can_Reach
           
  (Node : Bare_Lkt_Node
      ; From_Node : Bare_Lkt_Node
  )

            return Boolean
         renames Liblktlang.Impl_0.Lkt_Node_P_Can_Reach;
         function Lkt_Node_P_Xref_Entry_Point
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Lkt_Node_P_Xref_Entry_Point;
         function Lkt_Node_P_Xref_Equation
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Lkt_Node_P_Xref_Equation;
         function Lkt_Node_P_Expected_Type_Equation
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Lkt_Node_P_Expected_Type_Equation;
         function Lkt_Node_P_Generic_Type_Equation
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Lkt_Node_P_Generic_Type_Equation;
         function Lkt_Node_P_Complete
           
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Complete_Item_Array_Access
         renames Liblktlang.Impl_0.Lkt_Node_P_Complete;
         function Argument_P_Expected_Type_Equation
           
  (Node : Bare_Argument
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Argument_P_Expected_Type_Equation;
         function Argument_P_Xref_Equation
           
  (Node : Bare_Argument
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Argument_P_Xref_Equation;
         function Base_Import_P_Referenced_Units
           
  (Node : Bare_Base_Import
  )

            return Internal_Unit_Array_Access
         renames Liblktlang.Impl_0.Base_Import_P_Referenced_Units;
         function Dispatcher_Base_Import_P_Referenced_Modules
           
  (Node : Bare_Base_Import
  )

            return Bare_Id_Array_Access
         renames Liblktlang.Impl_0.Dispatcher_Base_Import_P_Referenced_Modules;
         function Import_P_Referenced_Modules
           
  (Node : Bare_Import
  )

            return Bare_Id_Array_Access
         renames Liblktlang.Impl_0.Import_P_Referenced_Modules;
         function Internal_Env_Do_16
           
  (Node : Bare_Import
  )

            return Internal_Unit_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Do_16;
         function Internal_Env_Mappings_17
           
  (Node : Bare_Import
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_17;
         function Import_All_From_P_Referenced_Modules
           
  (Node : Bare_Import_All_From
  )

            return Bare_Id_Array_Access
         renames Liblktlang.Impl_0.Import_All_From_P_Referenced_Modules;
         function Import_All_From_P_Referenced_Scope
           
  (Node : Bare_Import_All_From
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Import_All_From_P_Referenced_Scope;
         function Internal_Env_Do_20
           
  (Node : Bare_Import_All_From
  )

            return Internal_Unit_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Do_20;
         function Internal_Ref_Env_Nodes_21
           
  (Node : Bare_Import_All_From
  )

            return Bare_Lkt_Node_Array_Access
         renames Liblktlang.Impl_0.Internal_Ref_Env_Nodes_21;
         function Import_From_P_Referenced_Modules
           
  (Node : Bare_Import_From
  )

            return Bare_Id_Array_Access
         renames Liblktlang.Impl_0.Import_From_P_Referenced_Modules;
         function Internal_Env_Do_18
           
  (Node : Bare_Import_From
  )

            return Internal_Unit_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Do_18;
         function Internal_Env_Mappings_19
           
  (Node : Bare_Import_From
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_19;
         function Dispatcher_Base_Match_Branch_P_Match_Part
           
  (Node : Bare_Base_Match_Branch
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity
         renames Liblktlang.Impl_0.Dispatcher_Base_Match_Branch_P_Match_Part;
         function Match_Branch_P_Match_Part
           
  (Node : Bare_Match_Branch
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity
         renames Liblktlang.Impl_0.Match_Branch_P_Match_Part;
         function Pattern_Match_Branch_P_Match_Part
           
  (Node : Bare_Pattern_Match_Branch
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity
         renames Liblktlang.Impl_0.Pattern_Match_Branch_P_Match_Part;
         function Dispatcher_Class_Qualifier_P_As_Bool
           
  (Node : Bare_Class_Qualifier
  )

            return Boolean
         renames Liblktlang.Impl_0.Dispatcher_Class_Qualifier_P_As_Bool;
         function Class_Qualifier_Absent_P_As_Bool
           
  (Node : Bare_Class_Qualifier_Absent
  )

            return Boolean
         renames Liblktlang.Impl_0.Class_Qualifier_Absent_P_As_Bool;
         function Class_Qualifier_Present_P_As_Bool
           
  (Node : Bare_Class_Qualifier_Present
  )

            return Boolean
         renames Liblktlang.Impl_0.Class_Qualifier_Present_P_As_Bool;
         function Decl_P_Custom_Image
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Implementation.Extensions.Decl_P_Custom_Image;
         function Dispatcher_Decl_P_Decl_Type_Name
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Dispatcher_Decl_P_Decl_Type_Name;
         function Decl_P_Full_Decl
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Full_Decl
         renames Liblktlang.Impl_0.Decl_P_Full_Decl;
         function Decl_P_Def_Ids
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Def_Id_Array_Access
         renames Liblktlang.Impl_0.Decl_P_Def_Ids;
         function Decl_P_Implements_Node
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Implements_Node;
         function Decl_P_As_Bare_Decl
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Decl_P_As_Bare_Decl;
         function Decl_P_Is_Type_Decl
           
  (Node : Bare_Decl
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Is_Type_Decl;
         function Decl_P_Is_Defined
           
  (Node : Bare_Decl
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Is_Defined;
         function Decl_P_Infer_Function_Type
           
  (Node : Bare_Decl
      ; Expected_Call : Internal_Entity_Function_Type
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Decl_P_Infer_Function_Type;
         function Decl_P_Function_Type
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Decl_P_Function_Type;
         function Decl_P_Logic_Function_Type
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Decl_P_Logic_Function_Type;
         function Decl_P_Get_Type
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Decl_P_Get_Type;
         function Decl_P_Get_Cast_Type
           
  (Node : Bare_Decl
      ; Cast_To : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Decl_P_Get_Cast_Type;
         function Decl_P_Get_Keep_Type
           
  (Node : Bare_Decl
      ; Keep_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Decl_P_Get_Keep_Type;
         function Decl_P_Get_Suffix_Type
           
  (Node : Bare_Decl
      ; Prefix_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Decl_P_Get_Suffix_Type;
         function Decl_P_Type_Var_Suffix_Ref
           
  (Node : Bare_Decl
      ; Current_Name : Internal_Entity_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Decl_P_Type_Var_Suffix_Ref;
         function Decl_P_Ref_Var_Suffix_Ref
           
  (Node : Bare_Decl
      ; Type_Var : Internal_Entity_Type_Decl
      ; Current_Name : Internal_Entity_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Decl_P_Ref_Var_Suffix_Ref;
         function Decl_P_Get_Params
           
  (Node : Bare_Decl
      ; Is_Logic : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Resolved_Param_Array_Access
         renames Liblktlang.Impl_0.Decl_P_Get_Params;
         function Decl_P_Subdecl_If_Generic
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Decl_P_Subdecl_If_Generic;
         function Decl_P_Is_Generic
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Is_Generic;
         function Decl_P_Return_Type_Is_Instantiated
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Return_Type_Is_Instantiated;
         function Decl_P_Is_Instantiated
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Is_Instantiated;
         function Decl_P_Has_Correct_Type_Arg_Number
           
  (Node : Bare_Decl
      ; Nb_Types : Integer
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Has_Correct_Type_Arg_Number;
         function Decl_P_Could_Infer
           
  (Node : Bare_Decl
      ; Generic_Type : Internal_Entity_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Could_Infer;
         function Decl_P_Instantiate_Generic_Decl
           
  (Node : Bare_Decl
      ; Param_Types : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Decl_P_Instantiate_Generic_Decl;
         function Decl_P_Get_Rebinded_Decl
           
  (Node : Bare_Decl
      ; Rebindings_Env : Lexical_Env
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Decl_P_Get_Rebinded_Decl;
         function Decl_P_Is_Dynvar
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Is_Dynvar;
         function Dispatcher_Decl_P_Name
           
  (Node : Bare_Decl
  )

            return Symbol_Type
         renames Liblktlang.Impl_0.Dispatcher_Decl_P_Name;
         function Dispatcher_Decl_P_Full_Name_Internal
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Dispatcher_Decl_P_Full_Name_Internal;
         function Dispatcher_Decl_P_Full_Name
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Dispatcher_Decl_P_Full_Name;
         function Dispatcher_Decl_P_Defined_Scope
           
  (Node : Bare_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Dispatcher_Decl_P_Defined_Scope;
         function Dispatcher_Decl_P_Defined_Scope_As_Entity
           
  (Node : Bare_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Dispatcher_Decl_P_Defined_Scope_As_Entity;
         function Decl_P_Is_Directly_Referenceable
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Is_Directly_Referenceable;
         function Decl_P_Extraneous_Parameter
           
  (Node : Bare_Decl
      ; Callee_Type : Internal_Entity_Type_Decl
      ; Callee : Internal_Entity_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Extraneous_Parameter;
         function Decl_P_Unmatched_Argument
           
  (Node : Bare_Decl
      ; Callee_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_P_Unmatched_Argument;
         function Internal_Env_Mappings_1
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_1;
         function Decl_P_Name
           
  (Node : Bare_Decl
  )

            return Symbol_Type
         renames Liblktlang.Impl_0.Decl_P_Name;
         function Decl_P_Full_Name_Internal
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Decl_P_Full_Name_Internal;
         function Decl_P_Full_Name
           
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Decl_P_Full_Name;
         function Decl_P_Defined_Scope
           
  (Node : Bare_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Decl_P_Defined_Scope;
         function Decl_P_Defined_Scope_As_Entity
           
  (Node : Bare_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Decl_P_Defined_Scope_As_Entity;
         function Grammar_Rule_Decl_P_Decl_Type_Name
           
  (Node : Bare_Grammar_Rule_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Grammar_Rule_Decl_P_Decl_Type_Name;
         function Synthetic_Lexer_Decl_P_Name
           
  (Node : Bare_Synthetic_Lexer_Decl
  )

            return Symbol_Type
         renames Liblktlang.Impl_0.Synthetic_Lexer_Decl_P_Name;
         function Synthetic_Lexer_Decl_P_Decl_Type_Name
           
  (Node : Bare_Synthetic_Lexer_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Synthetic_Lexer_Decl_P_Decl_Type_Name;
         function Base_Val_Decl_P_Defined_Scope
           
  (Node : Bare_Base_Val_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Base_Val_Decl_P_Defined_Scope;
         function Node_Decl_P_Name
           
  (Node : Bare_Node_Decl
  )

            return Symbol_Type
         renames Liblktlang.Impl_0.Node_Decl_P_Name;
         function Node_Decl_P_Decl_Type_Name
           
  (Node : Bare_Node_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Node_Decl_P_Decl_Type_Name;
         function Node_Decl_P_Owning_Type
           
  (Node : Bare_Node_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Node_Decl_P_Owning_Type;
         function Self_Decl_P_Name
           
  (Node : Bare_Self_Decl
  )

            return Symbol_Type
         renames Liblktlang.Impl_0.Self_Decl_P_Name;
         function Self_Decl_P_Decl_Type_Name
           
  (Node : Bare_Self_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Self_Decl_P_Decl_Type_Name;
         function Self_Decl_P_Owning_Type
           
  (Node : Bare_Self_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Self_Decl_P_Owning_Type;
         function User_Val_Decl_P_Xref_Entry_Point
           
  (Node : Bare_User_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.User_Val_Decl_P_Xref_Entry_Point;
         function Binding_Val_Decl_P_Decl_Type_Name
           
  (Node : Bare_Binding_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Binding_Val_Decl_P_Decl_Type_Name;
         function Binding_Val_Decl_P_Xref_Entry_Point
           
  (Node : Bare_Binding_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Binding_Val_Decl_P_Xref_Entry_Point;
         function Enum_Lit_Decl_P_Decl_Type_Name
           
  (Node : Bare_Enum_Lit_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Enum_Lit_Decl_P_Decl_Type_Name;
         function Enum_Lit_Decl_P_Xref_Entry_Point
           
  (Node : Bare_Enum_Lit_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Enum_Lit_Decl_P_Xref_Entry_Point;
         function Enum_Lit_Decl_P_Defined_Scope
           
  (Node : Bare_Enum_Lit_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Enum_Lit_Decl_P_Defined_Scope;
         function Enum_Lit_Decl_P_Parent_Type
           
  (Node : Bare_Enum_Lit_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Enum_Lit_Decl_P_Parent_Type;
         function Component_Decl_P_Xref_Equation
           
  (Node : Bare_Component_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Component_Decl_P_Xref_Equation;
         function Component_Decl_P_To_Generic_Param
           
  (Node : Bare_Component_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Resolved_Param
         renames Liblktlang.Impl_0.Component_Decl_P_To_Generic_Param;
         function Field_Decl_P_Decl_Type_Name
           
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Field_Decl_P_Decl_Type_Name;
         function Field_Decl_P_Owning_Type
           
  (Node : Bare_Field_Decl
  )

            return Bare_Type_Decl
         renames Liblktlang.Impl_0.Field_Decl_P_Owning_Type;
         function Field_Decl_P_Lazy_Field_Function_Type
           
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Field_Decl_P_Lazy_Field_Function_Type;
         function Internal_Env_Mappings_2
           
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_2;
         function Internal_Env_Mappings_3
           
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_3;
         function Fun_Param_Decl_P_Decl_Type_Name
           
  (Node : Bare_Fun_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Fun_Param_Decl_P_Decl_Type_Name;
         function Lambda_Param_Decl_P_Decl_Type_Name
           
  (Node : Bare_Lambda_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Lambda_Param_Decl_P_Decl_Type_Name;
         function Dyn_Var_Decl_P_Decl_Type_Name
           
  (Node : Bare_Dyn_Var_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Dyn_Var_Decl_P_Decl_Type_Name;
         function Dyn_Var_Decl_P_Xref_Entry_Point
           
  (Node : Bare_Dyn_Var_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Dyn_Var_Decl_P_Xref_Entry_Point;
         function Match_Val_Decl_P_Decl_Type_Name
           
  (Node : Bare_Match_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Match_Val_Decl_P_Decl_Type_Name;
         function Match_Val_Decl_P_Xref_Entry_Point
           
  (Node : Bare_Match_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Match_Val_Decl_P_Xref_Entry_Point;
         function Match_Val_Decl_P_Match_Expr
           
  (Node : Bare_Match_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Expr
         renames Liblktlang.Impl_0.Match_Val_Decl_P_Match_Expr;
         function Val_Decl_P_Decl_Type_Name
           
  (Node : Bare_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Val_Decl_P_Decl_Type_Name;
         function Val_Decl_P_Xref_Equation
           
  (Node : Bare_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Val_Decl_P_Xref_Equation;
         function Fun_Decl_P_Decl_Type_Name
           
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Fun_Decl_P_Decl_Type_Name;
         function Fun_Decl_P_Owning_Type
           
  (Node : Bare_Fun_Decl
  )

            return Bare_Type_Decl
         renames Liblktlang.Impl_0.Fun_Decl_P_Owning_Type;
         function Fun_Decl_P_Is_Dynamic_Combiner
           
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Fun_Decl_P_Is_Dynamic_Combiner;
         function Fun_Decl_P_Xref_Equation
           
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Fun_Decl_P_Xref_Equation;
         function Fun_Decl_P_Function_Type_Aux
           
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Fun_Decl_P_Function_Type_Aux;
         function Fun_Decl_P_Find_All_Overrides_Helper
           
  (Node : Bare_Fun_Decl
      ; Current_Node : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Fun_Decl_Array_Access
         renames Liblktlang.Impl_0.Fun_Decl_P_Find_All_Overrides_Helper;
         function Fun_Decl_P_Find_All_Overrides
           
  (Node : Bare_Fun_Decl
      ; Units : Internal_Unit_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Fun_Decl_Array_Access
         renames Liblktlang.Impl_0.Fun_Decl_P_Find_All_Overrides;
         function Fun_Decl_P_Base_Fun_Decls
           
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Fun_Decl_Array_Access
         renames Liblktlang.Impl_0.Fun_Decl_P_Base_Fun_Decls;
         function Internal_Env_Mappings_4
           
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_4;
         function Internal_Env_Mappings_5
           
  (Node : Bare_Fun_Decl
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_5;
         function Env_Spec_Decl_P_Owning_Type
           
  (Node : Bare_Env_Spec_Decl
  )

            return Bare_Type_Decl
         renames Liblktlang.Impl_0.Env_Spec_Decl_P_Owning_Type;
         function Env_Spec_Decl_P_Decl_Type_Name
           
  (Node : Bare_Env_Spec_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Env_Spec_Decl_P_Decl_Type_Name;
         function Env_Spec_Decl_P_Xref_Entry_Point
           
  (Node : Bare_Env_Spec_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Env_Spec_Decl_P_Xref_Entry_Point;
         function Env_Spec_Decl_P_Xref_Equation
           
  (Node : Bare_Env_Spec_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Env_Spec_Decl_P_Xref_Equation;
         function Internal_Env_Mappings_6
           
  (Node : Bare_Env_Spec_Decl
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_6;
         function Error_Decl_P_Decl_Type_Name
           
  (Node : Bare_Error_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Error_Decl_P_Decl_Type_Name;
         function Generic_Decl_P_Name
           
  (Node : Bare_Generic_Decl
  )

            return Symbol_Type
         renames Liblktlang.Impl_0.Generic_Decl_P_Name;
         function Generic_Decl_P_Image_Suffix
           
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Generic_Decl_P_Image_Suffix;
         function Generic_Decl_P_Generic_Params
           
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Generic_Param_Type_Decl_Array_Access
         renames Liblktlang.Impl_0.Generic_Decl_P_Generic_Params;
         function Generic_Decl_P_Generic_Params_Names
           
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Symbol_Type_Array_Access
         renames Liblktlang.Impl_0.Generic_Decl_P_Generic_Params_Names;
         function Generic_Decl_P_Decl_Type_Name
           
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Generic_Decl_P_Decl_Type_Name;
         function Generic_Decl_P_Instantiated_Generic_Params
           
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl_Array_Access
         renames Liblktlang.Impl_0.Generic_Decl_P_Instantiated_Generic_Params;
         function Internal_Env_Mappings_7
           
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_7;
         function Grammar_Decl_P_Decl_Type_Name
           
  (Node : Bare_Grammar_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Grammar_Decl_P_Decl_Type_Name;
         function Internal_Env_Mappings_8
           
  (Node : Bare_Grammar_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_8;
         function Langkit_Root_P_Decl_Type_Name
           
  (Node : Bare_Langkit_Root
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Langkit_Root_P_Decl_Type_Name;
         function Langkit_Root_P_Name
           
  (Node : Bare_Langkit_Root
  )

            return Symbol_Type
         renames Liblktlang.Impl_0.Langkit_Root_P_Name;
         function Langkit_Root_P_Defined_Scope
           
  (Node : Bare_Langkit_Root
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Langkit_Root_P_Defined_Scope;
         function Langkit_Root_P_Module_Name
           
  (Node : Bare_Langkit_Root
  )

            return Symbol_Type
         renames Liblktlang.Implementation.Extensions.Langkit_Root_P_Module_Name;
         function Langkit_Root_P_Resolve_Metadata
           
  (Node : Bare_Langkit_Root
  )

            return Internal_Entity
         renames Liblktlang.Impl_0.Langkit_Root_P_Resolve_Metadata;
         function Langkit_Root_F_Empty_Type_Ref_List
           
  (Node : Bare_Langkit_Root
  )

            return Bare_Synthetic_Type_Ref_List
         renames Liblktlang.Impl_0.Langkit_Root_F_Empty_Type_Ref_List;
         function Internal_Env_Do_22
           
  (Node : Bare_Langkit_Root
  )

            return Internal_Unit
         renames Liblktlang.Impl_0.Internal_Env_Do_22;
         function Internal_Env_Mappings_23
           
  (Node : Bare_Langkit_Root
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_23;
         function Internal_Ref_Env_Nodes_24
           
  (Node : Bare_Langkit_Root
  )

            return Bare_Lkt_Node_Array_Access
         renames Liblktlang.Impl_0.Internal_Ref_Env_Nodes_24;
         function Internal_Ref_Cond_25
           
  (Node : Bare_Langkit_Root
  )

            return Boolean
         renames Liblktlang.Impl_0.Internal_Ref_Cond_25;
         function Internal_Env_Mappings_26
           
  (Node : Bare_Langkit_Root
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_26;
         function Lexer_Decl_P_Decl_Type_Name
           
  (Node : Bare_Lexer_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Lexer_Decl_P_Decl_Type_Name;
         function Lexer_Decl_P_Builtin_Decls
           
  (Node : Bare_Lexer_Decl
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Lexer_Decl_P_Builtin_Decls;
         function Internal_Env_Mappings_9
           
  (Node : Bare_Lexer_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_9;
         function Internal_Env_Mappings_10
           
  (Node : Bare_Lexer_Decl
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_10;
         function Lexer_Family_Decl_P_Decl_Type_Name
           
  (Node : Bare_Lexer_Family_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Lexer_Family_Decl_P_Decl_Type_Name;
         function Synth_Fun_Decl_P_Function_Type_Aux
           
  (Node : Bare_Synth_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Synth_Fun_Decl_P_Function_Type_Aux;
         function Synth_Fun_Decl_P_Decl_Type_Name
           
  (Node : Bare_Synth_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Synth_Fun_Decl_P_Decl_Type_Name;
         function Synth_Param_Decl_P_Full_Name_Internal
           
  (Node : Bare_Synth_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Synth_Param_Decl_P_Full_Name_Internal;
         function Synth_Param_Decl_P_Decl_Type_Name
           
  (Node : Bare_Synth_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Synth_Param_Decl_P_Decl_Type_Name;
         function Type_Decl_P_Self_Decl
           
  (Node : Bare_Type_Decl
  )

            return Bare_Self_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Self_Decl;
         function Type_Decl_P_Node_Decl
           
  (Node : Bare_Type_Decl
  )

            return Bare_Node_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Node_Decl;
         function Type_Decl_P_Full_Name
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Type_Decl_P_Full_Name;
         function Type_Decl_P_Def_Id
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Def_Id
         renames Liblktlang.Impl_0.Type_Decl_P_Def_Id;
         function Dispatcher_Type_Decl_P_Base_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Dispatcher_Type_Decl_P_Base_Type;
         function Type_Decl_P_Base_Types
           
  (Node : Bare_Type_Decl
      ; Include_Self : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl_Array_Access
         renames Liblktlang.Impl_0.Type_Decl_P_Base_Types;
         function Type_Decl_P_Is_Equation
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Equation;
         function Type_Decl_P_Is_Bool
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Bool;
         function Type_Decl_P_Is_String_Or_Array_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_String_Or_Array_Type;
         function Type_Decl_P_Is_Int_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Int_Type;
         function Type_Decl_P_Is_Int_Or_Node
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Int_Or_Node;
         function Type_Decl_P_Is_Stream
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Stream;
         function Type_Decl_P_Get_Entity_Node_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Get_Entity_Node_Type;
         function Type_Decl_P_Is_Subtype_Or_Eq
           
  (Node : Bare_Type_Decl
      ; Rhs : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Subtype_Or_Eq;
         function Type_Decl_P_Is_Entity_Subtype_Or_Eq
           
  (Node : Bare_Type_Decl
      ; Rhs : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Entity_Subtype_Or_Eq;
         function Type_Decl_P_Common_Ancestor_Helper
           
  (Node : Bare_Type_Decl
      ; Other_Types : Internal_Entity_Type_Decl_Array_Access
      ; Idx : Integer
      ; Imprecise : Boolean
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Common_Ancestor_Helper;
         function Type_Decl_P_Imprecise_Common_Ancestor_List
           
  (Node : Bare_Type_Decl
      ; Other_Types : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Imprecise_Common_Ancestor_List;
         function Type_Decl_P_Commutative_Matching_Type
           
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
      ; Allow_Common_Ancestor : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Commutative_Matching_Type;
         function Type_Decl_P_Could_Determine_Type
           
  (Node : Bare_Type_Decl
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Could_Determine_Type;
         function Type_Decl_P_Matching_Generic_Types
           
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Matching_Generic_Types;
         function Type_Decl_P_Matching_Type
           
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Matching_Type;
         function Type_Decl_P_Matching_Logic_Type
           
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Matching_Logic_Type;
         function Type_Decl_P_Is_Indexable_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Indexable_Type;
         function Type_Decl_P_Is_Callable
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Callable;
         function Type_Decl_P_Match_Param_Get_Type
           
  (Node : Bare_Type_Decl
      ; Current_Name : Internal_Entity_Argument
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Match_Param_Get_Type;
         function Type_Decl_P_Match_Param_Get_Decl
           
  (Node : Bare_Type_Decl
      ; Current_Name : Internal_Entity_Argument
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Match_Param_Get_Decl;
         function Type_Decl_P_Lambda_Param_Get_Type
           
  (Node : Bare_Type_Decl
      ; Param_Decl : Internal_Entity_Lambda_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Lambda_Param_Get_Type;
         function Type_Decl_P_Is_Valid_Call
           
  (Node : Bare_Type_Decl
      ; Args : Internal_Entity_Argument_List
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Valid_Call;
         function Type_Decl_P_Get_Return_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Get_Return_Type;
         function Type_Decl_P_Create_Function_Type
           
  (Node : Bare_Type_Decl
      ; Params : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Create_Function_Type;
         function Type_Decl_P_Make_Array_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Make_Array_Type;
         function Type_Decl_P_Make_Stream_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Make_Stream_Type;
         function Type_Decl_P_Get_Indexable_Content_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Get_Indexable_Content_Type;
         function Type_Decl_P_Get_Super_Of_Parent
           
  (Node : Bare_Type_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Get_Super_Of_Parent;
         function Type_Decl_P_Basic_Trait_From_Self
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Basic_Trait_From_Self;
         function Type_Decl_P_Find_Types_That_Replace_Ty
           
  (Node : Bare_Type_Decl
      ; Ty : Internal_Entity_Type_Decl
      ; Origin : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl_Array_Access
         renames Liblktlang.Impl_0.Type_Decl_P_Find_Types_That_Replace_Ty;
         function Type_Decl_P_As_Node_Builder_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_As_Node_Builder_Type;
         function Dispatcher_Type_Decl_P_Is_Subtype
           
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Dispatcher_Type_Decl_P_Is_Subtype;
         function Type_Decl_P_Common_Ancestor
           
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
      ; Imprecise : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Common_Ancestor;
         function Type_Decl_P_Node_Builder_Scope
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Type_Decl_P_Node_Builder_Scope;
         function Type_Decl_P_Instantiate_Generic_Type_Decl
           
  (Node : Bare_Type_Decl
      ; Param_Types : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Instantiate_Generic_Type_Decl;
         function Type_Decl_P_Base_Type
           
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Decl_P_Base_Type;
         function Type_Decl_P_Is_Subtype
           
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Decl_P_Is_Subtype;
         function Any_Type_Decl_P_Full_Name_Internal
           
  (Node : Bare_Any_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Any_Type_Decl_P_Full_Name_Internal;
         function Any_Type_Decl_P_Decl_Type_Name
           
  (Node : Bare_Any_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Any_Type_Decl_P_Decl_Type_Name;
         function Enum_Class_Alt_Decl_P_Decl_Type_Name
           
  (Node : Bare_Enum_Class_Alt_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Enum_Class_Alt_Decl_P_Decl_Type_Name;
         function Enum_Class_Alt_Decl_P_Is_Subtype
           
  (Node : Bare_Enum_Class_Alt_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Enum_Class_Alt_Decl_P_Is_Subtype;
         function Enum_Class_Alt_Decl_P_Defined_Scope
           
  (Node : Bare_Enum_Class_Alt_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Enum_Class_Alt_Decl_P_Defined_Scope;
         function Enum_Class_Alt_Decl_P_Base_Type
           
  (Node : Bare_Enum_Class_Alt_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Enum_Class_Alt_Decl_P_Base_Type;
         function Function_Type_P_Full_Name_Internal
           
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Function_Type_P_Full_Name_Internal;
         function Function_Type_P_Decl_Type_Name
           
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Function_Type_P_Decl_Type_Name;
         function Function_Type_P_Defined_Scope
           
  (Node : Bare_Function_Type
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Function_Type_P_Defined_Scope;
         function Function_Type_P_Should_Ignore_Constructor_Arg
           
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Function_Type_P_Should_Ignore_Constructor_Arg;
         function Function_Type_P_Returns_Entity
           
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Function_Type_P_Returns_Entity;
         function Function_Type_P_Returns_Bool
           
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Function_Type_P_Returns_Bool;
         function Generic_Param_Type_Decl_P_Decl_Type_Name
           
  (Node : Bare_Generic_Param_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Generic_Param_Type_Decl_P_Decl_Type_Name;
         function Named_Type_Decl_P_Defined_Scope
           
  (Node : Bare_Named_Type_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Named_Type_Decl_P_Defined_Scope;
         function Basic_Class_Decl_P_Is_Subtype
           
  (Node : Bare_Basic_Class_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Basic_Class_Decl_P_Is_Subtype;
         function Basic_Class_Decl_P_Defined_Scope
           
  (Node : Bare_Basic_Class_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Basic_Class_Decl_P_Defined_Scope;
         function Basic_Class_Decl_P_Defined_Scope_As_Entity
           
  (Node : Bare_Basic_Class_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Basic_Class_Decl_P_Defined_Scope_As_Entity;
         function Class_Decl_P_Decl_Type_Name
           
  (Node : Bare_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Class_Decl_P_Decl_Type_Name;
         function Class_Decl_P_Constructor_Fields
           
  (Node : Bare_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Field_Decl_Array_Access
         renames Liblktlang.Impl_0.Class_Decl_P_Constructor_Fields;
         function Class_Decl_P_Function_Type_Aux
           
  (Node : Bare_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Class_Decl_P_Function_Type_Aux;
         function Enum_Class_Decl_P_Decl_Type_Name
           
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Enum_Class_Decl_P_Decl_Type_Name;
         function Enum_Class_Decl_P_Alts
           
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Enum_Class_Alt_Decl_Array_Access
         renames Liblktlang.Impl_0.Enum_Class_Decl_P_Alts;
         function Internal_Env_Mappings_11
           
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_11;
         function Internal_Env_Mappings_12
           
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_12;
         function Enum_Type_Decl_P_Decl_Type_Name
           
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Enum_Type_Decl_P_Decl_Type_Name;
         function Internal_Env_Mappings_13
           
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Internal_Env_Mappings_13;
         function Internal_Env_Mappings_14
           
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_14;
         function Internal_Env_Mappings_15
           
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Internal_Env_Mappings_15;
         function Struct_Decl_P_Decl_Type_Name
           
  (Node : Bare_Struct_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Struct_Decl_P_Decl_Type_Name;
         function Struct_Decl_P_Function_Type_Aux
           
  (Node : Bare_Struct_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Function_Type
         renames Liblktlang.Impl_0.Struct_Decl_P_Function_Type_Aux;
         function Struct_Decl_P_Entity_Scope
           
  (Node : Bare_Struct_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Struct_Decl_P_Entity_Scope;
         function Struct_Decl_P_Update_Func_Env
           
  (Node : Bare_Struct_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Struct_Decl_P_Update_Func_Env;
         function Struct_Decl_P_Defined_Scope
           
  (Node : Bare_Struct_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Struct_Decl_P_Defined_Scope;
         function Trait_Decl_P_Decl_Type_Name
           
  (Node : Bare_Trait_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Trait_Decl_P_Decl_Type_Name;
         function Trait_Decl_P_Defined_Scope_As_Entity
           
  (Node : Bare_Trait_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Trait_Decl_P_Defined_Scope_As_Entity;
         function Decl_Annotation_P_Xref_Entry_Point
           
  (Node : Bare_Decl_Annotation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Decl_Annotation_P_Xref_Entry_Point;
         function Decl_Annotation_P_With_Dynvars_Equation
           
  (Node : Bare_Decl_Annotation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Decl_Annotation_P_With_Dynvars_Equation;
         function Decl_Annotation_P_Xref_Equation
           
  (Node : Bare_Decl_Annotation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Decl_Annotation_P_Xref_Equation;
         function Dyn_Env_Wrapper_F_Dynenvwrapper_Instantiation_Env
           
  (Node : Bare_Dyn_Env_Wrapper
  )

            return Lexical_Env
         renames Liblktlang.Impl_0.Dyn_Env_Wrapper_F_Dynenvwrapper_Instantiation_Env;
         function Dyn_Env_Wrapper_P_Instantiation_Bindings
           
  (Node : Bare_Dyn_Env_Wrapper
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Inner_Env_Assoc_Array_Access
         renames Liblktlang.Impl_0.Dyn_Env_Wrapper_P_Instantiation_Bindings;
         function Dispatcher_Excludes_Null_P_As_Bool
           
  (Node : Bare_Excludes_Null
  )

            return Boolean
         renames Liblktlang.Impl_0.Dispatcher_Excludes_Null_P_As_Bool;
         function Excludes_Null_Absent_P_As_Bool
           
  (Node : Bare_Excludes_Null_Absent
  )

            return Boolean
         renames Liblktlang.Impl_0.Excludes_Null_Absent_P_As_Bool;
         function Excludes_Null_Present_P_As_Bool
           
  (Node : Bare_Excludes_Null_Present
  )

            return Boolean
         renames Liblktlang.Impl_0.Excludes_Null_Present_P_As_Bool;
         function Expr_P_Xref_Entry_Point
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Expr_P_Xref_Entry_Point;
         function Dispatcher_Expr_P_Get_Type
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Dispatcher_Expr_P_Get_Type;
         function Expr_P_Get_Generic_Type
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Expr_P_Get_Generic_Type;
         function Expr_P_Get_Expected_Type
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Expr_P_Get_Expected_Type;
         function Expr_P_Get_Rightmost_Refid
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Ref_Id
         renames Liblktlang.Impl_0.Expr_P_Get_Rightmost_Refid;
         function Expr_P_Expected_Type_Equation
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Expr_P_Expected_Type_Equation;
         function Expr_P_Match_Params
           
  (Node : Bare_Expr
      ; Params : Internal_Resolved_Param_Array_Access
      ; Args : Internal_Entity_Argument_List
  )

            return Internal_Param_Match_Array_Access
         renames Liblktlang.Impl_0.Expr_P_Match_Params;
         function Dispatcher_Expr_P_Xlogic_Equation
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dispatcher_Expr_P_Xlogic_Equation;
         function Dispatcher_Expr_P_Xtype_Equation
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dispatcher_Expr_P_Xtype_Equation;
         function Dispatcher_Expr_P_Referenced_Decl
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Dispatcher_Expr_P_Referenced_Decl;
         function Dispatcher_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Dispatcher_Expr_P_Has_Context_Free_Type;
         function Expr_P_Get_Type
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Expr_P_Get_Type;
         function Expr_P_Xlogic_Equation
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Expr_P_Xlogic_Equation;
         function Expr_P_Xtype_Equation
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Expr_P_Xtype_Equation;
         function Expr_P_Referenced_Decl
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Expr_P_Referenced_Decl;
         function Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Expr_P_Has_Context_Free_Type;
         function Any_Of_P_Xref_Equation
           
  (Node : Bare_Any_Of
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Any_Of_P_Xref_Equation;
         function Any_Of_P_Has_Context_Free_Type
           
  (Node : Bare_Any_Of
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Any_Of_P_Has_Context_Free_Type;
         function Array_Literal_P_Has_Context_Free_Type
           
  (Node : Bare_Array_Literal
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Array_Literal_P_Has_Context_Free_Type;
         function Array_Literal_P_Expected_Exprs_Type_Equation
           
  (Node : Bare_Array_Literal
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Array_Literal_P_Expected_Exprs_Type_Equation;
         function Array_Literal_P_Xref_Equation
           
  (Node : Bare_Array_Literal
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Array_Literal_P_Xref_Equation;
         function Base_Call_Expr_P_Generic_Type_Equation_Helper
           
  (Node : Bare_Base_Call_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Generic_Type_Equation_Helper;
         function Base_Call_Expr_P_Generic_Type_Equation
           
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Generic_Type_Equation;
         function Base_Call_Expr_P_Expected_Type_Equation
           
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Expected_Type_Equation;
         function Base_Call_Expr_P_Xref_Call_Args_Equation
           
  (Node : Bare_Base_Call_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Xref_Call_Args_Equation;
         function Base_Call_Expr_P_Xref_Call_Equation
           
  (Node : Bare_Base_Call_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Xref_Call_Equation;
         function Base_Call_Expr_P_Xref_Equation
           
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Xref_Equation;
         function Base_Call_Expr_P_Xlogic_Unknown
           
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Xlogic_Unknown;
         function Base_Call_Expr_P_Xlogic_Any_All
           
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Xlogic_Any_All;
         function Base_Call_Expr_P_Xlogic_Equation
           
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Base_Call_Expr_P_Xlogic_Equation;
         function Logic_Predicate_P_Generic_Type_Equation
           
  (Node : Bare_Logic_Predicate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Logic_Predicate_P_Generic_Type_Equation;
         function Logic_Predicate_P_Xref_Equation
           
  (Node : Bare_Logic_Predicate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Logic_Predicate_P_Xref_Equation;
         function Bin_Op_P_Xref_Equation
           
  (Node : Bare_Bin_Op
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Bin_Op_P_Xref_Equation;
         function Bin_Op_P_Has_Context_Free_Type
           
  (Node : Bare_Bin_Op
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Bin_Op_P_Has_Context_Free_Type;
         function Block_Expr_P_Expr
           
  (Node : Bare_Block_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Expr
         renames Liblktlang.Impl_0.Block_Expr_P_Expr;
         function Block_Expr_P_Xref_Equation
           
  (Node : Bare_Block_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Block_Expr_P_Xref_Equation;
         function Block_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Block_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Block_Expr_P_Has_Context_Free_Type;
         function Cast_Expr_P_Expected_Type_Equation
           
  (Node : Bare_Cast_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Cast_Expr_P_Expected_Type_Equation;
         function Cast_Expr_P_Xref_Equation
           
  (Node : Bare_Cast_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Cast_Expr_P_Xref_Equation;
         function Dot_Expr_P_Referenced_Decl
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Dot_Expr_P_Referenced_Decl;
         function Dot_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Dot_Expr_P_Has_Context_Free_Type;
         function Dot_Expr_P_First_Var_In_Prefix_Env
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Dot_Expr_P_First_Var_In_Prefix_Env;
         function Dot_Expr_P_Xtype_Equation
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dot_Expr_P_Xtype_Equation;
         function Dot_Expr_P_Is_Call_To_Super
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Dot_Expr_P_Is_Call_To_Super;
         function Dot_Expr_P_Generic_Type_Equation
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dot_Expr_P_Generic_Type_Equation;
         function Dot_Expr_P_Expected_Type_Equation
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dot_Expr_P_Expected_Type_Equation;
         function Dot_Expr_P_Xref_Typing_Equation
           
  (Node : Bare_Dot_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dot_Expr_P_Xref_Typing_Equation;
         function Dot_Expr_P_Xref_Equation
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dot_Expr_P_Xref_Equation;
         function Dot_Expr_P_Xlogic_Equation
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Dot_Expr_P_Xlogic_Equation;
         function Dot_Expr_P_Complete
           
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Complete_Item_Array_Access
         renames Liblktlang.Impl_0.Dot_Expr_P_Complete;
         function Error_On_Null_P_Xref_Equation
           
  (Node : Bare_Error_On_Null
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Error_On_Null_P_Xref_Equation;
         function Error_On_Null_P_Has_Context_Free_Type
           
  (Node : Bare_Error_On_Null
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Error_On_Null_P_Has_Context_Free_Type;
         function Generic_Instantiation_P_Xref_Equation
           
  (Node : Bare_Generic_Instantiation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Generic_Instantiation_P_Xref_Equation;
         function Token_Lit_P_Denoted_Value
           
  (Node : Bare_Token_Lit
  )

            return Internal_Decoded_String_Value
         renames Liblktlang.Implementation.Extensions.Token_Lit_P_Denoted_Value;
         function Token_Pattern_Lit_P_Denoted_Value
           
  (Node : Bare_Token_Pattern_Lit
  )

            return Internal_Decoded_String_Value
         renames Liblktlang.Implementation.Extensions.Token_Pattern_Lit_P_Denoted_Value;
         function Id_P_Custom_Image
           
  (Node : Bare_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Implementation.Extensions.Id_P_Custom_Image;
         function Id_P_Is_Type_Name
           
  (Node : Bare_Id
  )

            return Boolean
         renames Liblktlang.Implementation.Extensions.Id_P_Is_Type_Name;
         function Id_P_Is_Not_Type_Name
           
  (Node : Bare_Id
  )

            return Boolean
         renames Liblktlang.Impl_0.Id_P_Is_Not_Type_Name;
         function Def_Id_P_Decl
           
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Def_Id_P_Decl;
         function Def_Id_P_Name
           
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Def_Id_P_Name;
         function Def_Id_P_Get_Type
           
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Def_Id_P_Get_Type;
         function Def_Id_P_Get_Implementatinons
           
  (Node : Bare_Def_Id
      ; Units : Internal_Unit_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Def_Id_Array_Access
         renames Liblktlang.Impl_0.Def_Id_P_Get_Implementatinons;
         function Def_Id_P_Decl_Detail
           
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Def_Id_P_Decl_Detail;
         function Def_Id_P_Completion_Item_Kind
           
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Integer
         renames Liblktlang.Impl_0.Def_Id_P_Completion_Item_Kind;
         function Def_Id_P_Doc
           
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return String_Type
         renames Liblktlang.Impl_0.Def_Id_P_Doc;
         function Def_Id_P_Find_All_References_Helper
           
  (Node : Bare_Def_Id
      ; Current_Node : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Ref_Id_Array_Access
         renames Liblktlang.Impl_0.Def_Id_P_Find_All_References_Helper;
         function Def_Id_P_Find_All_References
           
  (Node : Bare_Def_Id
      ; Units : Internal_Unit_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Ref_Result_Array_Access
         renames Liblktlang.Impl_0.Def_Id_P_Find_All_References;
         function Imported_Id_P_Referenced_Decl_From_Module
           
  (Node : Bare_Imported_Id
      ; Module_Name : Bare_Module_Id
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Imported_Id_P_Referenced_Decl_From_Module;
         function Imported_Id_P_Referenced_Decl
           
  (Node : Bare_Imported_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Imported_Id_P_Referenced_Decl;
         function Imported_Id_P_Get_Type
           
  (Node : Bare_Imported_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Imported_Id_P_Get_Type;
         function Module_Id_P_Referenced_Decl
           
  (Node : Bare_Module_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Module_Id_P_Referenced_Decl;
         function Module_Id_P_Xref_Equation
           
  (Node : Bare_Module_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Module_Id_P_Xref_Equation;
         function Module_Id_P_Get_Type
           
  (Node : Bare_Module_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Module_Id_P_Get_Type;
         function Ref_Id_P_From_Node
           
  (Node : Bare_Ref_Id
  )

            return Bare_Lkt_Node
         renames Liblktlang.Impl_0.Ref_Id_P_From_Node;
         function Ref_Id_P_First_Var_In_Env
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Ref_Id_P_First_Var_In_Env;
         function Ref_Id_P_Is_Being_Called
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Ref_Id_P_Is_Being_Called;
         function Ref_Id_P_Referenced_Decl
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Decl
         renames Liblktlang.Impl_0.Ref_Id_P_Referenced_Decl;
         function Ref_Id_P_Xtype_Equation
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Ref_Id_P_Xtype_Equation;
         function Ref_Id_P_Referenced_Defining_Name
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Def_Id
         renames Liblktlang.Impl_0.Ref_Id_P_Referenced_Defining_Name;
         function Ref_Id_P_Generic_Type_Equation
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Ref_Id_P_Generic_Type_Equation;
         function Ref_Id_P_Bind_Actual_Type_Equation
           
  (Node : Bare_Ref_Id
      ; First_Var : Internal_Entity_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Ref_Id_P_Bind_Actual_Type_Equation;
         function Ref_Id_P_Xref_Equation
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Ref_Id_P_Xref_Equation;
         function Ref_Id_P_Xlogic_Equation
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Ref_Id_P_Xlogic_Equation;
         function Ref_Id_P_Complete
           
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Complete_Item_Array_Access
         renames Liblktlang.Impl_0.Ref_Id_P_Complete;
         function If_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.If_Expr_P_Has_Context_Free_Type;
         function If_Expr_P_Branch_Exprs
           
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Expr_Array_Access
         renames Liblktlang.Impl_0.If_Expr_P_Branch_Exprs;
         function If_Expr_P_Expected_Branch_Type_Equation
           
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.If_Expr_P_Expected_Branch_Type_Equation;
         function If_Expr_P_Cond_Branches_Equation
           
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.If_Expr_P_Cond_Branches_Equation;
         function If_Expr_P_Xref_Equation
           
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.If_Expr_P_Xref_Equation;
         function Isa_P_Expected_Type_Equation
           
  (Node : Bare_Isa
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Isa_P_Expected_Type_Equation;
         function Isa_P_Xref_Equation
           
  (Node : Bare_Isa
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Isa_P_Xref_Equation;
         function Isa_P_Has_Context_Free_Type
           
  (Node : Bare_Isa
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Isa_P_Has_Context_Free_Type;
         function Keep_Expr_P_Xref_Equation
           
  (Node : Bare_Keep_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Keep_Expr_P_Xref_Equation;
         function Lambda_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Lambda_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Lambda_Expr_P_Has_Context_Free_Type;
         function Lambda_Expr_P_Expected_Type_Equation
           
  (Node : Bare_Lambda_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Lambda_Expr_P_Expected_Type_Equation;
         function Lambda_Expr_P_Generic_Type_Equation
           
  (Node : Bare_Lambda_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Lambda_Expr_P_Generic_Type_Equation;
         function Lambda_Expr_P_Xref_Equation
           
  (Node : Bare_Lambda_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Lambda_Expr_P_Xref_Equation;
         function Big_Num_Lit_P_Xref_Equation
           
  (Node : Bare_Big_Num_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Big_Num_Lit_P_Xref_Equation;
         function Char_Lit_P_Denoted_Value
           
  (Node : Bare_Char_Lit
  )

            return Internal_Decoded_Char_Value
         renames Liblktlang.Implementation.Extensions.Char_Lit_P_Denoted_Value;
         function Char_Lit_P_Xref_Equation
           
  (Node : Bare_Char_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Char_Lit_P_Xref_Equation;
         function Null_Lit_P_Xref_Equation
           
  (Node : Bare_Null_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Null_Lit_P_Xref_Equation;
         function Null_Lit_P_Has_Context_Free_Type
           
  (Node : Bare_Null_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Null_Lit_P_Has_Context_Free_Type;
         function Num_Lit_P_Xref_Equation
           
  (Node : Bare_Num_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Num_Lit_P_Xref_Equation;
         function Dispatcher_String_Lit_P_Denoted_Value
           
  (Node : Bare_String_Lit
  )

            return Internal_Decoded_String_Value
         renames Liblktlang.Impl_0.Dispatcher_String_Lit_P_Denoted_Value;
         function Dispatcher_String_Lit_P_Is_Prefixed_String
           
  (Node : Bare_String_Lit
  )

            return Boolean
         renames Liblktlang.Impl_0.Dispatcher_String_Lit_P_Is_Prefixed_String;
         function Dispatcher_String_Lit_P_Prefix
           
  (Node : Bare_String_Lit
  )

            return Character_Type
         renames Liblktlang.Impl_0.Dispatcher_String_Lit_P_Prefix;
         function String_Lit_P_Is_Regexp_Literal
           
  (Node : Bare_String_Lit
  )

            return Boolean
         renames Liblktlang.Impl_0.String_Lit_P_Is_Regexp_Literal;
         function String_Lit_P_Xref_Equation
           
  (Node : Bare_String_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.String_Lit_P_Xref_Equation;
         function Block_String_Lit_P_Denoted_Value
           
  (Node : Bare_Block_String_Lit
  )

            return Internal_Decoded_String_Value
         renames Liblktlang.Implementation.Extensions.Block_String_Lit_P_Denoted_Value;
         function Block_String_Lit_P_Is_Prefixed_String
           
  (Node : Bare_Block_String_Lit
  )

            return Boolean
         renames Liblktlang.Impl_0.Block_String_Lit_P_Is_Prefixed_String;
         function Block_String_Lit_P_Prefix
           
  (Node : Bare_Block_String_Lit
  )

            return Character_Type
         renames Liblktlang.Impl_0.Block_String_Lit_P_Prefix;
         function Module_Doc_String_Lit_P_Denoted_Value
           
  (Node : Bare_Module_Doc_String_Lit
  )

            return Internal_Decoded_String_Value
         renames Liblktlang.Implementation.Extensions.Module_Doc_String_Lit_P_Denoted_Value;
         function Module_Doc_String_Lit_P_Is_Prefixed_String
           
  (Node : Bare_Module_Doc_String_Lit
  )

            return Boolean
         renames Liblktlang.Impl_0.Module_Doc_String_Lit_P_Is_Prefixed_String;
         function Module_Doc_String_Lit_P_Prefix
           
  (Node : Bare_Module_Doc_String_Lit
  )

            return Character_Type
         renames Liblktlang.Impl_0.Module_Doc_String_Lit_P_Prefix;
         function Single_Line_String_Lit_P_Denoted_Value
           
  (Node : Bare_Single_Line_String_Lit
  )

            return Internal_Decoded_String_Value
         renames Liblktlang.Implementation.Extensions.Single_Line_String_Lit_P_Denoted_Value;
         function Single_Line_String_Lit_P_Is_Prefixed_String
           
  (Node : Bare_Single_Line_String_Lit
  )

            return Boolean
         renames Liblktlang.Implementation.Extensions.Single_Line_String_Lit_P_Is_Prefixed_String;
         function Single_Line_String_Lit_P_Prefix
           
  (Node : Bare_Single_Line_String_Lit
  )

            return Character_Type
         renames Liblktlang.Implementation.Extensions.Single_Line_String_Lit_P_Prefix;
         function Logic_Assign_P_Xref_Equation
           
  (Node : Bare_Logic_Assign
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Logic_Assign_P_Xref_Equation;
         function Logic_Expr_P_Xref_Equation
           
  (Node : Bare_Logic_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Logic_Expr_P_Xref_Equation;
         function Logic_Propagate_P_Generic_Type_Equation
           
  (Node : Bare_Logic_Propagate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Logic_Propagate_P_Generic_Type_Equation;
         function Logic_Propagate_P_Xref_Equation
           
  (Node : Bare_Logic_Propagate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Logic_Propagate_P_Xref_Equation;
         function Logic_Unify_P_Xref_Equation
           
  (Node : Bare_Logic_Unify
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Logic_Unify_P_Xref_Equation;
         function Match_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Match_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Match_Expr_P_Has_Context_Free_Type;
         function Match_Expr_P_Branch_Exprs
           
  (Node : Bare_Match_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Expr_Array_Access
         renames Liblktlang.Impl_0.Match_Expr_P_Branch_Exprs;
         function Match_Expr_P_Expected_Branch_Type_Equation
           
  (Node : Bare_Match_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Match_Expr_P_Expected_Branch_Type_Equation;
         function Match_Expr_P_Xref_Equation
           
  (Node : Bare_Match_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Match_Expr_P_Xref_Equation;
         function Not_Expr_P_Xref_Equation
           
  (Node : Bare_Not_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Not_Expr_P_Xref_Equation;
         function Paren_Expr_P_Expected_Type_Equation
           
  (Node : Bare_Paren_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Paren_Expr_P_Expected_Type_Equation;
         function Paren_Expr_P_Xref_Equation
           
  (Node : Bare_Paren_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Paren_Expr_P_Xref_Equation;
         function Paren_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Paren_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Paren_Expr_P_Has_Context_Free_Type;
         function Raise_Expr_P_Xref_Equation
           
  (Node : Bare_Raise_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Raise_Expr_P_Xref_Equation;
         function Raise_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Raise_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Raise_Expr_P_Has_Context_Free_Type;
         function Subscript_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Subscript_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Subscript_Expr_P_Has_Context_Free_Type;
         function Subscript_Expr_P_Xref_Equation
           
  (Node : Bare_Subscript_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Subscript_Expr_P_Xref_Equation;
         function Try_Expr_P_Exprs
           
  (Node : Bare_Try_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Expr_Array_Access
         renames Liblktlang.Impl_0.Try_Expr_P_Exprs;
         function Try_Expr_P_Expected_Exprs_Type_Equation
           
  (Node : Bare_Try_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Try_Expr_P_Expected_Exprs_Type_Equation;
         function Try_Expr_P_Xref_Equation
           
  (Node : Bare_Try_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Try_Expr_P_Xref_Equation;
         function Try_Expr_P_Has_Context_Free_Type
           
  (Node : Bare_Try_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Try_Expr_P_Has_Context_Free_Type;
         function Un_Op_P_Xref_Equation
           
  (Node : Bare_Un_Op
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Un_Op_P_Xref_Equation;
         function Full_Decl_P_Has_Annotation
           
  (Node : Bare_Full_Decl
      ; Name : Symbol_Type
  )

            return Boolean
         renames Liblktlang.Impl_0.Full_Decl_P_Has_Annotation;
         function Full_Decl_P_Get_Annotation
           
  (Node : Bare_Full_Decl
      ; Name : Symbol_Type
  )

            return Bare_Decl_Annotation
         renames Liblktlang.Impl_0.Full_Decl_P_Get_Annotation;
         function Imported_Name_P_Resolve_Simple_Import
           
  (Node : Bare_Imported_Name
  )

            return Internal_Entity
         renames Liblktlang.Impl_0.Imported_Name_P_Resolve_Simple_Import;
         function Imported_Name_P_Resolve_Import_From
           
  (Node : Bare_Imported_Name
  )

            return Internal_Entity
         renames Liblktlang.Impl_0.Imported_Name_P_Resolve_Import_From;
         function Imported_Name_P_Env_Assoc
           
  (Node : Bare_Imported_Name
  )

            return Internal_Env_Assoc
         renames Liblktlang.Impl_0.Imported_Name_P_Env_Assoc;
         function Dispatcher_Null_Cond_Qualifier_P_As_Bool
           
  (Node : Bare_Null_Cond_Qualifier
  )

            return Boolean
         renames Liblktlang.Impl_0.Dispatcher_Null_Cond_Qualifier_P_As_Bool;
         function Null_Cond_Qualifier_Absent_P_As_Bool
           
  (Node : Bare_Null_Cond_Qualifier_Absent
  )

            return Boolean
         renames Liblktlang.Impl_0.Null_Cond_Qualifier_Absent_P_As_Bool;
         function Null_Cond_Qualifier_Present_P_As_Bool
           
  (Node : Bare_Null_Cond_Qualifier_Present
  )

            return Boolean
         renames Liblktlang.Impl_0.Null_Cond_Qualifier_Present_P_As_Bool;
         function Op_P_Is_Equation_Op
           
  (Node : Bare_Op
  )

            return Boolean
         renames Liblktlang.Impl_0.Op_P_Is_Equation_Op;
         function Op_P_Is_Bool_Op
           
  (Node : Bare_Op
  )

            return Boolean
         renames Liblktlang.Impl_0.Op_P_Is_Bool_Op;
         function Op_P_Is_Arith_Op
           
  (Node : Bare_Op
  )

            return Boolean
         renames Liblktlang.Impl_0.Op_P_Is_Arith_Op;
         function Op_P_Is_Order_Op
           
  (Node : Bare_Op
  )

            return Boolean
         renames Liblktlang.Impl_0.Op_P_Is_Order_Op;
         function Op_P_Is_Commutative
           
  (Node : Bare_Op
  )

            return Boolean
         renames Liblktlang.Impl_0.Op_P_Is_Commutative;
         function Regex_Pattern_P_Denoted_Value
           
  (Node : Bare_Regex_Pattern
  )

            return Internal_Decoded_String_Value
         renames Liblktlang.Implementation.Extensions.Regex_Pattern_P_Denoted_Value;
         function Type_Ref_P_Xref_Entry_Point
           
  (Node : Bare_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Type_Ref_P_Xref_Entry_Point;
         function Type_Ref_P_Referenced_Decl
           
  (Node : Bare_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Internal_Entity_Type_Decl
         renames Liblktlang.Impl_0.Type_Ref_P_Referenced_Decl;
         function Function_Type_Ref_P_Xref_Equation
           
  (Node : Bare_Function_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Function_Type_Ref_P_Xref_Equation;
         function Generic_Type_Ref_P_Xref_Equation
           
  (Node : Bare_Generic_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Generic_Type_Ref_P_Xref_Equation;
         function Simple_Type_Ref_P_Xref_Equation
           
  (Node : Bare_Simple_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Simple_Type_Ref_P_Xref_Equation;
         function Var_Bind_P_Xref_Entry_Point
           
  (Node : Bare_Var_Bind
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Boolean
         renames Liblktlang.Impl_0.Var_Bind_P_Xref_Entry_Point;
         function Var_Bind_P_Xref_Equation
           
  (Node : Bare_Var_Bind
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

            return Logic_Equation
         renames Liblktlang.Impl_0.Var_Bind_P_Xref_Equation;

end Liblktlang.All_Properties;
