## vim: filetype=makoada

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Langkit_Support.Bump_Ptr;           use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics;        use Langkit_Support.Diagnostics;
with Langkit_Support.Symbols;            use Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handler; use Langkit_Support.Token_Data_Handler;

with ${_self.ada_api_settings.lib_name}.AST_Root;
use ${_self.ada_api_settings.lib_name}.AST_Root;

--  This package provides types and primitives to analyze source files as
--  analysis units.

package ${_self.ada_api_settings.lib_name}.Analysis is

   ----------------------
   -- Analysis context --
   ----------------------

   type Analysis_Context_Type;
   ${ada_doc('langkit.analysis_context_type', 3)}

   type Analysis_Unit_Type;
   ${ada_doc('langkit.analysis_unit_type', 3)}

   type Analysis_Context is access all Analysis_Context_Type;
   type Analysis_Unit is access all Analysis_Unit_Type;

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Analysis_Unit,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   type Analysis_Context_Type is record
      Units_Map : Units_Maps.Map;
      Symbols   : Symbol_Table;

      Charset   : Unbounded_String;
      --  Default charset to use in analysis units
   end record;

   type Analysis_Unit_Type is record
      Context         : Analysis_Context;
      Ref_Count       : Natural;
      AST_Root        : ${root_node_type_name};
      File_Name       : Unbounded_String;
      Charset         : Unbounded_String;
      TDH             : aliased Token_Data_Handler;
      Diagnostics     : Diagnostics_Vectors.Vector;
      With_Trivia     : Boolean;

      AST_Mem_Pool    : Bump_Ptr_Pool;
      --  This memory pool shall only be used for AST parsing. Stored here
      --  because it is more convenient, but one shall not allocate from it.
   end record;

   function Create (Charset : String) return Analysis_Context;
   ${ada_doc('langkit.create_context', 3)}

   function Get_From_File
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False)
      return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_file', 3)}

   function Get_From_Buffer
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Buffer      : String;
      With_Trivia : Boolean := False)
      return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_buffer', 3)}

   procedure Remove (Context   : Analysis_Context;
                     File_Name : String);
   ${ada_doc('langkit.remove_unit', 3)}

   procedure Destroy (Context : in out Analysis_Context);
   ${ada_doc('langkit.destroy_context', 3)}

   procedure Inc_Ref (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_incref')}

   procedure Dec_Ref (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_decref')}

   procedure Reparse (Unit : Analysis_Unit; Charset : String := "");
   ${ada_doc('langkit.unit_reparse_file', 3)}

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "";
      Buffer  : String);
   ${ada_doc('langkit.unit_reparse_buffer', 3)}

   procedure Print (Unit : Analysis_Unit);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output.

   procedure PP_Trivia (Unit : Analysis_Unit);
   --  Debug helper: output a minimal AST with mixed trivias

   procedure Populate_Lexical_Env (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_populate_lexical_env')}


end ${_self.ada_api_settings.lib_name}.Analysis;
