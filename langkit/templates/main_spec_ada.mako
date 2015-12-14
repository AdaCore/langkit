## vim: filetype=makoada

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Interfaces; use Interfaces;

with Langkit_Support.AST;                use Langkit_Support.AST;
with Langkit_Support.AST.List;
with Langkit_Support.Bump_Ptr;           use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics;        use Langkit_Support.Diagnostics;
with Langkit_Support.Symbols;            use Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handler; use Langkit_Support.Token_Data_Handler;
with Langkit_Support.Tokens;             use Langkit_Support.Tokens;

package ${_self.ada_api_settings.lib_name} is

   procedure Initialize
     with Export        => True,
          Convention    => C,
          External_Name =>
             "${get_context().ada_api_settings.lib_name.lower()}_initialize";
   ${ada_doc('langkit.initialize', 3)}

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
      AST_Root        : AST_Node;
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

   -----------------------
   -- Enumeration types --
   -----------------------

   ## Output constants so that all concrete AST_Node subclasses get their own
   ## AST_Node_Kind. Nothing can be an instance of an abstract subclass, so
   ## these do not need their own kind. Note that we start from 2 because 1 is
   ## reserved for all lists.
   List_Kind : constant AST_Node_Kind := 1;
   % for cls in _self.astnode_types:
      % if not cls.abstract:
         ${cls.name()}_Kind : constant AST_Node_Kind :=
            ${ctx.node_kind_constants[cls]};
      % endif
   % endfor

   function Image (Value : Boolean) return String is
     (if Value then "True" else "False");

   % for decl in _self.enum_declarations:
   ${decl.public_part}
   % endfor

   % for decl in _self.array_types_declarations:
   ${decl.public_part}
   % endfor

   ---------------------------
   -- ASTNode derived types --
   ---------------------------

   % for decl in _self.incomplete_types_declarations:
   ${decl.public_part}
   % endfor

   % for decl in _self.list_types_declarations:
   ${decl.public_part}
   % endfor

   % for decl in _self.types_declarations:
   ${decl.public_part}
   % endfor

private

   % for decl in _self.types_declarations:
   ${decl.private_part}
   % endfor

   % for decl in _self.list_types_declarations:
   ${decl.private_part}
   % endfor

end ${_self.ada_api_settings.lib_name};
