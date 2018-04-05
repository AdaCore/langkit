## vim: filetype=makoada

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

package ${ada_lib_name}.Unparsing.Implementation is

   type Unparsing_Buffer is limited record
      Content : Unbounded_Wide_Wide_String;
      --  Append-only text buffer for the unparsed tree

      Last_Sloc : Source_Location := (1, 1);
      --  Source location of the next character to append to Content
   end record;

   procedure Append
     (Buffer : in out Unparsing_Buffer; Char : Wide_Wide_Character);
   procedure Append (Buffer : in out Unparsing_Buffer; Text : Text_Type);
   --  Append text to Buffer, updating Buffer.Last_Sloc accordingly

   function Unparse
     (Node                : access Abstract_Node_Type'Class;
      Unit                : Analysis_Unit;
      Preserve_Formatting : Boolean) return String;
   --  Turn the Node tree into a string that can be re-parsed to yield the same
   --  tree (source locations excepted). The encoding used is the same as the
   --  one that was used to parse Node's analysis unit.
   --
   --  If Preserve_Formatting is true, use token/trivia information when
   --  available to preserve original source code formatting.

   function Unparse
     (Node                : access Abstract_Node_Type'Class;
      Unit                : Analysis_Unit;
      Preserve_Formatting : Boolean) return String_Access;
   --  Likewise, but return a string access. Callers must deallocate the result
   --  when done with it.

   function Unparse
     (Node                : access Abstract_Node_Type'Class;
      Preserve_Formatting : Boolean) return Text_Type;
   --  Likewise, but return a text access. Callers must deallocate the result
   --  when done with it.

end ${ada_lib_name}.Unparsing.Implementation;
