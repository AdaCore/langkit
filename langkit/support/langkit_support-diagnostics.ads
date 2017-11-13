with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package Langkit_Support.Diagnostics is

   type Diagnostic is record
      Sloc_Range : Source_Location_Range;
      --  The source location range that the diagnostics message refers to

      Message    : Unbounded_Wide_Wide_String;
      --  Message for this diagnostics.  Parsers allocates such messages and it
      --  is up to the user to free them.
   end record;

   function To_Pretty_String (D : Diagnostic) return String;

   package Diagnostics_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Diagnostic);

   type Diagnostics_Array is array (Positive range <>) of Diagnostic;

   function Create
     (Sloc_Range : Source_Location_Range;
      Message    : Wide_Wide_String) return Diagnostic;
   --  Shortcut to create a diagnostic

   procedure Append
     (Diagnostics : in out Diagnostics_Vectors.Vector;
      Sloc_Range  : Source_Location_Range;
      Message     : Wide_Wide_String);
   --  Shortcut to append a diagnostic to a vector

end Langkit_Support.Diagnostics;
