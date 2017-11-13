with Langkit_Support.Text;   use Langkit_Support.Text;

package body Langkit_Support.Diagnostics is

   ----------------------
   -- To_Pretty_String --
   ----------------------

   function To_Pretty_String (D : Diagnostic) return String is
      Sloc        : constant Source_Location := Start_Sloc (D.Sloc_Range);
      Line        : constant String := Sloc.Line'Img;
      Column      : constant String := Sloc.Column'Img;
      Sloc_Prefix : constant String :=
        (if Sloc = No_Source_Location
         then ""
         else (Line (Line'First + 1 .. Line'Last) &
               ":" & Column (Column'First + 1 .. Column'Last)
               & ": "));
   begin
      return Sloc_Prefix & Image (To_Wide_Wide_String (D.Message));
   end To_Pretty_String;

   ------------
   -- Create --
   ------------

   function Create
     (Sloc_Range : Source_Location_Range;
      Message    : Wide_Wide_String) return Diagnostic
   is
   begin
      return (Sloc_Range, To_Unbounded_Wide_Wide_String (Message));
   end Create;

   ------------
   -- Append --
   ------------

   procedure Append
     (Diagnostics : in out Diagnostics_Vectors.Vector;
      Sloc_Range  : Source_Location_Range;
      Message     : Wide_Wide_String)
   is
   begin
      Diagnostics.Append (Create (Sloc_Range, Message));
   end Append;

end Langkit_Support.Diagnostics;
