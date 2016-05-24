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

end Langkit_Support.Diagnostics;
