with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   --------------------------
   -- Raise_Property_Error --
   --------------------------

   procedure Raise_Property_Error (Message : String := "") is
   begin
      raise Program_Error;
   end Raise_Property_Error;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Elements : Envs.Entity_Array) is
   begin
      if Elements'Length = 0 then
         Put_Line ("  <none>");
      else
         for E of Elements loop
            Put_Line ("  * '" & E.El & "'");
         end loop;
      end if;
   end Put_Line;

end Support;
