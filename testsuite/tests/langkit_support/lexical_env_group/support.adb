with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Elements : Envs.Entity_Array) is
   begin
      if Elements'Length = 0 then
         Put_Line ("  <none>");
      else
         for E of Elements loop
            Put_Line ("  * '" & E.Node & "'");
         end loop;
      end if;
   end Put_Line;

end Support;
