with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   ------------
   -- Create --
   ------------

   function Create return Analysis_Unit is
   begin
      return U : constant Analysis_Unit :=
        Create_Context.Get_From_File ("main.txt")
      do
         if U.Has_Diagnostics then
            for D of U.Diagnostics loop
               Put_Line ("  " & U.Format_GNU_Diagnostic (D));
            end loop;
            raise Program_Error;
         end if;
      end return;
   end Create;

end Support;
