with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   ---------------
   -- Say_Hello --
   ---------------

   procedure Say_Hello is
   begin
      Put_Line ("Hello " & Label & "!");
   end Say_Hello;

end Support;
