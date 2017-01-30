with Ada.Text_IO; use Ada.Text_IO;

package body Langkit_Support.Adalog.Debug is

   -----------
   -- Trace --
   -----------

   procedure Trace (Str : String) is
   begin
      if Debug then
         Put_Line (Str);
      end if;
   end Trace;

end Langkit_Support.Adalog.Debug;
