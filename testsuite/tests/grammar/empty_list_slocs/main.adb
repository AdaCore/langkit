with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Main is

   function Token_Image (T : Lk_Token) return String
   is ("token " & Image (T.Text, With_Quotes => True) & " at "
       & Image (T.Sloc_Range));

   procedure Check (Buffer : String);
   function Process (N : Lk_Node) return Visit_Status;

   -------------
   -- Process --
   -------------

   function Process (N : Lk_Node) return Visit_Status is
   begin
      if not N.Is_Null and then N.Is_List_Node then
         Put_Line (N.Image);
         Put_Line ("  start:      " & Token_Image (N.Token_Start));
         Put_Line ("  end:        " & Token_Image (N.Token_End));
         Put_Line ("  sloc range: " & Image (N.Sloc_Range));
      end if;
      return Into;
   end Process;

   -----------
   -- Check --
   -----------

   procedure Check (Buffer : String) is
      U : Lk_Unit;
   begin
      Put_Line ("# " & Image (To_Text (Buffer), With_Quotes => True));
      New_Line;

      U := Create_Context (Self_Id).Get_From_Buffer
        (Filename => "buffer",
         Buffer   => Buffer);
      if U.Has_Diagnostics then
         Put_Line ("Diagnostics:");
         for D of U.Diagnostics loop
            Put_Line ("  " & U.Format_GNU_Diagnostic (D));
         end loop;
         New_Line;
      end if;

      U.Root.Traverse (Process'Access);
      New_Line;
   end Check;

begin
   for C in Character'('A') .. 'C' loop
      Check ((1 => C) & ";");
      Check ((1 => C) & ASCII.LF & "# comment" & ASCII.LF & ";");
   end loop;
end Main;
