with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Libfoolang.Analysis;   use Libfoolang.Analysis;
with Libfoolang.Common;     use Libfoolang.Common;

procedure Main is
begin
   Put_Line ("main.adb: Running...");

   declare
      U : constant Analysis_Unit := Create_Context.Get_From_Buffer
        (Filename => "main.txt", Buffer => "example");
      N : constant Synth_Node := U.Root.As_Example.P_Get;
   begin
      Put_Line ("Root.Is_Synthetic = " & Boolean'Image (U.Root.Is_Synthetic));

      Put_Line (N.Image);

      Put_Line ("Is_Synthetic = " & Boolean'Image (N.Is_Synthetic));

      begin
         Put_Line
           ("Text = No exception: " & Image (N.Text, With_Quotes => True));
      exception
         when Exc : Property_Error =>
            Put_Line ("Text = Property_Error: "
                      & Exception_Message (Exc));
      end;

      begin
         Put_Line ("Sloc_Range = No exception: " & Image (N.Sloc_Range));
      exception
         when Exc : Property_Error =>
            Put_Line ("Sloc_Range = Property_Error: "
                      & Exception_Message (Exc));
      end;

   end;

   Put_Line ("main.adb: Done.");
end Main;
