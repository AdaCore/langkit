with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Mode : constant String := (if Argument_Count = 0 then "" else Argument (1));

   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt", Buffer => "example example example");
   N   : constant Foo_Node := U.Root.Child (1);

   Dummy_Int : Integer;
begin
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   if Mode = "" then
      null;

   elsif Mode = "printers" then
      declare
         Dummy : Analysis_Unit;
      begin
         Dummy := N.P_Id_Unit (U);
         Dummy := N.P_Id_Unit (No_Analysis_Unit);
         Dummy := N.P_Id_Unit
           (Ctx.Get_From_Buffer (Filename => "error.txt", Buffer => "foo"));
      end;

      declare
         Dummy : Foo_Node;
      begin
         Dummy := N.P_Id_Node (N);
         Dummy := N.P_Id_Node (No_Foo_Node);
      end;

      Dummy_Int := N.P_Test_Strings;
      Dummy_Int := N.P_Test_Symbols;
      Dummy_Int := N.P_Test_Rebindings;
      Dummy_Int := N.P_Test_Envs;
      Dummy_Int := N.P_Test_Entities;
      Dummy_Int := N.P_Test_Arrays;
      Dummy_Int := N.P_Test_Vectors;
      Dummy_Int := N.P_Test_Tokens;
      Dummy_Int := N.P_Test_Struct (1);

   elsif Mode = "control_flow" then
      Dummy_Int := N.P_Test_Control_Flow (1);
      Dummy_Int := N.P_Test_Control_Flow (2);
      Dummy_Int := N.P_Test_Control_Flow (3);

   elsif Mode = "recursive_cf" then
      U.Reparse (Buffer => "(example example) (example)");

      Dummy_Int := U.Root.P_Test_Recursive_Cf;

   else
      Put_Line ("Invalid mode argument");
   end if;
end Main;
