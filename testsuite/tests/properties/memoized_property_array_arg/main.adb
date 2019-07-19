with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Unit   : constant Analysis_Unit :=
      Create_Context.Get_From_Buffer ("foo.txt", Buffer => "example");
   Node   : Example;
   Result : Integer;
begin
   if Unit.Has_Diagnostics then
      for D of Unit.Diagnostics loop
         Put_Line (To_Pretty_String (D));
      end loop;
      raise Program_Error;
   end if;
   Node := Unit.Root.As_Example;
   Put_Line ("Calling P_Test_Prop...");

   Result := Node.P_Test_Prop (Node.P_Get_Array, "one");
   Put_Line (Integer'Image (Result));

   if Node.P_Test_Prop (Node.P_Get_Array, "one") /= 1 then
      raise Program_Error;
   end if;

   Put_Line ("Calling P_Test_Prop2...");
   if Node.P_Test_Prop2 ((1 => Unit.Root)) then
      raise Program_Error;
   end if;
   if not Node.P_Test_Prop2 ((1 .. 0 => <>)) then
      raise Program_Error;
   end if;

   Put_Line ("main.adb: done");
end Main;
