with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx          : constant Analysis_Context := Create_Context;
   Unit         : constant Analysis_Unit :=
      Get_From_Buffer (Ctx, "foo.txt", Buffer => "example");
   Node   : Example;
   Result : Integer;
begin
   if Has_Diagnostics (Unit) then
      for D of Diagnostics (Unit) loop
         Put_Line (To_Pretty_String (D));
      end loop;
      raise Program_Error;
   end if;
   Node := Root (Unit).As_Example;
   Put_Line ("Calling P_Test_Prop...");

   Result := Node.P_Test_Prop (Node.P_Get_Array, "one");
   Put_Line (Integer'Image (Result));

   if Node.P_Test_Prop (Node.P_Get_Array, "one") /= 1 then
      raise Program_Error;
   end if;
end Main;
