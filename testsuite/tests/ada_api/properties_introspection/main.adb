with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is
begin
   for Kind in Foo_Node_Kind_Type'Range loop
      Put_Line ("Properties for " & Kind'Image & ":");
      declare
         P_List : constant Property_Reference_Array := Properties (Kind);
      begin
         if P_List'Length = 0 then
            Put_Line ("   <none>");
         else
            for P of Properties (Kind) loop
               Put_Line ("   " & Property_Name (P));
            end loop;
         end if;
      end;
      New_Line;
   end loop;

   Put_Line ("Done.");
end Main;
