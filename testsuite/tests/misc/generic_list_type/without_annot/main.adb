with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

with Support; use Support;

procedure Main is
   U : constant Analysis_Unit := Create;
begin
   for L of U.Root.Children loop
      Put_Line ("# " & L.Image);

      case L.Kind is
         when Foo_Mixed_List =>
            declare
               ML : constant Foo_Node_Base_List := L.As_Mixed_List.F_List_Node;
            begin
               ML.Print;
            end;

         when Foo_Single_List =>
            declare
               SL : constant Example_List := L.As_Single_List.F_List_Node;
            begin
               SL.Print;
            end;

         when others =>
            raise Program_Error;
      end case;
   end loop;
end Main;
