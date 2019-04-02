with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is
begin
   for Id in Node_Type_Id'Range loop
      Put_Line (Id'Image);
      Put_Line ("  " & (if Is_Abstract (Id) then "abstract" else "concrete"));

      if Is_Root_Node (Id) then
         Put_Line ("  is root node");
      else
         Put_Line ("  base = " & Base_Type (Id)'Image);
      end if;

      if Is_Concrete (Id) then
         Put_Line ("  kind = " & Kind_For (Id)'Image);
      end if;

      Put_Line ("  derivations:");
      declare
         Derivations : constant Node_Type_Id_Array := Derived_Types (Id);
      begin
         if Derivations'Length = 0 then
            Put_Line ("    <none>");
         end if;
         for D of Derivations loop
            Put_Line ("    " & D'Image);
         end loop;
      end;

      New_Line;
   end loop;

   Put_Line ("Done.");
end Main;
