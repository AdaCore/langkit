with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is
begin
   if Lookup_DSL_Name ("unknown") /= None then
      raise Program_Error;
   end if;

   for Id in Node_Type_Id'Range loop
      Put_Line (DSL_Name (Id) & " (" & Id'Image & ")");
      Put_Line ("  " & (if Is_Abstract (Id) then "abstract" else "concrete"));

      if Id /= Lookup_DSL_Name (DSL_Name (Id)) then
         raise Program_Error;
      end if;

      if Is_Root_Node (Id) then
         Put_Line ("  is root node");
      else
         Put_Line ("  base = " & DSL_Name (Base_Type (Id)));
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
            Put_Line ("    " & DSL_Name (D));
         end loop;
      end;

      New_Line;
   end loop;

   Put_Line ("Done.");
end Main;
