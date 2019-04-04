with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is

   procedure Assert (Predicate : Boolean; Message : String);
   --  Print Message and raise a Program_Error if Predicate is false

   ------------
   -- Assert --
   ------------

   procedure Assert (Predicate : Boolean; Message : String) is
   begin
      if not Predicate then
         Put_Line ("Error: " & Message);
         raise Program_Error;
      end if;
   end Assert;

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

   Assert (Is_Derived_From (Type_Foo_Node, Type_Foo_Node),
           "root derives from root");

   Assert (Is_Derived_From (Type_Expr, Type_Foo_Node),
           "expr derives from root");

   Assert (Is_Derived_From (Type_Addition, Type_Foo_Node),
           "addition derives from root");

   Assert (not Is_Derived_From (Type_Ref, Type_Number),
           "ref does not derive from number");

   Put_Line ("Done.");
end Main;
