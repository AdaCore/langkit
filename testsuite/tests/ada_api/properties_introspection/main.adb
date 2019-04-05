with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is

   function Constraint_Image (C : Value_Constraint) return String;
   --  Return a human readable text representation for C

   ----------------------
   -- Constraint_Image --
   ----------------------

   function Constraint_Image (C : Value_Constraint) return String is
      Kind_Img : constant String := C.Kind'Image;
   begin
      if C.Kind = Node_Value then
         return Kind_Img & " (" & DSL_Name (C.Node_Type) & ")";
      else
         return Kind_Img;
      end if;
   end Constraint_Image;

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
               Put_Line ("   return type: "
                         & Constraint_Image (Property_Return_Type (P)));

               Put_Line ("   arguments:");
               declare
                  A_List : constant Value_Constraint_Array :=
                     Property_Argument_Types (P);
               begin
                  if A_List'Length = 0 then
                     Put_Line ("      <none>");
                  else
                     for I in A_List'Range loop
                        Put_Line ("      " & Property_Argument_Name (P, I)
                                  & ": " & Constraint_Image (A_List (I)));
                     end loop;
                     New_Line;
                  end if;
               end;

               New_Line;
            end loop;
         end if;
      end;
      New_Line;
   end loop;

   Put_Line ("Done.");
end Main;
