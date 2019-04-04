with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is
begin
   for Id in Node_Type_Id'Range loop
      Put_Line (DSL_Name (Id) & " (" & Id'Image & ")");

      Put_Line ("   Fields (with abstract ones):");
      for F of Fields (Id) loop
         Put_Line ("   field " & Field_Name (F)
                   & " (" & DSL_Name (Field_Type (F)) & ")");
      end loop;
      New_Line;

      if Is_Concrete (Id) then
         declare
            Kind  : constant Foo_Node_Kind_Type := Kind_For (Id);
         begin
            Put_Line ("   Fields (only concrete ones):");
            for F of Fields (Kind) loop
               if Is_Concrete (Id) then
                  declare
                     I : constant Positive := Index (Kind, F);
                  begin
                     Put_Line ("   " & Field_Name (F) & ":" & I'Image);

                     --  Make sure Field_Reference_From_Index is consistent
                     --  with Index.

                     if Field_Reference_From_Index (Kind, I) /= F then
                        raise Program_Error;
                     end if;
                  end;
               end if;
            end loop;
            New_Line;
         end;
      end if;

   end loop;

   Put_Line ("Done.");
end Main;
