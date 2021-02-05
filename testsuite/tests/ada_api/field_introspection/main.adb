with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is
begin
   --  Do an exhaustive dump of all available fields (names, types, indexes)
   --  for this language.

   for Id in Node_Type_Id'Range loop
      Put_Line (Image (DSL_Name (Id)) & " (" & Id'Image & ")");

      Put_Line ("   Fields (with abstract ones):");
      for F of Syntax_Fields (Id) loop
         Put_Line ("   field " & Image (Member_Name (F))
                   & " (" & Image (DSL_Name (Member_Type (F))) & ")");
      end loop;
      New_Line;

      if Is_Concrete (Id) then
         declare
            Kind  : constant Foo_Node_Kind_Type := Kind_For (Id);
         begin
            Put_Line ("   Fields (only concrete ones):");
            for F of Syntax_Fields (Kind) loop
               if Is_Concrete (Id) then
                  declare
                     I : constant Positive := Index (Kind, F);
                  begin
                     Put_Line
                       ("   " & Image (Member_Name (F)) & ":" & I'Image);

                     --  Make sure Syntax_Field_Reference_From_Index is
                     --  consistent with Index.

                     if Syntax_Field_Reference_From_Index (Kind, I) /= F then
                        raise Program_Error;
                     end if;
                  end;
               end if;
            end loop;
            New_Line;
         end;
      end if;

   end loop;

   --  Test that Eval_Syntax_Field works as expected

   declare
      procedure Test
        (Label : String;
         Node  : Foo_Node'Class;
         Field : Syntax_Field_Reference);

      ----------
      -- Test --
      ----------

      procedure Test
        (Label : String;
         Node  : Foo_Node'Class;
         Field : Syntax_Field_Reference)
      is
         Result : Foo_Node;
         Error  : Boolean := False;
      begin
         begin
            Result := Eval_Syntax_Field (Node, Field);
         exception
            when Bad_Type_Error =>
               Error := True;
         end;
         Put_Line (Label & " = " & (if Error
                                    then "<no such field>"
                                    else Result.Image));
      end Test;

      U : constant Analysis_Unit := Create_Context.Get_From_Buffer
        (Filename => "foo.txt", Buffer => "def a; var b = 1;");
      F : constant Fun_Decl := U.Root.Child (1).As_Fun_Decl;
      V : constant Var_Decl := U.Root.Child (2).As_Var_Decl;
   begin
      Test ("F.F_Name", F, Decl_F_Name);
      Test ("F.F_Value", F, Decl_F_Value);
      Test ("F.F_Var_Kw", F, Var_Decl_F_Var_Kw);

      Test ("V.F_Name", V, Decl_F_Name);
      Test ("V.F_Value = ", V, Decl_F_Value);
      Test ("V.F_Var_Kw", V, Var_Decl_F_Var_Kw);
   end;

   Put_Line ("Done.");
end Main;
