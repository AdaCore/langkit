with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "var myvar = 1;" & ASCII.LF
                  & "def myfunc1;" & ASCII.LF);
begin
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   for D of U.Root.As_Decl_List loop
      Put_Line (D.Image & ".F_Name =");
      Put_Line ("  (direct accessor) " & D.F_Name.Image);
      declare
         I : constant Positive := Index (D.Kind, Decl_F_Name);
      begin
         if Syntax_Field_Reference_From_Index (D.Kind, I) /= Decl_F_Name then
            raise Program_Error;
         end if;
         Put_Line
           ("  (introspection) " & D.Child (I).Image
            & " [index=" & I'Image & "]");
      end;
   end loop;

   Put_Line ("Done.");
end Main;
