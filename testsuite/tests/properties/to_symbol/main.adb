with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Text;   use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt", Buffer => "example");
   N : constant Example := U.Root.As_Example;

   procedure Run (S1, S2 : Text_Type);

   ---------
   -- Run --
   ---------

   procedure Run (S1, S2 : Text_Type) is
   begin
      Put (Image (S1, With_Quotes => True)
           & " = "
           & Image (S2, With_Quotes => True)
           & ": ");

      begin
         Put_Line (N.P_Compare_As_Symbols (S1, S2)'Image);
      exception
         when Exc : Property_Error =>
            Put_Line ("Property_Error: " & Exception_Message (Exc));
      end;
   end Run;

begin
   Run ("Hello", "HELLO");
   Run ("Foo", "Bar");
   Run ("Foo.Bar", "Foo.Bar");

   New_Line;
   Put_Line ("main.adb: Done");
end Main;
