with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is

   procedure Check (Label : String; T : Token_Reference);
   --  Try to access to the token data referenced by ``T``

   -----------
   -- Check --
   -----------

   procedure Check (Label : String; T : Token_Reference) is
   begin
      Put_Line (Label & ":");

      begin
         Put_Line ("  Image: " & Image (T));
      exception
         when Stale_Reference_Error =>
            Put_Line ("Got a Stale_Reference_Error");
      end;

      begin
         Put_Line ("  Unit: " & Simple_Name (Get_Filename (Unit (T))));
      exception
         when Stale_Reference_Error =>
            Put_Line ("Got a Stale_Reference_Error");
      end;

      New_Line;
   end Check;

   U : Analysis_Unit;
   T : Token_Reference;
begin
   Put_Line ("main.adb: Running...");

   --  Create an analysis unit and get a reference to one of its tokens. Then
   --  perform the only legitimate use of this token referenc.

   U := Create_Context.Get_From_Buffer
     (Filename => "foo.txt",
      Buffer   => "example",
      Charset  => "ascii");
   T := U.First_Token;
   Check ("Valid", T);

   --  Reparse the analysis unit, making the token reference stale even though
   --  the analysis context and its token data handlers are still the same.

   U.Reparse (Buffer => "# example");
   Check ("After reparse", T);

   --  Now destroy the analysis unit (and its context), making the token
   --  reference stale.
   --
   T := U.First_Token;
   U := No_Analysis_Unit;
   Check ("After context destruction", T);

   Put_Line ("main.adb: Done.");
end Main;
