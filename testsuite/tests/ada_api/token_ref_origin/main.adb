with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is

   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "foo.txt",
      Buffer   => "example",
      Charset  => "ascii");

   procedure Check;
   --  Print the origin filename/charset for U's only token

   -----------
   -- Check --
   -----------

   procedure Check is
      T : constant Token_Reference := U.First_Token;
      F : constant String := +Create (+Origin_Filename (T)).Base_Name;
   begin
      Put_Line ("filename: """ & F & """");
      Put_Line ("charset: " & Origin_Charset (T));
      New_Line;
   end Check;

begin
   Check;
   U.Reparse (Charset => "utf-8");
   Check;
   Put_Line ("main.adb: Done.");
end Main;
