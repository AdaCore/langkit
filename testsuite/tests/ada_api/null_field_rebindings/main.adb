with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;

   procedure Check (Label, Decl_Text : String);

   -----------
   -- Check --
   -----------

   procedure Check (Label, Decl_Text : String) is
      Buffer : constant String := "example example " & Decl_Text;
      U : constant Analysis_Unit :=
        Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => Buffer);

      E1 : constant Example := U.Root.Child (1).As_Example;
      E2 : constant Example := U.Root.Child (2).As_Example;
      D  : constant Decl := U.Root.Child (3).As_Decl;

      I1 : constant Identifier := D.F_Name;
      I2 : constant Identifier := D.P_Rebind (E1, E2).F_Name;
   begin
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         raise Program_Error;
      end if;

      Put_Line ("== " & Label & "==");
      Put_Line (I1.Image);
      Put_Line (I2.Image);
      Put_Line ("Equality: " & Boolean'Image (I1 = I2));
      New_Line;
   end Check;

begin
   Check ("Non-null field", "var v");
   Check ("Null field", "var");

   Put_Line ("main.adb: Done");
end Main;
