with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.Traces;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "example null");

   E : Example;
   N : Null_Node;
begin
   GNATCOLL.Traces.Parse_Config_File;
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   E := U.Root.Child (1).As_Example;
   N := U.Root.Child (2).As_Null_Node;

   Put_Line ("== Lazy_Int ==");
   Put_Line ("On " & E.Image);
   for I in 1 .. 2 loop
      Put ("Attempt" & I'Image & "... ");
      Put_Line (E.F_Lazy_Int'Image);
   end loop;
   New_Line;
   Put_Line ("On " & N.Image);
   Put_Line (N.F_Lazy_Int'Image);
   New_Line;

   Put_Line ("== Lazy_Refcounted ==");
   for I in 1 .. 2 loop
      Put ("Attempt" & I'Image & "... ");
      declare
         A : constant Integer_Array := E.F_Lazy_Refcounted;
      begin
         Put ("(");
         for N of A loop
            Put (N'Image);
         end loop;
         Put_Line (")");
      end;
   end loop;
   New_Line;

   Put_Line ("== Lazy_In_Abstract ==");
   Put_Line (E.F_Lazy_In_Abstract'Image);
   New_Line;

   Put_Line ("== Lazy_Node ==");
   declare
      EF : constant Example := E.P_With_Flag (True);
   begin
      Put_Line ("E.P_Is_Flag_Enabled = " & E.P_Is_Flag_Enabled'Image);
      Put_Line
        ("E.F_Lazy_Node.P_Is_Flag_Enabled = "
         & E.F_Lazy_Node.P_Is_Flag_Enabled'Image);
      New_Line;
      Put_Line ("EF.P_Is_Flag_Enabled = " & EF.P_Is_Flag_Enabled'Image);
      Put_Line
        ("EF.F_Lazy_Node.P_Is_Flag_Enabled = "
         & EF.F_Lazy_Node.P_Is_Flag_Enabled'Image);
   end;
   New_Line;

   Put_Line ("== Lazy_Error ==");
   for I in 1 .. 2 loop
      Put ("Attempt" & I'Image & "... ");
      begin
         Put_Line (E.F_Lazy_Error'Image);
      exception
         when Exc : Property_Error =>
            Put_Line ("Property_Error: " & Exception_Message (Exc));
      end;
   end loop;
   New_Line;

   Put_Line ("main.adb: Done.");
end Main;
