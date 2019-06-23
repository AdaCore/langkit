with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

with GNATCOLL.Traces;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt", Buffer  => "a 1");

   R     : constant Foo_Node := U.Root;
   Ident : constant Root_Node := R.Child (1).As_Root_Node;
   Lit   : constant Root_Node := R.Child (2).As_Root_Node;

   procedure Run (Arg1, Arg2 : Root_Node);

   ---------
   -- Run --
   ---------

   procedure Run (Arg1, Arg2 : Root_Node) is
      Success : Boolean;
   begin
      Put_Line ("== " & Arg1.Short_Image
                & ", " & Arg2.Short_Image & " ==");
      Success := Lit.As_Literal.P_Solve_Eq (Arg1, Arg2);
      Put_Line ("   No exception (" & Success'Image & ")");
   exception
      when Exc : Property_Error =>
      Put_Line ("   Property_Error: " & Exception_Message (Exc));
   end Run;

begin
   GNATCOLL.Traces.Parse_Config_File;
   if U.Has_Diagnostics then
      Put_Line ("Parsing errors...");
      return;
   end if;
   Assign_Names_To_Logic_Vars (R);
   Run (Ident, Ident);
   Run (Ident, Lit);
   Run (Lit, Ident);
   Run (Lit, Lit);
end Main;
