with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Generic_API.Introspection;
use Libfoolang.Generic_API.Introspection;
with Libfoolang.Rewriting;     use Libfoolang.Rewriting;

procedure Iter_Units is
   Buffer_A : constant String := "def a = 1" & ASCII.LF;
   Buffer_B : constant String := "def b = 2" & ASCII.LF;
   Buffer_C : constant String := "def c = 3" & ASCII.LF;

   Ctx    : constant Analysis_Context := Create_Context;
   Unit_A : constant Analysis_Unit :=
      Get_From_Buffer (Ctx, "a.txt", Buffer => Buffer_A);
   Unit_B : constant Analysis_Unit :=
      Get_From_Buffer (Ctx, "b.txt", Buffer => Buffer_B);
   Dummy  : constant Analysis_Unit :=
      Get_From_Buffer (Ctx, "c.txt", Buffer => Buffer_C);

   RH : Rewriting_Handle := Start_Rewriting (Ctx);

   DA : constant Node_Rewriting_Handle := Handle (Root (Unit_A).Child (1));
   DB : constant Node_Rewriting_Handle := Handle (Root (Unit_B).Child (1));
begin
   Set_Child
     (DA,
      Syntax_Field_Index (Member_Refs.Def_F_Expr, Type_Refs.Def),
      Create_Token_Node (RH, Foo_Literal, "11"));
   Set_Child
     (DB,
      Syntax_Field_Index (Member_Refs.Def_F_Expr, Type_Refs.Def),
      Create_Token_Node (RH, Foo_Literal, "22"));

   declare
      function Is_Before (Left, Right : Unit_Rewriting_Handle) return Boolean;

      ---------------
      -- Is_Before --
      ---------------

      function Is_Before (Left, Right : Unit_Rewriting_Handle) return Boolean
      is
      begin
         return Get_Filename (Unit (Left)) < Get_Filename (Unit (Right));
      end Is_Before;

      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Positive,
         Unit_Rewriting_Handle,
         Unit_Rewriting_Handle_Array,
         Is_Before);

      Units : Unit_Rewriting_Handle_Array := Unit_Handles (RH);
   begin
      --  Sort units per filename so that we get stable output ordering across
      --  runs.
      Sort (Units);

      for U of Units loop
         declare
            F : constant String :=
               +Create (+Get_Filename (Unit (U))).Base_Name;
         begin
            Put_Line ("Rewritten unit " & F & ": "
                      & Image (Unparse (Root (U))));
         end;
      end loop;
   end;

   Abort_Rewriting (RH);

   Put_Line ("iter_units.adb: Done.");
end Iter_Units;
