with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations; use Langkit_Support.Adalog.Operations;

with Support; use Support;

procedure Main is
   use Eq_Int, Eq_Int.Raw_Impl, Eq_Int.Refs;

   X : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
   Y : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   Relations : constant array (Positive range <>) of Relation :=
     (+Logic_All ((+Member (X, (6, 9)),
                   +Member (Y, (9, 16)),
                   +Square (X, 3),
                   +Square (4, Y))),

      +"and" (+Member (Y, (2, 3)), +Square (X, 4)),

      +Logic_All ((+Is_Even (X),
                   +Square (X, 3),
                   +Square (X, Y))));
begin
   X.Dbg_Name := new String'("X");
   Y.Dbg_Name := new String'("Y");

   for R of Relations loop
      Put_Line ((1 .. 72 => '='));
      Print_Relation (R);
      New_Line;
      declare
         N : Natural := 0;
      begin
         while Solve (R) loop
            Put_Line ("Solution: { X =" & Get_Value (X)'Img
                      & "; Y =" & Get_Value (Y)'Img & " }");
            N := N + 1;
         end loop;
         if N = 0 then
            Put_Line ("No solution found");
         end if;
      end;
   end loop;

   Destroy (X.all);
   Destroy (Y.all);
   Free (X);
   Free (Y);
   Release_Relations;
end Main;
