with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations; use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates; use Langkit_Support.Adalog.Predicates;

with Support; use Support;

procedure Main is
   use Eq_Int, Eq_Int.Raw_Impl, Eq_Int.Refs;

   X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
   Y : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   Relations : array (Positive range <>) of Relation :=
     (Member (X, (1, 2)) and Member (Y, (2, 3)) and Equals (X, Y),
      Member (X, (1, 2)) and Equals (X, Y),
      Member (X, (1, 2)) and Equals (Y, X),
      Member (X, (1, 2)) and Is_Even (Y) and Equals (X, Y),
      Member (Y, (1, 2)) and Is_Even (X) and Equals (X, Y));
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
      Free_Relation_Tree (R);
   end loop;

   Destroy (X.all);
   Destroy (Y.all);
end Main;
