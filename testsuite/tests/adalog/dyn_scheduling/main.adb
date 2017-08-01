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

   X : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
   Y : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   Relations : array (Positive range <>) of Relation :=
     (Equals (X, Y) and Member (X, (1, 2, 3)),
      --  Simple dynamic scheduling: the second relation must be evaluated
      --  before the first one.

      Member (X, (1, 2, 3))
      and (Member (X, (10, 20)) or Is_Even (Y))
      and Member (Y, (1, 3, 5, 10)),
      --  The second AND relation (OR) cannot be evaluated completely, but it
      --  makes progress.

      Is_Even (Y) and Member (X, (1, 2, 3)),
      --  Unsolvable equation: nothing provides a value for Y, but the equation
      --  still makes progress.

      Is_Even (Y) and Is_Even (X),
      --  Likewise, but the equation makes no progress at all

      Is_Even (Y) or Member (X, (1, 2)),
      --  Likewise, but for ANY relations

      Is_Even (X) or Is_Even (Y),

      Is_Even (X) or (Member (X, (1, 2, 3)) and Is_Even (Y)),

      Member (X, (1, 2, 3))
      and Is_Even (Y)
      and Member (X, (1 => 2))
      and Equals (X, Y)
      --  Make sure that back-tracking, which happens for the second Member,
      --  properly resets the state so that the second evaluation of this
      --  second Member actually checks something. Without a proper reset, this
      --  stateful relation just yields Unsatisfied.
     );

begin
   X.Dbg_Name := new String'("X");
   Y.Dbg_Name := new String'("Y");

   for R of Relations loop
      Put_Line ((1 .. 72 => '='));
      Print_Relation (R);
      declare
         N : Natural := 0;
      begin
         New_Line;
         Reset (X);
         Reset (Y);
         while Solve (R) loop
            Put_Line ("Solution: { X =" & Get_Value (X)'Img
                      & "; Y =" & Get_Value (Y)'Img & " }");
            N := N + 1;
         end loop;
         if N = 0 then
            Put_Line ("No solution found");
         end if;
      exception
         when Langkit_Support.Adalog.Early_Binding_Error =>
            Put_Line ("Got an Early_Binding_Error exception");
      end;
      Dec_Ref (R);
   end loop;

   Destroy (X.all);
   Destroy (Y.all);
end Main;
