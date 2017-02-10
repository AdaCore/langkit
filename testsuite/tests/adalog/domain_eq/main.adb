with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations; use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates; use Langkit_Support.Adalog.Predicates;

--  Test that Member primitives goes along correctly with the "=" operator

procedure Main is
   use Eq_Int; use Eq_Int.Raw_Impl; use Eq_Int.Refs;
begin
   declare
      X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      R : Relation :=
        Member (X, (1, 2, 3, 4, 5, 6)) or Equals (X, 7) or Equals (X, 8);
   begin
      while R.Solve loop
         Put_Line ("X =" & Get_Value (X)'Img);
      end loop;
   end;

   declare
      X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      Y : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      R : Relation :=
        (Equals (X, 1) or
         Equals (X, 2) or
         Equals (X, 3) or
         Equals (X, 4) or
         Equals (X, 5) or
         Equals (X, 6))
         and Equals (X, Y)
         and (Equals (Y, 3) or
              Equals (Y, 2) or
              Equals (Y, 1));
   begin
      while R.Solve loop
         Put_Line ("X =" & Get_Value (X)'Img & ", Y =" & Get_Value (Y)'Img);
      end loop;
   end;
end Main;
