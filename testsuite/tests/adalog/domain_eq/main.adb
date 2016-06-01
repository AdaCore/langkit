with Ada.Text_IO;              use Ada.Text_IO;

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Main_Support;      use Adalog.Main_Support;
with Adalog.Operations;        use Adalog.Operations;
with Adalog.Predicates;        use Adalog.Predicates;

--  Test that Member primitives goes along correctly with the "=" operator

procedure Main is
   use Eq_Int; use Eq_Int.Raw_Impl; use Eq_Int.Refs;
begin
   declare
      X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      R : Relation := Member (X, (1, 2, 3, 4, 5, 6)) or X = 7 or x = 8;
   begin
      while R.Solve loop
         Put_Line ("X =" & GetL (X)'Img);
      end loop;
   end;

   declare
      X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      Y : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      R : Relation :=
        (X = 1 or X = 2 or X = 3 or X = 4 or X = 5 or X = 6)
         and X = Y
         and (Y = 3 or Y = 2 or Y = 1);
   begin
      while R.Solve loop
         Put_Line ("X =" & GetL (X)'Img & ", Y =" & GetL (Y)'Img);
      end loop;
   end;
end Main;
