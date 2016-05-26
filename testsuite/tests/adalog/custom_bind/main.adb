with Ada.Text_IO;              use Ada.Text_IO;

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Dynamic_Ops;       use Adalog.Dynamic_Ops;
with Adalog.Main_Support;      use Adalog.Main_Support;
with Adalog.Predicates;        use Adalog.Predicates;

--  Test that Member primitives goes along correctly with the "=" operator

procedure Main is
   use Eq_Int; use Eq_Int.Raw_Impl; use Eq_Int.Refs;

   type Dummy_Data is null record;
   D : Dummy_Data;

   function Transform (D : Dummy_Data; I : Integer) return Integer is (I ** 2);
   package Bind is new Eq_Int.Raw_Custom_Bind (Dummy_Data, Transform);
   function Square (X, Y : Eq_Int.Refs.Raw_Var) return Relation
   is (Bind.Create (X, Y, D));
begin
   declare
      X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      Y : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      R : Relation := Member (X, (1, 2, 3, 4, 5, 6)) and Square (X, Y);
   begin
      while Call (R) loop
         Put_Line ("X =" & GetL (X)'Img & ", Y =" & GetL (Y)'Img);
      end loop;
      Free (R);
   end;

   declare
      X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      Y : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

      R : Relation := Member (X, (1, 2, 3, 4, 5, 6))
                      and Square (X, Y) and (Y = 36);
   begin
      while Call (R) loop
         Put_Line ("X =" & GetL (X)'Img & ", Y =" & GetL (Y)'Img);
      end loop;
      Free (R);
   end;

end Main;
