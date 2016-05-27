with Ada.Text_IO;              use Ada.Text_IO;

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Main_Support;      use Adalog.Main_Support;
with Adalog.Operations;        use Adalog.Operations;
with Adalog.Predicates;        use Adalog.Predicates;

--  Test member primitive

procedure Main is
   use Eq_Int; use Eq_Int.Raw_Impl; use Eq_Int.Refs;

   X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
   R : Relation := Member (X, (1, 2, 3, 4, 5, 6));
begin
   while Call (R) loop
      Put_Line ("X =" & GetL (X)'Img);
   end loop;
end Main;
