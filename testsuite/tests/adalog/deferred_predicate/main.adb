with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations;
use Langkit_Support.Adalog.Operations;

with Support; use Support;

--  Test that the solver works properly when you have a predicate applied to a
--  variable *before* this variable can have a value, either through a domain
--  or through a bind/equality operation.

procedure Main is

   use Eq_Int; use Eq_Int.Raw_Impl;

   X : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   R3 : constant Relation :=
     +(+Support.Pred_Int.Create (X, Pred'(null record))
       and (+Member (X, (1, 2, 3, 4, 5, 6))));

   Discard : Boolean;

   use Eq_Int.Refs;

begin
   while Solve (R3) loop
      Put_Line ("X =" & Get_Value (X)'Img);
   end loop;
   Free (X);
   Release_Relations;
end Main;
