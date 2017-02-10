with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations;
use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

with Support; use Support;

--  Test that the solver works properly when you have a predicate applied to a
--  variable *before* this variable can have a value, either through a domain
--  or through a bind/equality operation.

procedure Main is

   use Eq_Int; use Eq_Int.Raw_Impl;

   X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   R3 : Relation :=
     (Support.Pred_Int.Create (X, Pred'(others => <>))
      and Member (X, (1, 2, 3, 4, 5, 6)));

   Discard : Boolean;

   use Eq_Int.Refs;

begin
   while R3.Solve loop
      Put_Line ("X =" & Get_Value (X)'Img);
   end loop;
end Main;
