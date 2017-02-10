with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations; use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates; use Langkit_Support.Adalog.Predicates;

with Support; use Support;

--  Test a combination of features at the same time:
--  * Predicates
--  * Custom bind
--  * Domains.

procedure Main is

   pragma Warnings (Off, "reference");

   use Eq_Int; use Eq_Int.Raw_Impl;

   X, Y    : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   R3      : Relation :=
     Pred_2.Create (X, Y, Pred'(others => <>))
     and Member (X, (1, 2, 3, 4, 5, 6))
     and Member (Y, (1, 2, 3, 4, 5, 6));

   Discard : Boolean;

   use Eq_Int.Refs;

begin
   while R3.Solve loop
      Put_Line ("X =" & Get_Value (X)'Img);
      Put_Line ("Y =" & Get_Value (Y)'Img);
   end loop;
end Main;
