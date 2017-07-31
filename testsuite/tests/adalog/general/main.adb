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

   function Is_Even (X : Integer) return Boolean is ((X mod 2) = 0);

   use Eq_Int; use Eq_Int.Raw_Impl;

   X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
   Y : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   D : Dummy_Data;

   R3 : Relation :=
     (Member (X, (1, 2, 3, 4, 5, 6))
      and Bind.Create (X, Y, D, Support.No_Data)
      and Pred_Int.Create (X, Is_Even'Unrestricted_Access)
      and Member (Y, (12, 18)));

   Discard : Boolean;

   use Eq_Int.Refs;

begin
   while Solve (R3) loop
      Put_Line ("X =" & Get_Value (X)'Img & ", Y =" & Get_Value (Y)'Img);
   end loop;
end Main;
