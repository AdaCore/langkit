with Ada.Text_IO;              use Ada.Text_IO;

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Dynamic_Ops;       use Adalog.Dynamic_Ops;
with Adalog.Main_Support;      use Adalog.Main_Support;
with Adalog.Predicates;        use Adalog.Predicates;

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

   type Dummy_Data is null record;
   function Transform (D : Dummy_Data; I : Integer) return Integer is (I * 3);
   package Bind is new Eq_Int.Raw_Custom_Bind (Dummy_Data, Transform);

   D : Dummy_Data;

   R3 : Relation :=
     (Member (X, (1, 2, 3, 4, 5, 6))
      and Bind.Create (X, Y, D)
      and Pred_Int.Create (X, Is_Even'Unrestricted_Access)
      and Member (Y, (12, 18)));

   Discard : Boolean;

   use Eq_Int.Refs;

begin
   while Call (R3) loop
      Put_Line ("X =" & GetL (X)'Img & ", Y =" & GetL (Y)'Img);
   end loop;
end Main;
