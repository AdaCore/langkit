--  Check that we properly handle exceptions raised from predicates, i.e. that
--  the solver just propagates the exception.

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   My_Error : exception;

   function Is_Even (Dummy : Integer) return Boolean
   is (raise My_Error);
   P_Is_Even : constant Predicate_Type'Class := Predicate (Is_Even'Access);

   X : constant Refs.Logic_Var := Create ("X");

   --  Build a relation so that the Simplify optimization tries to evaluate the
   --  predicate.

   R : constant Relation :=
     R_All ((X = 1, R_Any ((X = 2, Predicate (X, P_Is_Even)))));
begin
   Solve_All (R);
exception
   when My_Error =>
      Put_Line ("... got a My_Error");
end Main;
