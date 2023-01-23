with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that contradictions generated for topo sort failures are accuracte
--  enough that solving the following problem does not need too many
--  iterations.

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   procedure Solve_First (Rel : Relation; Timeout : Natural := 0);
   --  Wrapper around ``Langkit_Support.Adalog.Solve_First``

   -----------------
   -- Solve_First --
   -----------------

   procedure Solve_First (Rel : Relation; Timeout : Natural := 0) is
   begin
      if Solve_First (Rel, Timeout => Timeout) then
         Put_Line ("Solved!");
      else
         Put_Line ("Failed to solve!");
      end if;
   exception
      when Langkit_Support.Adalog.Timeout_Error =>
         Put_Line ("Failed to solve! (timeout error)");
   end Solve_First;

   function Square (S : Integer) return Integer is (S ** 2);

   Square_Conv : constant Converter_Type'Class :=
      Converter (Square'Access, "Square");

   X1 : constant Refs.Logic_Var := Create ("X1");
   X2 : constant Refs.Logic_Var := Create ("X2");
   X3 : constant Refs.Logic_Var := Create ("X3");
   X4 : constant Refs.Logic_Var := Create ("X4");
   X5 : constant Refs.Logic_Var := Create ("X5");
   X6 : constant Refs.Logic_Var := Create ("X6");
   X7 : constant Refs.Logic_Var := Create ("X7");
   X8 : constant Refs.Logic_Var := Create ("X8");
   X9 : constant Refs.Logic_Var := Create ("X9");

   R1 : constant Relation := Propagate (X1, X2, Square_Conv);
   R2 : constant Relation := Propagate (X1, X2, Square_Conv);
   R3 : constant Relation := Propagate (X3, X4, Square_Conv);
   R4 : constant Relation := Propagate (X3, X4, Square_Conv);
   R5 : constant Relation := Propagate (X5, X6, Square_Conv);
   R6 : constant Relation := Propagate (X5, X6, Square_Conv);
   R7 : constant Relation := Propagate (X7, X8, Square_Conv);
   R8 : constant Relation := Propagate (X7, X8, Square_Conv);
   R9 : constant Relation := Propagate (X7, X9, Square_Conv);

   U1 : constant Relation := Unify (X1, X2);
   U2 : constant Relation := Unify (X1, X2);
   U3 : constant Relation := Unify (X3, X4);
   U4 : constant Relation := Unify (X3, X4);
   U5 : constant Relation := Unify (X5, X6);
   U6 : constant Relation := Unify (X5, X6);
   U7 : constant Relation := Unify (X7, X8);
   U8 : constant Relation := Unify (X7, X8);
   U9 : constant Relation := Unify (X7, X9);

   V1 : constant Relation := Unify (X1, X3);
   V2 : constant Relation := Unify (X1, X3);
   V3 : constant Relation := Unify (X1, X3);
   V4 : constant Relation := Unify (X4, X6);
   V5 : constant Relation := Unify (X4, X6);
   V6 : constant Relation := Unify (X4, X6);
   V7 : constant Relation := Unify (X7, X9);
   V8 : constant Relation := Unify (X7, X9);
   V9 : constant Relation := Unify (X7, X9);

   W1 : constant Relation := Assign (X1, 1);
   W2 : constant Relation := Assign (X2, 2);
   W3 : constant Relation := Assign (X3, 3);
   W4 : constant Relation := Assign (X4, 4);
   W5 : constant Relation := Assign (X5, 5);
   W6 : constant Relation := Assign (X6, 6);
   W7 : constant Relation := Assign (X7, 7);
   W8 : constant Relation := Assign (X8, 8);
   W9 : constant Relation := Assign (X9, 9);

   A1 : constant Relation := R1 or U1 or V1 or W1;
   A2 : constant Relation := R2 or U2 or V2 or W2;
   A3 : constant Relation := R3 or U3 or V3 or W3;
   A4 : constant Relation := R4 or U4 or V4 or W4;
   A5 : constant Relation := R5 or U5 or V5 or W5;
   A6 : constant Relation := R6 or U6 or V6 or W6;
   A7 : constant Relation := R7 or U7 or V7 or W7;
   A8 : constant Relation := R8 or U8 or V8 or W8;
   A9 : constant Relation := R9 or U9 or V9 or W9;

   R : constant Relation :=
      A1 and A2 and A3 and A4 and A5 and A6 and A7 and A8 and A9;
begin
   Solve_First (R);
   Solve_First (R, Timeout => 10);
end Main;

