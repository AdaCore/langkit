with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an atomic boolean rel solves correctly. No!

procedure Main is
   use T_Solver;
   use Refs;

   X : constant Refs.Logic_Var := Create("X");
   Y : constant Refs.Logic_Var := Create("Y");
   Z : constant Refs.Logic_Var := Create("Z");
   A : constant Refs.Logic_Var := Create("A");
   B : constant Refs.Logic_Var := Create("B");
   C : constant Refs.Logic_Var := Create("C");
   D : constant Refs.Logic_Var := Create("D");
   E : constant Refs.Logic_Var := Create("E");

   R : constant Relation :=
       R_All ((X = Y,
               Y = Z,
               A = Z,
               E = A,
               B = C,
               A = C,
               D = X,
               X = 1));
begin
   Solve_All (R);
end Main;
