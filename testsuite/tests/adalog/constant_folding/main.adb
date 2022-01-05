--  Check that the constant folding done on relations during the preparation
--  for the solver work correctly.

with Langkit_Support.Adalog; use Langkit_Support.Adalog;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver;
   use Refs;

   X : constant Refs.Logic_Var := Create ("X");
begin
   Cst_Folding_Trace.Set_Active (True);

   --  Check that neutral elements are omitted, or replace their parent
   --  compound relation when they are alone.

   Solve_All (R_Any ((Logic_False, X = 1), "neutral-any-1"));
   Solve_All (R_Any ((Logic_False, Logic_False), "neutral-any-2"));

   Solve_All (R_All ((Logic_True, X = 1), "neutral-all-1"));
   Solve_All (R_Any ((Logic_False,
                      R_All ((Logic_True, Logic_True))), "neutral-all-2"));

   --  Check that absorbing elements... do absorb the others

   Solve_All (R_Any ((X = 2, Logic_True, X = 1), "absorb-any"));
   Solve_All (R_All ((X = 2, Logic_False, X = 1), "absorb-all"));
end Main;
