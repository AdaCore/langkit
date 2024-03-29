with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that associations of two domains via the 'or' logic operators is
--  exactly similar to having a single domain that is the concatenation of
--  both.

procedure Main is
   use T_Solver; use Refs;

   X : constant Refs.Logic_Var := Create ("X");
   R : constant Relation :=
     "or" (Domain (X, (1, 2, 3)), Domain (X, (4, 5, 6)));
begin
   Solve_All (R);
end Main;
