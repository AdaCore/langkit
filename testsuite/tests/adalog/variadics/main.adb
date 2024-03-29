with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test a combination of features at the same time:
--  * Predicates
--  * Custom bind
--  * Domains.

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");

   function Is_Even (V : Integer) return Boolean is (V mod 2 = 0);
   function Convert (I : Integer) return Integer is (I * 3);

   R3 : constant Relation :=
     R_All
       ((R_Any
           ((X = 1,
             X = 2,
             X = 3,
             X = 4,
             X = 5,
             X = 6)),
         Propagate (X, Y, Converter (Convert'Access, "*3")),
         Predicate (X, Predicate (Is_Even'Access, "Is_Even")),
         Domain (Y, (12, 18))));

begin
   Solve_All (R3);
end Main;
