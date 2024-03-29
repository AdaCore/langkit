with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that the solver works properly when you have a predicate applied to a
--  variable *before* this variable can have a value, either through a domain
--  or through a bind/equality operation.

procedure Main is

   use T_Solver, Refs, Solver_Ifc;

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");

   function Is_Even (V : Integer) return Boolean is (V mod 2 = 0);

   R : constant Relation :=
     (((Y = 1 and Predicate (X, Predicate (Is_Even'Access, "Is_Even")))
       or X = 1)
      and X = 2);

begin
   Solve_All (R);
end Main;
