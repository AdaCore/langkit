with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test stateless predicate constructor

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Is_Odd (X : Integer) return Boolean is (X mod 2 = 1);
   function Double (X : Integer) return Integer is (X * 2);

   X : constant Refs.Logic_Var := Create ("x");
   Y : constant Refs.Logic_Var := Create ("y");
   R : constant Relation :=
     Domain (X, (1, 2, 3, 4, 5, 6))
     and Propagate (X, Y, Converter (Double'Access, "double"))
     and Predicate (X, Predicate (Is_Odd'Access, "is_odd"));
begin
   Solve_All (R);
end Main;
