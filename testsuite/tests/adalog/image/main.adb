with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs;

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");

   Ors : constant Relation :=
      "or" (Domain (X, (1, 2, 3, 4, 5, 6)),
             "or" (Domain (Y, (10, 11)),
                    "or" (R_Any ((1 => Logic_False)),
                "or" (R_Any (No_Relation_Array),
                  R_All (No_Relation_Array)))));

   R : constant Relation :=
     "and" (Ors,
             "and" (X = Y,
              "and" (R_Any ((1 => Logic_True)),
                R_All ((1 => Logic_True)))));
begin
   Solve_All (R);
end Main;
