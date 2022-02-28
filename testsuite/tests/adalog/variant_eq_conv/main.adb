with Main_Support; use Main_Support;

--  Test stateless predicate constructor

procedure Main is

   use Val_Support;
   use T_Solver;
   use Refs;
   use Solver_Ifc;

   function I (V : Integer) return Val_Type
   is (Val_Type'(K => Int, Int_Val => V));

   function S (V : String) return Val_Type
   is (Val_Type'(K => Str, String_Val => -V));

   X : constant Refs.Logic_Var := Create ("x");
   Y : constant Refs.Logic_Var := Create ("y");

   Str_1    : constant Val_Type := S ("one");
   Str_2    : constant Val_Type := S ("two");
   Str_3    : constant Val_Type := S ("three");
   Str_4    : constant Val_Type := S ("four");
   Str_5    : constant Val_Type := S ("five");
   Str_Many : constant Val_Type := S ("many");

   function To_Str (V : Val_Type) return Val_Type
   is (case V.Int_Val is
       when 1      => Str_1,
       when 2      => Str_2,
       when 3      => Str_3,
       when 4      => Str_4,
       when 5      => Str_5,
       when others => Str_Many);

   function Is_Odd (V : Val_Type) return Boolean is
     (V.Int_Val mod 2 = 1);

   R : constant Relation := R_All
     ((Domain (X, (I (1), I (2), I (3), I (4), I (5), I (6))),
       Domain (Y, (Str_1, Str_5, Str_4)),
       Propagate (X, Y,
                  Converter (To_Str'Access, "to_str")),
       Predicate (X, Predicate (Is_Odd'Access, "is_odd"))));
begin
   Solve_All (R);
end Main;
