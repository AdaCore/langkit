with Ada.Characters.Handling; use Ada.Characters.Handling;

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

   function To_Str (V : Val_Type) return Val_Type is
     (S (case V.Int_Val is
            when 1      => "One",
            when 2      => "Two",
            when 3      => "Three",
            when 4      => "Four",
            when 5      => "Five",
            when others => "Many"));

   function Eq_Str_Lower (L, R : Val_Type) return Boolean is
     (L.String_Val'Length = R.String_Val'Length
      and then (for all I in L.String_Val'Range
                => To_Lower (L.String_Val (I)) = To_Lower (R.String_Val (I))));

   function Is_Odd (V : Val_Type) return Boolean is
     (V.Int_Val mod 2 = 1);

   R : constant Relation := R_All
     ((Domain (X, (I (1), I (2), I (3), I (4), I (5), I (6))),
       Domain (Y, (S ("one"), S ("five"), S ("four"))),
       Propagate (X, Y,
                  Converter (To_Str'Access, "to_str"),
                  Comparer (Eq_Str_Lower'Access, "eq_str_lower")),
       Predicate (X, Predicate (Is_Odd'Access, "is_odd"))));
begin
   Solve_All (R);
end Main;
