with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNATCOLL.Strings; use GNATCOLL.Strings;
with Ada.Text_IO;      use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Generic_Main_Support;
with Langkit_Support.Images;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  Test stateless predicate constructor

procedure Main is

   type Kind is (Int, Str);

   type Val_Type (K : Kind := Int) is record
      case K is
         when Int => Int_Val : Integer;
         when Str => String_Val : String_Access;
      end case;
   end record;

   function Image (Self : Val_Type) return String is
     (case Self.K is
         when Int => Langkit_Support.Images.Stripped_Image (Self.Int_Val),
         when Str => Self.String_Val.all);

   package Val_Support
   is new Langkit_Support.Adalog.Generic_Main_Support (Val_Type);

   use Val_Support;
   use T_Solver;
   use Refs;

   function I (V : Integer) return Val_Type
   is (Val_Type'(K => Int, Int_Val => V));

   function S (V : String) return Val_Type
   is (Val_Type'(K => Str, String_Val => new String'(V)));


   X : Raw_Var := Create ("x");
   Y : Raw_Var := Create ("y");

   function To_Str (V : Val_Type) return Val_Type is
     (S (case V.Int_Val is
            when 1 => "One",
            when 2 => "Two",
            when 3 => "Three",
            when 4 => "Four",
            when 5 => "Five",
            when others => "Many"));

   function Eq_Str_Lower (L, R : Val_Type) return Boolean is
     (L.String_Val'Length = R.String_Val'Length
      and then (for all I in L.String_Val'Range
                => To_Lower (L.String_Val (I)) =
                  To_Lower (R.String_Val (I))));

   function Is_Odd (V : Val_Type) return Boolean is
     (V.Int_Val mod 2 = 1);

   R : constant Relation :=
     Domain (X, (I (1), I (2), I (3), I (4), I (5), I (6)))
     and Domain (Y, (S ("one"), S ("five"), S ("four")))
     and Propagate (X, Y,
                    Converter (To_Str'Access, "to_str"),
                    Comparer (Eq_Str_Lower'Access, "eq_str_lower"))
     and Predicate (X, Predicate (Is_Odd'Access, "is_odd"));
begin
   GNATCOLL.Traces.Parse_Config_File;
   Solve_All (R, Show_Relation => True);
end Main;
