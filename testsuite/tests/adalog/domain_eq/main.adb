with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations; use Langkit_Support.Adalog.Operations;

--  Test that Member primitives goes along correctly with the "=" operator

procedure Main is
   use Eq_Int; use Eq_Int.Raw_Impl; use Eq_Int.Refs;
begin
   declare
      X : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      R : constant Relation :=
        +(+(+Member (X, (1, 2, 3, 4, 5, 6))
            or (+Equals (X, 7)))
          or (+Equals (X, 8)));
   begin
      while Solve (R) loop
         Put_Line ("X =" & Get_Value (X)'Img);
      end loop;

      Free (X);
   end;

   declare
      X : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
      Y : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

      X_Constraint : constant Relation :=
         +"or" (+Equals (X, 1),
                +"or" (+Equals (X, 2),
                       +"or" (+Equals (X, 3),
                              +"or" (+Equals (X, 4),
                                     +"or" (+Equals (X, 5),
                                            +Equals (X, 6))))));

      Y_Constraint : constant Relation :=
         +"or" (+Equals (Y, 3), +"or" (+Equals (Y, 2), +Equals (Y, 1)));
      R : constant Relation :=
         +"and" (X_Constraint, +"and" (+Equals (X, Y), Y_Constraint));
   begin
      while Solve (R) loop
         Put_Line ("X =" & Get_Value (X)'Img & ", Y =" & Get_Value (Y)'Img);
      end loop;
      Free (X);
      Free (Y);
   end;

   Release_Relations;
end Main;
