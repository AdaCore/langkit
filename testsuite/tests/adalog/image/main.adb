with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations;
use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;
with Langkit_Support.Adalog.Pure_Relations;
use Langkit_Support.Adalog.Pure_Relations;

procedure Main is
   use Eq_Int; use Eq_Int.Raw_Impl; use Eq_Int.Refs;

   X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
   Y : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   R : Relation :=
     (Member (X, (1, 2, 3, 4, 5, 6))
      or Member (Y, (10, 11))
      or Logic_Any ((1 => False_Rel))
      or Logic_Any (Empty_Array)
      or Logic_All (Empty_Array))
     and Equals (X, Y)
     and Logic_Any ((1 => True_Rel))
     and Logic_All ((1 => True_Rel));
begin
   X.Dbg_Name := new String'("X");
   Y.Dbg_Name := new String'("Y");

   Print_Relation (R);

   Dec_Ref (R);
   Destroy (X.all);
   Destroy (Y.all);
end Main;
