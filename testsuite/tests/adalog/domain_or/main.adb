with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Adalog.Operations; use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates; use Langkit_Support.Adalog.Predicates;

--  Test that associations of two domains via the 'or' logic operators is
--  exactly similar to having a single domain that is the concatenation of
--  both.

procedure Main is
   use Eq_Int; use Eq_Int.Raw_Impl; use Eq_Int.Refs;

   X : Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
   R : Relation := +"or" (+Member (X, (1, 2, 3)), +Member (X, (4, 5, 6)));
begin
   while Solve (R) loop
      Put_Line ("X =" & Get_Value (X)'Img);
   end loop;
   Free (X);
   Release_Relations;
end Main;
