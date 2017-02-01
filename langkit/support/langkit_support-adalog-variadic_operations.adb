with Langkit_Support.Adalog.Operations; use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Pure_Relations;
use Langkit_Support.Adalog.Pure_Relations;

package body Langkit_Support.Adalog.Variadic_Operations is

   ------------------
   -- Variadic_And --
   ------------------

   function Variadic_And (Rels : Relation_Array) return Relation is
      Ret, Old_Ret : Relation;
   begin
      if Rels'Length = 0 then
         return True_Rel;
      end if;

      Ret := Rels (Rels'First);
      Inc_Ref (Ret);

      --  We Inc_Ref here because:
      --
      --  - If Rels has only one element, we return a new ownership share for
      --    an already existing relation.
      --
      --  - If Rels has several elements, this share will be automatically
      --    Dec_Ref'd in the for loop below.

      for I in Rels'First + 1 .. Rels'Last loop
         Old_Ret := Ret;
         Ret := Relation (Ret and Rels (I));

         Dec_Ref (Old_Ret);
         --  Here we Dec_Ref, because either Old_Ret is the first rel and
         --  has been Inc_Ref'd before, either it is an And relation that was
         --  created with an ownership that we now renounce, because it is
         --  owned by the new And.
      end loop;

      return Ret;
   end Variadic_And;

   -----------------
   -- Variadic_Or --
   -----------------

   function Variadic_Or (Rels : Relation_Array) return Relation is
      Ret, Old_Ret : Relation;
   begin
      if Rels'Length = 0 then
         return False_Rel;
      end if;

      Ret := Rels (Rels'First);
      Inc_Ref (Ret);

      for I in Rels'First + 1 .. Rels'Last loop
         Old_Ret := Ret;
         Ret := Ret or Rels (I);
         Dec_Ref (Old_Ret);
      end loop;

      return Ret;
   end Variadic_Or;

end Langkit_Support.Adalog.Variadic_Operations;
