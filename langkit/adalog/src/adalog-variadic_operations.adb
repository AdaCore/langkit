with Adalog.Operations; use Adalog.Operations;

package body Adalog.Variadic_Operations is

   ------------------
   -- Variadic_And --
   ------------------

   function Variadic_And (Rels : Relation_Array) return Relation is
      Ret : Relation;
   begin
      pragma Assert (Rels'Length > 0);

      Ret := Rels (1);

      for I in 2 .. Rels'Last loop
         Ret := Relation (Ret and Rels (I));
      end loop;

      return Ret;
   end Variadic_And;

   -----------------
   -- Variadic_Or --
   -----------------

   function Variadic_Or (Rels : Relation_Array) return Relation is
      Ret : Relation;
   begin
      pragma Assert (Rels'Length > 0);

      Ret := Rels (1);

      for I in 2 .. Rels'Last loop
         Ret := Ret or Rels (I);
      end loop;

      return Ret;
   end Variadic_Or;

end Adalog.Variadic_Operations;
