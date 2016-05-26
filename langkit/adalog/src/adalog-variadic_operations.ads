with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;

package Adalog.Variadic_Operations is
   type Rel_Array is array (Positive range <>) of Rel;

   function Variadic_And (Rels : Rel_Array) return Rel;
   function Variadic_Or (Rels : Rel_Array) return Rel;

end Adalog.Variadic_Operations;
