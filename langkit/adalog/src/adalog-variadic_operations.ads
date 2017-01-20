with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;

package Adalog.Variadic_Operations is
   function Variadic_And (Rels : Relation_Array) return Relation;
   function Variadic_Or (Rels : Relation_Array) return Relation;

end Adalog.Variadic_Operations;
