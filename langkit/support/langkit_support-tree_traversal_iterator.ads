with Ada.Finalization;

with Langkit_Support.Iterators;
with Langkit_Support.Vectors;

generic
   type Element_Type is private;
   Null_Value : Element_Type;

   with function Children_Count (T : Element_Type) return Natural is <>;
   with function Get_Child
     (T : Element_Type; Child_Index : Natural) return Element_Type is <>;
   with function Get_Parent (T : Element_Type) return Element_Type is <>;

   with package Iterators is new Langkit_Support.Iterators (Element_Type);
package Langkit_Support.Tree_Traversal_Iterator is

   type Traverse_Iterator is new Ada.Finalization.Controlled
     and Iterators.Iterator with private;
   --  Iterator type for Traverse (see below)

   overriding function Next
     (It       : in out Traverse_Iterator;
      Element  : out Element_Type) return Boolean;

   overriding procedure Finalize (It : in out Traverse_Iterator);

   function Create (Root : Element_Type) return Traverse_Iterator;
private

   package Natural_Vectors is new Langkit_Support.Vectors (Natural);

   type Traverse_Iterator is new Ada.Finalization.Controlled
     and Iterators.Iterator with
      record
         Node, Parent : Element_Type := Null_Value;
         Stack        : Natural_Vectors.Vector;
         Continue     : Boolean := True;
      end record;

end Langkit_Support.Tree_Traversal_Iterator;
