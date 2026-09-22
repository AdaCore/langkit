with Ada.Containers.Generic_Array_Sort;

package body Libfoolang.Implementation.Extensions is

   function "<" (Left, Right : Internal_Entity) return Boolean
   is (Left.Node < Right.Node);

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Internal_Entity,
      Array_Type   => Internal_Internal_Entity_Array);

   -----------------------------
   -- Foo_Node_P_Set_To_Array --
   -----------------------------

   function Foo_Node_P_Set_To_Array
     (Node : Bare_Foo_Node;
      S    : Internal_Entity_Set_Access)
      return Internal_Entity_Array_Access
   is
      Result : constant Internal_Entity_Array_Access :=
        Create_Internal_Entity_Array (Natural (S.Items.Length));
      I      : Positive := 1;
   begin
      for Item of S.Items loop
         Result.Items (I) := Item;
         I := I + 1;
      end loop;
      Sort (Result.Items);
      return Result;
   end Foo_Node_P_Set_To_Array;

end Libfoolang.Implementation.Extensions;
