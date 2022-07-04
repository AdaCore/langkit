--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

package body Langkit_Support.Tree_Traversal_Iterator is

   ----------
   -- Next --
   ----------

   function Next
     (Iterator : in out Traverse_Iterator;
      Element  : out Node_Type) return Boolean
   is
      use Natural_Vectors;

      It    : Traverse_Iterator_Record renames Iterator.Unchecked_Get.all;
      Child : Node_Type;
   begin

      --  We don't have anything to return

      if It.Node = No_Node then
         return False;
      end if;

      --  We have a next element to yield: put it aside and then look for
      --  the element we'll yield at the next iteration: non-null children
      --  first, then siblings.

      Element := It.Node;

      for I in First_Child_Index (It.Node) .. Last_Child_Index (It.Node) loop
         Child := Get_Child (It.Node, I);

         if Child /= No_Node then
            It.Stack.Append (I + 1);
            It.Node := Child;
            return True;
         end if;
      end loop;

      --  We could not find non-null children: look for the next non-null
      --  sibling. If there's none, look for the parent's sibling and so on.

      while not It.Stack.Is_Empty loop
         It.Node := Get_Parent (It.Node);

         for J in It.Stack.Pop .. Last_Child_Index (It.Node) loop
            Child := Get_Child (It.Node, J);

            if Child /= No_Node then
               --  We found a sibling! Remember to look for the next one
               --  when we get back to the parent and proceed.

               It.Stack.Append (J + 1);
               It.Node := Child;
               return True;
            end if;
         end loop;
      end loop;

      --  If we fall through to this, it means that we haven't found a next
      --  node, so put It.Node to null so that Next returns False on the next
      --  run.

      It.Node := No_Node;
      return True;
   end Next;

   -------------
   -- Release --
   -------------

   procedure Release (It : in out Traverse_Iterator_Record) is
   begin
      Natural_Vectors.Destroy (It.Stack);
   end Release;

   --------------------------
   -- Create_Tree_Iterator --
   --------------------------

   procedure Create_Tree_Iterator
     (Root     : Node_Type;
      Iterator : in out Traverse_Iterator'Class) is
   begin
      Iterator.Set (Traverse_Iterator_Record'(Node => Root, others => <>));
   end Create_Tree_Iterator;

end Langkit_Support.Tree_Traversal_Iterator;
