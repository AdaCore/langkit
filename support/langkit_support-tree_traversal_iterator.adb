------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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
