package body Langkit_Support.Tree_Traversal_Iterator is

   ----------
   -- Next --
   ----------

   function Next (It      : in out Traverse_Iterator;
                  Element : out Element_Type) return Boolean
   is
      use Natural_Vectors;

      Child  : Element_Type;
   begin

      --  We don't have anything to return

      if It.Node = Null_Value then
         return False;
      end if;

      --  We have a next element to yield: put it aside and then look for
      --  the element we'll yield at the next iteration: first non-null
      --  children first, then siblings.

      Element := It.Node;

      for I in First_Child_Index (It.Node) .. Last_Child_Index (It.Node) + 1
      loop
         Child := Get_Child (It.Node, I);

         if Child /= Null_Value then
            Append (It.Stack, I + 1);
            It.Node := Child;
            return True;
         end if;
      end loop;

      --  We could not find non-null children: look for the next non-null
      --  sibling. If there's none, look for the parent's sibling and so on.

      while Length (It.Stack) > 0 loop
         It.Node := Get_Parent (It.Node);

         for J in Pop (It.Stack) .. Children_Count (It.Node) loop
            Child := Get_Child (It.Node, J);

            if Child /= Null_Value then
               --  We found a sibling! Remember to look for the next one
               --  when we get back to the parent and proceed.

               Append (It.Stack, J + 1);
               It.Node := Child;
               return True;
            end if;
         end loop;
      end loop;

      --  If we fall through to this, it means that we haven't found a next
      --  node, so put It.Node to null so that Next returns False on the next
      --  run.

      It.Node := Null_Value;
      return True;
   end Next;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (It : in out Traverse_Iterator) is
   begin
      Natural_Vectors.Destroy (It.Stack);
   end Finalize;

   --------------
   -- Traverse --
   --------------

   function Create (Root : Element_Type) return Traverse_Iterator is
   begin
      return Traverse_Iterator'
        (Ada.Finalization.Limited_Controlled with
         Root, Null_Value, others => <>);
   end Create;

end Langkit_Support.Tree_Traversal_Iterator;
