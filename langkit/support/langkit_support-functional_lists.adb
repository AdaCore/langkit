pragma Ada_2012;
package body Langkit_Support.Functional_Lists is

   package Alloc_List is new Bump.Alloc (List_Node, Node_Ptr);

   function Create return List is (List'(Pool => Bump.Create, others => <>));

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out List) is
   begin
      Bump.Free (Self.Pool);
      Self := No_List;
   end Destroy;

   ---------
   -- "&" --
   ---------

   function "&" (L : T; R : List) return List is
      Tmp : Node_Ptr;
      Ls : constant List := (if R = No_List then Create else R);
   begin

      return Ret : List do
         Ret := Ls;
         Ret.Length := Ls.Length + 1;
         Tmp := Ls.First;
         Ret.First := Alloc_List.Alloc (Ls.Pool);
         Ret.First.El := L;
         Ret.First.Next := Tmp;
      end return;
   end "&";

   ----------
   -- Head --
   ----------

   function Head (Self : List) return T is
   begin
      return Self.First.El;
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail (Self : List) return List is
   begin
      if Self.First.Next /= null then
         return List'(Self.Pool, Self.First.Next, Self.Length - 1);
      else
         return No_List;
      end if;
   end Tail;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : List) return Boolean is
   begin
      return Self.First /= null;
   end Has_Element;

   ------------
   -- Length --
   ------------

   function Length (Self : List) return Natural is (Self.Length);

   --------------
   -- To_Array --
   --------------

   function To_Array (Self : List) return T_Array is
      Ret : T_Array (1 .. Self.Length);
      I : Positive := 1;
      Current : List := Self;
   begin
      while Has_Element (Current) loop
         Ret (I) := Head (Current);
         Current := Tail (Current);
         I := I + 1;
      end loop;
      return Ret;

--        for El of Self loop
--           Ret (I) := El;
--           I := I + 1;
--        end loop;
--        return Ret;
   end To_Array;

end Langkit_Support.Functional_Lists;
