with Langkit_Support.Vectors;

package body Langkit_Support.Iterators is

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (I    : in out Iterator'Class;
      Proc : access procedure (Element : Element_Type))
   is
      Element  : Element_Type;
   begin
      while I.Next (Element) loop
         Proc (Element);
      end loop;
   end Iterate;

   -------------
   -- Consume --
   -------------

   function Consume (I : Iterator'Class) return Elements_Array is
      package Element_Vectors
      is new Langkit_Support.Vectors (Element_Type);

      Element  : Element_Type;
      V        : Element_Vectors.Vector;
   begin
      --  Note: This is bad design in Ada: We're hiding mutation of the
      --  Iterator object, because if we make it mutable, then you can no
      --  longer call consume on an expression that returns an Iterator, which
      --  in user APIs is not very friendly, because it means you cannot write
      --  write::
      --
      --      for Element of Node.Find (...).Consume loop
      --         ...
      --      end loop;
      --
      --  You have to declare the iterator explicitly.

      while I'Unrestricted_Access.Next (Element) loop
         Element_Vectors.Append (V, Element);
      end loop;

      return
         Result : constant Elements_Array :=
            Elements_Array (Element_Vectors.To_Array (V))
      do
         Element_Vectors.Destroy (V);
      end return;
   end Consume;

end Langkit_Support.Iterators;
