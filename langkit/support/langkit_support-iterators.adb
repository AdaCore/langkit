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

   function Consume
     (I : in out Iterator'Class) return Element_Vectors.Elements_Array
   is
      Element  : Element_Type;
      V        : Element_Vectors.Vector;
   begin
      while I.Next (Element) loop
         Element_Vectors.Append (V, Element);
      end loop;

      return Element_Vectors.To_Array (V);
   end Consume;

end Langkit_Support.Iterators;
