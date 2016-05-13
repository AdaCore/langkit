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

end Langkit_Support.Iterators;
