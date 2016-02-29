package body Langkit_Support.Iterators is

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (I    : in out Iterator'Class;
      Proc : access procedure (Element : Element_Type))
   is
      Has_Next : Boolean;
      Element  : Element_Type;
   begin
      loop
         I.Next (Has_Next, Element);
         exit when not Has_Next;
         Proc (Element);
      end loop;
   end Iterate;

end Langkit_Support.Iterators;
