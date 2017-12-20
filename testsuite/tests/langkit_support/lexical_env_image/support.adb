package body Support is

   procedure Raise_Property_Error (Message : String := "") is
   begin
      raise Program_Error;
   end Raise_Property_Error;

end Support;
