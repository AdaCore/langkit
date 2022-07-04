package body Libfoolang.Implementation.Extensions is

   ---------------------------
   -- Example_P_Raise_Error --
   ---------------------------

   function Example_P_Raise_Error
     (Node       : Bare_Example;
      Prop_Error : Boolean) return Boolean
   is
      pragma Unreferenced (Node);
   begin
      return
        (if Prop_Error
         then raise Property_Error with "This is a Property_Error"
         else raise Precondition_Failure
                    with "This is a Precondition_Failure");
   end Example_P_Raise_Error;

end Libfoolang.Implementation.Extensions;
