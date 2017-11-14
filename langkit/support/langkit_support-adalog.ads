package Langkit_Support.Adalog with Pure is

   Early_Binding_Error : exception;
   --  Exception raised when an equation cannot be solved because a logic
   --  variable is referenced but is never assigned a value.

   Timeout_Error : exception;
   --  Exception raised when the resolution of a complex relation exceeded the
   --  number of steps allowed.

end Langkit_Support.Adalog;
