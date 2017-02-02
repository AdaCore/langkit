package Langkit_Support.Adalog.Debug is
   type Debug_State_Type is (None, Trace, Step);
   --  If Trace, will trace the execution. If step, will trace and stop at
   --  every step of the solve so that you can trace the solve operation
   --  step-by-step.
   --  WARNING!!! Trace and step mode are not thread safe. It would not make
   --  any sense to try to use them with solving happning in several threads at
   --  the same time.

   Debug_Enabled : constant Boolean := True;

   pragma Warnings (Off, "always");
   function Debug return Boolean
   with Inline;
   pragma Warnings (On, "always");

   procedure Set_Debug_State (Val : Debug_State_Type);
   function Debug_State return Debug_State_Type;

   procedure Trace (Str : String);
   --  Will output a string to stdout only if Debug is True
end Langkit_Support.Adalog.Debug;
