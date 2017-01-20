package Adalog.Debug is
   type Debug_State_Type is (None, Trace, Step);

   Debug_State : constant Debug_State_Type := None;
   --  If Trace, will trace the execution. If step, will trace and stop at
   --  every step of the solve so that you can trace the solve operation
   --  step-by-step.
   --  WARNING!!! Trace and step mode are not thread safe. It would not make
   --  any sense to try to use them with solving happning in several threads at
   --  the same time.

   pragma Warnings (Off, "always");
   function Debug return Boolean is (Debug_State in Trace | Step)
   with Inline;
   pragma Warnings (On, "always");

   procedure Trace (Str : String);
   --  Will output a string to stdout only if Debug is True
end Adalog.Debug;
