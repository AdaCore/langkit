--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package Contains the debug configuration for Adalog. There are two
--  main ways of configuring the debug mode in Adalog:
--
--  - The Debug_Enabled constant will determine at compile time whether
--    debugging is enabled or not. If it is False, no traces will be output,
--    and the Step mode won't be usable.
--  - If you set the Debug_Enabled constant to True, you still need to activate
--    a debug mode at runtime via Set_Debug_State.

package Langkit_Support.Adalog.Debug is
   type Debug_State_Type is (None, Trace, Step, Step_At_First_Unsat);
   --  Set the debug state for Adalog:
   --
   --  - If Trace, will trace the execution.
   --  - If Step, will trace and stop at every step of the solve so that you
   --    can trace the solve operation step-by-step.
   --  - If Step_At_First_Unsat, will trace the execution, and set the mode to
   --    Step as soon as *any* relation solving returns Unsatisfied.
   --
   --  WARNING: Trace and step mode are not thread safe. It would not make any
   --  sense to try to use them with solving happening in several threads at
   --  the same time.

   Debug_Enabled : constant Boolean := True;

   function Debug return Boolean
   with Inline;

   procedure Set_Debug_State (Val : Debug_State_Type);
   --  Set the debug state

   function Debug_State return Debug_State_Type;
   --  Get the debug state

   procedure Trace (Str : String);
   --  Will output a string to stdout only if Debug is in Trace | Step

end Langkit_Support.Adalog.Debug;
