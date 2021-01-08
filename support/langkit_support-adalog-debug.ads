------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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
