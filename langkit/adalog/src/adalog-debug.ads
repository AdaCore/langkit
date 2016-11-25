------------------------------------------------------------------------------
--                               A D A L O G                                --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

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
