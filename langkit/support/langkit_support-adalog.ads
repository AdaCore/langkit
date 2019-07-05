------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with GNATCOLL.Traces;

package Langkit_Support.Adalog is

   Early_Binding_Error : exception;
   --  Exception raised when an equation cannot be solved because a logic
   --  variable is referenced but is never assigned a value.

   Timeout_Error : exception;
   --  Exception raised when the resolution of a complex relation exceeded the
   --  number of steps allowed.

   Solver_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER", Default => GNATCOLL.Traces.From_Config);

   Verbose_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.VERBOSE", Default => GNATCOLL.Traces.From_Config);

   Trav_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.TRAVERSAL", Default => GNATCOLL.Traces.From_Config);

   Solv_Trace  : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.SOLVE", Default => GNATCOLL.Traces.From_Config);

   Sol_Trace  : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.SOLUTION", Default => GNATCOLL.Traces.From_Config);

end Langkit_Support.Adalog;
