------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
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

   type Solver_Kind is (None, Symbolic, State_Machine);
   --  Different kind of solvers available in Adalog. ``None`` is for no
   --  solver.

   subtype Valid_Solver_Kind is Solver_Kind range Symbolic .. State_Machine;
   --  Kind subtype for valid solver kinds

   ---------------------
   -- Solving options --
   ---------------------

   type Solve_Options_Type is record
      Cut_Dead_Branches : Boolean := True;
      --  Whether to enable an optimization that will cut branches that
      --  necessarily contain falsy solutions.
   end record;

   Default_Options : Solve_Options_Type := (others => <>);
   --  Mutate this to affect the behavior of all calls to the solver which just
   --  use the default options.

   Default_Timeout_Ticks_Number : constant := 50_000_000;

   -----------------------
   -- Adalog exceptions --
   -----------------------

   Early_Binding_Error : exception;
   --  Exception raised when an equation cannot be solved because a logic
   --  variable is referenced but is never assigned a value.

   Timeout_Error : exception;
   --  Exception raised when the resolution of a complex relation exceeded the
   --  number of steps allowed.

   -------------------
   -- Adalog traces --
   -------------------

   Solver_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER", Default => GNATCOLL.Traces.From_Config);
   --  Trace whose only purpose is to show when we start solving an equation,
   --  and show when solving aborts because of an exception.

   Stats_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.STATS", Default => GNATCOLL.Traces.From_Config);
   --  Trace to output statistics about the equation to solve (number of atoms,
   --  of Any/All relations).

   Timing_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.TIMING", Default => GNATCOLL.Traces.From_Config);
   --  Trace to show the time taken by each equation resolution step

   Verbose_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.VERBOSE", Default => GNATCOLL.Traces.From_Config);
   --  Trace to show when:
   --
   --  * a variable gets assigned a value;
   --  * a variable gets assigned an Id;
   --  * a variable gets (un)aliased.

   Trav_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.TRAVERSAL", Default => GNATCOLL.Traces.From_Config);
   --  Trace to show when starting the processing of a compound relation in the
   --  symbolic solver.

   Solv_Trace  : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.SOLVE", Default => GNATCOLL.Traces.From_Config);
   --  Trace to show:
   --
   --  * the progress of solving a sequence of atoms (both in the symbolic
   --    solver's Try_Solution and in the dead branch cut optimization);
   --
   --  * the progress of solving individual atoms.

   Sol_Trace  : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.SOLUTION", Default => GNATCOLL.Traces.From_Config);
   --  Trace to show:
   --
   --  * the number of tried solutions;
   --  * valid solutions found.

   Cst_Folding_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.CONSTANT_FOLDING",
      Default => GNATCOLL.Traces.From_Config);
   --  Trace to show the result of relation constant folding pass done during
   --  the preparation stage in the symbolic solver.

end Langkit_Support.Adalog;
