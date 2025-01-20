--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.

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
      Report_Errors : Boolean;
      --  If set the True, the solver will generate diagnostics explaining
      --  which each attempted solution was rejected: it will call the
      --  ``Failed`` primitive of the atom which evaluation failed (if
      --  it is supported; see ``Langkit_Support.Adalog.Solver_Interface``),
      --  together with the logic contexts that are relevant for the failure,
      --  that is, the contexts attached to the atoms that are part of the
      --  explanation for the failure).
   end record;

   Default_Options : Solve_Options_Type := (Report_Errors => False);
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
   --  * the progress of solving individual atoms.

   Sol_Trace  : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.SOLUTION", Default => GNATCOLL.Traces.From_Config);
   --  Trace to show:
   --
   --  * the number of tried solutions;
   --  * valid solutions found.

   Diags_Trace  : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.SOLVER.DIAGNOSTICS", Default => GNATCOLL.Traces.From_Config);
   --  Trace to show:
   --
   --  * internal diagnostics emitted during solving;
   --  * what happens during the diagnostic filtering process (see the
   --    ``Langkit_Support.Adalog.Solver.Diagnostics`` package).

end Langkit_Support.Adalog;
