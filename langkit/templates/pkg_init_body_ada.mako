## vim: filetype=makoada

with GNATCOLL.Iconv;

% if emitter.coverage:
with Ada.Environment_Variables;

with Interfaces.C;

with GNATcov_RTS.Buffers.Lists.${ada_lib_name};
with GNATcov_RTS.Traces.Output;
% endif

package body ${ada_lib_name}.Init is

   procedure Initialize is null;

   % if emitter.coverage:
      type Atexit_Callback is access procedure with Convention => C;

      function atexit (Callback : Atexit_Callback) return Interfaces.C.int
         with Import, Convention => C;

      procedure Dump_Source_Trace with Convention => C;
      --  Output a source trace file for all the coverage state of
      --  ${ada_lib_name}.

      -----------------------
      -- Dump_Source_Trace --
      -----------------------

      procedure Dump_Source_Trace is
         package Env renames Ada.Environment_Variables;

         --  If the $_TRACE_FILE environment variable is defined, use its
         --  content as the source trace filename. Otherwise, default to
         --  GNATcov_RTS.Traces.Output's naming scheme.

         <% env_var = '{}_TRACE_FILE'.format(emitter.lib_name_up) %>
         Filename : constant String :=
           (if Env.Exists ("${env_var}")
            then Env.Value ("${env_var}")
            else "");
      begin
         GNATcov_RTS.Traces.Output.Write_Trace_File
           (Buffers  => GNATcov_RTS.Buffers.Lists.${ada_lib_name}.List,
            Filename => Filename);
      end Dump_Source_Trace;

   % endif

begin
   --  Check that we actually have full Libiconv support: as nothing works
   --  without it, we explicitly check support here instead of letting
   --  user-unfriendly errors happen during lexing.

   if not GNATCOLL.Iconv.Has_Iconv then
      raise Program_Error with "Libiconv is not available";
   end if;

   % if emitter.coverage:
      --  Register a call to Dump_Source_Trace on process exit
      declare
         Dummy : Interfaces.C.int := atexit (Dump_Source_Trace'Access);
      begin
         null;
      end;
   % endif
end ${ada_lib_name}.Init;
