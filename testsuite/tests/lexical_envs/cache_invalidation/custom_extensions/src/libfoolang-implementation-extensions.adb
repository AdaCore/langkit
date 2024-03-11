package body Libfoolang.Implementation.Extensions is

   function Should_Collect_Env_Caches
     (Ctx                        : Internal_Context;
      Unit                       : Internal_Unit;
      All_Env_Caches_Entry_Count : Long_Long_Natural) return Boolean
   is
      Collect : constant Boolean := Unit.Env_Caches_Stats.Entry_Count > 40;
   begin
      if Collect then
         Cache_Invalidation_Trace.Trace
           ("Heuristic decides to collect unit " & Trace_Image (Unit));
      else
         Cache_Invalidation_Trace.Trace
           ("Heuristic decides not to collect unit " & Trace_Image (Unit));
      end if;
      return Collect;
   end Should_Collect_Env_Caches;

end Libfoolang.Implementation.Extensions;


