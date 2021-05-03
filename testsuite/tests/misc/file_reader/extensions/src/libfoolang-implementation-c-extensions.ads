package Libfoolang.Implementation.C.Extensions is

   function foo_create_my_file_reader return foo_file_reader
      with Export, Convention => C;

   function foo_get_internal_unit
     (Context : foo_analysis_context) return foo_analysis_unit
      with Export, Convention => C;

end Libfoolang.Implementation.C.Extensions;
