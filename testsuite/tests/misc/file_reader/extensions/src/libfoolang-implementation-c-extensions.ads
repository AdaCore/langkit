package Libfoolang.Implementation.C.Extensions is

   function foo_create_my_file_reader return foo_file_reader
      with Export, Convention => C;

end Libfoolang.Implementation.C.Extensions;
