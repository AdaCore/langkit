package body Libfoolang.Implementation.C.Extensions is

   procedure foo_do_something is
   begin
      Clear_Last_Exception;
      Set_Last_Exception
        (Id          => Property_Error'Identity,
         Message     => (1 => 'A', 2 .. 9_999 => 'B', 10_000 => 'C'),
         Stack_Trace => (1 .. 0 => <>));
   end foo_do_something;

end Libfoolang.Implementation.C.Extensions;
