with Libfoolang.Analysis;          use Libfoolang.Analysis;
with Libfoolang.Public_Converters; use Libfoolang.Public_Converters;
with Pkg;

package body Libfoolang.Implementation.C.Extensions is

   -------------------------------
   -- foo_create_my_file_reader --
   -------------------------------

   function foo_create_my_file_reader return foo_file_reader is
      FR     : constant Pkg.My_File_Reader := (null record);
      FR_Ref : constant File_Reader_Reference :=
         Create_File_Reader_Reference (FR);
      FR_Int : constant Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (FR_Ref);
   begin
      return Wrap_Private_File_Reader (FR_Int);
   end foo_create_my_file_reader;

end Libfoolang.Implementation.C.Extensions;
