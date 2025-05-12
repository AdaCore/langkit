with Libfoolang.Analysis;          use Libfoolang.Analysis;
with Libfoolang.Pkg;               use Libfoolang.Pkg;
with Libfoolang.Public_Converters; use Libfoolang.Public_Converters;

package body Libfoolang.Implementation.C.Extensions is

   -------------------------------
   -- foo_create_my_file_reader --
   -------------------------------

   function foo_create_my_file_reader return foo_file_reader is
      FR_Ref : constant File_Reader_Reference := Create_My_File_Reader;
      FR_Int : constant Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (FR_Ref);
   begin
      return Wrap_Private_File_Reader (FR_Int);
   end foo_create_my_file_reader;

   ---------------------------
   -- foo_get_internal_unit --
   ---------------------------

   function foo_get_internal_unit
     (Context : foo_analysis_context) return foo_analysis_unit
   is
      Ctx : constant Analysis_Context := Wrap_Context (Context);
   begin
      return Unwrap_Unit (Get_Internal_Unit (Ctx));
   end foo_get_internal_unit;

end Libfoolang.Implementation.C.Extensions;
