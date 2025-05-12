with Langkit_Support.Diagnostics;  use Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

with Libfoolang.Analysis; use Libfoolang.Analysis;

package Libfoolang.Pkg is

   type My_File_Fetcher is new File_Fetcher_Interface with record
      Filesystem_Fetcher : File_Fetcher_Reference;
   end record;

   overriding procedure Fetch
     (Self        : My_File_Fetcher;
      Filename    : String;
      Contents    : out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release (Self : in out My_File_Fetcher);

   type My_File_Refiner is new File_Refiner_Interface with null record;

   overriding procedure Refine
     (Self        : My_File_Refiner;
      Filename    : String;
      Contents    : in out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release (Self : in out My_File_Refiner);

   function Create_My_File_Reader return File_Reader_Reference;

   Internal_Unit_Name : constant String := "__internal_unit";

   function Get_Internal_Unit
     (Context : Analysis_Context) return Analysis_Unit;

end Libfoolang.Pkg;
