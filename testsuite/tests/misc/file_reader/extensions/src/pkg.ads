with Langkit_Support.Diagnostics;  use Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

package Pkg is

   type My_File_Reader is new File_Reader_Interface with null record;

   overriding procedure Read
     (Self        : My_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release (Self : in out My_File_Reader);

end Pkg;
