with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

package body Support is

   type My_FR is new File_Reader_Interface with null record;

   overriding procedure Read
     (Self        : My_FR;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release (Self : in out My_FR) is null;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : My_FR;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
   begin
      Direct_Read (Filename, Charset, Read_BOM, Contents, Diagnostics);
      if Diagnostics.Is_Empty and then Contents.Buffer.all'Length > 79 then
         Contents.Buffer.all (Contents.First .. Contents.First + 79) :=
           (1 .. 80 => ' ');
      end if;
   end Read;

   ---------------------
   -- Get_File_Reader --
   ---------------------

   function Get_File_Reader return File_Reader_Reference is
   begin
      return Create_File_Reader_Reference (My_FR'(null record));
   end Get_File_Reader;

end Support;
