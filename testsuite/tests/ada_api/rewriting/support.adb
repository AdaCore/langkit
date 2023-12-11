with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;

package body Support is

   type My_File_Reader is new File_Reader_Interface with null record;

   overriding procedure Read
     (Self        : My_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   overriding procedure Release (Self : in out My_File_Reader) is null;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : My_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Temp : Decoded_File_Contents;
   begin
      Direct_Read (Filename, Charset, Read_BOM, Temp, Diagnostics);
      Contents :=
        (Buffer => new Text_Type (1 .. Temp.Buffer.all'Length),
         First  => 1,
         Last   => 0);

      declare
         Is_Line_Start : Boolean := True;
         Ignore        : Boolean := False;
         Ignore_Next   : Boolean := False;
      begin
         for C of Temp.Buffer.all loop
            if C = Chars.LF then
               Is_Line_Start := True;
               Ignore_Next := False;
            else
               if Is_Line_Start and then C = '!' then
                  Ignore := True;
                  Ignore_Next := True;
               end if;
               Is_Line_Start := False;
            end if;

            if not Ignore then
               Contents.Last := Contents.Last + 1;
               Contents.Buffer.all (Contents.Last) := C;
            end if;
            Ignore := Ignore_Next;
         end loop;
      end;
      Free (Temp.Buffer);
   end Read;

   ------------------------
   -- Create_File_Reader --
   ------------------------

   function Create_File_Reader return File_Reader_Reference is
   begin
      return Create_File_Reader_Reference
        (My_File_Reader'(File_Reader_Interface with null record));
   end Create_File_Reader;

end Support;
