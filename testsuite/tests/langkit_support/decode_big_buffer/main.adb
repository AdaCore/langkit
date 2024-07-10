--  Check that Langkit_Support.File_Readers.Decode_Buffer properly reject
--  input buffers that are too big.

with Ada.Text_IO; use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;

with Langkit_Support.Diagnostics;  use Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Text;         use Langkit_Support.Text;

procedure Main is

   --  Create a fake bytes buffer that is too big to be decoded. Note that we
   --  do not allocate a real one so that this testcase does not consume too
   --  many host resources.

   Buffer_Addr : constant System.Address := To_Address (1);
   Buffer_Size : constant Natural := Natural'Last / 4 + 1;
   Buffer      : constant String (1 .. Buffer_Size)
     with Import, Address => Buffer_Addr;

   Contents    : Decoded_File_Contents;
   Diagnostics : Diagnostics_Vectors.Vector;
begin
   Put_Line ("Call Decode_Buffer...");
   Decode_Buffer (Buffer, "ascii", True, Contents, Diagnostics);

   Put_Line ("Returned diagnostics:");
   Print (Diagnostics, Prefix => "  ");

   Free (Contents.Buffer);
   Put_Line ("Done.");
end Main;
