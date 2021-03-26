------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  This packages provides an interface to abstract away the action of reading
--  a source file to parse. Depending on use cases, it allows to override
--  bytes-to-text decoding and preprocess sources (before actual
--  lexing/parsing).

with GNATCOLL.Refcount;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;

package Langkit_Support.File_Readers is

   type Decoded_File_Contents is record
      Buffer : Text_Access;
      First  : Positive;
      Last   : Natural;
   end record;
   --  The "Buffer (First .. Last)" slice contains the decoded file contents as
   --  a sequence of codepoints. We keep track of First/Last indexes in
   --  addition to Ada's Buffer'First/'Last attributes because source buffers
   --  may be oversized.

   type File_Reader_Interface is interface;
   --  Interface to override how source files are fetched and decoded

   procedure Read
     (Self        : File_Reader_Interface;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is abstract;
   --  Read the content of the source at Filename, decoding it using the given
   --  Charset and decoding the byte order mark if Read_BOM is True.
   --
   --  If there is an error during this process, append an error message to
   --  Diagnostics. In that case, Contents is considered uninitialized.
   --
   --  Otherwise, allocate a Text_Type buffer, fill it and initialize Contents
   --  to refer to it.

   procedure Decode_Buffer
     (Buffer      : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Decode the bytes in Buffer according to Charset/Read_BOM into Contents.
   --  The bytes decoding itself is delegated to GNATCOLL.Iconv.
   --
   --  If there is an error during this process, append an error message to
   --  Diagnostics. In that case, Contents is considered uninitialized.

   procedure Direct_Read
     (Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Simple implementation of Read to read the source file through
   --  GNATCOLL.Mmap and to decode it using GNATCOLL.Iconv.

   procedure Release (Self : in out File_Reader_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Self

   procedure Do_Release (Self : in out File_Reader_Interface'Class);
   --  Helper for the instantiation below

   package File_Reader_References is new GNATCOLL.Refcount.Shared_Pointers
     (File_Reader_Interface'Class, Do_Release);

   subtype File_Reader_Reference is File_Reader_References.Ref;
   No_File_Reader_Reference : File_Reader_Reference renames
      File_Reader_References.Null_Ref;

   function Create_File_Reader_Reference
     (File_Reader : File_Reader_Interface'Class) return File_Reader_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create file reader
   --  references.

end Langkit_Support.File_Readers;
