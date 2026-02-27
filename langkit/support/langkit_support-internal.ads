--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with System;

with GNAT.Strings;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Text;        use Langkit_Support.Text;

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.

package Langkit_Support.Internal is

   type Text_Access is not null access constant Text_Type;
   type Text_Access_Or_Null is access constant Text_Type;
   --  Reference to a static Unicode string. Used in descriptor tables whenever
   --  we need to provide a name.

   type Bytes_Access is not null access constant String;
   type Bytes_Access_Or_Null is access constant String;
   --  Reference to a static string of bytes

   type Memory_Buffer is record
      Address   : System.Address;
      Byte_Size : Natural;
   end record;
   --  Like Bytes_Access/Bytes_Access_Or_Null, but without requiring a fat
   --  pointer.

   No_Memory_Buffer : constant Memory_Buffer := (System.Null_Address, 0);

   function Is_Null (Self : Memory_Buffer) return Boolean
   is (System."=" (Self.Address, System.Null_Address));

   subtype Debug_String_Access is Bytes_Access;
   --  Reference to a statically allocated String. Used in descriptor tables
   --  whenever we need to provide string for debug (compatible with
   --  Ada.Text_IO).

   --  Descriptors for token kinds

   type Token_Kind_Descriptor is record
      Name       : Text_Access;
      Family     : Token_Family_Index;
      Is_Comment : Boolean;
   end record;
   type Token_Kind_Descriptor_Array is
     array (Token_Kind_Index range <>) of Token_Kind_Descriptor;
   type Token_Kind_Descriptor_Array_Access is
     not null access constant Token_Kind_Descriptor_Array;

   --  Descriptors for token families

   type Token_Family_Name_Array is
     array (Token_Family_Index range <>) of Text_Access;
   type Token_Family_Name_Array_Access is
     not null access constant Token_Family_Name_Array;

   -------------------
   -- Builtin files --
   -------------------

   Builtin_Filename_Prefix : constant String := "builtin://";
   --  Prefix to use in filenames to designate a file that is built in the
   --  generated library.

   function Builtin_Filename (Filename : String) return String;
   --  If ``Filename`` designates a builtin file, return the internal
   --  identifier for this file. Return the empty string otherwise.

   type Memory_Buffer_And_Access is record
      Buffer : Memory_Buffer;
      --  Holder for the actual memory buffer

      Str_Access : GNAT.Strings.String_Access;
      --  If this memory buffer is backed by dynamically allocated memory,
      --  access to it. Used to free the allocated memory once done with the
      --  memory buffer.
   end record;

   No_Memory_Buffer_And_Access : constant Memory_Buffer_And_Access :=
     (No_Memory_Buffer, null);

   procedure Read_File
     (Language : Language_Id;
      Filename : String;
      Buffer   : out Memory_Buffer_And_Access);
   --  Read a (possibly builtin) file.
   --
   --  ``Language`` is used to find builtin files.
   --
   --  ``Filename`` is the name of the file to read.
   --
   --  If the file cannot be read, ``Buffer`` is set to ``No_Memory_Buffer``
   --  upon return.
   --
   --  If it could be read, ``Buffer`` is set to refer to the file contents.
   --  ``Str_Access`` is set to null if this is a builtin file, and to a
   --  dynamically allocated string if it was read from the filesystem: it is
   --  up to the caller to free it.

   type Memory_Buffer_And_Access_Array is
     array (Positive range <>) of Memory_Buffer_And_Access;

   function Load_Buffers
     (Language    : Language_Id;
      Filenames   : File_Array;
      Buffers     : out Memory_Buffer_And_Access_Array;
      Diagnostics : in out Diagnostics_Vectors.Vector) return Boolean
   with Pre => Filenames'Length = Buffers'Length;
   --  Load a series of buffers from filenames, handling possible builtin
   --  files.
   --
   --  If one file cannot be read, append error messages in ``Diagnostics``,
   --  set all buffers to ``No_Memory_Buffer_And_Access`` and return False.
   --  Return True otherwise.

   procedure Free (Buffers : in out Memory_Buffer_And_Access_Array);
   --  If ``Buffers`` contains an allocated string, free it. Set all buffers to
   --  ``No_Memory_Buffer_And_Access``.

end Langkit_Support.Internal;
