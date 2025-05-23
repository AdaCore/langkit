--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This packages provides an interface to abstract away the action of reading
--  a source file to parse. Depending on use cases, it allows to override
--  bytes-to-text decoding and preprocess sources (before actual
--  lexing/parsing).

private with Ada.Containers.Hashed_Maps;
private with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
private with Ada.Unchecked_Deallocation;

with GNATCOLL.Refcount;
private with GNATCOLL.VFS;

with Liblktlang_Support.Diagnostics; use Liblktlang_Support.Diagnostics;
private with Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Text;        use Liblktlang_Support.Text;

package Liblktlang_Support.File_Readers is

   -------------------
   -- File_Contents --
   -------------------

   type File_Contents is record
      Buffer : String_Access;
      First  : Positive;
      Last   : Natural;
   end record;
   --  The "Buffer (First .. Last)" slice contains the file contents as a
   --  sequence of bytes. We keep track of First/Last indexes in addition to
   --  Ada's Buffer'First/'Last attributes because source buffers may be
   --  oversized.

   function Create_File_Contents (Buffer : String) return File_Contents;
   --  Create a ``File_Contents`` value that contains a copy of ``Buffer``

   ---------------------------
   -- Decoded_File_Contents --
   ---------------------------

   type Decoded_File_Contents is record
      Buffer : Text_Access;
      First  : Positive;
      Last   : Natural;
   end record;
   --  The "Buffer (First .. Last)" slice contains the decoded file contents as
   --  a sequence of codepoints. We keep track of First/Last indexes in
   --  addition to Ada's Buffer'First/'Last attributes because source buffers
   --  may be oversized.

   function Create_Decoded_File_Contents
     (Buffer : Text_Type) return Decoded_File_Contents;
   --  Create a ``Decoded_File_Contents`` value that contains a copy of
   --  ``Buffer``.

   ----------------------------
   -- File_Fetcher_Interface --
   ----------------------------

   type File_Fetcher_Interface is interface;
   --  Interface to override how source files are fetched

   procedure Fetch
     (Self        : File_Fetcher_Interface;
      Filename    : String;
      Contents    : out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is abstract;
   --  Fetch the content of the source at ``Filename``.
   --
   --  If there is an error during this process, append an error message to
   --  ``Diagnostics``.
   --
   --  Whether there are errors or not, allocate a ``String`` buffer, fill it
   --  and initialize ``Contents`` to refer to it.

   procedure Release (Self : in out File_Fetcher_Interface) is abstract;
   --  Actions to perform when releasing resources associated to ``Self``

   procedure Do_Release (Self : in out File_Fetcher_Interface'Class);
   --  Helper for the instantiation below

   package File_Fetcher_References is new GNATCOLL.Refcount.Shared_Pointers
     (File_Fetcher_Interface'Class, Do_Release);

   subtype File_Fetcher_Reference is File_Fetcher_References.Ref;
   No_File_Fetcher_Reference : File_Fetcher_Reference renames
      File_Fetcher_References.Null_Ref;

   function Create_File_Fetcher_Reference
     (File_Fetcher : File_Fetcher_Interface'Class)
      return File_Fetcher_Reference;
   --  Simple wrapper around the ``GNATCOLL.Refcount`` API to create file
   --  fetcher references.

   function Create_Filesystem_Fetcher return File_Fetcher_Reference;
   --  Return a file fetcher instance that just reads file from the filesystem

   type File_Stub_Store is private;
   --  Store for file stubs: filename/file content associations that "override"
   --  the actual files on the filesystem. This is the data structure that
   --  powers the "stubbing" file fetcher: see the
   --  ``Create_File_Stubbing_Fetcher`` function below.
   --
   --  Note that this type has by-reference semantics, so that the store passed
   --  to ``Create_File_Stubbing_Store`` can be mutated with the ``Stub_File``
   --  and ``Reset_File`` procedures even after the file fetcher has been
   --  created.

   function Create_File_Stub_Store return File_Stub_Store;
   --  Return an empty file stub store

   procedure Stub_File
     (Self : File_Stub_Store; Filename : String; Content : Unbounded_String);
   --  Add an association for ``Filename``/``Content`` to ``Self``. If there
   --  aws already an association for ``Filename``, this new association
   --  replaces it.

   procedure Reset_File (Self : File_Stub_Store; Filename : String);
   --  If ``Self`` contains a stub for ``Filename``, remove it. This is a no-op
   --  otherwise.

   function Create_Stubbing_Fetcher
     (Store : File_Stub_Store) return File_Fetcher_Reference;
   --  Create a file fetcher backed by the given file stub ``Store``

   ----------------------------
   -- File_Refiner_Interface --
   ----------------------------

   type File_Refiner_Interface is interface;
   --  Interface to override how the contents of source files (as bytes) are
   --  refined.

   procedure Refine
     (Self        : File_Refiner_Interface;
      Filename    : String;
      Contents    : in out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is abstract;
   --  Refine the content of a source file (``Contents``, the bytes content for
   --  the file fetched at ``Filename``) in place.
   --
   --  If there is an error during this process, append an error message to
   --  ``Diagnostics``.
   --
   --  Whether there are errors or not, ``Contents`` must refer to a correctly
   --  initialized buffer.

   procedure Release (Self : in out File_Refiner_Interface) is abstract;
   --  Actions to perform when releasing resources associated to ``Self``

   procedure Do_Release (Self : in out File_Refiner_Interface'Class);
   --  Helper for the instantiation below

   package File_Refiner_References is new GNATCOLL.Refcount.Shared_Pointers
     (File_Refiner_Interface'Class, Do_Release);

   subtype File_Refiner_Reference is File_Refiner_References.Ref;
   No_File_Refiner_Reference : File_Refiner_Reference renames
      File_Refiner_References.Null_Ref;

   function Create_File_Refiner_Reference
     (File_Refiner : File_Refiner_Interface'Class)
      return File_Refiner_Reference;
   --  Simple wrapper around the ``GNATCOLL.Refcount`` API to create file
   --  fetcher references.

   type File_Refiner_Array is
     array (Positive range <>) of File_Refiner_Reference;

   Empty_File_Refiner_Array : constant File_Refiner_Array := (1 .. 0 => <>);

   ---------------------------
   -- File_Reader_Interface --
   ---------------------------

   type File_Reader_Interface is interface;
   --  Interface to override how source files are fetched and decoded

   procedure Read
     (Self        : File_Reader_Interface;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is abstract;
   --  Read the content of the source at ``Filename``, decoding it using the
   --  given ``Charset`` and decoding the byte order mark if ``Read_BOM`` is
   --  True.
   --
   --  If there is an error during this process, append an error message to
   --  ``Diagnostics``.
   --
   --  Whether there are errors or not, allocate a ``Text_Type`` buffer, fill
   --  it and initialize Contents to refer to it.

   procedure Release (Self : in out File_Reader_Interface) is abstract;
   --  Actions to perform when releasing resources associated to ``Self``

   procedure Do_Release (Self : in out File_Reader_Interface'Class);
   --  Helper for the instantiation below

   package File_Reader_References is new GNATCOLL.Refcount.Shared_Pointers
     (File_Reader_Interface'Class, Do_Release);

   subtype File_Reader_Reference is File_Reader_References.Ref;
   No_File_Reader_Reference : File_Reader_Reference renames
      File_Reader_References.Null_Ref;

   function Create_File_Reader_Reference
     (File_Reader : File_Reader_Interface'Class) return File_Reader_Reference;
   --  Simple wrapper around the ``GNATCOLL.Refcount`` API to create file
   --  reader references.

   function Create_File_Reader_Reference
     (Fetcher  : File_Fetcher_Reference;
      Refiners : File_Refiner_Array) return File_Reader_Reference;
   --  Create a file reader as the composition of a file fetcher and a chain of
   --  file refiners. The resulting file buffers are decoded using
   --  ``Decode_Buffer``.

   -------------
   -- Helpers --
   -------------

   procedure Decode_Buffer
     (Buffer      : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Decode the bytes in ``Buffer`` according to ``Charset``/``Read_BOM``
   --  into ``Contents``.  The bytes decoding itself is delegated to
   --  ``GNATCOLL.Iconv``.
   --
   --  If there is an error during this process, append an error message to
   --  ``Diagnostics``. In that case, Contents is considered uninitialized.

   procedure Direct_Read
     (Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Simple implementation of ``Read`` to read the source file through
   --  ``GNATCOLL.Mmap`` and to decode it using ``GNATCOLL.Iconv``.

   procedure Canonicalize_Line_Endings (Self : in out Decoded_File_Contents);
   --  Canonicalize CRLF to LF in place

private
   use GNATCOLL.VFS;
   use Liblktlang_Support.Internal.Analysis;

   ----------------------------
   -- Filesystem_File_Reader --
   ----------------------------

   type Filesystem_File_Fetcher is new File_Fetcher_Interface with null record;

   overriding procedure Fetch
     (Self        : Filesystem_File_Fetcher;
      Filename    : String;
      Contents    : out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release
     (Self : in out Filesystem_File_Fetcher) is null;

   ---------------------------------------------
   -- File_Stub_Store / Stubbing_File_Fetcher --
   ---------------------------------------------

   --  To implement the by-reference semantics, the public file stub store just
   --  contains a ``Ref`` type from the instantiation of ``GNATCOLL.Refcount``.
   --
   --  To avoid all the trouble that comes with null references,
   --  ``File_Stub_Store`` is also automatically initialized to a reference to
   --  an empty store.

   package Stub_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Unbounded_String,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=");

   type Stub_Store_Data is record
      Filenames : Virtual_File_Cache;
      Stubs     : Stub_Maps.Map;
   end record;

   Empty_Stub_Store_Data : constant Stub_Store_Data :=
     (Empty_Virtual_File_Cache, Stub_Maps.Empty_Map);

   package Stub_Store_Data_References is new GNATCOLL.Refcount.Shared_Pointers
     (Stub_Store_Data);

   type File_Stub_Store is new Ada.Finalization.Controlled with record
      Data : Stub_Store_Data_References.Ref;
   end record;

   overriding procedure Initialize (Self : in out File_Stub_Store);

   type Stubbing_File_Fetcher is new File_Fetcher_Interface with record
      FS    : Filesystem_File_Fetcher;
      Stubs : File_Stub_Store;
   end record;

   overriding procedure Fetch
     (Self        : Stubbing_File_Fetcher;
      Filename    : String;
      Contents    : out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release
     (Self : in out Stubbing_File_Fetcher) is null;

   --------------------------
   -- Composed_File_Reader --
   --------------------------

   type File_Refiner_Array_Access is access File_Refiner_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (File_Refiner_Array, File_Refiner_Array_Access);

   type Composed_File_Reader is new File_Reader_Interface with record
      Fetcher  : File_Fetcher_Reference;
      Refiners : File_Refiner_Array_Access;
   end record;

   overriding procedure Read
     (Self        : Composed_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release (Self : in out Composed_File_Reader);

end Liblktlang_Support.File_Readers;
