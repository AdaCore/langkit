--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.IO_Exceptions;

pragma Warnings (Off, "internal");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "internal");

with System;

with GNAT.Byte_Order_Mark;

with GNATCOLL.Iconv;
with GNATCOLL.Mmap;

with Liblktlang_Support.Slocs; use Liblktlang_Support.Slocs;

package body Liblktlang_Support.File_Readers is

   procedure Decode_Buffer_Impl
     (Buffer      : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Result      : in out Text_Access;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Implementation helper for ``Decode_Buffer``: assuming that a correctly
   --  sized output buffer is allocated for the given input (``Result``),
   --  perfom the decoding that ``Decode_Buffer`` is supposed to do.

   --------------------------
   -- Create_File_Contents --
   --------------------------

   function Create_File_Contents (Buffer : String) return File_Contents is
   begin
      return (Buffer => new String'(Buffer),
              First  => Buffer'First,
              Last   => Buffer'Last);
   end Create_File_Contents;

   ----------------------------------
   -- Create_Decoded_File_Contents --
   ----------------------------------

   function Create_Decoded_File_Contents
     (Buffer : Text_Type) return Decoded_File_Contents is
   begin
      return (Buffer => new Text_Type'(Buffer),
              First  => Buffer'First,
              Last   => Buffer'Last);
   end Create_Decoded_File_Contents;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Self : in out File_Fetcher_Interface'Class) is
   begin
      Self.Release;
   end Do_Release;

   -----------------------------------
   -- Create_File_Fetcher_Reference --
   -----------------------------------

   function Create_File_Fetcher_Reference
     (File_Fetcher : File_Fetcher_Interface'Class)
      return File_Fetcher_Reference is
   begin
      return Result : File_Fetcher_Reference do
         Result.Set (File_Fetcher);
      end return;
   end Create_File_Fetcher_Reference;

   -------------------------------
   -- Create_Filesystem_Fetcher --
   -------------------------------

   function Create_Filesystem_Fetcher return File_Fetcher_Reference is
      FF : constant Filesystem_File_Fetcher := (null record);
   begin
      return Create_File_Fetcher_Reference (FF);
   end Create_Filesystem_Fetcher;

   ----------------------------
   -- Create_File_Stub_Store --
   ----------------------------

   function Create_File_Stub_Store return File_Stub_Store is
      Dummy : File_Stub_Store;
   begin
      return Dummy;
   end Create_File_Stub_Store;

   ---------------
   -- Stub_File --
   ---------------

   procedure Stub_File
     (Self : File_Stub_Store; Filename : String; Content : Unbounded_String)
   is
      Data : Stub_Store_Data renames Self.Data.Unchecked_Get.all;
      Key  : constant Virtual_File :=
        Normalized_Unit_Filename (Data.Filenames, Filename);
   begin
      Data.Stubs.Include (Key, Content);
   end Stub_File;

   ----------------
   -- Reset_File --
   ----------------

   procedure Reset_File (Self : File_Stub_Store; Filename : String) is
      Data : Stub_Store_Data renames Self.Data.Unchecked_Get.all;
      Key : constant Virtual_File :=
        Normalized_Unit_Filename (Data.Filenames, Filename);
   begin
      Data.Stubs.Exclude (Key);
   end Reset_File;

   -----------------------------
   -- Create_Stubbing_Fetcher --
   -----------------------------

   function Create_Stubbing_Fetcher
     (Store : File_Stub_Store) return File_Fetcher_Reference
   is
      FF : constant Stubbing_File_Fetcher :=
        (FS => (null record), Stubs => Store);
   begin
      return Create_File_Fetcher_Reference (FF);
   end Create_Stubbing_Fetcher;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Self : in out File_Refiner_Interface'Class) is
   begin
      Self.Release;
   end Do_Release;

   -----------------------------------
   -- Create_File_Refiner_Reference --
   -----------------------------------

   function Create_File_Refiner_Reference
     (File_Refiner : File_Refiner_Interface'Class)
      return File_Refiner_Reference is
   begin
      return Result : File_Refiner_Reference do
         Result.Set (File_Refiner);
      end return;
   end Create_File_Refiner_Reference;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Self : in out File_Reader_Interface'Class) is
   begin
      Self.Release;
   end Do_Release;

   ----------------------------------
   -- Create_File_Reader_Reference --
   ----------------------------------

   function Create_File_Reader_Reference
     (File_Reader : File_Reader_Interface'Class) return File_Reader_Reference
   is
   begin
      return Result : File_Reader_Reference do
         Result.Set (File_Reader);
      end return;
   end Create_File_Reader_Reference;

   ----------------------------------
   -- Create_File_Reader_Reference --
   ----------------------------------

   function Create_File_Reader_Reference
     (Fetcher  : File_Fetcher_Reference;
      Refiners : File_Refiner_Array) return File_Reader_Reference
   is
      FR : constant Composed_File_Reader :=
        (Fetcher, new File_Refiner_Array'(Refiners));
   begin
      return Create_File_Reader_Reference (FR);
   end Create_File_Reader_Reference;

   -------------------
   -- Decode_Buffer --
   -------------------

   procedure Decode_Buffer
     (Buffer      : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      --  In the worst case, we have one character per input byte, so
      --  allocating an output buffer with one codepoint per input byte will be
      --  big enough.
      --
      --  Defensive code: abort early with a clean error if this would make us
      --  allocate a buffer that is too big to be accessed as a String (indexed
      --  by Natural).

      Result : Text_Access;
   begin
      if Buffer'Length > Natural'Last / 4 then
         declare
            Size_Str : constant String := Buffer'Length'Image;
         begin
            Contents := Create_Decoded_File_Contents ("");
            Append
              (Diagnostics,
               Message => To_Text
                            ("Source file is too big ("
                             & Size_Str (Size_Str'First + 1 .. Size_Str'Last)
                             & " bytes)"));
         end;
      else
         Result := new Text_Type (1 .. Buffer'Length);
         Decode_Buffer_Impl
           (Buffer, Charset, Read_BOM, Result, Contents, Diagnostics);
      end if;
   end Decode_Buffer;

   ------------------------
   -- Decode_Buffer_Impl --
   ------------------------

   procedure Decode_Buffer_Impl
     (Buffer      : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Result      : in out Text_Access;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      use GNAT.Byte_Order_Mark;
      use GNATCOLL.Iconv;

      State  : Iconv_T;
      Status : Iconv_Result;
      BOM    : BOM_Kind := Unknown;

      Input_Index, Output_Index : Positive;

      First_Output_Index : constant Positive := Result'First;
      --  Index of the first byte in Result at which Iconv must decode Buffer

      Output : Byte_Sequence (1 .. 4 * Buffer'Length);
      for Output'Address use Result.all'Address;
      --  Iconv works on mere strings, so this is a kind of a view conversion

   begin
      Contents.Buffer := Result;
      Contents.First := Result'First;

      --  If we have a byte order mark, it overrides the requested Charset

      Input_Index := Buffer'First;
      if Read_BOM then
         declare
            Len : Natural;
         begin
            GNAT.Byte_Order_Mark.Read_BOM (Buffer, Len, BOM);
            Input_Index := Input_Index + Len;
         end;
      end if;

      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.

      if Input_Index > Buffer'Last then
         Contents.Last := Contents.First - 1;
         return;
      end if;

      --  Create the Iconv converter. We will notice unknown charsets here

      declare
         type String_Access is access all String;
         BOM_Kind_To_Charset : constant
            array (UTF8_All .. UTF32_BE) of String_Access :=
           (UTF8_All => UTF8'Unrestricted_Access,
            UTF16_LE => UTF16LE'Unrestricted_Access,
            UTF16_BE => UTF16BE'Unrestricted_Access,
            UTF32_LE => UTF32LE'Unrestricted_Access,
            UTF32_BE => UTF32BE'Unrestricted_Access);

         Actual_Charset : constant String :=
           (if BOM in UTF8_All .. UTF32_BE
            then BOM_Kind_To_Charset (BOM).all
            else Charset);
      begin
         State := Iconv_Open (Text_Charset, Actual_Charset);
      exception
         when Unsupported_Conversion =>
            Free (Result);
            Contents := Create_Decoded_File_Contents ("");
            Append
              (Diagnostics,
               Message => To_Text ("Unknown charset """ & Charset & """"));
            return;
      end;

      --  Perform the conversion itself

      Output_Index := First_Output_Index;
      Iconv (State,
             Buffer, Input_Index,
             Output (Output_Index .. Output'Last), Output_Index,
             Status);
      Contents.Last := (Output_Index - 1 - Output'First) / 4 + Result'First;

      --  Report an error if the input was invalid

      case Status is
         when Invalid_Multibyte_Sequence | Incomplete_Multibyte_Sequence =>

            --  TODO??? It may be more helpful to actually perform lexing on an
            --  incomplete buffer. The user would get both a diagnostic for the
            --  charset error and a best-effort list of tokens.

            declare
               Line_Offset : Natural := Result.all'First;
               Sloc        : Source_Location := (1, 0);
            begin
               --  Compute the sloc of the codepoint past the last codepoint
               --  that was successfully decoded, to be used as the sloc for
               --  the decoding error.

               --  When no codepoint could be decoded at all, ``Output_Index``
               --  (and thus ``Contents.Last``) still contain 1, while it
               --  should be 0 (impossible due to its ``Positive`` type).
               --  Fortunately we can detect this situation thanks to
               --  ``Input_Index``, which contains the index of the first byte
               --  of the sequence that could not be decoded. Correctly compute
               --  the column number in that case.

               if Input_Index = 1 then
                  Sloc.Column := 1;
               else
                  --  In this branch, we know that ``Output_Index`` and
                  --  ``Contents.Last`` contain indexes that refer to the last
                  --  decoded codepoint.
                  --
                  --  First, compute the line number where the error happened
                  --  (i.e. the number of line feed (LF) codepoints that were
                  --  decoded plus 1).  Also keep track of the index of the
                  --  first codepoint that follows a line feed.

                  for I in Result.all'First .. Contents.Last loop
                     if Result.all (I) = Chars.LF then
                        Sloc.Line := Sloc.Line + 1;
                        Line_Offset := I + 1;
                     end if;
                  end loop;

                  --  We can then compute the column number at which the error
                  --  occurred (right after the last codepoint that was
                  --  decoded, hence the +1).

                  declare
                     Last_Decoded_Line : Text_Type renames
                       Result.all (Line_Offset .. Contents.Last);
                  begin
                     Sloc.Column := 1 + Column_Count (Last_Decoded_Line);
                  end;
               end if;

               --  We no longer need the result buffer: free it and create the
               --  diagnostic.

               Free (Result);
               Contents := Create_Decoded_File_Contents ("");
               Append
                 (Diagnostics,
                  Make_Range (Sloc, Sloc),
                  To_Text ("Could not decode source as """ & Charset & """"));
            end;

         when Full_Buffer =>

            --  This is not supposed to happen: we allocated Result to be big
            --  enough in all cases.

            raise Program_Error;

         when Success =>
            null;
      end case;

      Iconv_Close (State);
   end Decode_Buffer_Impl;

   -----------------
   -- Direct_Read --
   -----------------

   procedure Direct_Read
     (Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      use GNATCOLL.Mmap;

      File   : Mapped_File;
      Region : Mapped_Region;
   begin
      begin
         File := Open_Read (Filename);
      exception
         when Exc : Ada.IO_Exceptions.Name_Error =>
            Contents := Create_Decoded_File_Contents ("");
            Append (Diagnostics, Exc => Exc);
            return;
      end;

      Region := Read (File);
      declare
         Buffer_Addr : constant System.Address := Data (Region).all'Address;
         Buffer      : String (1 .. Last (Region))
            with Import  => True,
                 Address => Buffer_Addr;
      begin
         Decode_Buffer (Buffer, Charset, Read_BOM, Contents, Diagnostics);
      end;

      Free (Region);
      Close (File);
   end Direct_Read;

   -------------------------------
   -- Canonicalize_Line_Endings --
   -------------------------------

   procedure Canonicalize_Line_Endings (Self : in out Decoded_File_Contents) is
      Buffer   : Text_Type renames Self.Buffer.all (Self.First .. Self.Last);
      New_Last : Natural := Buffer'First - 1;
   begin
      for I in Buffer'Range loop

         --  Unless I points at a CR that is followed by a LF, preserve it in
         --  the final buffer contents.

         if not (I < Buffer'Last
                 and then Buffer (I) = Chars.CR
                 and then Buffer (I + 1) = Chars.LF)
         then
            New_Last := New_Last + 1;
            Buffer (New_Last) := Buffer (I);
         end if;
      end loop;
      Self.Last := New_Last;
   end Canonicalize_Line_Endings;

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch
     (Self        : Filesystem_File_Fetcher;
      Filename    : String;
      Contents    : out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      use GNATCOLL.Mmap;

      File   : Mapped_File;
      Region : Mapped_Region;
   begin
      begin
         File := Open_Read (Filename);
      exception
         when Exc : Ada.IO_Exceptions.Name_Error =>
            Contents := Create_File_Contents ("");
            Append (Diagnostics, Exc => Exc);
            return;
      end;

      Region := Read (File);
      declare
         Buffer_Addr : constant System.Address := Data (Region).all'Address;
         Buffer      : String (1 .. Last (Region))
            with Import  => True,
                 Address => Buffer_Addr;
      begin
         Contents.Buffer := new String'(Buffer);
         Contents.First := Buffer'First;
         Contents.Last := Buffer'Last;
      end;
      Free (Region);
      Close (File);
   end Fetch;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out File_Stub_Store) is
   begin
      Self.Data.Set (Empty_Stub_Store_Data);
   end Initialize;

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch
     (Self        : Stubbing_File_Fetcher;
      Filename    : String;
      Contents    : out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      use Stub_Maps;

      Data : Stub_Store_Data renames Self.Stubs.Data.Unchecked_Get.all;
      Key  : constant Virtual_File :=
        Normalized_Unit_Filename (Data.Filenames, Filename);
      Cur  : constant Cursor := Data.Stubs.Find (Key);
   begin
      if Has_Element (Cur) then

         --  The stubs store has an entry for this file: allocate a bytes
         --  buffer for it.

         declare
            use Ada.Strings.Unbounded.Aux;
            Stub : constant Unbounded_String := Element (Cur);
            BS   : Big_String_Access;
         begin
            Contents.First := 1;
            Get_String (Stub, BS, Contents.Last);
            Contents.Buffer := new String (1 .. Contents.Last);
            Contents.Buffer.all := BS.all (1 .. Contents.Last);
         end;
      else
         --  No stub for this file: delegate the work to the filesystem fetcher

         Self.FS.Fetch (Filename, Contents, Diagnostics);
      end if;
   end Fetch;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : Composed_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Bytes_Contents : File_Contents;
   begin
      Self.Fetcher.Unchecked_Get.Fetch (Filename, Bytes_Contents, Diagnostics);
      for Refiner of Self.Refiners.all loop
         Refiner.Unchecked_Get.Refine (Filename, Bytes_Contents, Diagnostics);
      end loop;
      Decode_Buffer
        (Bytes_Contents.Buffer (Bytes_Contents.First .. Bytes_Contents.Last),
         Charset,
         Read_BOM,
         Contents,
         Diagnostics);
      Free (Bytes_Contents.Buffer);
   end Read;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Composed_File_Reader) is
   begin
      Free (Self.Refiners);
   end Release;

end Liblktlang_Support.File_Readers;
