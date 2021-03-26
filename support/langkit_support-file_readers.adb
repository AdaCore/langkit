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

with Ada.Exceptions;
with Ada.IO_Exceptions;

with System;

with GNAT.Byte_Order_Mark;

with GNATCOLL.Iconv;
with GNATCOLL.Mmap;

package body Langkit_Support.File_Readers is

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
      use GNAT.Byte_Order_Mark;
      use GNATCOLL.Iconv;

      --  In the worst case, we have one character per input byte, so the
      --  following is supposed to be big enough.

      Result : Text_Access := new Text_Type (1 .. Buffer'Length);
      State  : Iconv_T;
      Status : Iconv_Result;
      BOM    : BOM_Kind := Unknown;

      Input_Index, Output_Index : Positive;

      First_Output_Index : constant Positive := Result'First;
      --  Index of the first byte in Result at which Iconv must decode Buffer

      Output : Byte_Sequence (1 .. 4 * Buffer'Size);
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

            Free (Result);
            Append (Diagnostics,
                    Message => To_Text ("Could not decode source as """
                                        & Charset & """"));

         when Full_Buffer =>

            --  This is not supposed to happen: we allocated Result to be big
            --  enough in all cases.

            raise Program_Error;

         when Success =>
            null;
      end case;

      Iconv_Close (State);
   end Decode_Buffer;

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
            Append
              (Diagnostics,
               Message => To_Text (Ada.Exceptions.Exception_Message (Exc)));
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

end Langkit_Support.File_Readers;
