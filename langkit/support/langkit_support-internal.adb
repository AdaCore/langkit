--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package body Langkit_Support.Internal is

   ----------------------
   -- Builtin_Filename --
   ----------------------

   function Builtin_Filename (Filename : String) return String is
      Prefix_Last : constant Natural :=
        Filename'First + Builtin_Filename_Prefix'Length - 1;
   begin
      if Prefix_Last >= Filename'Last
         or else Filename (Filename'First .. Prefix_Last)
                 /= Builtin_Filename_Prefix
      then
         return "";
      else
         return Filename (Prefix_Last + 1 .. Filename'Last);
      end if;
   end Builtin_Filename;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (Language : Language_Id;
      Filename : String;
      Buffer   : out Memory_Buffer_And_Access)
   is
      use type GNAT.Strings.String_Access;

      Desc : constant Language_Descriptor_Access := +Language;
      BF   : constant String := Builtin_Filename (Filename);
   begin
      if BF /= "" then
         Buffer := (Desc.Get_Builtin_File (BF), null);
      else
         Buffer.Str_Access := Create (+Filename).Read_File;
         if Buffer.Str_Access = null then
            Buffer.Buffer := No_Memory_Buffer;
         else
            Buffer.Buffer.Address := Buffer.Str_Access.all'Address;
            Buffer.Buffer.Byte_Size := Buffer.Str_Access.all'Length;
         end if;
      end if;
   end Read_File;

   ------------------
   -- Load_Buffers --
   ------------------

   function Load_Buffers
     (Language    : Language_Id;
      Filenames   : File_Array;
      Buffers     : out Memory_Buffer_And_Access_Array;
      Diagnostics : in out Diagnostics_Vectors.Vector) return Boolean is
   begin
      Buffers := (others => No_Memory_Buffer_And_Access);
      for I in 1 .. Filenames'Length loop
         declare
            F : Virtual_File renames Filenames (Filenames'First + I - 1);
            B : Memory_Buffer_And_Access renames
              Buffers (Buffers'First + I - 1);
         begin
            Read_File (Language, +F.Full_Name, B);
            if Is_Null (B.Buffer) then
               Append
                 (Diagnostics,
                  No_Source_Location_Range,
                  To_Text ("cannot read " & (+F.Full_Name)));
               Free (Buffers);
               return False;
            end if;
         end;
      end loop;
      return True;
   end Load_Buffers;

   ----------
   -- Free --
   ----------

   procedure Free (Buffers : in out Memory_Buffer_And_Access_Array) is
   begin
      for B of Buffers loop
         GNAT.Strings.Free (B.Str_Access);
         B := No_Memory_Buffer_And_Access;
      end loop;
   end Free;

end Langkit_Support.Internal;
