--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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

end Langkit_Support.Internal;
