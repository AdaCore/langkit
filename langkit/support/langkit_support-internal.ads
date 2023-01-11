--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Text;        use Langkit_Support.Text;

--  This package and its children provide common implementation details for
--  Langkit-generated libraries. Even though it is not private (to allow
--  Langkit-generated libraries to use it), it is not meant to be used beyond
--  this. As such, this API is considered unsafe and unstable.

package Langkit_Support.Internal is

   type Text_Access is not null access constant Text_Type;
   type Text_Access_Or_Null is access constant Text_Type;
   --  Reference to a static Unicode string. Used in descriptor tables whenever
   --  we need to provide a name.

   type Debug_String_Access is not null access constant String;
   --  Reference to a statically allocated String. Used in descriptor tables
   --  whenever we need to provide string for debug (compatible with
   --  Ada.Text_IO).

   --  Descriptors for token types

   type Token_Kind_Name_Array is
     array (Token_Kind_Index range <>) of Text_Access;
   type Token_Kind_Name_Array_Access is
     not null access constant Token_Kind_Name_Array;

end Langkit_Support.Internal;
