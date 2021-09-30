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

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;

--  Even though we don't directly use entities from the Internal.Descriptor
--  package, we still need to import it to get visibility over the
--  Language_Descriptor type (and access its components).

pragma Unreferenced (Langkit_Support.Internal.Descriptor);

package body Langkit_Support.Generic_API.Introspection is

   procedure Check_Value_Type (Id : Language_Id; T : Value_Type);
   --  If ``T`` is not a valid value type for the given language, raise a
   --  ``Precondition_Failure`` exception.

   ---------------------
   -- Last_Value_Type --
   ---------------------

   function Last_Value_Type (Id : Language_Id) return Value_Type is
   begin
      return Id.Value_Types.all'Last;
   end Last_Value_Type;

   ----------------------
   -- Check_Value_Type --
   ----------------------

   procedure Check_Value_Type (Id : Language_Id; T : Value_Type) is
   begin
      if T > Last_Value_Type (Id) then
         raise Precondition_Failure with "invalid value type";
      end if;
   end Check_Value_Type;

   ----------------
   -- Debug_Name --
   ----------------

   function Debug_Name (Id : Language_Id; T : Value_Type) return String is
   begin
      Check_Value_Type (Id, T);
      return Id.Value_Types (T).Debug_Name.all;
   end Debug_Name;

end Langkit_Support.Generic_API.Introspection;
