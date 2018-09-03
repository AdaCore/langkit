------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package body Langkit_Support.Extensions is

   Extensions_Registered : Boolean := False;

   package Extension_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Extension_ID,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   Next_Extension_ID : Extension_ID := 1;
   Extensions        : Extension_Maps.Map;

   function Register_Extension (Name : String) return Extension_ID is
      use Extension_Maps;

      Key : constant Unbounded_String := To_Unbounded_String (Name);
      Cur : constant Extension_Maps.Cursor := Extensions.Find (Key);
   begin

      Extensions_Registered := True;

      if Cur = No_Element then
         declare
            Result : constant Extension_ID := Next_Extension_ID;
         begin
            Next_Extension_ID := Next_Extension_ID + 1;
            Extensions.Insert (Key, Result);
            return Result;
         end;
      else
         return Element (Cur);
      end if;
   end Register_Extension;

   function Has_Extensions return Boolean is (Extensions_Registered);

end Langkit_Support.Extensions;
