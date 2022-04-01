------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
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

package body Langkit_Support.Names.Maps is

   use Helper_Maps;

   function Create_Name (Name : Text_Type; M : Map) return Name_Type
   is (Create_Name (Name, M.Casing));

   function Create_Key (Name : Name_Type; M : Map) return Unbounded_String
   is (To_Unbounded_String (Image (Format_Name (Name, M.Casing))));

   ---------------------
   -- Create_Name_Map --
   ---------------------

   function Create_Name_Map (Casing : Casing_Convention) return Map is
   begin
      return (Casing => Casing, Map => Empty_Map);
   end Create_Name_Map;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self : in out Map; Name : Name_Type; Element : Element_Type)
   is
   begin
      Self.Map.Insert (Create_Key (Name, Self), Element);
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self : in out Map; Name : Name_Type; Element : Element_Type)
   is
   begin
      Self.Map.Include (Create_Key (Name, Self), Element);
   end Include;

   ------------
   -- Lookup --
   ------------

   function Lookup (Self : Map; Name : Name_Type) return Lookup_Result is
      Cur : constant Cursor := Self.Map.Find (Create_Key (Name, Self));
   begin
      if Has_Element (Cur) then
         return (Present => True, Element => Element (Cur));
      else
         return Absent_Lookup_Result;
      end if;
   end Lookup;

   ---------
   -- Get --
   ---------

   function Get (Self : Map; Name : Name_Type) return Element_Type is
      Result : constant Lookup_Result := Lookup (Self, Name);
   begin
      return (if Result.Present
              then Result.Element
              else raise Constraint_Error with "no such name");
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self : in out Map; Name : Text_Type; Element : Element_Type)
   is
   begin
      Insert (Self, Create_Name (Name, Self), Element);
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self : in out Map; Name : Text_Type; Element : Element_Type)
   is
   begin
      Include (Self, Create_Name (Name, Self), Element);
   end Include;

   ------------
   -- Lookup --
   ------------

   function Lookup (Self : Map; Name : Text_Type) return Lookup_Result is
   begin
      return Lookup (Self, Create_Name (Name, Self));
   end Lookup;

   ---------
   -- Get --
   ---------

   function Get (Self : Map; Name : Text_Type) return Element_Type is
   begin
      return Get (Self, Create_Name (Name, Self));
   end Get;

end Langkit_Support.Names.Maps;
