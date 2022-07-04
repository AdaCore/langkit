--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
