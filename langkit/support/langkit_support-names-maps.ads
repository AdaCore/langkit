--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

--  This package provides a generic map type, mapping keys of type
--  Langkit_Support.Names.Name to any value (see Element_Type below).

generic
   type Element_Type is private;
package Langkit_Support.Names.Maps is

   type Map (Casing : Casing_Convention := Camel_With_Underscores)
   is tagged limited private;

   type Lookup_Result (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True  => Element : Element_Type;
      end case;
   end record;

   Absent_Lookup_Result : constant Lookup_Result := (Present => False);

   function Create_Name_Map (Casing : Casing_Convention) return Map;
   --  Create an empty mapping from names using the given convention to
   --  ``Element_Type`` values.

   procedure Insert
     (Self : in out Map; Name : Name_Type; Element : Element_Type);
   --  Insert the ``Name``/``Element`` association into ``Self``.
   --
   --  Raise a ``Constraint_Error`` if there is already an entry for ``Name``.

   procedure Include
     (Self : in out Map; Name : Name_Type; Element : Element_Type);
   --  Insert the ``Name``/``Element`` association into ``Self``.
   --
   --  If there is already an entry for ``Name``, just replace its element with
   --  ``Element``.

   function Lookup (Self : Map; Name : Name_Type) return Lookup_Result;
   --  Look for the association corresponding to ``Name`` in ``Self``. If there
   --  is one, return the corresponding element, otherwise return
   --  ``Absent_Lookup_Result``.

   function Get (Self : Map; Name : Name_Type) return Element_Type;
   --  Like ``Lookup``, but return the element directly instead. Raise a
   --  ``Constraint_Error`` if there is no association.

   --  The following overloads take string names instead of ``Name_Type``
   --  values: they work similarly to the overloads accepting ``Name_Type``
   --  values, except that they first try to decode the string into a name
   --  according to the map convention, raising an ``Invalid_Name_Error`` if
   --  the name is invalid according to the casing convention.

   procedure Insert
     (Self : in out Map; Name : Text_Type; Element : Element_Type);

   procedure Include
     (Self : in out Map; Name : Text_Type; Element : Element_Type);

   function Lookup (Self : Map; Name : Text_Type) return Lookup_Result;

   function Get (Self : Map; Name : Text_Type) return Element_Type;

private

   package Helper_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Element_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Map (Casing : Casing_Convention := Camel_With_Underscores)
   is tagged limited record
      Map    : Helper_Maps.Map;
   end record;

end Langkit_Support.Names.Maps;
