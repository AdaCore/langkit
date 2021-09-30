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

--  This package provides a generic API so that programs can work with the
--  $.Introspection packages of all Langkit-generated libraries.
--
--  Note that it is experimental at this stage, and thus not officially
--  supported.

package Langkit_Support.Generic_API.Introspection is

   ------------------------
   -- Polymorphic values --
   ------------------------

   type Any_Value_Type is new Natural;
   --  Generic type to designate the type of a polymorphic value: boolean,
   --  integer, character, ...
   --
   --  A given language defines types for the ``1 .. Last_Value_Type
   --  (Language)`` range: see the ``Last_Value_Type`` function below. All the
   --  subprograms below raise a ``Precondition_Failure`` exception if passed a
   --  value type index that is not in range for the given language.

   No_Value_Type : constant Any_Value_Type;
   --  Special ``Any_Value_Type`` to mean: no reference to a type

   subtype Value_Type is Any_Value_Type range 1 .. Any_Value_Type'Last;

   type Value_Type_Array is array (Positive range <>) of Value_Type;

   function Last_Value_Type (Id : Language_Id) return Value_Type;
   --  Return the last type index that is valid for the given language

   function Debug_Name (Id : Language_Id; T : Value_Type) return String;
   --  Return the free-form name of this type for debug purposes, according to
   --  the given language.

private

   No_Value_Type : constant Any_Value_Type := 0;

end Langkit_Support.Generic_API.Introspection;
