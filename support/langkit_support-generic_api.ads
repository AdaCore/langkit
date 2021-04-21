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

--  This package and its children provide generic APIs so that programs can
--  work with all Langkit-generated libraries.
--
--  Note that it is experimental at this stage, and thus not officially
--  supported.

limited private with Langkit_Support.Internal.Descriptor;
with Langkit_Support.Names; use Langkit_Support.Names;

package Langkit_Support.Generic_API is

   type Language_Id is private;
   --  Unique identifier for a Langkit-generated library

   function Language_Name (Id : Language_Id) return Name_Type;
   --  Return the name of the language that the library corresponding to ``Id``
   --  analyzes.

   type Any_Grammar_Rule_Index is new Natural;
   subtype Grammar_Rule_Index is
     Any_Grammar_Rule_Index range 1 ..  Any_Grammar_Rule_Index'Last;
   No_Grammar_Rule_Index : constant Any_Grammar_Rule_Index := 0;
   --  Generic types to designate a grammar rule.
   --
   --  A given languages accepts N grammar rules, so the only valid indexes for
   --  it are 1 .. N. The Last_Grammar_Rule function below gives the actual N
   --  for a given language.
   --
   --  0 designates the default grammar rule, which is used only in arguments
   --  passing. The default grammar rule depends on the language, so it is not
   --  possible to use the corresponding index directly as default formal value
   --  in generic subprogram specs.

   function Last_Grammar_Rule (Id : Language_Id) return Grammar_Rule_Index;
   --  Return the last grammar rule for the given language

   function Default_Grammar_Rule (Id : Language_Id) return Grammar_Rule_Index;
   --  Return the default grammar rule for the given language

   function Grammar_Rule_Name
     (Id : Language_Id; Rule : Grammar_Rule_Index) return Name_Type;
   --  Return the name for the given grammar rule according to the given
   --  language. Raise a ``Precondition_Failure`` exception if ``Rule`` is not
   --  a valid grammar rule for that language.

private

   type Language_Id is
     access constant Langkit_Support.Internal.Descriptor.Language_Descriptor;

   procedure Check_Grammar_Rule (Id : Language_Id; Rule : Grammar_Rule_Index);
   --  If Rule is not a valid grammar rule for Id, raise a
   --  ``Precondition_Failure`` exception.

end Langkit_Support.Generic_API;
