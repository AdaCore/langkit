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

with Langkit_Support.Errors;   use Langkit_Support.Errors;
with Langkit_Support.Internal; use Langkit_Support.Internal;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;

--  Even though we don't directly use entities from the Internal.Descriptor
--  package, we still need to import it to get visibility over the
--  Language_Descriptor type (and access its components).

pragma Unreferenced (Langkit_Support.Internal.Descriptor);

package body Langkit_Support.Generic_API is

   -------------------
   -- Language_Name --
   -------------------

   function Language_Name (Id : Language_Id) return Name_Type is
   begin
      return Create_Name (Id.Language_Name.all);
   end Language_Name;

   --------------
   -- Language --
   --------------

   function Language (Rule : Grammar_Rule_Ref) return Language_Id is
   begin
      Check_Grammar_Rule (Rule);
      return Rule.Id;
   end Language;

   --------------------------
   -- Default_Grammar_Rule --
   --------------------------

   function Default_Grammar_Rule (Id : Language_Id) return Grammar_Rule_Ref
   is
   begin
      return (Id, Id.Default_Grammar_Rule);
   end Default_Grammar_Rule;

   -----------------------
   -- Grammar_Rule_Name --
   -----------------------

   function Grammar_Rule_Name (Rule : Grammar_Rule_Ref) return Name_Type is
   begin
      return Create_Name (Rule.Id.Grammar_Rule_Names (Rule.Index).all);
   end Grammar_Rule_Name;

   --------------
   -- To_Index --
   --------------

   function To_Index (Rule : Grammar_Rule_Ref) return Grammar_Rule_Index is
   begin
      Check_Grammar_Rule (Rule);
      return Rule.Index;
   end To_Index;

   ----------------
   -- From_Index --
   ----------------

   function From_Index
     (Id : Language_Id; Rule : Grammar_Rule_Index) return Grammar_Rule_Ref is
   begin
      Check_Grammar_Rule (Id, Rule);
      return (Id, Rule);
   end From_Index;

   -----------------------
   -- Last_Grammar_Rule --
   -----------------------

   function Last_Grammar_Rule (Id : Language_Id) return Grammar_Rule_Index is
   begin
      return Id.Grammar_Rule_Names'Last;
   end Last_Grammar_Rule;

   ------------------------
   -- Check_Grammar_Rule --
   ------------------------

   procedure Check_Grammar_Rule (Rule : Grammar_Rule_Ref) is
   begin
      if Rule.Id = null then
         raise Precondition_Failure with "null grammar rule reference";
      end if;
   end Check_Grammar_Rule;

   ------------------------
   -- Check_Grammar_Rule --
   ------------------------

   procedure Check_Grammar_Rule (Id : Language_Id; Rule : Grammar_Rule_Index)
   is
   begin
      if Rule not in Id.Grammar_Rule_Names.all'Range then
         raise Precondition_Failure with
           "invalid grammar rule for this language";
      end if;
   end Check_Grammar_Rule;

   --------------
   -- Language --
   --------------

   function Language (Kind : Token_Kind_Ref) return Language_Id is
   begin
      Check_Token_Kind (Kind);
      return Kind.Id;
   end Language;

   ---------------------
   -- Token_Kind_Name --
   ---------------------

   function Token_Kind_Name (Kind : Token_Kind_Ref) return Name_Type is
   begin
      return Create_Name (Kind.Id.Token_Kind_Names (Kind.Index).all);
   end Token_Kind_Name;

   --------------
   -- To_Index --
   --------------

   function To_Index (Kind : Token_Kind_Ref) return Token_Kind_Index is
   begin
      Check_Token_Kind (Kind);
      return Kind.Index;
   end To_Index;

   ----------------
   -- From_Index --
   ----------------

   function From_Index
     (Id : Language_Id; Kind : Token_Kind_Index) return Token_Kind_Ref is
   begin
      Check_Token_Kind (Id, Kind);
      return (Id, Kind);
   end From_Index;

   ---------------------
   -- Last_Token_Kind --
   ---------------------

   function Last_Token_Kind (Id : Language_Id) return Token_Kind_Index is
   begin
      return Id.Token_Kind_Names'Last;
   end Last_Token_Kind;

   ----------------------
   -- Check_Token_Kind --
   ----------------------

   procedure Check_Token_Kind (Kind : Token_Kind_Ref) is
   begin
      if Kind.Id = null then
         raise Precondition_Failure with "null token kind reference";
      end if;
   end Check_Token_Kind;

   ----------------------
   -- Check_Token_Kind --
   ----------------------

   procedure Check_Token_Kind (Id : Language_Id; Kind : Token_Kind_Index)
   is
   begin
      if Kind not in Id.Token_Kind_Names.all'Range then
         raise Precondition_Failure with
           "invalid token kind for this language";
      end if;
   end Check_Token_Kind;

end Langkit_Support.Generic_API;
