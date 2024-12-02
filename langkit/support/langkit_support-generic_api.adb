--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
      return Create_Name (Rule.Id.Grammar_Rules (Rule.Index).Name.all);
   end Grammar_Rule_Name;

   ---------------
   -- Is_Public --
   ---------------

   function Is_Public (Rule : Grammar_Rule_Ref) return Boolean is
   begin
      Check_Grammar_Rule (Rule);
      return Rule.Id.Grammar_Rules (Rule.Index).Is_Public;
   end Is_Public;

   ----------------------
   -- Grammar_Rule_Doc --
   ----------------------

   function Grammar_Rule_Doc (Rule : Grammar_Rule_Ref) return Text_Type is
   begin
      Check_Grammar_Rule (Rule);
      return Rule.Id.Grammar_Rules (Rule.Index).Doc.all;
   end Grammar_Rule_Doc;

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
      return Id.Grammar_Rules'Last;
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
      if Rule not in Id.Grammar_Rules.all'Range then
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
      Check_Token_Kind (Kind);
      return Create_Name (Kind.Id.Token_Kinds (Kind.Index).Name.all);
   end Token_Kind_Name;

   ----------------
   -- Is_Comment --
   ----------------

   function Is_Comment (Kind : Token_Kind_Ref) return Boolean is
   begin
      Check_Token_Kind (Kind);
      return Kind.Id.Token_Kinds (Kind.Index).Is_Comment;
   end Is_Comment;

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
      return Id.Token_Kinds'Last;
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
      if Kind not in Id.Token_Kinds.all'Range then
         raise Precondition_Failure with
           "invalid token kind for this language";
      end if;
   end Check_Token_Kind;

   --------------
   -- Language --
   --------------

   function Language (Family : Token_Family_Ref) return Language_Id is
   begin
      Check_Token_Family (Family);
      return Family.Id;
   end Language;

   -----------------------
   -- Token_Family_Name --
   -----------------------

   function Token_Family_Name (Family : Token_Family_Ref) return Name_Type is
   begin
      Check_Token_Family (Family);
      return Create_Name (Family.Id.Token_Family_Names (Family.Index).all);
   end Token_Family_Name;

   ------------------
   -- Token_Family --
   ------------------

   function Token_Family (Kind : Token_Kind_Ref) return Token_Family_Ref is
      Family : Token_Family_Index;
   begin
      Check_Token_Kind (Kind);
      Family := Kind.Id.Token_Kinds (Kind.Index).Family;
      return From_Index (Kind.Id, Family);
   end Token_Family;

   --------------
   -- To_Index --
   --------------

   function To_Index (Family : Token_Family_Ref) return Token_Family_Index is
   begin
      Check_Token_Family (Family);
      return Family.Index;
   end To_Index;

   ----------------
   -- From_Index --
   ----------------

   function From_Index
     (Id : Language_Id; Family : Token_Family_Index) return Token_Family_Ref is
   begin
      Check_Token_Family (Id, Family);
      return (Id, Family);
   end From_Index;

   -----------------------
   -- Last_Token_Family --
   -----------------------

   function Last_Token_Family (Id : Language_Id) return Token_Family_Index is
   begin
      return Id.Token_Family_Names'Last;
   end Last_Token_Family;

   ------------------------
   -- Check_Token_Family --
   ------------------------

   procedure Check_Token_Family (Family : Token_Family_Ref) is
   begin
      if Family.Id = null then
         raise Precondition_Failure with "null token family reference";
      end if;
   end Check_Token_Family;

   ------------------------
   -- Check_Token_Family --
   ------------------------

   procedure Check_Token_Family (Id : Language_Id; Family : Token_Family_Index)
   is
   begin
      if Family not in Id.Token_Family_Names.all'Range then
         raise Precondition_Failure with
           "invalid token family for this language";
      end if;
   end Check_Token_Family;

end Langkit_Support.Generic_API;
