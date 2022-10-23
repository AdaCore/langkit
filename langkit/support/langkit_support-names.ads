--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;

with Langkit_Support.Text; use Langkit_Support.Text;

--  This package provides helpers to deal with names regarding of casing
--  conventions (camel case, lower case, ...). What we call a "name" here is a
--  sequence of at least one word, a word being a sequence of at least one
--  non-blank ASCII alphanumericals. In addition, the first word must start
--  with a letter, and each first alphanumerical in a word must be upper case.

package Langkit_Support.Names is

   type Casing_Convention is (Camel_With_Underscores, Camel, Lower, Upper);
   --  Designate a specific casing convention for names formatting. For
   --  instance, to format the ``HTML_Document_Root`` name::
   --
   --     Camel_With_Underscores: HTML_Document_Root
   --     Camel:                  HTMLDocumentRoot
   --     Lower:                  html_document_root
   --     Upper:                  HTML_DOCUMENT_ROOT
   --
   --  Note that ``Camel_With_Underscores`` is the convention which preserves
   --  the most information about a name: for instance it is not possible to
   --  know from ``HTML_DOCUMENT_ROOT`` (an ``Upper`` formatted name) whether
   --  its ``Camel_With_Underscores`` format is ``HTML_Document_ROOT``,
   --  ``Html_Document_Root`` or any other casing variation, while the
   --  reciprocal is true.
   --
   --  Because of this, different names can have different formattings in some
   --  conventions and same formattings in other conventions.

   type Name_Type is private;

   Invalid_Name_Error : exception;

   function Is_Valid_Name
     (Name   : Text_Type;
      Casing : Casing_Convention := Camel_With_Underscores) return Boolean;
   --  Return whether ``Name`` is a valid name in the given casing convention

   function Create_Name
     (Name   : Text_Type;
      Casing : Casing_Convention := Camel_With_Underscores) return Name_Type;
   --  Create a name, decoding ``Name`` according to the given casing
   --  convention. Raise an ``Invalid_Name_Error`` exception if Name is not a
   --  valid in this convention.

   function Format_Name
     (Name : Name_Type; Casing : Casing_Convention) return Text_Type;
   --  Format a name to the given casing convention. Raise an
   --  ``Invalid_Name_Error`` exception if ``Name`` is not initialized.

   function Hash (Name : Name_Type) return Ada.Containers.Hash_Type;

private

   use Ada.Strings.Unbounded;

   type Name_Type is new Unbounded_String;
   --  Internally, we represent names in the equivalent ASCII string in
   --  camel-with-underscores convention.

   function Hash (Name : Name_Type) return Ada.Containers.Hash_Type
   is (Hash (Unbounded_String (Name)));

end Langkit_Support.Names;
