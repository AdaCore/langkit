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
   --  reciprocical is true.
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
