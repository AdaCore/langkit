--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides helper on top of the Prettier_Ada library to build
--  documents incremenally: create a document, inspect it, possibly modify it,
--  and at the end produce the final Prettier_Ada document.

with Ada.Containers.Vectors;

with Prettier_Ada.Documents;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Text;        use Langkit_Support.Text;

private package Langkit_Support.Prettier_Utils is

   package Prettier renames Prettier_Ada.Documents;

   --  The Document_Type data structure serves two joint purposes:
   --
   --  * Represent unparsing configuration templates: these contain pure
   --    formatting directives and Recurse items (i.e. anything but Token
   --    items).
   --
   --  * Represent actual unparsing documents: tokens and formatting directives
   --    (i.e. anything but Recurse items).
   --
   --  Formatting directives map the Prettier IR/commands as closely as
   --  possible, with exceptions (for instance there is no Token or Whitespace
   --  command in Prettier) that allow us to refine raw unparsing documents,
   --  for example insert necessary whitespaces/newlines between tokens.

   type Document_Record;
   type Document_Type is access all Document_Record;

   package Document_Vectors is new Ada.Containers.Vectors
     (Positive, Document_Type);

   type Document_Kind is
     (Hard_Line,
      Indent,
      Line,
      List,
      Recurse,
      Soft_Line,
      Token,
      Whitespace);
   type Document_Record (Kind : Document_Kind := Document_Kind'First) is record
      case Kind is
         when Hard_Line =>
            null;

         when Indent =>
            Indent_Document : Document_Type;

         when Line =>
            null;

         when List =>
            List_Documents : Document_Vectors.Vector;

         when Recurse =>
            null;

         when Soft_Line =>
            null;

         when Token =>
            Token_Kind : Token_Kind_Ref;
            Token_Text : Unbounded_Text_Type;

         when Whitespace =>
            Whitespace_Length : Positive;
      end case;
   end record;

   function Is_Correct_Template (Self : Document_Type) return Boolean;
   --  Return whether ``Self`` is a valid template document. This ensures that
   --  formatting the instantiated template will yield exactly once the
   --  sub-document corresponding to the Recurse item.
   --
   --  An example to clarify: suppose we need a template to unparse a node::
   --
   --    ["recurse", "recurse"]
   --
   --  Will be invalid, as the node is unparse twice. Similarly::
   --
   --    ["whitespace"]
   --
   --  Will be invalid, as the node will not be included in the unparsing.

   function To_Prettier_Document
     (Document : Document_Type) return Prettier.Document_Type;
   --  Turn an unparsing document into an actual Prettier document

   type Document_Pool is tagged private;
   --  Allocation pool for ``Document_Type`` nodes

   procedure Release (Self : in out Document_Pool);
   --  Free all the Document_Type nodes allocated in ``Self``

   function Create_Hard_Line
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Hard_Line`` node

   function Create_Indent
     (Self     : in out Document_Pool;
      Document : Document_Type) return Document_Type;
   --  Return an ``Indent`` node

   function Create_Line (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Line`` node

   function Create_List
     (Self      : in out Document_Pool;
      Documents : in out Document_Vectors.Vector) return Document_Type;
   --  Transfer all nodes in ``Documents`` to a new ``List`` node and return
   --  that new node.

   function Create_Empty_List
     (Self : in out Document_Pool) return Document_Type;
   --  Return a new empty ``List`` node

   function Create_Recurse (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Recurse`` node

   function Create_Soft_Line
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Soft_Line`` node

   function Create_Token
     (Self : in out Document_Pool;
      Kind : Token_Kind_Ref;
      Text : Unbounded_Text_Type) return Document_Type;
   --  Return a ``Token`` node

   function Create_Whitespace
     (Self   : in out Document_Pool;
      Length : Positive := 1) return Document_Type;
   --  Return a ``Whitespace`` node for the given length

   procedure Insert_Required_Spacing
     (Pool : in out Document_Pool; Document : in out Document_Type);
   --  Adjust the tree of nodes in ``Document`` so that formatting that
   --  unparsing document will leave the mandatory spacing between tokens (i.e.
   --  so that the formatted document can be re-parsed correctly).

   procedure Dump (Document : Document_Type);
   --  Debug helper: dump a textual representation of Document on the standard
   --  output.

private

   type Document_Pool is new Document_Vectors.Vector with null record;

   procedure Register (Self : in out Document_Pool; Document : Document_Type);
   --  Register ``Document`` as allocated by ``Self``

end Langkit_Support.Prettier_Utils;
