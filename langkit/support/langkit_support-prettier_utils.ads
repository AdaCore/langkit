--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.
--
--  This package provides helper on top of the Prettier_Ada library to build
--  documents incremenally: create a document, inspect it, possibly modify it,
--  and at the end produce the final Prettier_Ada document.

with Ada.Containers.Vectors;

with Prettier_Ada.Documents;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Text;        use Langkit_Support.Text;

private package Langkit_Support.Prettier_Utils is

   package Prettier renames Prettier_Ada.Documents;
   use type Prettier.Symbol_Type;

   package Type_Vectors is new Ada.Containers.Vectors (Positive, Type_Ref);

   function Node_Matches
     (Node : Lk_Node; Types : Type_Vectors.Vector) return Boolean
   with Pre => not Node.Is_Null;
   --  Return whether ``Node`` matches at least one type in ``Types``

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

   type Template_Symbol is new Natural;
   subtype Some_Template_Symbol is
     Template_Symbol range 1 ..  Template_Symbol'Last;
   No_Template_Symbol : constant Template_Symbol := 0;

   type Document_Record;
   type Document_Type is access all Document_Record;

   package Document_Vectors is new Ada.Containers.Vectors
     (Positive, Document_Type);

   type Document_Kind is
     (Align,
      Break_Parent,
      Fill,
      Group,
      Hard_Line,
      Hard_Line_Without_Break_Parent,
      If_Break,
      If_Empty,
      Indent,
      Line,
      List,
      Literal_Line,
      Recurse,
      Recurse_Field,
      Recurse_Flatten,
      Soft_Line,
      Token,
      Trim,
      Whitespace);
   type Document_Record (Kind : Document_Kind := Document_Kind'First) is record
      case Kind is
         when Align =>
            Align_Data     : Prettier.Alignment_Data_Type;
            Align_Contents : Document_Type;

         when Break_Parent =>
            null;

         when Fill =>
            Fill_Document : Document_Type;

         when Group =>
            Group_Document     : Document_Type;
            Group_Should_Break : Boolean;
            Group_Id           : Template_Symbol;

         when Hard_Line =>
            null;

         when Hard_Line_Without_Break_Parent =>
            null;

         when If_Break =>
            If_Break_Contents      : Document_Type;
            If_Break_Flat_Contents : Document_Type;
            If_Break_Group_Id      : Template_Symbol;

         when If_Empty =>
            If_Empty_Then : Document_Type;
            If_Empty_Else : Document_Type;

         when Indent =>
            Indent_Document : Document_Type;

         when Line =>
            null;

         when List =>
            List_Documents : Document_Vectors.Vector;

         when Literal_Line =>
            null;

         when Recurse =>
            null;

         when Recurse_Field =>
            Recurse_Field_Ref : Struct_Member_Ref;
            --  Node field on which to recurse

            Recurse_Field_Position : Positive;
            --  1-based index for this field in the list of fields for the
            --  owning node.
            --
            --  This information is in theory redundant with the field
            --  reference, but using an index allows template instantantiation
            --  code to use an array rather than a map to store information
            --  related to fields: more simple and probably more efficient.

         when Recurse_Flatten =>
            Recurse_Flatten_Types : Type_Vectors.Vector;

         when Soft_Line =>
            null;

         when Token =>
            Token_Kind : Token_Kind_Ref;
            Token_Text : Unbounded_Text_Type;

         when Trim =>
            null;

         when Whitespace =>
            Whitespace_Length : Positive;
      end case;
   end record;

   function To_Prettier_Document
     (Document : Document_Type) return Prettier.Document_Type;
   --  Turn an unparsing document into an actual Prettier document

   --  Templates have different kinds depending on how they should be
   --  instantiated:
   --
   --  * No_Template_Kind: Special value to designate the absence of template.
   --
   --  * With_Recurse: Node or field template to instantiate with a single node
   --    document argument. For node templates, the argument must embed all the
   --    tokens for that node. For field templates, the argument must embed
   --    only the unparsing of the node that the field contains.
   --
   --  * With_Recurse_Field: Node template to instantiate with one argument per
   --    field.
   --
   --  * With_Text_Recurse: Field template to instantiate with a single node
   --    argument (the field). The argument must embed all the tokens for the
   --    field (pre/post tokens plus the unparsing of the node that the field
   --    contains).

   type Template_Kind is
     (No_Template_Kind,
      With_Recurse,
      With_Recurse_Field,
      With_Text_Recurse);
   subtype Some_Template_Kind is
     Template_Kind range With_Recurse ..  With_Text_Recurse;
   type Template_Type (Kind : Template_Kind := No_Template_Kind) is record
      case Kind is
         when No_Template_Kind =>
            null;

         when Some_Template_Kind =>
            Root : Document_Type;
      end case;
   end record;
   --  Template document extended with information about how to instantiate it

   No_Template : constant Template_Type := (Kind => No_Template_Kind);

   type Document_Pool is tagged private;
   --  Allocation pool for ``Document_Type`` nodes

   procedure Release (Self : in out Document_Pool);
   --  Free all the Document_Type nodes allocated in ``Self``

   function Create_Align
     (Self     : in out Document_Pool;
      Data     : Prettier.Alignment_Data_Type;
      Contents : Document_Type) return Document_Type;
   --  Return an ``Align`` node

   function Create_Break_Parent
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Break_Parent`` node

   function Create_Fill
     (Self     : in out Document_Pool;
      Document : Document_Type) return Document_Type;
   --  Return a ``Fill`` node

   function Create_Group
     (Self         : in out Document_Pool;
      Document     : Document_Type;
      Should_Break : Boolean;
      Id           : Template_Symbol) return Document_Type;
   --  Return a ``Group`` node

   function Create_Hard_Line
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Hard_Line`` node

   function Create_Hard_Line_Without_Break_Parent
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Hard_Line_Without_Break_Parent`` node

   function Create_If_Break
     (Self          : in out Document_Pool;
      Contents      : Document_Type;
      Flat_Contents : Document_Type := null;
      Group_Id      : Template_Symbol := No_Template_Symbol)
      return Document_Type;
   --  Return an ``If_Break`` node

   function Create_If_Empty
     (Self          : in out Document_Pool;
      Then_Contents : Document_Type;
      Else_Contents : Document_Type) return Document_Type;
   --  Return an ``If_Empty`` node

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

   function Create_Literal_Line
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Literal_Line`` node

   function Create_Empty_List
     (Self : in out Document_Pool) return Document_Type;
   --  Return a new empty ``List`` node

   function Create_Recurse (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Recurse`` node

   function Create_Recurse (Self : in out Document_Pool) return Template_Type;
   --  Return a ``Recurse`` node wrapped in a ``With_Recurse`` template

   function Create_Recurse_Field
     (Self     : in out Document_Pool;
      Field    : Struct_Member_Ref;
      Position : Positive) return Document_Type;
   --  Return a ``Recurse_Field`` node

   function Create_Recurse_Flatten
     (Self  : in out Document_Pool;
      Types : in out Type_Vectors.Vector) return Document_Type;
   --  Return a ``Recurse_Flatten`` node

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

   function Create_Trim (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Trim`` node

   type Spacing_Kind is (None, Whitespace, Newline);
   --  Spacing required between two tokens:
   --
   --  * ``None``: no spacing required, the two tokens can be unparsed next to
   --    each other in the source buffer (spacing is permitted, but not
   --    necessary).
   --
   --  * ``Whitespace``: at least one whitespace (line break, space, horizontal
   --    tabulation: whatever the language accepts as a whitespace) is required
   --    after the first token.
   --
   --  * ``Newline``: a line break is required right after the first token.
   --    Extra spacing is permitted after that line break.

   function Required_Spacing
     (Left, Right : Token_Kind_Ref) return Spacing_Kind;
   --  Return the spacing that is required when unparsing a token of kind
   --  ``Right`` just after a token of kind ``Left`` to a source buffer.
   --
   --  For convenience, ``Required_Spacing`` is allowed to be
   --  ``No_Token_Kind_Ref``: the result is always ``None`` in this case. The
   --  intended use case for this is when processing the first token to unparse
   --  to a source buffer: ``Left`` is ``No_Token_Kind_Ref`` (no token were
   --  unprase in the source buffer yet) and ``Right`` is the first token to
   --  unparse to the source buffer.

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
