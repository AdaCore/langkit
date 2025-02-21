--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.
--
--  This package implements a tree unparser that is suitable for the rewriting
--  API: unlike `Liblktlang_Support.Generic_API.Unparser`, which handles only
--  parse trees as the input, this unparser also deals with rewritten trees.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Liblktlang_Support.Generic_API;     use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;
with Liblktlang_Support.Rewriting.Types; use Liblktlang_Support.Rewriting.Types;
with Liblktlang_Support.Text;            use Liblktlang_Support.Text;

package Liblktlang_Support.Rewriting.Unparsing is

   --------------------
   -- Abstract nodes --
   --------------------

   --  Unparsing deals with two kinds of nodes: regular ones, coming from the
   --  parsing of a source file, and rewriting ones, that were created using
   --  the rewriting API. The following types provide an abstraction so that
   --  unparsing code can transparently recurse over both kinds of nodes.

   type Abstract_Node_Kind is (From_Parsing, From_Rewriting);

   type Abstract_Node (Kind : Abstract_Node_Kind := Abstract_Node_Kind'First)
   is record
      case Kind is
         when From_Parsing   => Parsing_Node   : Lk_Node;
         when From_Rewriting => Rewriting_Node : Node_Rewriting_Handle_Access;
      end case;
   end record;

   type Abstract_Cursor (Kind : Abstract_Node_Kind := Abstract_Node_Kind'First)
   is record
      case Kind is
         when From_Parsing   =>
            Parsing_List     : Lk_Node;
            Next_Child_Index : Positive;
         when From_Rewriting =>
            Rewriting_Child  : Node_Rewriting_Handle_Access;
      end case;
   end record;

   function Abstract_Node_From_Parsing
     (Parsing_Node : Lk_Node) return Abstract_Node;
   function Abstract_Node_From_Rewriting
     (Rewriting_Node : Node_Rewriting_Handle_Access) return Abstract_Node;
   --  Wrapping shortcuts

   function Is_Null (Node : Abstract_Node) return Boolean;
   --  Return whether Node references a null node

   function Type_Of (Node : Abstract_Node) return Type_Ref;
   --  Return the kind for Node

   function Children_Count (Node : Abstract_Node) return Natural;
   --  Return the number of children that Node has

   function Child
     (Node : Abstract_Node; Index : Positive) return Abstract_Node;
   --  Return the Node's child number Index. Index is a 1-based index. If it is
   --  out of bounds, a Constraint_Error is raised.

   function Iterate_List (Node : Abstract_Node) return Abstract_Cursor;
   --  Assuming that ``Node`` designates a list node, return a cursor to
   --  iterate on its children starting with the first one.

   function Has_Element (Cursor : Abstract_Cursor) return Boolean;
   function Element (Cursor : Abstract_Cursor) return Abstract_Node;
   function Next (Cursor : Abstract_Cursor) return Abstract_Cursor;

   function Text (Node : Abstract_Node) return Text_Type;
   --  Assuming Node is a token node, return the associated text

   function Rewritten_Node (Node : Abstract_Node) return Lk_Node;
   --  If Node is a parsing node, return it. If Node is a rewritten node,
   --  return the original node (i.e. of which Node is a rewritten version), or
   --  null if there is no original node.

   type Unparsing_Buffer is limited record
      Content : Unbounded_Text_Type;
      --  Append-only text buffer for the unparsed tree

      Last_Token : Token_Kind_Index;
      --  If Content is not emply, kind of the last token/trivia that was
      --  unparsed. Undefined otherwise.
   end record;

   procedure Append
     (Buffer : in out Unparsing_Buffer; Char : Character_Type);

   procedure Append
     (Buffer : in out Unparsing_Buffer;
      Kind   : Token_Kind_Index;
      Text   : Text_Type)
      with Pre => Text'Length > 0;
   --  Append Text, to unparse the given token Kind, to Buffer

   procedure Unparse
     (Node                : Abstract_Node;
      Unit                : Lk_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean;
      Result              : out Unparsing_Buffer);
   --  Turn the Node tree into a buffer that can be re-parsed to yield the same
   --  tree (source locations excepted).
   --
   --  If Preserve_Formatting is true, use token/trivia information when
   --  available to preserve original source code formatting.
   --
   --  If As_Unit is true, consider that Node is the root of Unit in order to
   --  preserve the formatting of leading/trailing tokens/trivia. Note that
   --  this has no effect unless Preserve_Formatting itself is true.

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Lk_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return String;
   --  Likewise, but directly return a string. The encoding used is the same as
   --  the one that was used to parse Unit.

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Lk_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return String_Access;
   --  Likewise, but return a string access. Callers must deallocate the result
   --  when done with it.

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Lk_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return Unbounded_Text_Type;
   --  Likewise, but return an unbounded text value

end Liblktlang_Support.Rewriting.Unparsing;
