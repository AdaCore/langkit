
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Liblktlang.Common;         use Liblktlang.Common;
with Liblktlang.Implementation; use Liblktlang.Implementation;
with Liblktlang.Rewriting_Implementation;
use Liblktlang.Rewriting_Implementation;

private package Liblktlang.Unparsing_Implementation is

   use Support.Slocs, Support.Text;

   --------------------
   -- Abstract nodes --
   --------------------

   --  Unparsing deals with two kinds of nodes: regular ones, coming from the
   --  parsing of an analysis unit, and rewriting ones, that were created using
   --  the rewriting API. The following types provide an abstraction so that
   --  unparsing code can handle both kinds of nodes.

   type Abstract_Node_Kind is (From_Parsing, From_Rewriting);

   type Abstract_Node (Kind : Abstract_Node_Kind := Abstract_Node_Kind'First)
   is record
      case Kind is
         when From_Parsing   => Parsing_Node   : Bare_Lkt_Node;
         when From_Rewriting => Rewriting_Node : Node_Rewriting_Handle;
      end case;
   end record;

   type Abstract_Cursor (Kind : Abstract_Node_Kind := Abstract_Node_Kind'First)
   is record
      case Kind is
         when From_Parsing   =>
            Parsing_List     : Bare_Lkt_Node;
            Next_Child_Index : Positive;
         when From_Rewriting =>
            Rewriting_Child  : Node_Rewriting_Handle;
      end case;
   end record;

   function Create_Abstract_Node
     (Parsing_Node : Bare_Lkt_Node) return Abstract_Node;
   function Create_Abstract_Node
     (Rewriting_Node : Node_Rewriting_Handle) return Abstract_Node;
   --  Wrapping shortcuts

   function Is_Null (Node : Abstract_Node) return Boolean;
   --  Return whether Node references a null node

   function Kind (Node : Abstract_Node) return Lkt_Node_Kind_Type;
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

   function Rewritten_Node
     (Node : Abstract_Node) return Bare_Lkt_Node;
   --  If Node is a parsing node, return it. If Node is a rewritten node,
   --  return the original node (i.e. of which Node is a rewritten version), or
   --  null if there is no original node.

   type Unparsing_Buffer is limited record
      Content : Unbounded_Wide_Wide_String;
      --  Append-only text buffer for the unparsed tree

      Last_Sloc : Source_Location := (1, 1);
      --  Source location of the next character to append to Content

      Last_Token : Token_Kind;
      --  If Content is not emply, kind of the last token/trivia that was
      --  unparsed. Undefined otherwise.
   end record;

   procedure Append
     (Buffer : in out Unparsing_Buffer; Char : Wide_Wide_Character);

   procedure Append
     (Buffer : in out Unparsing_Buffer;
      Kind   : Token_Kind;
      Text   : Text_Type)
      with Pre => Text'Length > 0;
   --  Append Text, to unparse the given token Kind, to Buffer, updating
   --  Buffer.Last_Sloc and Buffer.Last_Token accordingly.

   procedure Apply_Spacing_Rules
     (Buffer     : in out Unparsing_Buffer;
      Next_Token : Token_Kind);
   --  Add a whitespace or a newline to buffer if mandated by spacing rules
   --  given the next token to emit.

   procedure Unparse
     (Node                : Abstract_Node;
      Unit                : Internal_Unit;
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
      Unit                : Internal_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return String;
   --  Likewise, but directly return a string. The encoding used is the same as
   --  the one that was used to parse Unit.

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Internal_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return String_Access;
   --  Likewise, but return a string access. Callers must deallocate the result
   --  when done with it.

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Internal_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return Unbounded_Text_Type;
   --  Likewise, but return an unbounded text value

end Liblktlang.Unparsing_Implementation;
