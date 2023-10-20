## vim: filetype=makoada

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with ${ada_lib_name}.Common;         use ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Rewriting_Implementation;
use ${ada_lib_name}.Rewriting_Implementation;

private package ${ada_lib_name}.Unparsing_Implementation is

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
         when From_Parsing   => Parsing_Node   : ${T.root_node.name};
         when From_Rewriting => Rewriting_Node : Node_Rewriting_Handle;
      end case;
   end record;

   type Abstract_Cursor (Kind : Abstract_Node_Kind := Abstract_Node_Kind'First)
   is record
      case Kind is
         when From_Parsing   =>
            Parsing_List     : ${T.root_node.name};
            Next_Child_Index : Positive;
         when From_Rewriting =>
            Rewriting_Child  : Node_Rewriting_Handle;
      end case;
   end record;

   function Create_Abstract_Node
     (Parsing_Node : ${T.root_node.name}) return Abstract_Node;
   function Create_Abstract_Node
     (Rewriting_Node : Node_Rewriting_Handle) return Abstract_Node;
   --  Wrapping shortcuts

   function Is_Null (Node : Abstract_Node) return Boolean;
   --  Return whether Node references a null node

   function Kind (Node : Abstract_Node) return ${T.node_kind};
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
     (Node : Abstract_Node) return ${T.root_node.name};
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

   ----------------------
   -- Unparsing tables --
   ----------------------

   --  At the token level

   type Token_Unparser is record
      Kind : Token_Kind;
      --  Token kind, which is what is used most of the time to determine the
      --  text to emit for a token.

      Text : Text_Cst_Access;
      --  Sometimes, we must emit a token whose kind is not associated to a
      --  specific literal: for such token kinds, this must be the text to emit
      --  for that token.
   end record;

   type Token_Sequence is array (Positive range <>) of Token_Unparser;
   type Token_Sequence_Access is access constant Token_Sequence;
   type Token_Sequence_Access_Array is
      array (Positive range <>) of Token_Sequence_Access;

   Empty_Token_Sequence : aliased constant Token_Sequence := (1 .. 0 => <>);

   --  At the node field level

   type Field_Unparser is record
      Pre_Tokens, Post_Tokens : Token_Sequence_Access;
      --  Lists of tokens to emit before, and after unparsing some node field

      Empty_List_Is_Absent : Boolean;
      --  Whether this field is to be considered as absent when it is an empty
      --  list node.
   end record;

   type Field_Unparser_Array is array (Positive range <>) of Field_Unparser;

   Empty_Field_Unparser : aliased constant Field_Unparser :=
     (Empty_Token_Sequence'Access, Empty_Token_Sequence'Access, False);

   type Field_Unparser_List (N : Natural) is record
      Field_Unparsers : Field_Unparser_Array (1 .. N);
      --  For each field in the node, describe how to unparse it when it's
      --  present. Nothing must be emitted when it's absent.

      Inter_Tokens    : Token_Sequence_Access_Array (1 .. N);
      --  Inter_Tokens (I) specifies the list of tokens to emit after unparsing
      --  field I-1 and field I. When it exists, Inter_Tokens (1) is always
      --  Empty_Token_Sequence'Access.
   end record;
   --  Unparsing table for the fields corresponding to a specific regular node

   type Field_Unparser_List_Access is access constant Field_Unparser_List;

   Empty_Field_Unparser_List : aliased constant Field_Unparser_List :=
     (N => 0, others => <>);

   --  At the node level

   type Node_Unparser_Kind is (Regular, List, Token);

   type Node_Unparser (Kind : Node_Unparser_Kind := Regular) is record
      case Kind is
         when Regular =>
            Pre_Tokens : Token_Sequence_Access;
            --  List of tokens to emit first when unparsing this node

            Field_Unparsers : Field_Unparser_List_Access;
            --  Description of how to unparse fields

            Post_Tokens : Token_Sequence_Access;
            --  List of tokens to emit after fields are unparsed

         when List =>
            Has_Separator : Boolean;
            --  Whether to emit a token between two list items

            Separator : Token_Unparser;
            --  If Has_Separator is true, describe what separator token to emit
            --  between two list items.

         when Token =>
            null;
      end case;
   end record;
   --  Unparsing descriptor for a non-abstract and non-synthetic node

   subtype Regular_Node_Unparser is Node_Unparser (Regular);
   subtype List_Node_Unparser is Node_Unparser (List);
   subtype Token_Node_Unparser is Node_Unparser (Token);

   type Node_Unparser_Map is array (${T.node_kind}) of Node_Unparser;
   type Node_Unparser_Map_Access is access constant Node_Unparser_Map;

   Node_Unparsers : Node_Unparser_Map_Access;
   --  Unparsing table, describing how to unparse all parse nodes in
   --  ${ctx.lang_name}.
   ## TODO??? how can we make this constant without exposing the underlying
   ## array?

end ${ada_lib_name}.Unparsing_Implementation;
