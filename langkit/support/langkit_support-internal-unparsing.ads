--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package defines the data structures used to represent unparsing
--  information in Langkit-generated libraries. These tables are enough to turn
--  a parse tree to synthetic source code.
--
--  Note that even though it is not private (to allow Langkit-generated
--  libraries to use it), it is not meant to be used beyond this. As such, this
--  API is considered unsafe and unstable.

with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;

package Langkit_Support.Internal.Unparsing is

   ---------------------
   -- Token unparsers --
   ---------------------

   type Token_Unparser_Impl is record
      Kind : Token_Kind_Index;
      --  Token kind for the token to unparse. This is used to apply
      --  spacing/newline rules during the unparsing of a sequence of tokens.

      Text : Text_Access;
      --  Text to emit for that token
   end record;
   type Token_Unparser is access constant Token_Unparser_Impl;
   --  Description of how to unparse a specific token

   type Token_Sequence_Impl is
     array (Positive range <>) of aliased Token_Unparser;
   type Token_Sequence is access constant Token_Sequence_Impl;
   --  Description of how to unparse a specific sequence of tokens

   Empty_Token_Sequence_Impl : aliased constant Token_Sequence_Impl :=
     (1 .. 0 => <>);
   Empty_Token_Sequence      : constant Token_Sequence :=
     Empty_Token_Sequence_Impl'Access;

   type Token_Sequence_Array is array (Positive range <>) of Token_Sequence;
   --  List of sequence unparsers

   type Token_Spacing_Table_Impl is
     array (Token_Family_Index range <>, Token_Family_Index range <>)
     of Boolean;
   type Token_Spacing_Table is access constant Token_Spacing_Table_Impl;
   --  ``Token_Spacing_Table_Impl (TF1, TF2)`` is whether a token that belongs
   --  to the token family ``TF1`` followed by a token that belongs to the
   --  token family ``TF2`` must be separated by at least a whitespace.

   type Token_Newline_Table_Impl is
     array (Token_Kind_Index range <>) of Boolean;
   type Token_Newline_Table is access constant Token_Newline_Table_Impl;
   --  For each token kind, whether it must be followed by a line break

   ---------------------
   -- Field unparsers --
   ---------------------

   type Field_Unparser_Impl is record
      Member : Struct_Member_Index;
      --  Member reference for the field to unparse

      Pre_Tokens, Post_Tokens : Token_Sequence;
      --  Lists of tokens to emit before and after unparsing some node field

      Empty_List_Is_Absent : Boolean;
      --  Whether this field is to be considered as absent when it is an empty
      --  list node.
   end record;
   type Field_Unparser is access constant Field_Unparser_Impl;
   --  Description of how to unparse a specific node field

   type Field_Unparser_Array is
     array (Positive range <>) of aliased Field_Unparser_Impl;

   type Field_Unparser_List_Impl (N : Natural) is record
      Field_Unparsers : Field_Unparser_Array (1 .. N);
      --  For each field in the node, describe how to unparse it when it's
      --  present. Nothing must be emitted when it's absent.

      Inter_Tokens    : Token_Sequence_Array (1 .. N);
      --  Inter_Tokens (I) specifies the list of tokens to emit after unparsing
      --  field I-1 and field I. When it exists, Inter_Tokens (1) is always
      --  Empty_Token_Sequence.
   end record;
   --  Unparsing table for the fields corresponding to a specific regular node

   type Field_Unparser_List is access constant Field_Unparser_List_Impl;

   Empty_Field_Unparser_List : aliased constant Field_Unparser_List_Impl :=
     (N => 0, others => <>);

   --------------------
   -- Node unparsers --
   --------------------

   type Node_Unparser_Kind is (Regular, List, Token);

   type Node_Unparser_Impl (Kind : Node_Unparser_Kind := Regular) is record
      case Kind is
         when Regular =>
            Pre_Tokens : Token_Sequence;
            --  List of tokens to emit first when unparsing this node

            Field_Unparsers : Field_Unparser_List;
            --  Description of how to unparse fields

            Post_Tokens : Token_Sequence;
            --  List of tokens to emit after fields are unparsed

         when List =>
            Separator : Token_Unparser;
            --  If Has_Separator is true, describe what separator token to emit
            --  between two list items.

         when Token =>
            null;
      end case;
   end record;
   type Node_Unparser is access constant Node_Unparser_Impl;
   --  Unparsing descriptor for a non-abstract and non-synthetic concrete node

   subtype Regular_Node_Unparser is Node_Unparser_Impl (Regular);
   subtype List_Node_Unparser is Node_Unparser_Impl (List);
   subtype Token_Node_Unparser is Node_Unparser_Impl (Token);

   type Node_Unparser_Map_Impl is
     array (Type_Index range <>) of aliased Node_Unparser;
   type Node_Unparser_Map is access constant Node_Unparser_Map_Impl;
   --  Associate an unparser to all concrete nodes. Note that since type
   --  indexes for concrete nodes are not continuous, some null items are
   --  expected in Node_Unparser_Map values.

   type Unparsers_Impl is record
      Token_Spacings : Token_Spacing_Table;
      Token_Newlines : Token_Newline_Table;
      Node_Unparsers : Node_Unparser_Map;
   end record;
   type Unparsers is access constant Unparsers_Impl;

end Langkit_Support.Internal.Unparsing;
