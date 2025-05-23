--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Diagnostics;    use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API;    use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Unparsing;
use Langkit_Support.Internal.Unparsing;
with Langkit_Support.Prettier_Utils; use Langkit_Support.Prettier_Utils;
with Langkit_Support.Symbols;        use Langkit_Support.Symbols;
with Langkit_Support.Text;           use Langkit_Support.Text;

private package Langkit_Support.Unparsing_Config is

   type Any_List_Sep_Template_Kind is
     (None, Sep_Template, Leading_Sep_Template, Trailing_Sep_Template);
   subtype List_Sep_Template_Kind is
     Any_List_Sep_Template_Kind range Sep_Template .. Trailing_Sep_Template;
   --  Kind of template for list node separators

   function Node_Unparser_For (Node : Type_Ref) return Node_Unparser
   is ("+" (Language (Node)).Unparsers.Node_Unparsers (To_Index (Node)));

   function Field_Unparser_For
     (Node : Type_Ref; Field_Index : Positive) return Field_Unparser
   is (Node_Unparser_For (Node)
         .Field_Unparsers
         .Field_Unparsers (Field_Index)'Access);

   ----------------------
   -- Linear templates --
   ----------------------

   --  While templates encoded with Template_Type/Document_Type are recursive
   --  data structures (they more or less map to prettier documents) with
   --  disjunction semantics, what we call "linear templates" here are either:
   --
   --  * lists of tokens (Token_Item) and recurse_field (Field_Item) items used
   --    to unparse a concrete regular node type.
   --
   --  * lists of tokens (Token_Item) and recurse (Recurse_Item) items used to
   --    unparse a specific field in a concrete node.
   --
   --  * lists of recurses for join templates (Recurse_Left, then
   --    Recurse_Right).
   --
   --  They are used to process templates that embed text.

   type Linear_Template_Item_Kind is
     (Token_Item,
      Field_Item,
      Recurse_Item,
      Recurse_Left,
      Recurse_Right);
   type Linear_Template_Item
     (Kind : Linear_Template_Item_Kind := Linear_Template_Item_Kind'First)
   is record
      case Kind is
         when Token_Item =>
            Token_Kind : Token_Kind_Ref;
            Token_Text : Unbounded_Text_Type;
            --  Same semantics as the homonym Unparsing_Fragment components

         when Field_Item =>
            Field_Ref      : Struct_Member_Ref;
            Field_Position : Positive;
            --  Same semantics as the corresponding Document_Type components

         when Recurse_Item | Recurse_Left | Recurse_Right =>
            null;
      end case;
   end record;

   function Image (Item : Linear_Template_Item) return String;
   function Is_Equivalent (Left, Right : Linear_Template_Item) return Boolean;

   package Linear_Template_Vectors is new Ada.Containers.Vectors
     (Positive, Linear_Template_Item);

   procedure Linear_Template_From_Unparser
     (Linear_Template : in out Linear_Template_Vectors.Vector;
      Id              : Language_Id;
      Tokens          : Token_Sequence);
   --  Append all items in ``Tokens`` to ``Linear_Template``

   function Linear_Template
     (Node : Type_Ref) return Linear_Template_Vectors.Vector;
   --  Return the linear template that correspond to ``Node``

   function Linear_Template
     (Node  : Type_Ref;
      Field : Struct_Member_Ref) return Linear_Template_Vectors.Vector;
   --  Return the linear template that correspond to the ``Field`` member of
   --  ``Node``.

   --------------
   -- Fragment --
   --------------

   --  We define here a fragment as a slice of source code to unparse. Token
   --  fragments are atomic and directly translate to the corresponding source
   --  excerpt while node fragments must be decomposed into further fragments.

   type Unparsing_Fragment_Kind is
     (Token_Fragment,
      List_Separator_Fragment,
      Field_Fragment,
      List_Child_Fragment,
      Suffix_Comment,
      Line_Comment,
      Whitespaces,
      Line_Breaks);
   subtype Non_Trivia_Fragment_Kind is
     Unparsing_Fragment_Kind range Token_Fragment .. List_Child_Fragment;
   subtype Trivia_Fragment_Kind is
     Unparsing_Fragment_Kind range Suffix_Comment .. Line_Breaks;
   type Unparsing_Fragment
     (Kind : Unparsing_Fragment_Kind := Unparsing_Fragment_Kind'First)
   is record
      case Kind is
         when Token_Fragment | List_Separator_Fragment =>
            Token_Kind : Token_Kind_Ref;
            --  Token kind corresponding to this token fragment. This component
            --  is used to determine the spacing required in the source buffer
            --  between two consecutive tokens.

            Token_Text : Unbounded_Text_Type;
            --  Text to emit when unparsing this token fragment

            case Kind is
               when List_Separator_Fragment =>
                  List_Sep_Kind : List_Sep_Template_Kind;
                  --  Separator template to use for this separator fragment

               when others =>
                  null;
            end case;

         when Field_Fragment | List_Child_Fragment =>
            Node : Lk_Node;
            --  Node for this fragment

            case Kind is
               when Token_Fragment | List_Separator_Fragment =>
                  null;

               when Field_Fragment =>
                  Field : Struct_Member_Ref;
                  --  Syntax field for this fragment (the parent node is a
                  --  regular node).

                  Field_Unparser_Ref : Field_Unparser;
                  --  Unparser for this field

               when List_Child_Fragment =>
                  Child_Index : Positive;
                  --  Index of this field for this fragment (the parent node is
                  --  a list node).

               when Trivia_Fragment_Kind =>
                  null;
            end case;

         when Suffix_Comment | Line_Comment =>

            --  Fragment for:
            --
            --  *  ``Suffix_Comment``: a comment that appears as a line suffix
            --      (i.e. following a non-comment token on the same line).
            --
            --  * ``Line_Comment``: a comment that is alone on its line.

            Comment_Token : Lk_Token;

         when Whitespaces =>

            --  Fragment to record the presence of whitespaces before a suffix
            --  comment. This fragment is materialized only before a suffix
            --  comment: other whitespaces are ignored.

            Whitespaces_Count : Positive;
            --  Number of whitespaces found

         when Line_Breaks =>

            --  Fragment to record the presence of line breaks in the original
            --  source code.

            Line_Breaks_Count : Positive;
            --  Number of line breaks found
      end case;
   end record;
   --  Source code unparsing fragment used to unparse source code. Fragments
   --  are either tokens or nodes (that must be decomposed themselves into
   --  fragments).

   function Fragment_For
     (Id       : Language_Id;
      Token    : Token_Unparser;
      List_Sep : Any_List_Sep_Template_Kind := None) return Unparsing_Fragment;
   --  Return the unparsing fragment corresponding to the given token.
   --
   --  ``List_Sep`` must be ``None`` if this fragment is not for a list
   --  separator, and the corresponding separate kind otherwise.

   function Hash (Self : Struct_Member_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   function Hash (Self : Type_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   package Field_Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Struct_Member_Index,
      Element_Type    => Template_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function JSON_Key_For (Kind : List_Sep_Template_Kind) return String
   is (case Kind is
       when Sep_Template          => "sep",
       when Leading_Sep_Template  => "leading_sep",
       when Trailing_Sep_Template => "trailing_sep");

   type Sep_Templates is array (List_Sep_Template_Kind) of Template_Type;

   type List_Table_Split_Kind is (Empty_Line, Line_Comment);
   type List_Table_Split_Options is array (List_Table_Split_Kind) of Boolean;
   --  Flags to determine what trivias trigger a table split:
   --
   --  * Empty_Line: at least one empty line splits the current table.
   --  * Line_Comment: at least one line comment splits the current table.

   type Table_Config_Record (Enabled : Boolean := False) is record
      case Enabled is
         when False =>
            null;

         when True =>
            Sep_Before : Boolean;
            --  If we generate a table for this list node, whether list
            --  separators must be inserted at the end of the previous row
            --  (``Table_Sep_Before => True``) or at the beginning of the next
            --  row (``Table_Sep_Before => False``).

            Split : List_Table_Split_Options;
            --  If we generate a table for this list node, conditions to split
            --  a table in two.

            Must_Break : Boolean;
            --  If we generate a table for this list node, whether its rows
            --  must break the current group.

            Join_Predicate : Struct_Member_Ref;
            --  If table row join behavior is enabled, reference to the
            --  predicate that determines whether a list element must be joined
            --  to the previous one. ``No_Struct_Member_Ref`` otherwise.

            Join_Template : Template_Type;
            --  Table row join template
      end case;
   end record;

   type List_Config_Record is record
      Seps : Sep_Templates;
      --  Templates to decorate the unparsing of list separators

      Table_Config : Table_Config_Record;
      --  Table config for this list type

      Flush_Before_Children : Boolean;
      --  Whether to insert a Flush_Line_Breaks document before each list child
   end record;

   No_List_Config : constant List_Config_Record :=
     (Seps                  => (others => No_Template),
      Table_Config          => (Enabled => False),
      Flush_Before_Children => True);

   type Node_Config_Record is limited record
      Is_Automatic : Boolean;
      --  Whether this configuration was automatically created (not present in
      --  the JSON configuration and not derived from a non-automatic
      --  configuration).

      Node_Template : Template_Type;
      --  Template to decorate the unparsing of the whole node

      Field_Configs : Field_Config_Maps.Map;
      --  For each non-null syntax field in this node, template to decorate the
      --  unparsing of the field.

      List_Config : List_Config_Record;
      --  Configuration bits that are specific to list nodes
   end record;
   type Node_Config_Access is access all Node_Config_Record;
   --  Unparsing configuration for a given node type

   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Config_Record, Node_Config_Access);

   package Node_Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Type_Index,
      Element_Type    => Node_Config_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Unparsing_Configuration_Record is limited record
      Ref_Count : Natural;
      --  Number of references to this unparsing configuration. When it drops
      --  to 0, we can deallocate it.

      Language : Language_Id;
      --  Language for which this unparsing configuration was created

      Pool : Document_Pool;
      --  Allocation pool for all Document_Type values, to be released when the
      --  unparsing configuration is destroyed.

      Symbols : Symbol_Table;
      --  Symbol table for group ids

      Node_Configs : Node_Config_Maps.Map;
      --  Node configurations for all node types in Language

      Max_Empty_Lines : Integer;
      --  Maximum number of consecutive empty lines to preserve during
      --  source code reformatting. If negative, all empty lines are preserved.
   end record;

   type Unparsing_Configuration_Access is
     access all Unparsing_Configuration_Record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unparsing_Configuration_Record, Unparsing_Configuration_Access);

   procedure Release (Self : in out Unparsing_Configuration_Access);
   --  Release all the memory that was allocated for ``Self``

   function Load_Unparsing_Config_From_Buffer
     (Language        : Language_Id;
      Buffer          : String;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      Check_All_Nodes : Boolean)
      return Unparsing_Configuration_Access;
   --  Implementation for
   --  ``Langkit_Support.Generic_API.Unparsing.Load_Unparsing_Config``, but
   --  loading the unparsing configuration from an in-memory buffer rather than
   --  from a file, and returning a bare access type (or null if the
   --  configuration could not be loaded).

end Langkit_Support.Unparsing_Config;
