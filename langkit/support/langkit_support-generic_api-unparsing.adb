--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;  use GNATCOLL.VFS;
with Prettier_Ada.Documents.Json;

with Langkit_Support.Errors;         use Langkit_Support.Errors;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Unparsing;
use Langkit_Support.Internal.Unparsing;
with Langkit_Support.Prettier_Utils; use Langkit_Support.Prettier_Utils;
with Langkit_Support.Slocs;          use Langkit_Support.Slocs;
with Langkit_Support.Symbols;        use Langkit_Support.Symbols;

package body Langkit_Support.Generic_API.Unparsing is

   use type Ada.Containers.Count_Type;

   function Load_Unparsing_Config_From_Buffer
     (Language    : Language_Id;
      Buffer      : String;
      Diagnostics : in out Diagnostics_Vectors.Vector)
      return Unparsing_Configuration;
   --  Like ``Load_Unparsing_Config``, but loading the unparsing configuration
   --  from an in-memory buffer rather than from a file.

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Unbounded_String,
      Equivalent_Elements => "=",
      Hash                => Hash);
   Traces_To_Enable : String_Sets.Set;

   procedure Process_Enable_Traces (Trace : GNATCOLL.Traces.Trace_Handle);
   --  Callback for GNATCOLL.Traces.For_Each_Handle. If Traces's name matches
   --  an element of Traces_To_Enable, activate it.

   function Is_Field_Present
     (Field          : Lk_Node;
      Field_Unparser : Field_Unparser_Impl) return Boolean;
   --  Return whether, according to ``Field_Unparser``, the field ``Field``
   --  must be considered as present for unparsing.

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
      List_Child_Fragment);
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

            end case;
      end case;
   end record;
   --  Source code unparsing fragment used to unparse source code. Fragments
   --  are either tokens or nodes (that must be decomposed themselves into
   --  fragments).

   function Fragment_For
     (Id          : Language_Id;
      Token       : Token_Unparser;
      Is_List_Sep : Boolean := False) return Unparsing_Fragment;
   --  Return the unparsing fragment corresponding to the given token.
   --
   --  ``Is_List_Sep`` designates whether this token is used as a list
   --  separator.

   procedure Iterate_On_Fragments
     (Node    : Lk_Node;
      Process : access procedure (Fragment : Unparsing_Fragment));
   --  Decompose ``Node`` into a list of unparsing fragments and call
   --  ``Process`` on each fragment.

   -----------------------
   --  Template symbols --
   -----------------------

   --  The following map type is used during templates parsing to validate the
   --  names used as symbols in JSON templates, and to turn them into their
   --  internal representation: ``Template_Symbol``.

   type Symbol_Info is record
      Source_Name : Unbounded_String;
      --  Name for this symbol as found in the unparsing configuration

      Template_Sym : Template_Symbol;
      --  Unique identifier for this symbol

      Has_Definition : Boolean;
      --  Whether we have found one definition for this symbol
   end record;

   package Symbol_Parsing_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Symbol_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Lookup
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Symbol_Parsing_Maps.Reference_Type;
   --  Return a reference to the entry in ``Symbol_Map`` corresponding to the
   --  ```Source_Name`` symbol (converted to a ``Symbol_Type`` using
   --  ``Symbols``). Create a map entry if it does not exist yet.

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
   --  They are used to process templates that embed text.

   type Linear_Template_Item_Kind is (Token_Item, Field_Item, Recurse_Item);
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

         when Recurse_Item =>
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

   function Hash (Self : Struct_Member_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   function Hash (Self : Type_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   package Field_Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Struct_Member_Index,
      Element_Type    => Template_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Node_Config_Record is limited record
      Node_Template : Template_Type;
      --  Template to decorate the unparsing of the whole node

      Field_Configs : Field_Config_Maps.Map;
      --  For each non-null syntax field in this node, template to decorate the
      --  unparsing of the field.

      List_Sep : Template_Type;
      --  For list nodes only: template to decorate the unparsing of the list
      --  separator.
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

      Node_Configs : Node_Config_Maps.Map;
      --  Node configurations for all node types in Language
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unparsing_Configuration_Record, Unparsing_Configuration_Access);

   type Single_Template_Instantiation_Argument is record
      Document : Document_Type;
      --  Document to substitute to "recurse*" nodes when instantiating a
      --  template.

      Node : Lk_Node;
      --  Node from which ``Document`` was generated. Keeping track of this is
      --  necessary in order to implement instantiation for "recurse_flatten".
   end record;

   package Template_Instantiation_Arg_Vectors is new Ada.Containers.Vectors
     (Positive, Single_Template_Instantiation_Argument);

   type Template_Instantiation_Args (Kind : Some_Template_Kind) is record
      case Kind is
         when With_Recurse | With_Text_Recurse =>
            With_Recurse_Doc : Single_Template_Instantiation_Argument;
            --  Document to use in order to replace "recurse"/"recurse_flatten"
            --  templates.

         when With_Recurse_Field =>
            Field_Docs : Template_Instantiation_Arg_Vectors.Vector;
            --  Documents to use in order to replace "recurse_field" templates
      end case;
   end record;

   function Instantiate_Template
     (Pool      : in out Document_Pool;
      Node      : Lk_Node;
      Template  : Template_Type;
      Arguments : Template_Instantiation_Args) return Document_Type;
   --  Instantiate the given template, i.e. create a copy of it, replacing
   --  "recurse*" documents with the relevant documents in ``Arguments``.
   --
   --  ``Node`` must be the node for which we instantiate this template: it is
   --  used to correctly initialize the ``Node`` component of instantiated
   --  documents.

   function Instantiate_Template_Helper
     (Pool      : in out Document_Pool;
      Node      : Lk_Node;
      Template  : Document_Type;
      Arguments : Template_Instantiation_Args) return Document_Type;
   --  Helper for ``Instantiate_Template_Helper``. Implement the recursive part
   --  of template instantiation: ``Instantiate_Template_Helper`` takes care of
   --  the template unwrapping.

   ---------------------------
   -- Process_Enable_Traces --
   ---------------------------

   procedure Process_Enable_Traces (Trace : GNATCOLL.Traces.Trace_Handle) is
   begin
      if Traces_To_Enable.Contains (To_Unbounded_String (Trace.Unit_Name)) then
         Trace.Set_Active (True);
      end if;
   end Process_Enable_Traces;

   ----------------------
   -- Is_Field_Present --
   ----------------------

   function Is_Field_Present
     (Field          : Lk_Node;
      Field_Unparser : Field_Unparser_Impl) return Boolean
   is
   begin
      --  Unparsing tables (Empty_List_Is_Absent component) determine whether a
      --  non-null child with no children of its own must be treated as absent.

      return not Field.Is_Null
             and then (not Field_Unparser.Empty_List_Is_Absent
                       or else Field.Children_Count > 0);
   end Is_Field_Present;

   ------------------
   -- Fragment_For --
   ------------------

   function Fragment_For
     (Id          : Language_Id;
      Token       : Token_Unparser;
      Is_List_Sep : Boolean := False) return Unparsing_Fragment
   is
      Kind : constant Token_Kind_Ref := From_Index (Id, Token.Kind);
      Text : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Token.Text.all);
   begin
      return
        (if Is_List_Sep
         then (List_Separator_Fragment, Kind, Text)
         else (Token_Fragment, Kind, Text));
   end Fragment_For;

   --------------------------
   -- Iterate_On_Fragments --
   --------------------------

   procedure Iterate_On_Fragments
     (Node    : Lk_Node;
      Process : access procedure (Fragment : Unparsing_Fragment))
   is
      Id        : constant Language_Id := Node.Language;
      Desc      : constant Language_Descriptor_Access := +Id;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      procedure Append (Tokens : Token_Sequence);

      ------------
      -- Append --
      ------------

      procedure Append (Tokens : Token_Sequence) is
      begin
         for T of Tokens.all loop
            Process.all (Fragment_For (Id, T));
         end loop;
      end Append;

      Node_Type     : constant Type_Ref := Type_Of (Node);
      Node_Unparser : Node_Unparser_Impl renames
        Unparsers.Node_Unparsers (To_Index (Node_Type)).all;
   begin
      case Node_Unparser.Kind is
         when Regular =>
            --  Append fragments that precede the first field

            Append (Node_Unparser.Pre_Tokens);

            --  Then append fragments for each field and the tokens between
            --  them.

            for I in 1 .. Node_Unparser.Field_Unparsers.N loop
               declare
                  Field_Unparser : Field_Unparser_Impl renames
                    Node_Unparser.Field_Unparsers.Field_Unparsers (I);
                  Inter_Tokens   : Token_Sequence renames
                    Node_Unparser.Field_Unparsers.Inter_Tokens (I);

                  Child : constant Lk_Node := Node.Child (I);
               begin
                  --  Append fragments that appear unconditionally between
                  --  fields.

                  Append (Inter_Tokens);

                  --  Then append fragments for the field itself, if present

                  if Is_Field_Present (Child, Field_Unparser) then
                     Process.all
                       ((Kind               => Field_Fragment,
                         Node               => Child,
                         Field              => From_Index
                                                 (Id, Field_Unparser.Member),
                         Field_Unparser_Ref => Field_Unparser'Access));
                  end if;
               end;
            end loop;

            --  Append fragments that follow the last field

            Append (Node_Unparser.Post_Tokens);

         when List =>
            for I in 1 .. Node.Children_Count loop
               if I > 1 then
                  if Node_Unparser.Separator = null then
                     Process.all
                       ((Kind => List_Separator_Fragment,
                         Token_Kind => No_Token_Kind_Ref,
                         Token_Text => To_Unbounded_Text ("")));
                  else
                     Process.all
                       (Fragment_For
                          (Id, Node_Unparser.Separator, Is_List_Sep => True));
                  end if;
               end if;

               Process.all
                 ((Kind        => List_Child_Fragment,
                   Node        => Node.Child (I),
                   Child_Index => I));
            end loop;

         when Token =>
            Process.all
              ((Kind       => Token_Fragment,
                Token_Kind => Token_Node_Kind (Node_Type),
                Token_Text => To_Unbounded_Text (Node.Text)));
      end case;
   end Iterate_On_Fragments;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Symbol_Parsing_Maps.Reference_Type
   is
      Symbol   : constant Symbol_Type :=
        Find (Symbols, To_Text (To_String (Source_Name)));
      Position : Symbol_Parsing_Maps.Cursor := Symbol_Map.Find (Symbol);
      Inserted : Boolean;
   begin
      if not Symbol_Parsing_Maps.Has_Element (Position) then

         --  This is the first time we see this symbol in the current template:
         --  create a new internal symbol for it. All internal symbols are
         --  tracked as entries in ``Symbol_Map``, so we can use its length to
         --  compute internal symbols that are unique for the current template.

         declare
            Info : constant Symbol_Info :=
              (Source_Name    => Source_Name,
               Template_Sym   => Template_Symbol (Symbol_Map.Length + 1),
               Has_Definition => False);
         begin
            Symbol_Map.Insert (Symbol, Info, Position, Inserted);
            pragma Assert (Inserted);
         end;
      end if;

      return Symbol_Map.Reference (Position);
   end Lookup;

   -----------
   -- Image --
   -----------

   function Image (Item : Linear_Template_Item) return String is
   begin
      case Item.Kind is
         when Token_Item =>
            return "token """ & Image (To_Text (Item.Token_Text)) & """";
         when Field_Item =>
            return "field " & Image
                                (Format_Name
                                  (Member_Name (Item.Field_Ref), Lower));
         when Recurse_Item =>
            return "recurse";
      end case;
   end Image;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (Left, Right : Linear_Template_Item) return Boolean
   is
   begin
      if Left.Kind /= Right.Kind then
         return False;
      end if;

      --  Omit Token_Kind and Field_Ref from comparisons: these fields are
      --  correctly initialized in the linear template computed from unparsing
      --  tables, but it is still uninitialized during parsing. It is precisely
      --  the comparison between what has been parse and the "expected" linear
      --  template that will allow to initialize these components.

      case Left.Kind is
         when Token_Item =>
            return Left.Token_Text = Right.Token_Text;
         when Field_Item =>
            return Left.Field_Ref = Right.Field_Ref;
         when Recurse_Item =>
            return True;
      end case;
   end Is_Equivalent;

   -----------------------------------
   -- Linear_Template_From_Unparser --
   -----------------------------------

   procedure Linear_Template_From_Unparser
     (Linear_Template : in out Linear_Template_Vectors.Vector;
      Id              : Language_Id;
      Tokens          : Token_Sequence)
   is
   begin
      for T of Tokens.all loop
         declare
            F : constant Unparsing_Fragment := Fragment_For (Id, T);
         begin
            Linear_Template.Append
              (Linear_Template_Item'
                 (Kind       => Token_Item,
                  Token_Kind => F.Token_Kind,
                  Token_Text => F.Token_Text));
         end;
      end loop;
   end Linear_Template_From_Unparser;

   ---------------------
   -- Linear_Template --
   ---------------------

   function Linear_Template
     (Node : Type_Ref) return Linear_Template_Vectors.Vector
   is
      Id        : constant Language_Id := Language (Node);
      Desc      : constant Language_Descriptor_Access := +Id;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      Result : Linear_Template_Vectors.Vector;

      Node_Unparser : Node_Unparser_Impl renames
        Unparsers.Node_Unparsers (To_Index (Node)).all;
      pragma Assert (Node_Unparser.Kind = Regular);
   begin
      --  Append tokens that precede the first field

      Linear_Template_From_Unparser (Result, Id, Node_Unparser.Pre_Tokens);

      --  Then append recurse_field documents for each field and the tokens
      --  between them.

      for I in 1 .. Node_Unparser.Field_Unparsers.N loop
         declare
            Field_Unparser : Field_Unparser_Impl renames
              Node_Unparser.Field_Unparsers.Field_Unparsers (I);
            Inter_Tokens   : Token_Sequence renames
              Node_Unparser.Field_Unparsers.Inter_Tokens (I);
         begin
            --  Append tokens that appear unconditionally between fields

            Linear_Template_From_Unparser (Result, Id, Inter_Tokens);

            --  Append fields themselves

            Result.Append
              (Linear_Template_Item'
                (Kind           => Field_Item,
                 Field_Ref      => From_Index (Id, Field_Unparser.Member),
                 Field_Position => I));
         end;
      end loop;

      --  Append tokens that follow the last field

      Linear_Template_From_Unparser (Result, Id, Node_Unparser.Post_Tokens);

      return Result;
   end Linear_Template;

   ---------------------
   -- Linear_Template --
   ---------------------

   function Linear_Template
     (Node  : Type_Ref;
      Field : Struct_Member_Ref) return Linear_Template_Vectors.Vector
   is
      Id        : constant Language_Id := Language (Node);
      Desc      : constant Language_Descriptor_Access := +Id;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      Node_Unparser : Node_Unparser_Impl renames
        Unparsers.Node_Unparsers (To_Index (Node)).all;
      pragma Assert (Node_Unparser.Kind = Regular);

      Result : Linear_Template_Vectors.Vector;

      --  Look for the unparser corresponding to Field
   begin
      for I in 1 .. Node_Unparser.Field_Unparsers.N loop
         declare
            Field_Unparser : Field_Unparser_Impl renames
              Node_Unparser.Field_Unparsers.Field_Unparsers (I);
         begin
            if Field_Unparser.Member = To_Index (Field) then
               Linear_Template_From_Unparser
                 (Result, Id, Field_Unparser.Pre_Tokens);
               Result.Append
                 (Linear_Template_Item'(Kind => Recurse_Item));
               Linear_Template_From_Unparser
                 (Result, Id, Field_Unparser.Post_Tokens);
               return Result;
            end if;
         end;
      end loop;

      --  Linear_Template is supposed to be called with a valid pair of
      --  Node/Field, so the loop above is supposed to return a value.

      raise Program_Error;
   end Linear_Template;

   -------------------------------------
   -- Default_Unparsing_Configuration --
   -------------------------------------

   function Default_Unparsing_Configuration
     (Language : Language_Id) return Unparsing_Configuration
   is
      Diagnostics : Diagnostics_Vectors.Vector;
      Result      : constant Unparsing_Configuration :=
        Load_Unparsing_Config_From_Buffer
          (Language, Language.Unparsers.Default_Config.all, Diagnostics);
   begin
      if not Diagnostics.Is_Empty then
         raise Program_Error;
      end if;
      return Result;
   end Default_Unparsing_Configuration;

   ---------------------------
   -- Load_Unparsing_Config --
   ---------------------------

   function Load_Unparsing_Config
     (Language    : Language_Id;
      Filename    : String;
      Diagnostics : in out Diagnostics_Vectors.Vector)
      return Unparsing_Configuration
   is
      use type GNAT.Strings.String_Access;

      JSON_Text : GNAT.Strings.String_Access := Create (+Filename).Read_File;
   begin
      if JSON_Text = null then
         Append
           (Diagnostics,
            No_Source_Location_Range,
            To_Text ("cannot read " & Filename));
         return No_Unparsing_Configuration;
      end if;

      return Result : constant Unparsing_Configuration :=
        Load_Unparsing_Config_From_Buffer
          (Language, JSON_Text.all, Diagnostics)
      do
         GNAT.Strings.Free (JSON_Text);
      end return;
   end Load_Unparsing_Config;

   ---------------------------------------
   -- Load_Unparsing_Config_From_Buffer --
   ---------------------------------------

   function Load_Unparsing_Config_From_Buffer
     (Language    : Language_Id;
      Buffer      : String;
      Diagnostics : in out Diagnostics_Vectors.Vector)
      return Unparsing_Configuration
   is
      --  Create a map so that we can lookup nodes/fields by name

      Symbols : Symbol_Table := Create_Symbol_Table;
      Map     : constant Name_Map := Create_Name_Map
        (Id             => Language,
         Symbols        => Symbols,
         Enum_Types     => Camel,
         Enum_Values    => Lower,
         Struct_Types   => Camel,
         Struct_Members => Lower);

      function To_Symbol (Name : String) return Symbol_Type
      is (Find (Symbols, To_Text (Name)));
      --  Convert a name to the corresponding symbol in Symbols

      function To_Type_Index (Name : String) return Type_Index;
      --  Return the type index for the node type that has the given
      --  camel-case Name. Raise an Invalid_Input exception if there is no such
      --  node.

      function To_Struct_Member_Index
        (Name : String; Node : Type_Ref) return Struct_Member_Index;
      --  Return the member index for the field that has the given lower-case
      --  Name for the given Node. Raise an Invalid_Input exception if there is
      --  no such field.

      function Node_Type_Image (Node : Type_Ref) return String
      is (Image (Format_Name (Node_Type_Name (Node), Camel)));
      --  Return the expected name for the given Node

      function Field_Image
        (Member : Struct_Member_Ref; Node : Type_Ref) return String
      is (Node_Type_Image (Node)
          & "." & Image (Format_Name (Member_Name (Member), Lower)));
      --  Return the expected name for the given Member in the given Node

      type Template_Parsing_State_Kind is
        (Simple_Recurse, Recurse_Field, Recurse_In_Field);
      --  There are two kinds of templates we expect to find in unparsing
      --  configurations:
      --
      --  * Simple_Recurse templates, that cannot contain "text" nodes and
      --    whose linearization must yield a single "recurse" node.
      --
      --  * Recurse_Field templates, whose linearization must yield the
      --    sequence of "text"/"recurse_field" that is expected for a node.
      --
      --  * Recurse_In_Field templates, whose linearization must yield the
      --    sequence of "text"/"recurse" that is expected for a node field.

      type Template_Parsing_State
        (Kind : Template_Parsing_State_Kind := Simple_Recurse)
      is record
         case Kind is
            when Simple_Recurse =>
               Recurse_Found : Boolean;
               --  Whether template parsing has found the "recurse" node
               --  expected for the current branch.

            when Recurse_Field | Recurse_In_Field =>
               Linear_Template : Linear_Template_Vectors.Vector;
               --  Sequence of tokens/fields that the parsed template is
               --  supposed to yield once instantiated/formatted.

               Linear_Position : Positive;
               --  Position in Linear_Template of the next item to be found in
               --  the template to parse.
         end case;
      end record;

      type Template_Parsing_Context_Kind is
        (Node_Template, Field_Template, Sep_Template);
      --  Indicate which kind of template we are parsing:
      --
      --  ``Node_Template``: a "node" template.
      --
      --  ``Field_Template``: a template in the "fields" mapping.
      --
      --  ``Sep_Template``: a "sep" template.

      type Template_Parsing_Context (Kind : Template_Parsing_Context_Kind) is
      record
         Node : Type_Ref;
         --  Node for which we parse this template

         State : Template_Parsing_State;
         --  Keep track of the parsing state for this template; used for
         --  validation.

         case Kind is
            when Node_Template | Sep_Template =>
               null;

            when Field_Template =>
               Field : Struct_Member_Ref;
               --  Field for which we parse this template
         end case;
      end record;

      function Template_Kind
        (Context : Template_Parsing_Context_Kind;
         JSON    : JSON_Value) return Template_Parsing_State_Kind;
      --  Determine the plausible kind for the given JSON-encoded template in
      --  the given context. Note that this is just a heuristic: if will
      --  return the right kind for a well-formed template, but will return an
      --  approximation for an ill-formed template.

      function Initial_State_For
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Context : Template_Parsing_Context) return Template_Parsing_State;
      --  Return an inital template parsing state for the "node" template of
      --  ``Node``, to create from ``JSON``. Raise an Invalid_Input exception
      --  if the initial state found is invalid in this context.

      function Initial_State_For
        (Node    : Type_Ref;
         Field   : Struct_Member_Ref;
         JSON    : JSON_Value) return Template_Parsing_State;
      --  Return an inital template parsing state for the template ``JSON``
      --  corresponding to the node field ``Node``/``Field``.
      --  ``Node``, to create from ``JSON``.

      function Parse_Template
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Template_Type;
      --  Parse a JSON-encoded temlate document. Raise an Invalid_Input
      --  exception if the JSON encoding is invalid or if the template is
      --  ill-formed.

      function Parse_Template_Helper
        (JSON       : JSON_Value;
         Context    : in out Template_Parsing_Context;
         Symbol_Map : in out Symbol_Parsing_Maps.Map) return Document_Type;
      --  Helper for ``Parse_Template``. Implement the recursive part of
      --  templates parsing: ``Parse_Template`` takes care of the post-parsing
      --  validation.

      procedure Process_Recurse (Context : in out Template_Parsing_Context);
      --  Record in ``Context.State``` that a "recurse" or "recurse_flatten"
      --  template item has been found. This raises an error if one has already
      --  been found.

      procedure Process_Linear_Template_Item
        (Item    : in out Linear_Template_Item;
         Context : in out Template_Parsing_Context);
      --  Record in ``Context.State``that the given ``Item`` (token or
      --  "recurse_field" template) has been found. This raises an error if
      --  ``Item`` was not expected at that point for this template.
      --
      --  On success, update ``Item`` so that its Token_Ref/Field_Position
      --  component reflect the ones found in the linear template fo the
      --  current nod.

      procedure Abort_Parsing
        (Context : Template_Parsing_Context; Message : String)
      with No_Return;
      --  Raise an Invalid_Input exception with the given message,
      --  contextualized.

      procedure Abort_Parsing_Linear
        (Context : Template_Parsing_Context; Message : String)
      with No_Return;
      --  Like ``Abort_Parsing``, but add the expected linear template at the
      --  end of the error message.

      procedure Load_Field_Configs
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Configs : out Field_Config_Maps.Map);
      --  Assuming that JSON is the "fields" configuration for Node, parse its
      --  field configurations and set Configs accordingly.

      procedure Abort_Parsing (Message : String) with No_Return;
      --  Append an item to ``Diagnostics`` and raise an Invalid_Input
      --  exception.

      Result : constant Unparsing_Configuration_Access :=
        new Unparsing_Configuration_Record;
      Pool   : Document_Pool renames Result.Pool;

      -------------------
      -- To_Type_Index --
      -------------------

      function To_Type_Index (Name : String) return Type_Index is
         T : constant Type_Ref := Map.Lookup_Type (To_Symbol (Name));
      begin
         if T = No_Type_Ref or else not Is_Node_Type (T) then
            Abort_Parsing ("invalid node name: " & Name);
         end if;
         return To_Index (T);
      end To_Type_Index;

      ----------------------------
      -- To_Struct_Member_Index --
      ----------------------------

      function To_Struct_Member_Index
        (Name : String; Node : Type_Ref) return Struct_Member_Index
      is
         M : constant Struct_Member_Ref :=
           Map.Lookup_Struct_Member (Node, To_Symbol (Name));
      begin
         if M = No_Struct_Member_Ref then
            Abort_Parsing
              ("invalid field for " & Node_Type_Image (Node) & ": " & Name);
         elsif not Is_Field (M) then
            Abort_Parsing
              (Name & " is not a syntax field for " & Node_Type_Image (Node));
         elsif Is_Null_For (M, Node) then
            Abort_Parsing
              (Name & " is a null field for " & Node_Type_Image (Node));
         else
            return To_Index (M);
         end if;
      end To_Struct_Member_Index;

      -------------------
      -- Template_Kind --
      -------------------

      function Template_Kind
        (Context : Template_Parsing_Context_Kind;
         JSON    : JSON_Value) return Template_Parsing_State_Kind
      is
         Result          : Template_Parsing_State_Kind := Simple_Recurse;
         Abort_Recursion : exception;

         function Kind_Matches
           (JSON : JSON_Value; Kind : String) return Boolean;
         --  Assuming that JSON is an object, return whether it has a "kind"
         --  field equal to Kind.

         procedure Process (JSON : JSON_Value);
         procedure Process_Map_Item (Name : String; JSON : JSON_Value);

         ------------------
         -- Kind_Matches --
         ------------------

         function Kind_Matches
           (JSON : JSON_Value; Kind : String) return Boolean
         is
         begin
            return JSON.Has_Field ("kind")
                   and then JSON.Get ("kind").Kind = JSON_String_Type
                   and then String'(JSON.Get ("kind")) = Kind;
         end Kind_Matches;

         -------------
         -- Process --
         -------------

         procedure Process (JSON : JSON_Value) is
         begin
            case JSON.Kind is
               when JSON_Object_Type =>

                  --   As soon as we find a "text" template node, we know this
                  --   is a Recurse_Field template (in "node") or a
                  --   Recurse_In_Field template (in "fields").
                  --
                  --   For a "node", we also know this is a Recurse_Field
                  --   template as soon as we find a "recurse_field" node.

                  case Context is
                     when Node_Template =>
                        if Kind_Matches (JSON, "text")
                           or else Kind_Matches (JSON, "recurse_field")
                        then
                           Result := Recurse_Field;
                           raise Abort_Recursion;
                        end if;

                     when Field_Template =>
                        if Kind_Matches (JSON, "text") then
                           Result := Recurse_In_Field;
                           raise Abort_Recursion;
                        end if;

                     when Sep_Template =>
                        null;
                  end case;

                  JSON.Map_JSON_Object (Process_Map_Item'Access);

               when JSON_Array_Type =>
                  for Item of JSON_Array'(JSON.Get) loop
                     Process (Item);
                  end loop;

               when others =>
                  null;
            end case;
         end Process;

         ----------------------
         -- Process_Map_Item --
         ----------------------

         procedure Process_Map_Item (Name : String; JSON : JSON_Value) is
            pragma Unreferenced (Name);
         begin
            Process (JSON);
         end Process_Map_Item;

      begin
         Process (JSON);
         return Result;
      exception
         when Abort_Recursion =>
            return Result;
      end Template_Kind;

      -----------------------
      -- Initial_State_For --
      -----------------------

      function Initial_State_For
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Context : Template_Parsing_Context) return Template_Parsing_State is
      begin
         return Result : Template_Parsing_State
                           (Template_Kind (Context.Kind, JSON))
         do
            case Result.Kind is
               when Simple_Recurse =>
                  Result.Recurse_Found := False;

               when Recurse_Field | Recurse_In_Field =>
                  if Result.Kind = Recurse_Field then

                     --  Ensure that "recurse_field" templates are valid for
                     --  this node.

                     if Is_Abstract (Node) then
                        Abort_Parsing
                          (Context,
                           "text/recurse_field are valid for concrete nodes"
                           & " only");
                     elsif Is_Token_Node (Node) then
                        Abort_Parsing
                          (Context,
                           "text/recurse_field are not valid for token nodes");
                     elsif Is_List_Node (Node) then
                        Abort_Parsing
                          (Context,
                           "text/recurse_field are not valid for list nodes");
                     end if;
                  end if;

                  Result.Linear_Template := Linear_Template (Node);
                  Result.Linear_Position := 1;
            end case;
         end return;
      end Initial_State_For;

      -----------------------
      -- Initial_State_For --
      -----------------------

      function Initial_State_For
        (Node    : Type_Ref;
         Field   : Struct_Member_Ref;
         JSON    : JSON_Value) return Template_Parsing_State
      is
      begin
         return Result : Template_Parsing_State
                           (Template_Kind (Field_Template, JSON))
         do
            case Result.Kind is
               when Simple_Recurse =>
                  Result.Recurse_Found := False;

               when Recurse_Field =>

                  --  Knowing that we are processing a field template,
                  --  Template_Kind is not supposed to return Recurse_Field.

                  raise Program_Error;

               when Recurse_In_Field =>
                  Result.Linear_Template := Linear_Template (Node, Field);
                  Result.Linear_Position := 1;
            end case;
         end return;
      end Initial_State_For;

      --------------------
      -- Parse_Template --
      --------------------

      function Parse_Template
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Template_Type
      is
         Symbol_Map : Symbol_Parsing_Maps.Map;
         --  Mapping from name symbols found in the JSON (Symbol_Type) and
         --  "ids" (Template_Symbol).

         Root : constant Document_Type :=
           Parse_Template_Helper (JSON, Context, Symbol_Map);
      begin
         --  Make sure that all symbols referenced in this template are also
         --  defined in this template.

         for Info of Symbol_Map loop
            if not Info.Has_Definition then
               Abort_Parsing
                 (Context,
                  "undefined symbol: " & To_String (Info.Source_Name));
            end if;
         end loop;

         case Context.State.Kind is
            when Simple_Recurse =>
               if Context.State.Recurse_Found then
                  return (Kind => With_Recurse, Root => Root);
               else
                  Abort_Parsing (Context, "recursion is missing");
               end if;

            when Recurse_Field | Recurse_In_Field =>

               --  Make sure that the template covers all items in the linear
               --  template.

               if Context.State.Linear_Position
                  /= Context.State.Linear_Template.Last_Index + 1
               then
                  Abort_Parsing_Linear
                    (Context,
                     "missing "
                     & Image
                         (Context.State.Linear_Template
                            (Context.State.Linear_Position)));
               end if;

               case Context.State.Kind is
                  when Simple_Recurse =>
                     raise Program_Error;
                  when Recurse_Field =>
                     return (Kind => With_Recurse_Field, Root => Root);
                  when Recurse_In_Field =>
                     return (Kind => With_Text_Recurse, Root => Root);
               end case;
         end case;
      end Parse_Template;

      ---------------------------
      -- Parse_Template_Helper --
      ---------------------------

      function Parse_Template_Helper
        (JSON       : JSON_Value;
         Context    : in out Template_Parsing_Context;
         Symbol_Map : in out Symbol_Parsing_Maps.Map) return Document_Type is
      begin
         case JSON.Kind is
         when JSON_Array_Type =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for D of JSON_Array'(JSON.Get) loop
                  Items.Append
                    (Parse_Template_Helper (D, Context, Symbol_Map));
               end loop;
               return Pool.Create_List (Items);
            end;

         when JSON_String_Type =>
            declare
               Value : constant String := JSON.Get;
            begin
               if Value = "breakParent" then
                  return Pool.Create_Break_Parent;
               elsif Value = "hardline" then
                  return Pool.Create_Hard_Line;
               elsif Value = "hardlineWithoutBreakParent" then
                  return Pool.Create_Hard_Line_Without_Break_Parent;
               elsif Value = "line" then
                  return Pool.Create_Line;
               elsif Value = "recurse" then
                  Process_Recurse (Context);
                  return Pool.Create_Recurse;
               elsif Value = "softline" then
                  return Pool.Create_Soft_Line;
               elsif Value = "literalline" then
                  return Pool.Create_Literal_Line;
               elsif Value = "trim" then
                  return Pool.Create_Trim;
               elsif Value = "whitespace" then
                  return Pool.Create_Whitespace;
               else
                  Abort_Parsing
                    (Context,
                     "invalid string template document kind: " & Value);
               end if;
            end;

         when JSON_Object_Type =>
            if not JSON.Has_Field ("kind") then
               Abort_Parsing (Context, "missing ""kind"" key");
            elsif JSON.Get ("kind").Kind /= JSON_String_Type then
               Abort_Parsing
                 (Context,
                  "invalid ""kind"": " & JSON.Get ("kind").Kind'Image);
            end if;

            declare
               Kind : constant String := JSON.Get ("kind");
            begin
               if Kind = "align" then
                  declare
                     Width : JSON_Value;
                     Data  : Prettier.Alignment_Data_Type;
                  begin
                     if not JSON.Has_Field ("width") then
                        Abort_Parsing
                          (Context, "missing ""width"" key for align");
                     end if;
                     Width := JSON.Get ("width");
                     case Width.Kind is
                        when JSON_Int_Type =>
                           Data := (Kind => Prettier.Width, N => Width.Get);
                        when JSON_String_Type =>
                           Data := (Kind => Prettier.Text, T => Width.Get);
                        when others =>
                           Abort_Parsing
                             (Context, "invalid ""width"" key for align");
                     end case;

                     if not JSON.Has_Field ("contents") then
                        Abort_Parsing
                          (Context, "missing ""contents"" key for align");
                     end if;

                     return Pool.Create_Align
                       (Data,
                        Parse_Template_Helper
                          (JSON.Get ("contents"), Context, Symbol_Map));
                  end;

               elsif Kind in
                  "dedent" | "dedentToRoot" | "markAsRoot" | "innerRoot"
               then
                  if not JSON.Has_Field ("contents") then
                     Abort_Parsing
                       (Context, "missing ""contents"" key for " & Kind);
                  end if;
                  return Pool.Create_Align
                    (Data     => (if Kind = "dedent"
                                  then (Kind => Prettier.Dedent)
                                  elsif Kind = "dedentToRoot"
                                  then (Kind => Prettier.Dedent_To_Root)
                                  elsif Kind = "markAsRoot"
                                  then (Kind => Prettier.Root)
                                  elsif Kind = "innerRoot"
                                  then (Kind => Prettier.Inner_Root)
                                  else raise Program_Error),
                     Contents => Parse_Template_Helper
                                   (JSON.Get ("contents"),
                                    Context,
                                    Symbol_Map));

               elsif Kind = "fill" then
                  declare
                     Document : Document_Type;
                  begin
                     if not JSON.Has_Field ("document") then
                        Abort_Parsing
                          (Context, "missing ""document"" key for fill");
                     end if;
                     Document :=
                       Parse_Template_Helper
                         (JSON.Get ("document"), Context, Symbol_Map);

                     return Pool.Create_Fill (Document);
                  end;

               elsif Kind = "group" then
                  declare
                     Document     : Document_Type;
                     Should_Break : Boolean := False;
                     Id           : Template_Symbol := No_Template_Symbol;
                  begin
                     if not JSON.Has_Field ("document") then
                        Abort_Parsing
                          (Context, "missing ""document"" key for group");
                     end if;
                     Document :=
                       Parse_Template_Helper
                         (JSON.Get ("document"), Context, Symbol_Map);

                     if JSON.Has_Field ("shouldBreak") then
                        declare
                           JSON_Should_Break : constant JSON_Value :=
                             JSON.Get ("shouldBreak");
                        begin
                           if JSON_Should_Break.Kind /= JSON_Boolean_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid group shouldBreak: "
                                 & JSON_Should_Break.Kind'Image);
                           end if;
                           Should_Break := JSON_Should_Break.Get;
                        end;
                     end if;

                     --  If a symbol is given to identify this group, create an
                     --  internal symbol for it.

                     if JSON.Has_Field ("id") then
                        declare
                           JSON_Id : constant JSON_Value := JSON.Get ("id");
                        begin
                           if JSON_Id.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid group id: "
                                 & JSON_Id.Kind'Image);
                           end if;

                           declare
                              Info : Symbol_Info renames
                                Lookup (JSON_Id.Get, Symbols, Symbol_Map);
                           begin
                              --  Ensure that there is no conflicting symbol
                              --  definition in this template.

                              if Info.Has_Definition then
                                 Abort_Parsing
                                   (Context,
                                    "duplicate group id: " & JSON_Id.Get);
                              else
                                 Info.Has_Definition := True;
                              end if;
                              Id := Info.Template_Sym;
                           end;
                        end;
                     end if;

                     return Pool.Create_Group (Document, Should_Break, Id);
                  end;

               elsif Kind = "ifBreak" then
                  declare
                     Contents      : Document_Type;
                     Flat_Contents : Document_Type;

                     Contents_Context : Template_Parsing_Context := Context;
                     Flat_Context     : Template_Parsing_Context := Context;

                     Group_Id : Template_Symbol := No_Template_Symbol;
                  begin
                     if not JSON.Has_Field ("breakContents") then
                        Abort_Parsing
                          (Context,
                           "missing ""breakContents"" key for ifBreak");
                     end if;

                     Contents :=
                       Parse_Template_Helper
                         (JSON.Get ("breakContents"),
                          Contents_Context,
                          Symbol_Map);

                     Flat_Contents :=
                       (if JSON.Has_Field ("flatContents")
                        then Parse_Template_Helper
                               (JSON.Get ("flatContents"),
                                Flat_Context,
                                Symbol_Map)
                        else null);

                     --  Unify the parsing state for both branches and update
                     --  Context accordingly.

                     if Contents_Context.State /= Flat_Context.State then
                        Abort_Parsing
                          (Context,
                           "ifBreak alternatives have inconsistent recurse"
                           & " structure");
                     end if;
                     Context.State := Contents_Context.State;

                     --  If present, get the symbol for the given group id

                     if JSON.Has_Field ("groupId") then
                        declare
                           JSON_Id : constant JSON_Value :=
                             JSON.Get ("groupId");
                        begin
                           if JSON_Id.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid group id: "
                                 & JSON_Id.Kind'Image);
                           end if;

                           Group_Id :=
                             Lookup (JSON_Id.Get, Symbols, Symbol_Map)
                             .Template_Sym;
                        end;
                     end if;

                     return Pool.Create_If_Break
                              (Contents, Flat_Contents, Group_Id);
                  end;

               elsif Kind = "ifEmpty" then
                  if Context.Kind /= Field_Template then
                     Abort_Parsing
                       (Context,
                        """ifEmpty"" is valid only in field templates");
                  end if;

                  declare
                     Then_Contents : Document_Type;
                     Else_Contents : Document_Type;

                     Then_Context : Template_Parsing_Context := Context;
                     Else_Context : Template_Parsing_Context := Context;
                  begin
                     if not JSON.Has_Field ("then") then
                        Abort_Parsing
                          (Context,
                           "missing ""then"" key for ifEmpty");
                     end if;
                     Then_Contents :=
                       Parse_Template_Helper
                         (JSON.Get ("then"), Then_Context, Symbol_Map);

                     if not JSON.Has_Field ("else") then
                        Abort_Parsing
                          (Context,
                           "missing ""else"" key for ifEmpty");
                     end if;
                     Else_Contents :=
                       Parse_Template_Helper
                         (JSON.Get ("else"), Else_Context, Symbol_Map);

                     --  Unify the parsing state for both branches and update
                     --  Context accordingly.

                     if Then_Context.State /= Else_Context.State then
                        Abort_Parsing
                          (Context,
                           "ifEmpty alternatives have inconsistent recurse"
                           & " structure");
                     end if;
                     Context.State := Else_Context.State;

                     return Pool.Create_If_Empty
                              (Then_Contents, Else_Contents);
                  end;

               elsif Kind = "indent" then
                  if not JSON.Has_Field ("contents") then
                     Abort_Parsing
                       (Context, "missing ""contents"" key for indent");
                  end if;
                  return Pool.Create_Indent
                    (Parse_Template_Helper
                       (JSON.Get ("contents"), Context, Symbol_Map));

               elsif Kind = "recurse_field" then
                  declare
                     F : JSON_Value;
                  begin
                     if not JSON.Has_Field ("field") then
                        Abort_Parsing
                          (Context, "missing ""field"" key for recurse_field");
                     end if;

                     F := JSON.Get ("field");
                     if F.Kind /= JSON_String_Type then
                        Abort_Parsing
                          (Context,
                           "invalid recurse_field field: " & F.Kind'Image);
                     end if;

                     --  Validate that "recurse_field" can appear in this
                     --  template at this place. Let
                     --  Process_Linear_Template_Item give us the field
                     --  position: initialize it with Positive'Last (an
                     --  obviously invalid position) to make it clear that this
                     --  component needs an update.

                     declare
                        Item : Linear_Template_Item :=
                          (Kind           => Field_Item,
                           Field_Ref      => From_Index
                                               (Language,
                                                To_Struct_Member_Index
                                                  (F.Get, Context.Node)),
                           Field_Position => Positive'Last);
                     begin
                        Process_Linear_Template_Item (Item, Context);
                        return Pool.Create_Recurse_Field
                                 (Item.Field_Ref, Item.Field_Position);
                     end;
                  end;

               elsif Kind = "recurse_flatten" then
                  declare
                     L     : JSON_Value;
                     T     : Type_Ref;
                     Types : Type_Vectors.Vector;
                  begin
                     --  Use the given type guard for the flattening, or use
                     --  the root node to flatten for all nodes.

                     if JSON.Has_Field ("if") then
                        L := JSON.Get ("if");
                        if L.Kind /= JSON_Array_Type then
                           Abort_Parsing
                             (Context,
                              "invalid recurse_flatten if: " & L.Kind'Image);
                        end if;
                        for Name of JSON_Array'(L.Get) loop
                           if Name.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid item in recurse_flatten if: "
                                 & Name.Kind'Image);
                           end if;

                           T := Map.Lookup_Type (To_Symbol (Name.Get));
                           if T = No_Type_Ref or else not Is_Node_Type (T) then
                              Abort_Parsing
                                (Context,
                                 "invalid node type in recurse_flatten if: "
                                 & Name.Get);
                           end if;

                           Types.Append (T);
                        end loop;
                     else
                        Types.Append (Root_Node_Type (Language));
                     end if;
                     Process_Recurse (Context);
                     return Pool.Create_Recurse_Flatten (Types);
                  end;

               elsif Kind = "text" then
                  declare
                     T : JSON_Value;
                  begin
                     if not JSON.Has_Field ("text") then
                        Abort_Parsing
                          (Context, "missing ""text"" key for text");
                     end if;

                     T := JSON.Get ("text");
                     if T.Kind /= JSON_String_Type then
                        Abort_Parsing
                          (Context, "invalid text field: " & T.Kind'Image);
                     end if;

                     --  Validate that this text template can appear in this
                     --  template at this place. Let
                     --  Process_Linear_Template_Item give us the token type:
                     --  initialize it with No_Token_Kind_Ref (an obviously
                     --  invalid position) to make it clear that this component
                     --  needs an update.

                     declare
                        Item : Linear_Template_Item :=
                          (Kind       => Token_Item,
                           Token_Kind => No_Token_Kind_Ref,
                           Token_Text => To_Unbounded_Text
                                           (From_UTF8 (T.Get)));
                     begin
                        Process_Linear_Template_Item (Item, Context);
                        return Pool.Create_Token
                                 (Item.Token_Kind, Item.Token_Text);
                     end;
                  end;

               elsif Kind = "whitespace" then
                  if not JSON.Has_Field ("length") then
                     Abort_Parsing (Context, "missing ""length"" key");
                  end if;
                  declare
                     Length : constant JSON_Value := JSON.Get ("length");
                  begin
                     if Length.Kind /= JSON_Int_Type then
                        Abort_Parsing
                          (Context,
                           "invalid whitespace length: " & Length.Kind'Image);
                     end if;
                     return Pool.Create_Whitespace (Length.Get);
                  end;

               else
                  Abort_Parsing
                    (Context, "invalid template document kind: " & Kind);
               end if;
            end;

         when others =>
            Abort_Parsing
              (Context, "invalid template JSON node: " & JSON.Kind'Image);
         end case;
      end Parse_Template_Helper;

      ---------------------
      -- Process_Recurse --
      ---------------------

      procedure Process_Recurse (Context : in out Template_Parsing_Context) is
      begin
         case Context.State.Kind is
            when Simple_Recurse =>
               if Context.State.Recurse_Found then
                  Abort_Parsing (Context, "too many recursions");
               else
                  Context.State.Recurse_Found := True;
               end if;

            when Recurse_Field =>
               Abort_Parsing
                 (Context,
                  "using ""recurse""/""recurse_flatten"" in the same template"
                  & " as ""recurse_field""/""text"" is invalid");

            when Recurse_In_Field =>
               declare
                  Item : Linear_Template_Item := (Kind => Recurse_Item);
               begin
                  Process_Linear_Template_Item (Item, Context);
               end;
         end case;
      end Process_Recurse;

      ----------------------------------
      -- Process_Linear_Template_Item --
      ----------------------------------

      procedure Process_Linear_Template_Item
        (Item    : in out Linear_Template_Item;
         Context : in out Template_Parsing_Context)
      is
         function What return String
         is (case Item.Kind is
             when Token_Item   => "text",
             when Field_Item   => "recurse_field",
             when Recurse_Item => "recurse");
      begin
         --  Ensure that it is valid to have a "recurse_field" node in this
         --  template.

         if Context.State.Kind not in Recurse_Field | Recurse_In_Field then
            Abort_Parsing
              (Context, What & " cannot appear in a ""recurse"" template");
         end if;

         --  Now validate this new item: it must match what the linear template
         --  expects next.

         if Context.State.Linear_Position
            > Context.State.Linear_Template.Last_Index
         then
            Abort_Parsing_Linear
              (Context,
               "unexpected extra template item: " & Image (Item));
         end if;

         declare
            Expected : constant Linear_Template_Item :=
              Context.State.Linear_Template (Context.State.Linear_Position);
         begin
            if not Is_Equivalent (Item, Expected) then
               Abort_Parsing_Linear
                 (Context,
                  Image (Expected) & " expected, got " & Image (Item));
            end if;

            --  As per this procedure's contract, update Item to given the
            --  expected value to all of its components.

            Item := Expected;
         end;

         --  If we reach this point, we know that Item was expected: update the
         --  linear template position for the next expected item.

         Context.State.Linear_Position := Context.State.Linear_Position + 1;
      end Process_Linear_Template_Item;

      -------------------
      -- Abort_Parsing --
      -------------------

      procedure Abort_Parsing
        (Context : Template_Parsing_Context; Message : String)
      is
         Prefix : constant String :=
           (case Context.Kind is
            when Node_Template =>
              """node"" template for " & Node_Type_Image (Context.Node),
            when Sep_Template =>
              """sep"" template for " & Node_Type_Image (Context.Node),
            when Field_Template =>
              "template for " & Field_Image (Context.Field, Context.Node));
      begin
         Abort_Parsing (Prefix & ": " & Message);
      end Abort_Parsing;

      --------------------------
      -- Abort_Parsing_Linear --
      --------------------------

      procedure Abort_Parsing_Linear
        (Context : Template_Parsing_Context; Message : String)
      is
         Suffix : Unbounded_String;
      begin
         Append (Suffix, ASCII.LF & "Expected:");
         for Item of Context.State.Linear_Template loop
            Append (Suffix, ASCII.LF & "* " & Image (Item));
         end loop;
         Abort_Parsing (Context, Message & To_String (Suffix));
      end Abort_Parsing_Linear;

      ------------------------
      -- Load_Field_Configs --
      ------------------------

      procedure Load_Field_Configs
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Configs : out Field_Config_Maps.Map)
      is
         procedure Process (Name : String; Value : JSON_Value);
         --  Load the Value template for the Node field called Name

         -------------
         -- Process --
         -------------

         procedure Process (Name : String; Value : JSON_Value) is
            Member  : constant Struct_Member_Index :=
              To_Struct_Member_Index (Name, Node);
            Context : Template_Parsing_Context :=
              (Kind  => Field_Template,
               State => Initial_State_For
                          (Node  => Node,
                           Field => From_Index (Language, Member),
                           JSON  => Value),
               Node  => Node,
               Field => From_Index (Language, Member));
         begin
            Configs.Insert (Member, Parse_Template (Value, Context));
         end Process;

      begin
         Configs.Clear;
         JSON.Map_JSON_Object (Process'Access);
      end Load_Field_Configs;

      -------------------
      -- Abort_Parsing --
      -------------------

      procedure Abort_Parsing (Message : String) is
      begin
         Append (Diagnostics, No_Source_Location_Range, To_Text (Message));
         raise Invalid_Input;
      end Abort_Parsing;

      --  First, parse the JSON document

      JSON_Result : constant Read_Result := Read (Buffer);
      JSON        : JSON_Value;
   begin
      if JSON_Result.Success then
         JSON := JSON_Result.Value;
      else
         declare
            Sloc       : constant Source_Location :=
              (Line_Number (JSON_Result.Error.Line),
               Column_Number (JSON_Result.Error.Column));
            Sloc_Range : constant Source_Location_Range :=
              Make_Range (Sloc, Sloc);
         begin
            Append
              (Diagnostics,
               Sloc_Range,
               To_Text (To_String (JSON_Result.Error.Message)));
            raise Invalid_Input;
         end;
      end if;

      --  Then load the unparsing configuration from it. Require a
      --  "node_configs" key.

      Result.Ref_Count := 1;
      Result.Language := Language;

      if not JSON.Has_Field ("node_configs") then
         Abort_Parsing ("missing ""node_configs"" key");
      end if;

      declare
         Node_Configs : constant JSON_Value := JSON.Get ("node_configs");

         procedure Process (Name : String; Value : JSON_Value);
         --  Load the unparsing configuration from Value for the node called
         --  Name.

         -------------
         -- Process --
         -------------

         procedure Process (Name : String; Value : JSON_Value) is
            Key     : constant Type_Index := To_Type_Index (Name);
            Node    : constant Type_Ref := From_Index (Language, Key);
            Config  : Node_Config_Access := new Node_Config_Record'
              (Node_Template => No_Template,
               Field_Configs => <>,
               List_Sep      => No_Template);
         begin
            Result.Node_Configs.Insert (Key, Config);

            if Value.Has_Field ("node") then
               declare
                  JSON_Template : constant JSON_Value := Value.Get ("node");
                  Context       : Template_Parsing_Context :=
                    (Kind  => Node_Template,
                     Node  => Node,
                     State => <>);
               begin
                  Context.State :=
                    Initial_State_For (Node, JSON_Template, Context);
                  Config.Node_Template :=
                    Parse_Template (JSON_Template, Context);
               end;
            else
               Config.Node_Template := Pool.Create_Recurse;
            end if;

            if Value.Has_Field ("fields") then
               Load_Field_Configs
                 (Node, Value.Get ("fields"), Config.Field_Configs);
            end if;

            if Value.Has_Field ("sep") then
               if not Is_List_Node (Node) then
                  Abort_Parsing
                    (Name & " is not a list node, invalid ""sep"""
                     & " configuration");
               end if;
               declare
                  Context : Template_Parsing_Context :=
                    (Kind  => Sep_Template,
                     Node  => Node,
                     State => (Kind          => Simple_Recurse,
                               Recurse_Found => False));
               begin
                  Config.List_Sep :=
                    Parse_Template (Value.Get ("sep"), Context);
               end;
            end if;

         exception
            when others =>
               Free (Config);
               raise;
         end Process;

      begin
         Node_Configs.Map_JSON_Object (Process'Access);
      end;

      --  Now that we all node customizations, compute the unparsing
      --  configuration for all nodes.

      for Node of All_Node_Types (Language) loop
         declare
            Key         : constant Type_Index := To_Index (Node);
            Cur         : constant Node_Config_Maps.Cursor :=
              Result.Node_Configs.Find (Key);
            Node_Config : Node_Config_Access;
            Base_Config : constant Node_Config_Access :=
              (if Node = Root_Node_Type (Language)
               then null
               else Result.Node_Configs.Element (To_Index (Base_Type (Node))));
         begin
            --  Create a configuration for this node if there isn't one yet

            if Node_Config_Maps.Has_Element (Cur) then
               Node_Config := Node_Config_Maps.Element (Cur);
            else
               Node_Config := new Node_Config_Record;
               Result.Node_Configs.Insert (Key, Node_Config);
            end if;

            --  Inherit configuration details from the base node, or provide
            --  the default one for the root node.

            if Node_Config.Node_Template = No_Template then
               Node_Config.Node_Template :=
                 (if Base_Config = null
                  then Pool.Create_Recurse
                  else Base_Config.Node_Template);
            end if;

            for Member of Members (Node) loop
               if Is_Field (Member) and then not Is_Null_For (Member, Node)
               then
                  declare
                     Key            : constant Struct_Member_Index :=
                       To_Index (Member);
                     Cur            : constant Field_Config_Maps.Cursor :=
                       Node_Config.Field_Configs.Find (Key);
                     Present        : constant Boolean :=
                        Field_Config_Maps.Has_Element (Cur);
                     Field_Template : Template_Type;
                  begin
                     if Present then
                        Field_Template := Field_Config_Maps.Element (Cur);
                     elsif Base_Config /= null then
                        declare
                           Cur : constant Field_Config_Maps.Cursor :=
                             Base_Config.Field_Configs.Find (Key);
                        begin
                           Field_Template :=
                             (if Field_Config_Maps.Has_Element (Cur)
                              then Field_Config_Maps.Element (Cur)
                              else Pool.Create_Recurse);
                        end;
                     end if;

                     if Field_Template = No_Template then
                        Field_Template := Pool.Create_Recurse;
                     end if;

                     if Present then
                        Node_Config.Field_Configs.Replace_Element
                          (Cur, Field_Template);
                     else
                        Node_Config.Field_Configs.Insert (Key, Field_Template);
                     end if;
                  end;
               end if;
            end loop;

            if Node_Config.List_Sep = No_Template then
               Node_Config.List_Sep :=
                 (if Base_Config = null
                  then Pool.Create_Recurse
                  else Base_Config.List_Sep);
            end if;
         end;
      end loop;

      Destroy (Symbols);
      return (Ada.Finalization.Controlled with Value => Result);

   exception
      when Invalid_Input =>
         pragma Assert (not Diagnostics.Is_Empty);
         Destroy (Symbols);
         Pool.Release;
         return No_Unparsing_Configuration;
   end Load_Unparsing_Config_From_Buffer;

   --------------------------
   -- Instantiate_Template --
   --------------------------

   function Instantiate_Template
     (Pool      : in out Document_Pool;
      Node      : Lk_Node;
      Template  : Template_Type;
      Arguments : Template_Instantiation_Args) return Document_Type is
   begin
      return Instantiate_Template_Helper
               (Pool, Node, Template.Root, Arguments);
   end Instantiate_Template;

   ---------------------------------
   -- Instantiate_Template_Helper --
   ---------------------------------

   function Instantiate_Template_Helper
     (Pool      : in out Document_Pool;
      Node      : Lk_Node;
      Template  : Document_Type;
      Arguments : Template_Instantiation_Args) return Document_Type is
   begin
      case Template.Kind is
         when Align =>
            return Pool.Create_Align
              (Template.Align_Data,
               Instantiate_Template_Helper
                 (Pool, Node, Template.Align_Contents, Arguments));

         when Break_Parent =>
            return Pool.Create_Break_Parent;

         when Fill =>
            return Pool.Create_Fill
              (Instantiate_Template_Helper
                 (Pool, Node, Template.Fill_Document, Arguments));

         when Group =>
            return Pool.Create_Group
              (Instantiate_Template_Helper
                 (Pool, Node, Template.Group_Document, Arguments),
               Template.Group_Should_Break,
               Template.Group_Id);

         when Hard_Line =>
            return Pool.Create_Hard_Line;

         when Hard_Line_Without_Break_Parent =>
            return Pool.Create_Hard_Line_Without_Break_Parent;

         when If_Break =>
            return Pool.Create_If_Break
              (Instantiate_Template_Helper
                 (Pool, Node, Template.If_Break_Contents, Arguments),
               Instantiate_Template_Helper
                 (Pool, Node, Template.If_Break_Flat_Contents, Arguments),
               Template.If_Break_Group_Id);

         when If_Empty =>
            declare
               Child       : constant Lk_Node :=
                 Arguments.With_Recurse_Doc.Node;
               Subtemplate : constant Document_Type :=
                 (if Child.Is_List_Node and then Child.Children_Count = 0
                  then Template.If_Empty_Then
                  else Template.If_Empty_Else);
            begin
               return Instantiate_Template_Helper
                 (Pool, Node, Subtemplate, Arguments);
            end;

         when Indent =>
            return Pool.Create_Indent
              (Instantiate_Template_Helper
                 (Pool, Node, Template.Indent_Document, Arguments));

         when Line =>
            return Pool.Create_Line;

         when List =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for I in 1 .. Template.List_Documents.Last_Index loop
                  Items.Append
                    (Instantiate_Template_Helper
                       (Pool,
                        Node,
                        Template.List_Documents.Element (I),
                        Arguments));
               end loop;
               return Pool.Create_List (Items);
            end;

         when Literal_Line =>
            return Pool.Create_Literal_Line;

         when Recurse =>
            return Arguments.With_Recurse_Doc.Document;

         when Recurse_Field =>
            return Arguments
                   .Field_Docs (Template.Recurse_Field_Position)
                   .Document;

         when Recurse_Flatten =>
            declare
               Arg : constant Single_Template_Instantiation_Argument :=
                 Arguments.With_Recurse_Doc;
            begin
               return Result : Document_Type := Arg.Document do

                  --  As long as Result is a document we can flatten and that
                  --  was created by a node that passes the flattening guard,
                  --  unwrap it.

                  while not Arg.Node.Is_Null
                        and then Node_Matches
                                   (Arg.Node, Template.Recurse_Flatten_Types)
                  loop
                     case Result.Kind is
                        when Align =>
                           Result := Result.Align_Contents;

                        when Fill =>
                           Result := Result.Fill_Document;

                        when Group =>
                           Result := Result.Group_Document;

                        when Indent =>
                           Result := Result.Indent_Document;

                        when List =>
                           exit when Result.List_Documents.Length /= 1;
                           Result := Result.List_Documents.First_Element;

                        when others =>
                           exit;
                     end case;
                  end loop;
               end return;
            end;

         when Soft_Line =>
            return Pool.Create_Soft_Line;

         when Token =>
            return Pool.Create_Token
              (Template.Token_Kind, Template.Token_Text);

         when Trim =>
            return Pool.Create_Trim;

         when Whitespace =>
            return Pool.Create_Whitespace (Template.Whitespace_Length);
      end case;
   end Instantiate_Template_Helper;

   -------------------------
   -- Unparse_To_Prettier --
   -------------------------

   function Unparse_To_Prettier
     (Node   : Lk_Node;
      Config : Unparsing_Configuration)
      return Prettier_Ada.Documents.Document_Type
   is
      Pool : Document_Pool;

      procedure Unparse_Tokens
        (Tokens : Token_Sequence; Items : in out Document_Vectors.Vector);
      --  Create template nodes for each element in ``Tokens`` and append them
      --  to ``Items``.

      function Unparse_Node (N : Lk_Node) return Document_Type;
      --  Using the unparsing configuration for N, unparse it to a Prettier
      --  document.

      procedure Unparse_Field
        (Node        : Lk_Node;
         Node_Config : Node_Config_Record;
         Child       : Lk_Node;
         Field_Ref   : Struct_Member_Index;
         Unparser    : Field_Unparser_Impl;
         Items       : in out Document_Vectors.Vector);
      --  Unparse ``Child``, which is the ``Field_Ref`` field of ``Node``. The
      --  Resulting items are appended to ``Items``. ``Node_Config`` must be
      --  the node unparsing configuration for ``Node``, and ``Unparser`` must
      --  be the unparser for this field.

      --------------------
      -- Unparse_Tokens --
      --------------------

      procedure Unparse_Tokens
        (Tokens : Token_Sequence; Items : in out Document_Vectors.Vector) is
      begin
         for T of Tokens.all loop
            declare
               Fragment : constant Unparsing_Fragment :=
                 Fragment_For (Config.Value.Language, T);
               pragma Assert (Fragment.Kind = Token_Fragment);
            begin
               Items.Append
                 (Pool.Create_Token
                    (Fragment.Token_Kind, Fragment.Token_Text));
            end;
         end loop;
      end Unparse_Tokens;

      ------------------
      -- Unparse_Node --
      ------------------

      function Unparse_Node (N : Lk_Node) return Document_Type is
         Node_Config : Node_Config_Record renames
           Config.Value.Node_Configs.Element (To_Index (Type_Of (N))).all;
         Items       : Document_Vectors.Vector;

         procedure Process_Fragment (F : Unparsing_Fragment);
         --  Append the documents to ``Items`` to represent the given unparsing
         --  fragment.

         ----------------------
         -- Process_Fragment --
         ----------------------

         procedure Process_Fragment (F : Unparsing_Fragment) is
         begin
            case F.Kind is
               when Token_Fragment | List_Separator_Fragment =>
                  declare
                     Token : Document_Type :=
                       (if F.Token_Kind = No_Token_Kind_Ref
                        then Pool.Create_Empty_List
                        else Pool.Create_Token (F.Token_Kind, F.Token_Text));
                  begin
                     if F.Kind = List_Separator_Fragment then
                        pragma Assert
                          (Node_Config.List_Sep.Kind = With_Recurse);
                        declare
                           Args : constant Template_Instantiation_Args :=
                             (Kind             => With_Recurse,
                              With_Recurse_Doc =>
                                (Document => Token, Node => N));
                        begin
                           Token := Instantiate_Template
                             (Pool      => Pool,
                              Node      => N,
                              Template  => Node_Config.List_Sep,
                              Arguments => Args);
                        end;
                     end if;
                     Items.Append (Token);
                  end;

               when Field_Fragment =>
                  Unparse_Field
                    (Node        => N,
                     Node_Config => Node_Config,
                     Child       => F.Node,
                     Field_Ref   => To_Index (F.Field),
                     Unparser    => F.Field_Unparser_Ref.all,
                     Items       => Items);

               when List_Child_Fragment =>
                  Items.Append (Unparse_Node (F.Node));
            end case;
         end Process_Fragment;

         Template : Template_Type renames Node_Config.Node_Template;
      begin
         case Some_Template_Kind (Template.Kind) is
            when With_Recurse =>

               --  First gather documents for all the fragments in this node,
               --  then group them in a list document, and use that list to
               --  instantiate the template for the node itself.

               Iterate_On_Fragments (N, Process_Fragment'Access);
               return Instantiate_Template
                 (Pool      => Pool,
                  Node      => N,
                  Template  => Template,
                  Arguments =>
                    (Kind             => With_Recurse,
                     With_Recurse_Doc =>
                       (Document => Pool.Create_List (Items),
                        Node     => N)));

            when With_Recurse_Field =>

               --  Compute sub-documents for all fields (do not forget the
               --  field's own pre/post tokensn) and let the template do its
               --  magic.

               declare
                  Id            : constant Language_Id := N.Language;
                  Desc          : constant Language_Descriptor_Access := +Id;
                  Unparsers     : Unparsers_Impl renames Desc.Unparsers.all;
                  Node_Type     : constant Type_Ref := Type_Of (N);
                  Node_Unparser : Node_Unparser_Impl renames
                    Unparsers.Node_Unparsers (To_Index (Node_Type)).all;

                  Arguments : Template_Instantiation_Args (With_Recurse_Field);
               begin
                  for I in 1 .. N.Children_Count loop
                     declare
                        Child          : constant Lk_Node := N.Child (I);
                        Field_Unparser : Field_Unparser_Impl renames
                          Node_Unparser.Field_Unparsers.Field_Unparsers (I);
                        Child_Doc      : Document_Type;
                     begin
                        if Is_Field_Present (Child, Field_Unparser) then
                           Items.Clear;
                           Unparse_Field
                             (Node        => N,
                              Node_Config => Node_Config,
                              Child       => Child,
                              Field_Ref   => Field_Unparser.Member,
                              Unparser    => Field_Unparser,
                              Items       => Items);
                           Child_Doc := Pool.Create_List (Items);
                        else
                           Child_Doc := Pool.Create_Empty_List;
                        end if;
                        Arguments.Field_Docs.Append
                          (Single_Template_Instantiation_Argument'
                             (Document => Child_Doc,
                              Node     => Child));
                     end;
                  end loop;

                  return Instantiate_Template
                    (Pool      => Pool,
                     Node      => N,
                     Template  => Template,
                     Arguments => Arguments);
               end;

            when With_Text_Recurse =>

               --  We are supposed to create these templates only for fields,
               --  never for nodes.

               raise Program_Error;
         end case;
      end Unparse_Node;

      -------------------
      -- Unparse_Field --
      -------------------

      procedure Unparse_Field
        (Node        : Lk_Node;
         Node_Config : Node_Config_Record;
         Child       : Lk_Node;
         Field_Ref   : Struct_Member_Index;
         Unparser    : Field_Unparser_Impl;
         Items       : in out Document_Vectors.Vector)
      is
         Field_Template : constant Template_Type :=
           Node_Config.Field_Configs.Element (Field_Ref);
         pragma Assert
           (Field_Template.Kind in With_Recurse | With_Text_Recurse);

         Handle_Tokens : constant Boolean :=
           Field_Template.Kind = With_Recurse;
         --  Whether we have to take care of pre/post tokens for this field.
         --  For With_Text_Recurse templates, it is Field_Template that takes
         --  care of them.

         Field_Template_Args : Template_Instantiation_Args
                                 (Field_Template.Kind);
      begin
         Field_Template_Args.With_Recurse_Doc :=
           (Document => Unparse_Node (Child),
            Node     => Child);

         if Handle_Tokens then
            Unparse_Tokens (Unparser.Pre_Tokens, Items);
         end if;

         Items.Append
           (Instantiate_Template
              (Pool      => Pool,
               Node      => Node,
               Template  => Field_Template,
               Arguments => Field_Template_Args));

         if Handle_Tokens then
            Unparse_Tokens (Unparser.Post_Tokens, Items);
         end if;
      end Unparse_Field;

   begin
      if Config.Value = null then
         raise Precondition_Failure with "null unparsing configuration";
      elsif Config.Value.Language /= Node.Language then
         raise Precondition_Failure with "inconsistent languages";
      end if;

      declare
         Internal_Result : Document_Type := Unparse_Node (Node);
      begin
         Dump (Internal_Result, Before_Spacing_Trace);
         Insert_Required_Spacing (Pool, Internal_Result);
         Dump (Internal_Result, Final_Doc_Trace);
         return Result : constant Prettier.Document_Type :=
           To_Prettier_Document (Internal_Result)
         do
            Pool.Release;
         end return;
      end;
   end Unparse_To_Prettier;

   -----------------------
   -- Pretty_Print_Main --
   -----------------------

   procedure Pretty_Print_Main (Language : Language_Id) is
      use Ada.Command_Line;
      use GNATCOLL.Opt_Parse;

      function Convert (Arg : String) return Grammar_Rule_Ref;
      --  Convert a rule name to the corresponding grammar rule for
      --  ``Language``. Raise a ``Constraint_Error`` if ``Arg`` is not a valid
      --  rule name.

      -------------
      -- Convert --
      -------------

      function Convert (Arg : String) return Grammar_Rule_Ref is
         A : constant Text_Type := To_Text (Arg);
         R : Grammar_Rule_Ref;
      begin
         for I in Grammar_Rule_Index'First .. Last_Grammar_Rule (Language) loop
            R := From_Index (Language, I);
            if A = Format_Name (Grammar_Rule_Name (R), Lower) then
               return R;
            end if;
         end loop;
         raise Opt_Parse_Error with "invalid grammar rule name";
      end Convert;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Pretty-print a source file");

      package Output_Filename is new Parse_Option
        (Parser      => Parser,
         Short       => "-o",
         Long        => "--output",
         Arg_Type    => Unbounded_String,
         Help        => "File to write",
         Default_Val => Null_Unbounded_String);

      package Width is new Parse_Option
        (Parser      => Parser,
         Short       => "-w",
         Long        => "--width",
         Arg_Type    => Natural,
         Help        => "Line width",
         Default_Val => 79);

      package Indentation_Kind is new Parse_Enum_Option
        (Parser      => Parser,
         Short       => "-k",
         Long        => "--indentation-kind",
         Arg_Type    => Prettier.Indentation_Kind,
         Help        => "Indentation kind: spaces or tabs",
         Default_Val => Prettier.Spaces);

      package Indentation_Width is new Parse_Option
        (Parser      => Parser,
         Short       => "-i",
         Long        => "--indentation-width",
         Arg_Type    => Natural,
         Help        => "Indentation width",
         Default_Val => 4);

      package End_Of_Line is new Parse_Enum_Option
        (Parser      => Parser,
         Short       => "-e",
         Long        => "--end-of-line",
         Arg_Type    => Prettier.End_Of_Line_Kind,
         Help        => "End of line: LF, CR, CRLF",
         Default_Val => Prettier.LF);

      package Rule is new Parse_Option
        (Parser      => Parser,
         Short       => "-r",
         Long        => "--rule-name",
         Arg_Type    => Grammar_Rule_Ref,
         Help        =>
           "Grammar rule name to parse the source file to pretty-print",
         Default_Val => Default_Grammar_Rule (Language));

      package Traces is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-t",
         Long        => "--trace",
         Arg_Type    => Unbounded_String,
         Help        =>
           "LANGKIT.UNPARSING.*. sub-trace name to activate");

      package Config_Filename is new Parse_Positional_Arg
        (Parser   => Parser,
         Name     => "config-file",
         Help     => "Name of the JSON pretty-printer configuration file",
         Arg_Type => Unbounded_String);

      package Source_Filename is new Parse_Positional_Arg
        (Parser   => Parser,
         Name     => "src-file",
         Help     => "Name of the source file to pretty-print",
         Arg_Type => Unbounded_String);

      package Dump_Document is new Parse_Flag
        (Parser      => Parser,
         Short       => "-d",
         Long        => "--dump-document",
         Help        => "Dump the Prettier document in ""doc.json""");

      Config  : Unparsing_Configuration;
      Context : Lk_Context;
      Unit    : Lk_Unit;
   begin
      GNATCOLL.Traces.Parse_Config_File;
      if not Parser.Parse then
         return;
      end if;

      --  Enable all requested traces

      for N of Traces.Get loop
         Traces_To_Enable.Include
           (To_Unbounded_String ("LANGKIT.UNPARSING.")
            & To_Unbounded_String
                (Ada.Characters.Handling.To_Upper (To_String (N))));
      end loop;
      GNATCOLL.Traces.For_Each_Handle (Process_Enable_Traces'Access);

      --  Parse the configuration file and the source file to pretty-print.
      --  Abort if there is a parsing failure.

      declare
         Diagnostics : Diagnostics_Vectors.Vector;
         Filename    : constant String := To_String (Config_Filename.Get);
      begin
         Config := Load_Unparsing_Config (Language, Filename, Diagnostics);
         if Config = No_Unparsing_Configuration then
            Put_Line ("Error when loading the unparsing configuration:");
            Print (Diagnostics);
            return;
         end if;
      end;
      Context := Create_Context (Language);
      Unit := Context.Get_From_File
        (Filename => To_String (Source_Filename.Get),
         Rule     => Rule.Get);
      if Unit.Has_Diagnostics then
         Put_Line ("Cannot parse source file: aborting...");
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         Set_Exit_Status (Failure);
         return;
      end if;

      --  Unparse the tree to a Prettier document

      declare
         F         : File_Type;
         Doc       : constant Prettier.Document_Type :=
           Unparse_To_Prettier (Unit.Root, Config);
         Formatted : constant Unbounded_String :=
           Prettier.Format
             (Document => Doc,
              Options  =>
                (Width              => Width.Get,
                 Indentation        =>
                   (Indentation_Kind.Get, Indentation_Width.Get),
                 End_Of_Line        => End_Of_Line.Get,
                 Indentation_Offset => 0));
      begin
         --  If requested, dump it as a JSON file

         if Dump_Document.Get then
            Create (F, Name => "doc.json");
            Put_Line (F, Prettier.Json.Serialize (Doc));
            Close (F);
         end if;

         --  Finally, write the formatted source code on the standard output

         if Length (Output_Filename.Get) > 0 then
            Create (F, Name => To_String (Output_Filename.Get));
            Put_Line (F, Formatted);
            Close (F);
         else
            Put_Line (Formatted);
         end if;
      end;
   end Pretty_Print_Main;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Unparsing_Configuration) is
   begin
      if Self.Value /= null then
         Self.Value.Ref_Count := Self.Value.Ref_Count + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Unparsing_Configuration) is
   begin
      if Self.Value /= null then
         Self.Value.Ref_Count := Self.Value.Ref_Count - 1;
         if Self.Value.Ref_Count = 0 then
            for Cur in Self.Value.Node_Configs.Iterate loop
               declare
                  Node_Config : Node_Config_Access renames
                    Self.Value.Node_Configs.Reference (Cur);
               begin
                  Free (Node_Config);
               end;
            end loop;
            Self.Value.Pool.Release;
            Free (Self.Value);
         end if;
      end if;
   end Finalize;

end Langkit_Support.Generic_API.Unparsing;
