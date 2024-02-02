--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;  use GNATCOLL.VFS;
with Prettier_Ada.Documents.Builders;
with Prettier_Ada.Documents.Json;

with Langkit_Support.Errors;         use Langkit_Support.Errors;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Unparsing;
use Langkit_Support.Internal.Unparsing;
with Langkit_Support.Prettier_Utils; use Langkit_Support.Prettier_Utils;
with Langkit_Support.Symbols;        use Langkit_Support.Symbols;

package body Langkit_Support.Generic_API.Unparsing is

   use type Ada.Containers.Count_Type;

   function Is_Field_Present
     (Field          : Lk_Node;
      Field_Unparser : Field_Unparser_Impl) return Boolean;
   --  Return whether, according to ``Field_Unparser``, the field ``Field``
   --  must be considered as present for unparsing.

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

   type Template_Instantiation_Args (Kind : Some_Template_Kind) is record
      case Kind is
         when With_Recurse =>
            With_Recurse_Doc : Document_Type;
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
                     Append (Field_Unparser.Pre_Tokens);
                     Process.all
                       ((Kind  => Field_Fragment,
                         Node  => Child,
                         Field => From_Index (Id, Field_Unparser.Member)));
                     Append (Field_Unparser.Post_Tokens);
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

   ---------------------------
   -- Load_Unparsing_Config --
   ---------------------------

   function Load_Unparsing_Config
     (Language : Language_Id; Filename : String) return Unparsing_Configuration
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

      type Template_Parsing_State_Kind is (Initial, Recurse_Found);
      type Template_Parsing_State
        (Kind : Template_Parsing_State_Kind := Initial)
      is null record;

      Initial_State : constant Template_Parsing_State := (Kind => Initial);

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

      function Parse_Template
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Template_Type;
      --  Parse a JSON-encoded temlate document. Raise an Invalid_Input
      --  exception if the JSON encoding is invalid or if the template is
      --  ill-formed.

      function Parse_Template_Helper
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Document_Type;
      --  Helper for ``Parse_Template``. Implement the recursive part of
      --  templates parsing: ``Parse_Template`` takes care of the post-parsing
      --  validation.

      procedure Process_Recurse (Context : in out Template_Parsing_Context);
      --  Record in ``Context.State``` that a "recurse" or "recurse_flatten"
      --  template item has been found. This raises an error if one has already
      --  been found.

      procedure Abort_Parsing
        (Context : Template_Parsing_Context; Message : String)
      with No_Return;
      --  Raise an Invalid_Input exception with the given message,
      --  contextualized.

      procedure Load_Field_Configs
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Configs : out Field_Config_Maps.Map);
      --  Assuming that JSON is the "fields" configuration for Node, parse its
      --  field configurations and set Configs accordingly.

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
            raise Invalid_Input with "invalid node name: " & Name;
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
            raise Invalid_Input with
              "invalid field for " & Node_Type_Image (Node) & ": " & Name;
         elsif not Is_Field (M) then
            raise Invalid_Input with
              Name & " is not a syntax field for " & Node_Type_Image (Node);
         elsif Is_Null_For (M, Node) then
            raise Invalid_Input with
              Name & " is a null field for " & Node_Type_Image (Node);
         else
            return To_Index (M);
         end if;
      end To_Struct_Member_Index;

      --------------------
      -- Parse_Template --
      --------------------

      function Parse_Template
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Template_Type
      is
         Root : constant Document_Type :=
           Parse_Template_Helper (JSON, Context);
      begin
         case Context.State.Kind is
            when Initial =>
               Abort_Parsing (Context, "recursion is missing");

            when Recurse_Found =>
               return (Kind => With_Recurse, Root => Root);
         end case;
      end Parse_Template;

      ---------------------------
      -- Parse_Template_Helper --
      ---------------------------

      function Parse_Template_Helper
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Document_Type is
      begin
         case JSON.Kind is
         when JSON_Array_Type =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for D of JSON_Array'(JSON.Get) loop
                  Items.Append (Parse_Template_Helper (D, Context));
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
                          (JSON.Get ("contents"), Context));
                  end;

               elsif Kind in
                  "dedent" | "dedentToRoot" | "markAsRoot"
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
                                  else raise Program_Error),
                     Contents => Parse_Template_Helper
                                   (JSON.Get ("contents"), Context));

               elsif Kind = "fill" then
                  declare
                     Document : Document_Type;
                  begin
                     if not JSON.Has_Field ("document") then
                        Abort_Parsing
                          (Context, "missing ""document"" key for fill");
                     end if;
                     Document :=
                       Parse_Template_Helper (JSON.Get ("document"), Context);

                     return Pool.Create_Fill (Document);
                  end;

               elsif Kind = "group" then
                  declare
                     Document : Document_Type;
                     Options  : Prettier.Builders.Group_Options_Type :=
                       Prettier.Builders.No_Group_Options;

                     Should_Break : JSON_Value;
                  begin
                     if not JSON.Has_Field ("document") then
                        Abort_Parsing
                          (Context, "missing ""document"" key for group");
                     end if;
                     Document :=
                       Parse_Template_Helper (JSON.Get ("document"), Context);

                     if JSON.Has_Field ("shouldBreak") then
                        Should_Break := JSON.Get ("shouldBreak");
                        if Should_Break.Kind /= JSON_Boolean_Type then
                           Abort_Parsing
                             (Context,
                              "invalid group shouldBreak: "
                              & Should_Break.Kind'Image);
                        end if;
                        Options.Should_Break := Should_Break.Get;
                     end if;

                     --  TODO??? (eng/libadalang/langkit#727) Handle the group
                     --  id.

                     return Pool.Create_Group (Document, Options);
                  end;

               elsif Kind = "ifBreak" then
                  declare
                     Contents      : Document_Type;
                     Flat_Contents : Document_Type;

                     Contents_Context : Template_Parsing_Context := Context;
                     Flat_Context     : Template_Parsing_Context := Context;
                  begin
                     if not JSON.Has_Field ("breakContents") then
                        Abort_Parsing
                          (Context,
                           "missing ""breakContents"" key for ifBreak");
                     end if;

                     Contents :=
                       Parse_Template_Helper
                         (JSON.Get ("breakContents"), Contents_Context);

                     Flat_Contents :=
                       (if JSON.Has_Field ("flatContents")
                        then Parse_Template_Helper
                               (JSON.Get ("flatContents"), Flat_Context)
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

                     --  TODO??? (eng/libadalang/langkit#727) Handle the group
                     --  id.

                     return Pool.Create_If_Break (Contents, Flat_Contents);
                  end;

               elsif Kind = "indent" then
                  if not JSON.Has_Field ("contents") then
                     Abort_Parsing
                       (Context, "missing ""contents"" key for indent");
                  end if;
                  return Pool.Create_Indent
                    (Parse_Template_Helper (JSON.Get ("contents"), Context));

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
            when Initial =>
               Context.State := (Kind => Recurse_Found);
            when Recurse_Found =>
               Abort_Parsing (Context, "too many recursions");
         end case;
      end Process_Recurse;

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
         raise Invalid_Input with Prefix & ": " & Message;
      end Abort_Parsing;

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
               State => Initial_State,
               Node  => Node,
               Field => From_Index (Language, Member));
         begin
            Configs.Insert (Member, Parse_Template (Value, Context));
         end Process;

      begin
         Configs.Clear;
         JSON.Map_JSON_Object (Process'Access);
      end Load_Field_Configs;

      use type GNAT.Strings.String_Access;

      --  First, parse the JSON document

      JSON_Text   : GNAT.Strings.String_Access := Create (+Filename).Read_File;
      JSON_Result : Read_Result;
      JSON        : JSON_Value;
   begin
      if JSON_Text = null then
         raise Invalid_Input with "cannot read " & Filename;
      end if;
      JSON_Result := Read (JSON_Text.all);
      GNAT.Strings.Free (JSON_Text);

      if JSON_Result.Success then
         JSON := JSON_Result.Value;
      else
         raise Invalid_Input with
           Filename & ":" & Format_Parsing_Error (JSON_Result.Error);
      end if;

      --  Then load the unparsing configuration from it. Require a
      --  "node_configs" key.

      Result.Ref_Count := 1;
      Result.Language := Language;

      if not JSON.Has_Field ("node_configs") then
         raise Invalid_Input with "missing ""node_configs"" key";
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
            Context : Template_Parsing_Context :=
              (Kind  => Node_Template,
               Node  => Node,
               State => Initial_State);
            Config  : constant Node_Config_Access := new Node_Config_Record'
              (Node_Template => No_Template,
               Field_Configs => <>,
               List_Sep      => No_Template);
         begin
            Result.Node_Configs.Insert (Key, Config);

            Config.Node_Template :=
              (if Value.Has_Field ("node")
               then Parse_Template (Value.Get ("node"), Context)
               else Pool.Create_Recurse);

            if Value.Has_Field ("fields") then
               Load_Field_Configs
                 (Node, Value.Get ("fields"), Config.Field_Configs);
            end if;

            if Value.Has_Field ("sep") then
               if not Is_List_Node (Node) then
                  raise Invalid_Input with
                    Name & " is not a list node, invalid ""sep"""
                    & " configuration";
               end if;
               declare
                  Context : Template_Parsing_Context :=
                    (Kind  => Sep_Template,
                     Node  => Node,
                     State => Initial_State);
               begin
                  Config.List_Sep :=
                    Parse_Template (Value.Get ("sep"), Context);
               end;
            end if;
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
         Destroy (Symbols);
         raise;
   end Load_Unparsing_Config;

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
                 (Pool, Node, Template.Align_Contents, Arguments),
               Node);

         when Break_Parent =>
            return Pool.Create_Break_Parent;

         when Fill =>
            return Pool.Create_Fill
              (Instantiate_Template_Helper
                 (Pool, Node, Template.Fill_Document, Arguments),
               Node);

         when Group =>
            return Pool.Create_Group
              (Instantiate_Template_Helper
                 (Pool, Node, Template.Group_Document, Arguments),
               Template.Group_Options,
               Node);

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

         when Indent =>
            return Pool.Create_Indent
              (Instantiate_Template_Helper
                 (Pool, Node, Template.Indent_Document, Arguments),
               Node);

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
               return Pool.Create_List (Items, Node);
            end;

         when Literal_Line =>
            return Pool.Create_Literal_Line;

         when Recurse =>
            return Arguments.With_Recurse_Doc;

         when Recurse_Flatten =>
            return Result : Document_Type := Arguments.With_Recurse_Doc do

               --  As long as Result is a document we can flatten and that was
               --  created by a node that passes the flattening guard, unwrap
               --  it.

               while not Result.Node.Is_Null
                     and then Node_Matches
                                (Result.Node, Template.Recurse_Flatten_Types)
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

      function Unparse_Node (N : Lk_Node) return Document_Type;
      --  Using the unparsing configuration for N, unparse it to a Prettier
      --  document.

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
                              With_Recurse_Doc => Token);
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
                  declare
                     Field    : constant Document_Type :=
                       Unparse_Node (F.Node);
                     Template : constant Template_Type :=
                       Node_Config.Field_Configs.Element (To_Index (F.Field));
                     Args     : constant Template_Instantiation_Args :=
                       (Kind => With_Recurse, With_Recurse_Doc => Field);
                  begin
                     pragma Assert (Template.Kind = With_Recurse);
                     Items.Append
                       (Instantiate_Template
                          (Pool      => Pool,
                           Node      => N,
                           Template  => Template,
                           Arguments => Args));
                  end;

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
                  Arguments => (With_Recurse, Pool.Create_List (Items)));
         end case;
      end Unparse_Node;

   begin
      if Config.Value = null then
         raise Precondition_Failure with "null unparsing configuration";
      elsif Config.Value.Language /= Node.Language then
         raise Precondition_Failure with "inconsistent languages";
      end if;

      declare
         Internal_Result : Document_Type := Unparse_Node (Node);
      begin
         Insert_Required_Spacing (Pool, Internal_Result);
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
      if not Parser.Parse then
         return;
      end if;

      --  Parse the configuration file and the source file to pretty-print.
      --  Abort if there is a parsing failure.

      begin
         Config :=
           Load_Unparsing_Config (Language, To_String (Config_Filename.Get));
      exception
         when Exc : Invalid_Input =>
            Put_Line ("Error when loading the unparsing configuration:");
            Put_Line (Exception_Message (Exc));
            return;
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
                (Width => Width.Get,
                 Indentation => (Indentation_Kind.Get, Indentation_Width.Get),
                 End_Of_Line => End_Of_Line.Get));
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
      procedure Free is new Ada.Unchecked_Deallocation
        (Unparsing_Configuration_Record,
         Unparsing_Configuration_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Config_Record, Node_Config_Access);
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
