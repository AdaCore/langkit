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
with Prettier_Ada.Documents.Json;

with Langkit_Support.Errors;         use Langkit_Support.Errors;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Unparsing;
use Langkit_Support.Internal.Unparsing;
with Langkit_Support.Prettier_Utils; use Langkit_Support.Prettier_Utils;
with Langkit_Support.Symbols;        use Langkit_Support.Symbols;

package body Langkit_Support.Generic_API.Unparsing is

   function Hash (Self : Struct_Member_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   function Hash (Self : Type_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   package Field_Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Struct_Member_Index,
      Element_Type    => Document_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Node_Config_Record is limited record
      Node_Template : Document_Type;
      --  Template to decorate the unparsing of the whole node

      Field_Configs : Field_Config_Maps.Map;
      --  For each non-null syntax field in this node, template to decorate the
      --  unparsing of the field.

      List_Sep : Document_Type;
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

   function Instantiate_Template
     (Pool             : in out Document_Pool;
      Filler, Template : Document_Type) return Document_Type;
   --  Instantiate the given template, i.e. create a copy of it, replacing
   --  "recurse" documents with Filler.

   ----------------------
   -- Required_Spacing --
   ----------------------

   function Required_Spacing (Left, Right : Token_Kind_Ref) return Spacing_Kind
   is
   begin
      if Left = No_Token_Kind_Ref then
         return None;
      elsif Language (Left) /= Language (Right) then
         raise Precondition_Failure with
           "inconsistent languages for requested token kinds";
      end if;

      declare
         Id : constant Language_Descriptor_Access := +Language (Left);
         LK : constant Token_Kind_Index := To_Index (Left);

         function Family (Kind : Token_Kind_Index) return Token_Family_Index
         is (Id.Token_Kinds (Kind).Family);
      begin
         --  If a newline is required after Left, we do not even need to check
         --  what Right is.

         if Id.Unparsers.Token_Newlines (LK) then
            return Newline;

         --  Otherwise, check if at least a space is required between Left and
         --  Right.

         elsif Id.Unparsers.Token_Spacings
                 (Family (LK), Family (To_Index (Right)))
         then
            return Whitespace;

         else
            return None;
         end if;
      end;
   end Required_Spacing;

   --------------------------
   -- Unparse_To_Fragments --
   --------------------------

   procedure Unparse_To_Fragments
     (Node : Lk_Node; Fragments : out Unparsing_Fragment_Vectors.Vector)
   is
      Id        : constant Language_Id := Node.Language;
      Desc      : constant Language_Descriptor_Access := +Id;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      procedure Append
        (Token : Token_Unparser; Is_List_Sep : Boolean := False);
      procedure Append (Tokens : Token_Sequence);

      ------------
      -- Append --
      ------------

      procedure Append (Token : Token_Unparser; Is_List_Sep : Boolean := False)
      is
         Kind : constant Token_Kind_Ref := From_Index (Id, Token.Kind);
         Text : constant Unbounded_Text_Type :=
           To_Unbounded_Text (Token.Text.all);

         Fragment : constant Unparsing_Fragment :=
           (if Is_List_Sep
            then (List_Separator_Fragment, Kind, Text)
            else (Token_Fragment, Kind, Text));
      begin
         Fragments.Append (Fragment);
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append (Tokens : Token_Sequence) is
      begin
         for T of Tokens.all loop
            Append (T);
         end loop;
      end Append;

      Node_Type     : constant Type_Ref := Type_Of (Node);
      Node_Unparser : Node_Unparser_Impl renames
        Unparsers.Node_Unparsers (To_Index (Node_Type)).all;
   begin
      Fragments.Clear;

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

                  --  Then append fragments for the field itself, if present.
                  --  Note that unparsing tables (Empty_List_Is_Absent
                  --  component) determine whether a non-null child with no
                  --  children of its own must be treated as absent.

                  if not Child.Is_Null
                     and then (not Field_Unparser.Empty_List_Is_Absent
                               or else Child.Children_Count > 0)
                  then
                     Append (Field_Unparser.Pre_Tokens);
                     Fragments.Append
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
                     Fragments.Append
                       ((Kind => List_Separator_Fragment,
                         Token_Kind => No_Token_Kind_Ref,
                         Token_Text => To_Unbounded_Text ("")));
                  else
                     Append (Node_Unparser.Separator, Is_List_Sep => True);
                  end if;
               end if;

               Fragments.Append
                 ((Kind        => List_Child_Fragment,
                   Node        => Node.Child (I),
                   Child_Index => I));
            end loop;

         when Token =>
            Fragments.Append
              ((Kind       => Token_Fragment,
                Token_Kind => Token_Node_Kind (Node_Type),
                Token_Text => To_Unbounded_Text (Node.Text)));
      end case;
   end Unparse_To_Fragments;

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

      function To_Document
        (JSON         : JSON_Value;
         Error_Prefix : String) return Document_Type;
      --  Parse a JSON-encoded document. Raise an Invalid_Input exception if
      --  the JSON encoding is invalid.

      function To_Template
        (JSON         : JSON_Value;
         Error_Prefix : String) return Document_Type;
      --  Parse a JSON-encoded document and check that it is a well-formed
      --  template. Raise an Invalid_Input exception in case of error.

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

      -----------------
      -- To_Document --
      -----------------

      function To_Document
        (JSON         : JSON_Value;
         Error_Prefix : String) return Document_Type is
      begin
         case JSON.Kind is
         when JSON_Array_Type =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for D of JSON_Array'(JSON.Get) loop
                  Items.Append (To_Document (D, Error_Prefix));
               end loop;
               return Pool.Create_List (Items);
            end;

         when JSON_String_Type =>
            declare
               Value : constant String := JSON.Get;
            begin
               if Value = "recurse" then
                  return Pool.Create_Recurse;
               elsif Value = "line" then
                  return Pool.Create_Line;
               elsif Value = "hardline" then
                  return Pool.Create_Hard_Line;
               elsif Value = "softline" then
                  return Pool.Create_Soft_Line;
               elsif Value = "whitespace" then
                  return Pool.Create_Whitespace;
               else
                  raise Invalid_Input with
                    Error_Prefix & ": invalid string template document kind: "
                    & Value;
               end if;
            end;

         when JSON_Object_Type =>
            if not JSON.Has_Field ("kind") then
               raise Invalid_Input with
                 Error_Prefix & ": missing ""kind"" key";
            elsif JSON.Get ("kind").Kind /= JSON_String_Type then
               raise Invalid_Input with
                 Error_Prefix & ": invalid ""kind"": "
                 & JSON.Get ("kind").Kind'Image;
            end if;

            declare
               Kind : constant String := JSON.Get ("kind");
            begin
               if Kind = "whitespace" then
                  if not JSON.Has_Field ("length") then
                     raise Invalid_Input with
                       Error_Prefix & ": missing ""length"" key";
                  end if;
                  declare
                     Length : constant JSON_Value := JSON.Get ("length");
                  begin
                     if Length.Kind /= JSON_Int_Type then
                        raise Invalid_Input with
                          Error_Prefix & ": invalid whitespace length: "
                          & Length.Kind'Image;
                     end if;
                     return Pool.Create_Whitespace (Length.Get);
                  end;

               elsif Kind = "indent" then
                  if not JSON.Has_Field ("contents") then
                     raise Invalid_Input with
                       Error_Prefix & ": missing ""contents"" key for indent";
                  end if;
                  return Pool.Create_Indent
                    (To_Document (JSON.Get ("contents"), Error_Prefix));

               else
                  raise Invalid_Input with
                    Error_Prefix & ": invalid template document kind: " & Kind;
               end if;
            end;

         when others =>
            raise Invalid_Input with
              Error_Prefix & ": invalid template JSON node: "
              & JSON.Kind'Image;
         end case;
      end To_Document;

      -----------------
      -- To_Template --
      -----------------

      function To_Template
        (JSON         : JSON_Value;
         Error_Prefix : String) return Document_Type is
      begin
         return Result : constant Document_Type :=
           To_Document (JSON, Error_Prefix)
         do
            if not Is_Correct_Template (Result) then
               raise Invalid_Input with
                 Error_Prefix & ": invalid ""recurse"" structure";
            end if;
         end return;
      end To_Template;

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
            Member       : constant Struct_Member_Index :=
              To_Struct_Member_Index (Name, Node);
            Error_Prefix : constant String :=
              "template for "
              & Field_Image (From_Index (Language, Member), Node);
         begin
            Configs.Insert (Member, To_Template (Value, Error_Prefix));
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
           Filename & Format_Parsing_Error (JSON_Result.Error);
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
            Key    : constant Type_Index := To_Type_Index (Name);
            Node   : constant Type_Ref := From_Index (Language, Key);
            Config : constant Node_Config_Access := new Node_Config_Record'
              (Node_Template =>
                 (if Value.Has_Field ("node")
                  then To_Template
                    (Value.Get ("node"),
                     """node"" template for " & Node_Type_Image (Node))
                  else Pool.Create_Recurse),
               Field_Configs => <>,
               List_Sep      => null);
         begin
            Result.Node_Configs.Insert (Key, Config);

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
               Config.List_Sep :=
                 To_Template
                   (Value.Get ("sep"),
                    """sep"" template for " & Node_Type_Image (Node));
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

            if Node_Config.Node_Template = null then
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
                     Field_Template : Document_Type;
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

                     if Field_Template = null then
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

            if Node_Config.List_Sep = null then
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
     (Pool             : in out Document_Pool;
      Filler, Template : Document_Type) return Document_Type is
   begin
      case Template.Kind is
         when Recurse =>
            return Filler;

         when Token =>
            return Pool.Create_Token
              (Template.Token_Kind, Template.Token_Text);

         when Line =>
            return Pool.Create_Line;

         when Hard_Line =>
            return Pool.Create_Hard_Line;

         when Soft_Line =>
            return Pool.Create_Soft_Line;

         when Whitespace =>
            return Pool.Create_Whitespace (Template.Whitespace_Length);

         when List =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for I in 1 .. Template.List_Documents.Last_Index loop
                  Items.Append
                    (Instantiate_Template
                       (Pool, Filler, Template.List_Documents.Element (I)));
               end loop;
               return Pool.Create_List (Items);
            end;

         when Indent =>
            return Pool.Create_Indent
              (Instantiate_Template (Pool, Filler, Template.Indent_Document));
      end case;
   end Instantiate_Template;

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
         Fragments   : Unparsing_Fragment_Vectors.Vector;
      begin
         Unparse_To_Fragments (N, Fragments);
         for F of Fragments loop
            case F.Kind is
               when Token_Fragment | List_Separator_Fragment =>
                  declare
                     Token : Document_Type :=
                       (if F.Token_Kind = No_Token_Kind_Ref
                        then Pool.Create_Empty_List
                        else Pool.Create_Token (F.Token_Kind, F.Token_Text));
                  begin
                     if F.Kind = List_Separator_Fragment then
                        Token := Instantiate_Template
                          (Pool, Token, Node_Config.List_Sep);
                     end if;
                     Items.Append (Token);
                  end;

               when Field_Fragment =>
                  declare
                     Field          : constant Document_Type :=
                       Unparse_Node (F.Node);
                     Field_Template : constant Document_Type :=
                       Node_Config.Field_Configs.Element (To_Index (F.Field));
                  begin
                     Items.Append
                       (Instantiate_Template (Pool, Field, Field_Template));
                  end;

               when List_Child_Fragment =>
                  Items.Append (Unparse_Node (F.Node));
            end case;
         end loop;

         return Instantiate_Template
           (Pool, Pool.Create_List (Items), Node_Config.Node_Template);
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
         return Result : constant Prettier_Ada.Documents.Document_Type :=
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
         Arg_Type    => Prettier_Ada.Documents.Indentation_Kind,
         Help        => "Indentation kind: spaces or tabs",
         Default_Val => Prettier_Ada.Documents.Spaces);

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
         Arg_Type    => Prettier_Ada.Documents.End_Of_Line_Kind,
         Help        => "End of line: LF, CR, CRLF",
         Default_Val => Prettier_Ada.Documents.LF);

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
      Unit := Context.Get_From_File (To_String (Source_Filename.Get));
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
         Doc       : constant Prettier_Ada.Documents.Document_Type :=
           Unparse_To_Prettier (Unit.Root, Config);
         Formatted : constant Unbounded_String :=
           Prettier_Ada.Documents.Format
             (Document => Doc,
              Options  =>
                (Width => Width.Get,
                 Indentation => (Indentation_Kind.Get, Indentation_Width.Get),
                 End_Of_Line => End_Of_Line.Get));
      begin
         --  If requested, dump it as a JSON file

         if Dump_Document.Get then
            Create (F, Name => "doc.json");
            Put_Line (F, Prettier_Ada.Documents.Json.Serialize (Doc));
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
