--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Names;  use Langkit_Support.Names;
with Langkit_Support.Slocs;  use Langkit_Support.Slocs;

package body Langkit_Support.Unparsing_Config is

   procedure Expand_Regular_Node_Template
     (Pool     : in out Document_Pool;
      Node     : Type_Ref;
      Template : in out Template_Type);
   --  If ``Node`` has a specific syntax (i.e. it is a regular node) and if
   --  ``Template.Kind`` is ``With_Recurse``, turn it into the corresponding
   --  ``With_Recurse_Field`` template.

   Linear_Template_For_Join : constant Linear_Template_Vectors.Vector :=
     [(Kind => Recurse_Left), (Kind => Recurse_Right)];
   --  Linear template for table row join templates

   ----------------------------------
   -- Expand_Regular_Node_Template --
   ----------------------------------

   procedure Expand_Regular_Node_Template
     (Pool     : in out Document_Pool;
      Node     : Type_Ref;
      Template : in out Template_Type)
   is
      Recurse_Doc : Document_Type;
      --  Document to used to replace "recurse" nodes

      procedure Replace (Self : in out Document_Type);
      --  Replace "recurse" documents in ``Self`` with ``Recurse_Doc``

      -------------
      -- Replace --
      -------------

      procedure Replace (Self : in out Document_Type) is
      begin
         if Self = null then
            return;
         end if;

         case Self.Kind is
            when Align =>
               Replace (Self.Align_Contents);

            when Break_Parent
               | Empty_Table_Separator
               | Expected_Line_Breaks
               | Expected_Whitespaces
            =>
               null;

            when Fill =>
               Replace (Self.Fill_Document);

            when Flush_Line_Breaks =>
               null;

            when Group =>
               Replace (Self.Group_Document);

            when Hard_Line | Hard_Line_Without_Break_Parent =>
               null;

            when If_Break =>
               Replace (Self.If_Break_Contents);
               Replace (Self.If_Break_Flat_Contents);

            when If_Empty =>
               Replace (Self.If_Empty_Then);
               Replace (Self.If_Empty_Else);

            when If_Kind =>
               Replace (Self.If_Kind_Default);
               for I in 1 .. Self.If_Kind_Matchers.Last_Index loop
                  Replace (Self.If_Kind_Matchers (I).Document);
               end loop;
               Replace (Self.If_Kind_Absent);

            when Indent =>
               Replace (Self.Indent_Document);

            when Line =>
               null;

            when List =>
               for I in 1 .. Self.List_Documents.Last_Index loop
                  Replace (Self.List_Documents (I));
               end loop;

            when Literal_Line =>
               null;

            when Recurse =>
               Self := Recurse_Doc;

            when Recurse_Field | Recurse_Flatten | Recurse_Left | Recurse_Right
            =>
               raise Program_Error;

            when Soft_Line =>
               null;

            when Table =>
               for I in 1 .. Self.Table_Rows.Last_Index loop
                  Replace (Self.Table_Rows (I));
               end loop;

            when Table_Separator | Token | Trim | Whitespace =>
               null;
         end case;
      end Replace;

   begin
      --  There is nothing we can do if ``Template`` is not a "recurse"
      --  template.

      if Template.Kind /= With_Recurse then
         return;
      end if;

      --  It is illegal to use "recurse_field" nodes for abstract or
      --  non-regular nodes.

      declare
         Unparser : constant Node_Unparser := Node_Unparser_For (Node);
      begin
         if Unparser = null or else Unparser.Kind /= Regular then
            return;
         end if;
      end;

      --  We are about to mutate the given template, including some nodes
      --  potentially shared with other templates: for correctness, we must
      --  work on a fresh copy.

      Template.Root := Deep_Copy (Pool, Template.Root);

      --  Create the text/recurse_field sequence that must replace "recurse"
      --  nodes.

      declare
         Doc_Items : Document_Vectors.Vector;
         Doc_Item  : Document_Type;
      begin
         for LT_Item of Linear_Template (Node) loop
            case LT_Item.Kind is
               when Token_Item =>
                  Doc_Item :=
                    Pool.Create_Token
                      (LT_Item.Token_Kind, LT_Item.Token_Text);
               when Field_Item =>
                  Doc_Item :=
                    Pool.Create_Recurse_Field
                      (LT_Item.Field_Ref, LT_Item.Field_Position);
               when Recurse_Item =>
                  Doc_Item := Pool.Create_Recurse;
               when others =>
                  raise Program_Error;
            end case;
            Doc_Items.Append (Doc_Item);
         end loop;
         Recurse_Doc := Pool.Create_List (Doc_Items);
      end;

      --  Finally do the replacement and change the type of template

      Replace (Template.Root);
      Template :=
        (Kind    => With_Recurse_Field,
         Root    => Template.Root,
         Symbols => Template.Symbols);
   end Expand_Regular_Node_Template;

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
         when Recurse_Left =>
            return "recurse_left";
         when Recurse_Right =>
            return "recurse_right";
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
         when Recurse_Item | Recurse_Left | Recurse_Right =>
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
      Id : constant Language_Id := Node.Language;

      Result : Linear_Template_Vectors.Vector;

      Node_Unparser : Node_Unparser_Impl renames
        Node_Unparser_For (Node).all;
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
      Id : constant Language_Id := Node.Language;

      Node_Unparser : Node_Unparser_Impl renames Node_Unparser_For (Node).all;
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

   ------------------
   -- Fragment_For --
   ------------------

   function Fragment_For
     (Id       : Language_Id;
      Token    : Token_Unparser;
      List_Sep : Any_List_Sep_Template_Kind := None) return Unparsing_Fragment
   is
      Kind : constant Token_Kind_Ref := From_Index (Id, Token.Kind);
      Text : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Token.Text.all);
   begin
      return
        (case List_Sep is
         when None                   => (Token_Fragment, Kind, Text),
         when List_Sep_Template_Kind =>
           (List_Separator_Fragment, Kind, Text, List_Sep));
   end Fragment_For;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Unparsing_Configuration_Access) is
   begin
      for Cur in Self.Node_Configs.Iterate loop
         declare
            Node_Config : Node_Config_Access renames
              Self.Node_Configs.Reference (Cur);
         begin
            Free (Node_Config);
         end;
      end loop;
      Self.Pool.Release;
      if Self.Symbols /= null then
         Destroy (Self.Symbols);
      end if;
      Free (Self);
   end Release;

   ---------------------------------------
   -- Load_Unparsing_Config_From_Buffer --
   ---------------------------------------

   function Load_Unparsing_Config_From_Buffer
     (Language        : Language_Id;
      Buffer          : String;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      Check_All_Nodes : Boolean)
      return Unparsing_Configuration_Access
   is
      Desc      : constant Language_Descriptor_Access := +Language;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      ------------------
      -- JSON helpers --
      ------------------

      package Node_JSON_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Type_Index,
         Element_Type    => JSON_Value,
         Hash            => Hash,
         Equivalent_Keys => "=");

      procedure Add_Node_Entries
        (JSON : JSON_Value; Map : in out Node_JSON_Maps.Map);
      --  Check that ``JSON`` (assumed to be an object) has a ``node_configs``
      --  key and that is is an object, then update ``Map`` to include its
      --  entries (string keys are converted to the  corresponding node types).

      package Field_JSON_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Struct_Member_Index,
         Element_Type    => JSON_Value,
         Hash            => Hash,
         Equivalent_Keys => "=");

      function Field_Entries
        (Node : Type_Ref; JSON : JSON_Value) return Field_JSON_Maps.Map;
      --  Assuming that ``JSON`` is an object whose keys are field type names,
      --  compute the corresponding map where keys are converted to struct
      --  member references.

      --  Create a map so that we can lookup nodes/fields by name

      Symbols : constant Symbol_Table := Create_Symbol_Table;
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

      function To_Predicate_Ref
        (Name : String; Node : Type_Ref) return Struct_Member_Ref;
      --  Look for the member of ``Node`` that matches ``Name``.
      --
      --  Return it if it matches a predicate (property that takes no argument
      --  and returns a boolean). Raise an ``Invalid_Input`` exception
      --  otherwise.

      function Node_Type_Image (Node : Type_Ref) return String
      is (Image (Format_Name (Node_Type_Name (Node), Camel)));
      --  Return the expected name for the given Node

      function Field_Image
        (Member : Struct_Member_Ref; Node : Type_Ref) return String
      is (Node_Type_Image (Node)
          & "." & Image (Format_Name (Member_Name (Member), Lower)));
      --  Return the expected name for the given Member in the given Node

      type Template_Parsing_State_Kind is
        (Simple_Recurse,
         Recurse_Field,
         Recurse_In_Field,
         Recurse_In_Join);
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
      --
      --  * Recurse_In_Join templates, whose linearization must yield the
      --    sequence: ["recurse_left", "recurse_right"].

      type Template_Parsing_State
        (Kind : Template_Parsing_State_Kind := Simple_Recurse)
      is record
         case Kind is
            when Simple_Recurse =>
               Recurse_Found : Boolean;
               --  Whether template parsing has found the "recurse" node
               --  expected for the current branch.

            when Recurse_Field | Recurse_In_Field | Recurse_In_Join =>
               Linear_Template : Linear_Template_Vectors.Vector;
               --  Sequence of tokens/fields that the parsed template is
               --  supposed to yield once instantiated/formatted.

               Linear_Position : Positive;
               --  Position in Linear_Template of the next item to be found in
               --  the template to parse.
         end case;
      end record;

      type Template_Parsing_Context_Kind is
        (Node_Template, Field_Template, Sep_Template, Join_Template);
      --  Indicate which kind of template we are parsing:
      --
      --  ``Node_Template``: a "node" template.
      --
      --  ``Field_Template``: a template in the "fields" mapping.
      --
      --  ``Sep_Template``: a "sep"/"leading_sep"/"trailing_sep" template.
      --
      --  ``Join_Template``: template for a table row join.

      type Template_Parsing_Context (Kind : Template_Parsing_Context_Kind) is
      record
         Node : Type_Ref;
         --  Node for which we parse this template

         State : Template_Parsing_State;
         --  Keep track of the parsing state for this template; used for
         --  validation.

         Symbols : Symbol_Parsing_Maps.Map;
         --  Symbols referenced/declared in this template. Initialize it
         --  with the symbols that can be referenced from the template, and
         --  read after template parsing what symbols the template
         --  declares/references.
         --
         --  TODO??? (eng/libadalang/langkit#805) For now, this is out of the
         --  parsing state, and thus it is not sensitive to control flow
         --  constructs: it is not possible to create groups with the same
         --  symbol in alternative paths. This also means that it is possible
         --  to refer to a group even in contexts where we are not guaranteed
         --  that the group will be printed. These limitations should be
         --  addressed at some point.

         case Kind is
            when Node_Template =>
               null;

            when Field_Template =>
               Field : Struct_Member_Ref;
               --  Field for which we parse this template

            when Sep_Template =>
               Sep_Kind : List_Sep_Template_Kind;
               --  Kind of separator for this template

            when Join_Template =>
               null;
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
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Document_Type;
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

      function Bubble_Up_Config
        (Context : Template_Parsing_Context;
         Kind    : Document_Kind;
         JSON    : JSON_Value) return Trivias_Bubble_Up.Config;
      --  Return the bubble up configuration for the document denoted by JSON.
      --  Kind must be the document kind for JSON.

      procedure Check_Symbols
        (Node            : Type_Ref;
         Parent_Name     : String;
         Parent_Template : Template_Type;
         Child_Name      : String;
         Child_Template  : Template_Type);
      --  Check that symbol usage for both templates is compatible. Names are
      --  used for error message formatting.

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

      procedure Abort_Parsing (Message : String) with No_Return;
      --  Append an item to ``Diagnostics`` and raise an Invalid_Input
      --  exception.

      Result : Unparsing_Configuration_Access :=
        new Unparsing_Configuration_Record;
      Pool   : Document_Pool renames Result.Pool;

      ----------------------
      -- Add_Node_Entries --
      ----------------------

      procedure Add_Node_Entries
        (JSON : JSON_Value; Map : in out Node_JSON_Maps.Map)
      is
         Node_Configs : JSON_Value;

         procedure Process (Name : String; Value : JSON_Value);
         --  Add Value to Map

         -------------
         -- Process --
         -------------

         procedure Process (Name : String; Value : JSON_Value) is
            Key : constant Type_Index := To_Type_Index (Name);
         begin
            Map.Include (Key, Value);
         end Process;
      begin
         --  Check that JSON has a "node_configs" entry and that its value is
         --  an object.

         if not JSON.Has_Field ("node_configs") then
            Abort_Parsing ("missing ""node_configs"" key");
         end if;
         Node_Configs := JSON.Get ("node_configs");
         if Node_Configs.Kind /= JSON_Object_Type then
            Abort_Parsing ("invalid ""node_configs"" entry: object expected");
         end if;

         --  Iterate on that object to update Map

         Node_Configs.Map_JSON_Object (Process'Access);
      end Add_Node_Entries;

      -------------------
      -- Field_Entries --
      -------------------

      function Field_Entries
        (Node : Type_Ref; JSON : JSON_Value) return Field_JSON_Maps.Map
      is
         Result : Field_JSON_Maps.Map;

         procedure Process (Name : String; Value : JSON_Value);
         --  Add Value to Result

         procedure Process (Name : String; Value : JSON_Value) is
            Key : constant Struct_Member_Index :=
              To_Struct_Member_Index (Name, Node);
         begin
            Result.Insert (Key, Value);
         end Process;
      begin
         JSON.Map_JSON_Object (Process'Access);
         return Result;
      end Field_Entries;

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

      ----------------------
      -- To_Predicate_Ref --
      ----------------------

      function To_Predicate_Ref
        (Name : String; Node : Type_Ref) return Struct_Member_Ref
      is
         M : constant Struct_Member_Ref :=
           Map.Lookup_Struct_Member (Node, To_Symbol (Name));
      begin
         if M = No_Struct_Member_Ref then
            Abort_Parsing
              (Node_Type_Image (Node) & " has no " & Name & " member");
         elsif not Is_Property (M) then
            Abort_Parsing
              (Name & " is not a property for " & Node_Type_Image (Node));
         elsif Member_Type (M) /= Type_Of (From_Bool (Language, False)) then
            Abort_Parsing (Name & " must return a boolean");
         elsif Member_Last_Argument (M) /= 0 then
            Abort_Parsing (Name & " must take no argument");
         else
            return M;
         end if;
      end To_Predicate_Ref;

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

                  --   As soon as we find a "tableSeparator"/"text" template
                  --   node, we know this is a Recurse_Field template (in
                  --   "node") or a Recurse_In_Field template (in "fields").
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
                        if Kind_Matches (JSON, "tableSeparator")
                           or else Kind_Matches (JSON, "text")
                        then
                           Result := Recurse_In_Field;
                           raise Abort_Recursion;
                        end if;

                     when Sep_Template =>
                        null;

                     when Join_Template =>
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
               when Recurse_In_Join =>

                  --  Knowing that we are processing a node template,
                  --  Template_Kind is not supposed to return Recurse_In_Join.

                  raise Program_Error;

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

               when Recurse_Field | Recurse_In_Join =>

                  --  Knowing that we are processing a field template,
                  --  Template_Kind is not supposed to return
                  --  Recurse_Field/Recurse_In_Join.

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
         Initial_Symbol_Map : constant Symbol_Parsing_Maps.Map :=
           Context.Symbols;
         --  Symbol map before template parsing, i.e. only symbols defined in a
         --  "parent" template.

         Result_Symbols : Symbol_Parsing_Maps.Map;
         --  Symbol map that represent this template only (its own definitions,
         --  its own references).

         Root : Document_Type;
      begin
         --  Clear symbol reference marks (so that we compute which symbols
         --  *this* template references) and run the template parser.

         for Info of Context.Symbols loop
            pragma Assert (Info.Has_Definition);
            Info.Is_Referenced := False;
         end loop;

         Root := Parse_Template_Helper (JSON, Context);

         --  Now, sanity check symbol usage and compute symbol usage that is
         --  specific to this template (Result_Symbols).

         for Ctx_Cur in Context.Symbols.Iterate loop
            declare
               use Symbol_Parsing_Maps;

               K        : constant Symbol_Type := Key (Ctx_Cur);
               Ctx_Info : Symbol_Info renames Context.Symbols (Ctx_Cur);
               Init_Cur : constant Cursor := Initial_Symbol_Map.Find (K);
            begin
               --  Make sure that all symbols referenced in this template are
               --  also defined in this template.

               if not Ctx_Info.Has_Definition then
                  Abort_Parsing
                    (Context,
                     "undefined symbol: " & To_String (Ctx_Info.Source_Name));
               end if;

               --  Report definitions

               if not Has_Element (Init_Cur) then

                  --  The template we parsed defines this symbol

                  Result_Symbols.Insert (K, Ctx_Info);

               elsif Ctx_Info.Is_Referenced then

                  --  The template we parse does not define this symbol, but it
                  --  references it.

                  Result_Symbols.Insert
                    (K,
                     (Ctx_Info.Source_Name,
                      Ctx_Info.Template_Sym,
                      Has_Definition => False,
                      Is_Referenced  => True));
               end if;
            end;
         end loop;

         case Context.State.Kind is
            when Simple_Recurse =>
               if Context.State.Recurse_Found then
                  return
                    (Kind    => With_Recurse,
                     Root    => Root,
                     Symbols => Result_Symbols);

               else
                  Abort_Parsing (Context, "recursion is missing");
               end if;

            when Recurse_Field | Recurse_In_Field | Recurse_In_Join =>

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
                     return
                       (Kind    => With_Recurse_Field,
                        Root    => Root,
                        Symbols => Result_Symbols);
                  when Recurse_In_Field =>
                     return
                       (Kind    => With_Text_Recurse,
                        Root    => Root,
                        Symbols => Result_Symbols);
                  when Recurse_In_Join =>
                     return
                       (Kind    => Join_Template,
                        Root    => Root,
                        Symbols => Result_Symbols);
               end case;
         end case;
      end Parse_Template;

      ---------------------------
      -- Parse_Template_Helper --
      ---------------------------

      function Parse_Template_Helper
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Document_Type
      is
         Symbol_Map : Symbol_Parsing_Maps.Map renames Context.Symbols;
      begin
         case JSON.Kind is
         when JSON_Array_Type =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for D of JSON_Array'(JSON.Get) loop
                  Items.Append
                    (Parse_Template_Helper (D, Context));
               end loop;
               return Pool.Create_List (Items);
            end;

         when JSON_String_Type =>
            declare
               Value : constant String := JSON.Get;
            begin
               if Value = "breakParent" then
                  return Pool.Create_Break_Parent;
               elsif Value = "flushLineBreaks" then
                  return Pool.Create_Flush_Line_Breaks;
               elsif Value = "hardline" then
                  return Pool.Create_Hard_Line;
               elsif Value = "hardlineWithoutBreakParent" then
                  return Pool.Create_Hard_Line_Without_Break_Parent;
               elsif Value = "line" then
                  return Pool.Create_Line;
               elsif Value = "literalline" then
                  return Pool.Create_Literal_Line;
               elsif Value = "recurse" then
                  Process_Recurse (Context);
                  return Pool.Create_Recurse;
               elsif Value = "recurse_left" then
                  declare
                     Item : Linear_Template_Item := (Kind => Recurse_Left);
                  begin
                     Process_Linear_Template_Item (Item, Context);
                  end;
                  return Pool.Create_Recurse_Left;
               elsif Value = "recurse_right" then
                  declare
                     Item : Linear_Template_Item := (Kind => Recurse_Right);
                  begin
                     Process_Linear_Template_Item (Item, Context);
                  end;
                  return Pool.Create_Recurse_Right;
               elsif Value = "softline" then
                  return Pool.Create_Soft_Line;
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
                          (JSON.Get ("contents"), Context),
                        Bubble_Up_Config (Context, Align, JSON));
                  end;

               elsif Kind in
                  "dedent"
                  | "dedentToRoot"
                  | "markAsRoot"
                  | "innerRoot"
                  | "continuationLineIndent"
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
                                  elsif Kind = "continuationLineIndent"
                                  then
                                    (Kind => Prettier.Continuation_Line_Indent)
                                  else raise Program_Error),
                     Contents  => Parse_Template_Helper
                                    (JSON.Get ("contents"), Context),
                     Bubble_Up => Bubble_Up_Config (Context, Align, JSON));

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

                     return Pool.Create_Fill
                       (Document, Bubble_Up_Config (Context, Fill, JSON));
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
                       Parse_Template_Helper (JSON.Get ("document"), Context);

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

                           begin
                              Id := Declare_Symbol
                                (JSON_Id.Get, Symbols, Symbol_Map);
                           exception
                              when Duplicate_Symbol_Definition =>
                                 Abort_Parsing
                                   (Context,
                                    "duplicate group id: " & JSON_Id.Get);
                           end;
                        end;
                     end if;

                     return Pool.Create_Group
                       (Document,
                        Should_Break,
                        Id,
                        Bubble_Up_Config (Context, Group, JSON));
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
                             Reference_Symbol
                               (JSON_Id.Get, Symbols, Symbol_Map);
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
                       Parse_Template_Helper (JSON.Get ("then"), Then_Context);

                     if not JSON.Has_Field ("else") then
                        Abort_Parsing
                          (Context,
                           "missing ""else"" key for ifEmpty");
                     end if;
                     Else_Contents :=
                       Parse_Template_Helper (JSON.Get ("else"), Else_Context);

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

               elsif Kind = "ifKind" then
                  if Context.Kind not in Node_Template | Field_Template then
                     Abort_Parsing
                       (Context,
                        """ifKind"" is valid in node templates and field"
                        & " templates only");
                  end if;

                  declare
                     Field_JSON    : constant JSON_Value :=
                       JSON.Get ("field");
                     Matchers_JSON : constant JSON_Value :=
                       JSON.Get ("matchers");
                     Default_JSON  : constant JSON_Value :=
                       JSON.Get ("default");
                     Absent_JSON   : constant JSON_Value :=
                       JSON.Get ("absent");

                     Field_Ref : Struct_Member_Ref;
                  begin
                     --  Validate that the keys are present and have the
                     --  correct type.

                     if Context.Kind = Node_Template then
                        if Field_JSON.Kind = JSON_Null_Type then
                           Abort_Parsing
                             (Context, "missing ""field"" key for ifKind");

                        elsif Field_JSON.Kind /= JSON_String_Type then
                           Abort_Parsing
                             (Context,
                              "invalid ""field"" key kind for ifKind: found "
                              & Field_JSON.Kind'Image
                              & "; expected "
                              & JSON_String_Type'Image);
                        end if;

                        Field_Ref := From_Index
                          (Language,
                           To_Struct_Member_Index
                             (Field_JSON.Get, Context.Node));

                     elsif Field_JSON.Kind /= JSON_Null_Type then
                        Abort_Parsing
                          (Context, "invalid ""field"" key for ifKind");
                     else
                        Field_Ref := Context.Field;
                     end if;

                     if Default_JSON.Kind = JSON_Null_Type then
                        Abort_Parsing
                          (Context, "missing ""default"" key for ifKind");
                     end if;

                     if Matchers_JSON.Kind = JSON_Null_Type then
                        Abort_Parsing
                          (Context, "missing ""matchers"" key for ifKind");

                     elsif Matchers_JSON.Kind /= JSON_Array_Type then
                        Abort_Parsing
                          (Context,
                           "invalid ""matchers"" key kind for ifKind: "
                           & "found "
                           & Matchers_JSON.Kind'Image
                           & "; expected "
                           & JSON_Array_Type'Image);
                     end if;

                     declare
                        --  Before parsing the "matchers", "default" or
                        --  "absent" keys, save the current context.
                        --
                        --  Start by parsing the "default" key with the current
                        --  context.
                        --
                        --  Then process each matcher with its own nested
                        --  context confirming that all end on the same state.

                        Initial_Context : constant Template_Parsing_Context :=
                          Context;

                        If_Kind_Default  : Document_Type;
                        If_Kind_Absent   : Document_Type := null;
                        If_Kind_Matchers : Matcher_Vectors.Vector;

                        procedure Process_Matcher (Matcher_JSON : JSON_Value);
                        --  Process Matcher_JSON with their own nested context
                        --  context confirming that it ends in the same state
                        --  as Context.

                        function Parse_Type_Ref
                          (JSON : JSON_Value) return Type_Ref;
                        --  Return the type reference corresponding to JSON,
                        --  expected to be a string. Abort parsing if this is
                        --  not a valid type reference.

                        ---------------------
                        -- Process_Matcher --
                        ---------------------

                        procedure Process_Matcher (Matcher_JSON : JSON_Value)
                        is
                           Kind           : constant JSON_Value :=
                             Matcher_JSON.Get ("kind");
                           Document_JSON  : constant JSON_Value :=
                             Matcher_JSON.Get ("document");
                           Types          : Type_Ref_Vectors.Vector;
                           Nested_Context : Template_Parsing_Context :=
                             Initial_Context;
                        begin
                           if Kind.Kind = JSON_String_Type then
                              Types.Append (Parse_Type_Ref (Kind));
                           elsif Kind.Kind = JSON_Array_Type then
                              for K of JSON_Array'(Kind.Get) loop
                                 Types.Append (Parse_Type_Ref (K));
                              end loop;
                           else
                              Abort_Parsing
                                (Context,
                                 "invalid matcher ""kind"" field for "
                                 & """ifKind"" - found "
                                 & Kind.Kind'Image
                                 & "; expected a string or array of strings");
                           end if;

                           --  Parse the matcher and store it in the table

                           If_Kind_Matchers.Append
                             (Matcher_Record'
                               (Types,
                                Parse_Template_Helper
                                 (Document_JSON, Nested_Context)));

                           --  Confirm that the final linear position is
                           --  homogeneous between all matchers.

                           if Nested_Context.State /= Context.State then
                              Abort_Parsing
                                (Context,
                                 "ifKind matcher """ & Kind.Get & """ has an "
                                 & "inconsistent recurse structure");
                           end if;
                        end Process_Matcher;

                        --------------------
                        -- Parse_Type_Ref --
                        --------------------

                        function Parse_Type_Ref
                          (JSON : JSON_Value) return Type_Ref is
                        begin
                           if JSON.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid matcher ""kind"" field for "
                                 & """ifKind"" - found "
                                 & JSON.Kind'Image
                                 & "; expected "
                                 & JSON_String_Type'Image);
                           end if;
                           return From_Index
                             (Language, To_Type_Index (JSON.Get));
                        end Parse_Type_Ref;

                     begin
                        If_Kind_Default :=
                          Parse_Template_Helper (Default_JSON, Context);

                        if Absent_JSON.Kind /= JSON_Null_Type then
                           declare
                              If_Kind_Absent_Context :
                                Template_Parsing_Context :=
                                  Initial_Context;

                           begin
                              If_Kind_Absent :=
                                Parse_Template_Helper
                                  (Absent_JSON, If_Kind_Absent_Context);

                              if If_Kind_Absent_Context.State /= Context.State
                              then
                                 Abort_Parsing
                                   (Context,
                                    "ifKind ""absent"" matcher has an "
                                    & "inconsistent recurse structure");
                              end if;
                           end;
                        end if;

                        for Matcher_JSON of
                          JSON_Array'(Get (Matchers_JSON))
                        loop
                           if Matcher_JSON.Kind /= JSON_Object_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid ""matchers"" element kind for "
                                 & """ifKind"" - found "
                                 & Matcher_JSON.Kind'Image
                                 & "; expected "
                                 & JSON_Object_Type'Image);
                           end if;

                           Process_Matcher (Matcher_JSON);
                        end loop;

                        return
                          Pool.Create_If_Kind
                            (Field_Ref,
                             If_Kind_Matchers,
                             If_Kind_Default,
                             If_Kind_Absent);
                     end;
                  end;

               elsif Kind = "indent" then
                  if not JSON.Has_Field ("contents") then
                     Abort_Parsing
                       (Context, "missing ""contents"" key for indent");
                  end if;
                  return Pool.Create_Indent
                    (Parse_Template_Helper (JSON.Get ("contents"), Context),
                     Bubble_Up_Config (Context, Indent, JSON));

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

               elsif Kind in "tableSeparator" | "text" then
                  declare
                     T : JSON_Value;
                  begin
                     if not JSON.Has_Field ("text") then
                        Abort_Parsing
                          (Context, "missing ""text"" key for " & Kind);
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
                     --
                     --  There is one special case: we allow empty table
                     --  separators, so that they can be put in table join
                     --  templates.

                     if Kind = "tableSeparator"
                        and then Length (Unbounded_String'(T.Get)) = 0
                     then
                        return Pool.Create_Empty_Table_Separator;
                     end if;

                     declare
                        Item : Linear_Template_Item :=
                          (Kind       => Token_Item,
                           Token_Kind => No_Token_Kind_Ref,
                           Token_Text => To_Unbounded_Text
                                           (From_UTF8 (T.Get)));
                     begin
                        Process_Linear_Template_Item (Item, Context);

                        if Kind = "text" then
                           return Pool.Create_Token
                                    (Item.Token_Kind, Item.Token_Text);
                        else
                           return Pool.Create_Table_Separator
                                    (Item.Token_Kind, Item.Token_Text);
                        end if;
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

            when Recurse_In_Join =>
               Abort_Parsing
                 (Context,
                  "only ""recurse_left"" or ""recurse_right"" are valid in"
                  & " table join templates");
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
             when Token_Item    => "text",
             when Field_Item    => "recurse_field",
             when Recurse_Item  => "recurse",
             when Recurse_Left  => "recurse_left",
             when Recurse_Right => "recurse_right");
      begin
         --  Ensure that it is valid to have a "recurse_field" node in this
         --  template.

         if Context.State.Kind
            not in Recurse_Field | Recurse_In_Field | Recurse_In_Join
         then
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
      -- Check_Symbols --
      -------------------

      procedure Check_Symbols
        (Node            : Type_Ref;
         Parent_Name     : String;
         Parent_Template : Template_Type;
         Child_Name      : String;
         Child_Template  : Template_Type)
      is
         use Symbol_Parsing_Maps;
      begin
         for Child_Cur in Child_Template.Symbols.Iterate loop
            declare
               Child_Info        : Symbol_Info renames
                 Child_Template.Symbols (Child_Cur);
               Defined_In_Parent : constant Boolean :=
                 Parent_Template.Symbols.Contains (Key (Child_Cur));
            begin
               if Child_Info.Has_Definition then
                  if Defined_In_Parent then
                     Abort_Parsing
                       (Debug_Name (Node) & ": group id "
                        & To_String (Child_Info.Source_Name)
                        & " in " & Parent_Name
                        & " is duplicated in " & Child_Name);
                  end if;
               else
                  if not Defined_In_Parent then
                     Abort_Parsing
                       (Debug_Name (Node) & ": group id "
                        & To_String (Child_Info.Source_Name)
                        & " is referenced in " & Child_Name
                        & " but not defined in " & Parent_Name);
                  end if;
               end if;
            end;
         end loop;
      end Check_Symbols;

      ----------------------
      -- Bubble_Up_Config --
      ----------------------

      function Bubble_Up_Config
        (Context : Template_Parsing_Context;
         Kind    : Document_Kind;
         JSON    : JSON_Value) return Trivias_Bubble_Up.Config
      is
         Result : Trivias_Bubble_Up.Config :=
           (case Kind is
            when Align  => Trivias_Bubble_Up.Align_Default_Config,
            when Fill   => Trivias_Bubble_Up.Fill_Default_Config,
            when Group  => Trivias_Bubble_Up.Group_Default_Config,
            when Indent => Trivias_Bubble_Up.Indent_Default_Config,
            when others => raise Program_Error);

         function Get (Key : String; Default : Boolean) return Boolean;
         --  If JSON has the given Key, check that it is a boolean and return
         --  it. Return Default otherwise.

         ---------
         -- Get --
         ---------

         function Get (Key : String; Default : Boolean) return Boolean is
         begin
            if JSON.Has_Field (Key) then
               declare
                  Value : constant JSON_Value := JSON.Get (Key);
               begin
                  if Value.Kind /= JSON_Boolean_Type then
                     Abort_Parsing
                       (Context, "invalid " & Key & ": " & Value.Kind'Image);
                  end if;
                  return Value.Get;
               end;
            else
               return Default;
            end if;
         end Get;

      begin
         Result.Leading := Get ("bubbleUpLeadingTrivias", Result.Leading);
         Result.Trailing := Get ("bubbleUpTrailingTrivias", Result.Trailing);
         return Result;
      end Bubble_Up_Config;

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
            when Field_Template =>
              "template for " & Field_Image (Context.Field, Context.Node),
            when Sep_Template =>
              """" & JSON_Key_For (Context.Sep_Kind) & """ template for "
              & Node_Type_Image (Context.Node),
            when Join_Template =>
              """table""/""join""/""template"" template for "
              & Node_Type_Image (Context.Node));
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

      -------------------
      -- Abort_Parsing --
      -------------------

      procedure Abort_Parsing (Message : String) is
      begin
         Append (Diagnostics, No_Source_Location_Range, To_Text (Message));
         raise Invalid_Input;
      end Abort_Parsing;

      Node_JSON_Map : Node_JSON_Maps.Map;
      --  For each node type described in the unparsing configuration,
      --  reference to the corresponding node configuration.

      --  First, parse the JSON document

      JSON_Result : constant Read_Result := Read (Buffer);
      JSON        : JSON_Value;
   begin
      Result.Ref_Count := 1;
      Result.Language := Language;
      Result.Symbols := Symbols;

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

      --  Then load the unparsing configuration from it

      Add_Node_Entries (JSON, Node_JSON_Map);

      --  Now, go through all node types: parse JSON configurations for nodes
      --  that have one, and implement configuration inheritance in general.

      for Node of All_Node_Types (Language) loop
         declare
            Node_Is_Error : constant Boolean := Is_Error_Node (Node);
            Node_Is_Synthetic : constant Boolean := Is_Synthetic (Node);

            Key     : constant Type_Index := To_Index (Node);
            Present : constant Boolean := Node_JSON_Map.Contains (Key);
            JSON    : constant JSON_Value :=
              (if Present
               then Node_JSON_Map.Element (Key)
               else JSON_Null);

            --  Create the configuration for this node, and look for the
            --  configuration of the node it derives.

            Base_Config : constant Node_Config_Access :=
              (if Node = Root_Node_Type (Language)
               then null
               else Result.Node_Configs.Element (To_Index (Base_Type (Node))));
            Node_Config : constant Node_Config_Access :=
              new Node_Config_Record'
                (Is_Automatic  =>
                   not Present
                   and then (Base_Config = null
                             or else Base_Config.Is_Automatic),
                 Node_Template => No_Template,
                 Field_Configs => <>,
                 List_Config   => No_List_Config);
         begin
            if Check_All_Nodes then

               --  Emit an error if this is a concrete parse node for which no
               --  explicit config from the JSON matches.

               if Is_Concrete (Node)
                  and then not Node_Is_Error
                  and then not Node_Is_Synthetic
                  and then Node_Config.Is_Automatic
               then
                  Append
                    (Diagnostics,
                     No_Source_Location_Range,
                     To_Text ("missing node config for " & Debug_Name (Node)));
               end if;
            end if;

            Result.Node_Configs.Insert (Key, Node_Config);

            --  Forbid node configurations for synthetic and error nodes:
            --  unparsing works on error-free syntax trees, so they cannot be
            --  used, and have no syntax anyway.

            if Present then
               if Node_Is_Error then
                  Abort_Parsing
                    ("error nodes cannot be unparsed (" & Debug_Name (Node)
                     & ")");
               elsif Node_Is_Synthetic then
                  Abort_Parsing
                    ("synthetic nodes cannot be unparsed (" & Debug_Name (Node)
                     & ")");
               end if;
            end if;

            --  Decode the JSON configuration:

            if JSON.Kind not in JSON_Null_Type | JSON_Object_Type then
               Abort_Parsing
                 ("invalid JSON configuration for " & Debug_Name (Node));
            end if;

            --  (1) the "node" entry (if present). If not present, inherit the
            --  config from the base field. As the last resort, use the default
            --  template.

            if JSON.Kind = JSON_Object_Type and then JSON.Has_Field ("node")
            then
               declare
                  JSON_Template : constant JSON_Value := JSON.Get ("node");
                  Context       : Template_Parsing_Context :=
                    (Kind    => Node_Template,
                     Node    => Node,
                     State   => <>,
                     Symbols => <>);
               begin
                  Context.State :=
                    Initial_State_For (Node, JSON_Template, Context);
                  Node_Config.Node_Template :=
                    Parse_Template (JSON_Template, Context);
               end;
            elsif Base_Config /= null then
               Node_Config.Node_Template := Base_Config.Node_Template;
            else
               Node_Config.Node_Template := Pool.Create_Recurse;
            end if;

            --  (2) the "fields" entry (if present)

            declare
               Field_JSON_Map : constant Field_JSON_Maps.Map :=
                 (if JSON.Kind = JSON_Object_Type
                     and then JSON.Has_Field ("fields")
                  then Field_Entries (Node, JSON.Get ("fields"))
                  else Field_JSON_Maps.Empty_Map);
            begin
               for Member of Members (Node) loop
                  if Is_Field (Member) and then not Is_Null_For (Member, Node)
                  then
                     declare
                        Key : constant Struct_Member_Index :=
                          To_Index (Member);
                        T   : Template_Type;
                     begin
                        --  If we have a JSON template for this field, just use
                        --  it.

                        if Field_JSON_Map.Contains (Key) then
                           declare
                              JSON    : constant JSON_Value :=
                                Field_JSON_Map.Element (Key);
                              Context : Template_Parsing_Context :=
                                (Kind    => Field_Template,
                                 State   => Initial_State_For
                                              (Node, Member, JSON),
                                 Symbols => Node_Config.Node_Template.Symbols,
                                 Node    => Node,
                                 Field   => Member);
                           begin
                              T := Parse_Template (JSON, Context);
                           end;

                        --  Otherwise, if the base node has this field, inherit
                        --  its configuration.

                        elsif Base_Config /= null
                              and then Base_Config.Field_Configs.Contains (Key)
                        then
                           T := Base_Config.Field_Configs.Element (Key);

                        --  If none of the above work, just provide the default
                        --  template.

                        else
                           T := Pool.Create_Recurse;
                        end if;

                        Node_Config.Field_Configs.Insert (Key, T);
                     end;
                  end if;
               end loop;
            end;

            --  (3) the "sep"/"leading_sep"/"trailing_sep" entries (if
            --  present). "sep" follows the usual inheritance rules, but for
            --  the two others:
            --
            --  * copy the "sep" template if provided for the same node
            --  * otherwise inherit the corresponding template from the base
            --    node.

            for Kind in List_Sep_Template_Kind'Range loop
               declare
                  JSON_Key : constant String := JSON_Key_For (Kind);

                  function From_Base return Template_Type
                  is (if Base_Config /= null
                      then Base_Config.List_Config.Seps (Kind)
                      else Pool.Create_Recurse);
               begin
                  if JSON.Kind = JSON_Object_Type
                     and then JSON.Has_Field (JSON_Key)
                  then
                     --  Make sure this is for a list node

                     if not Is_List_Node (Node) then
                        Abort_Parsing
                          (Debug_Name (Node)
                           & " is not a list node, invalid """ & JSON_Key
                           & """ configuration");
                     end if;

                     --  Make sure that the list node accepts leading/trailing
                     --  separators if we have a template for them.

                     declare
                        Sep_Extra : constant List_Sep_Extra :=
                          Unparsers.Node_Unparsers.all (Key).Sep_Extra;
                     begin
                        if Kind = Leading_Sep_Template
                           and then Sep_Extra /= Allow_Leading
                        then
                           Abort_Parsing
                             (Debug_Name (Node)
                              & " does not allow leading separators: invalid"
                              & " """ & JSON_Key & """ configuration");

                        elsif Kind = Trailing_Sep_Template
                           and then Sep_Extra /= Allow_Trailing
                        then
                           Abort_Parsing
                             (Debug_Name (Node) & " does not allow trailing"
                              & " separators: invalid """ & JSON_Key & """"
                              & " configuration");
                        end if;
                     end;

                     declare
                        Context : Template_Parsing_Context :=
                          (Kind     => Sep_Template,
                           Node     => Node,
                           Sep_Kind => Kind,
                           State    => (Kind          => Simple_Recurse,
                                        Recurse_Found => False),
                           Symbols  => Node_Config.Node_Template.Symbols);
                     begin
                        Node_Config.List_Config.Seps (Kind) :=
                          Parse_Template (JSON.Get (JSON_Key), Context);
                     end;

                  elsif Kind = Sep_Template then
                     Node_Config.List_Config.Seps (Kind) := From_Base;

                  elsif JSON.Kind = JSON_Object_Type
                        and then JSON.Has_Field (JSON_Key_For (Sep_Template))
                  then
                     Node_Config.List_Config.Seps (Kind) :=
                       Node_Config.List_Config.Seps (Sep_Template);
                  else
                     Node_Config.List_Config.Seps (Kind) := From_Base;
                  end if;
               end;
            end loop;

            --  (4) the "table" entry (if present). Inherit if possible/needed

            if JSON.Kind = JSON_Object_Type and then JSON.Has_Field ("table")
            then
               if not Is_List_Node (Node) then
                  Abort_Parsing
                    (Debug_Name (Node) & " is not a list node, invalid"
                    & " ""table"" configuration");
               end if;
               declare
                  Cfg             : Table_Config_Record renames
                    Node_Config.List_Config.Table_Config;
                  Table_JSON      : constant JSON_Value := JSON.Get ("table");
                  Sep_Before_JSON : JSON_Value;
                  Split_JSON      : JSON_Value;
                  Must_Break_JSON : JSON_Value;
                  Join_JSON       : JSON_Value;
               begin
                  if Table_JSON.Kind = JSON_Null_Type then
                     Cfg := (Enabled => False);
                  elsif Table_JSON.Kind /= JSON_Object_Type then
                     Abort_Parsing
                       ("invalid ""table"" entry for " & Debug_Name (Node)
                        & ": object expected");
                  else
                     Cfg :=
                       (Enabled        => True,
                        Sep_Before     => True,
                        Split          => (others => False),
                        Must_Break     => False,
                        Join_Predicate => No_Struct_Member_Ref,
                        Join_Template  => No_Template);

                     --  Process the optional "sep_before" entry

                     if Table_JSON.Has_Field ("sep_before") then
                        Sep_Before_JSON := Table_JSON.Get ("sep_before");
                        if Sep_Before_JSON.Kind /= JSON_Boolean_Type then
                           Abort_Parsing
                             ("invalid ""table""/""sep_before"" entry for "
                              & Debug_Name (Node) & ": boolean expected");
                        end if;
                        Cfg.Sep_Before := Sep_Before_JSON.Get;
                     end if;

                     --  Process the optional "split" entry

                     if Table_JSON.Has_Field ("split") then
                        Split_JSON := Table_JSON.Get ("split");
                        if Split_JSON.Kind /= JSON_Array_Type then
                           Abort_Parsing
                             ("invalid ""table""/""split"" entry for "
                              & Debug_Name (Node) & ": array expected");
                        end if;
                        for Item of JSON_Array'(Split_JSON.Get) loop
                           if Item.Kind /= JSON_String_Type then
                              Abort_Parsing
                                ("invalid ""table""/""split"" entry for "
                                 & Debug_Name (Node));
                           end if;

                           declare
                              Split : constant String := Item.Get;
                              Kind  : List_Table_Split_Kind;
                           begin
                              if Split = "empty_line" then
                                 Kind := Empty_Line;
                              elsif Split = "line_comment" then
                                 Kind := Line_Comment;
                              else
                                 Abort_Parsing
                                   ("invalid ""table""/""split"" entry for "
                                    & Debug_Name (Node));
                              end if;
                              Cfg.Split (Kind) := True;
                           end;
                        end loop;
                     end if;

                     --  Process the optional "must_break" entry

                     if Table_JSON.Has_Field ("must_break") then
                        Must_Break_JSON := Table_JSON.Get ("must_break");
                        if Must_Break_JSON.Kind /= JSON_Boolean_Type then
                           Abort_Parsing
                             ("invalid ""table""/""must_break"" entry for "
                              & Debug_Name (Node) & ": boolean expected");
                        end if;
                        Cfg.Must_Break := Must_Break_JSON.Get;
                     end if;

                     --  Process the optional "join" entry

                     if Table_JSON.Has_Field ("join") then
                        Join_JSON := Table_JSON.Get ("join");
                        if Join_JSON.Kind /= JSON_Object_Type then
                           Abort_Parsing
                             ("invalid ""table""/""join"" entry for "
                              & Debug_Name (Node) & ": object expected");
                        end if;

                        --  Validate the mandatory "predicate" property
                        --  reference.

                        if not Join_JSON.Has_Field ("predicate")
                           or else Join_JSON.Get ("predicate").Kind
                                   /= JSON_String_Type
                        then
                           Abort_Parsing
                             (Debug_Name (Node) & """table""/""join"" entries"
                              & " must have a ""predicate"" string entry");
                        end if;
                        Cfg.Join_Predicate :=
                          To_Predicate_Ref
                            (Join_JSON.Get ("predicate"),
                             List_Element_Type (Node));

                        --  Parse the join template if present. Provide a
                        --  default one that just concatenates the two rows
                        --  otherwise.

                        if Join_JSON.Has_Field ("template") then
                           declare
                              Context : Template_Parsing_Context :=
                                (Kind     => Join_Template,
                                 Node     => Node,
                                 State    =>
                                   (Kind            => Recurse_In_Join,
                                    Linear_Template =>
                                      Linear_Template_For_Join,
                                    Linear_Position => 1),
                                 Symbols  =>
                                   Node_Config.Node_Template.Symbols);
                           begin
                              Cfg.Join_Template :=
                                Parse_Template
                                  (Join_JSON.Get ("template"), Context);
                           end;
                        else
                           declare
                              Items : Document_Vectors.Vector;
                           begin
                              Items.Append (Pool.Create_Recurse_Left);
                              Items.Append (Pool.Create_Recurse_Right);
                              Cfg.Join_Template :=
                                (Kind    => Join_Template,
                                 Root    => Pool.Create_List (Items),
                                 Symbols => <>);
                           end;
                        end if;
                     end if;
                  end if;
               end;

            elsif Base_Config /= null then
               Node_Config.List_Config.Table_Config :=
                 Base_Config.List_Config.Table_Config;
            else
               Node_Config.List_Config.Table_Config := (Enabled => False);
            end if;

            --  Since inheritance may mix templates from different node
            --  configurations, we need to double check that there are no
            --  duplicate symbol definitions, and that all referenced symbols
            --  are defined.

            for Cur in Node_Config.Field_Configs.Iterate loop
               declare
                  Field : constant Struct_Member_Ref :=
                    From_Index (Language, Field_Config_Maps.Key (Cur));
               begin
                  Check_Symbols
                    (Node,
                     "the ""node"" template",
                     Node_Config.Node_Template,
                     "the """ & Debug_Name (Field) & """ template",
                     Field_Config_Maps.Element (Cur));
               end;
            end loop;
            for Kind in List_Sep_Template_Kind'Range loop
               declare
                  JSON_Key : constant String := JSON_Key_For (Kind);
               begin
                  Check_Symbols
                    (Node,
                     """node"" template",
                     Node_Config.Node_Template,
                     """" & JSON_Key & """ template",
                     Node_Config.List_Config.Seps (Kind));
               end;
            end loop;

            --  (5) the "flush_before_children" entry (if present). If not
            --  present, inherit the setting from the base node.

            if JSON.Kind = JSON_Object_Type
               and then JSON.Has_Field ("flush_before_children")
            then
               if not Is_List_Node (Node) then
                  Abort_Parsing
                    (Debug_Name (Node) & " is not a list node, invalid"
                     & " ""flush_before_children"" configuration");
               end if;

               declare
                  Value : constant JSON_Value :=
                    JSON.Get ("flush_before_children");
               begin
                  if Value.Kind /= JSON_Boolean_Type then
                     Abort_Parsing
                       ("invalid ""flush_before_children"" entry for "
                        & Debug_Name (Node) & ": boolean expected");
                  end if;

                  Node_Config.List_Config.Flush_Before_Children := Value.Get;
               end;
            elsif Base_Config /= null then
               Node_Config.List_Config.Flush_Before_Children :=
                 Base_Config.List_Config.Flush_Before_Children;
            else
               Node_Config.List_Config.Flush_Before_Children := True;
            end if;

            --  (6) the "independent_lines" entry (if present). If not present,
            --  inherit the setting from the base node.

            if JSON.Kind = JSON_Object_Type
               and then JSON.Has_Field ("independent_lines")
            then
               if not Is_List_Node (Node) then
                  Abort_Parsing
                    (Debug_Name (Node) & " is not a list node, invalid"
                     & " ""independent_lines"" configuration");
               end if;

               declare
                  Value : constant JSON_Value :=
                    JSON.Get ("independent_lines");
               begin
                  if Value.Kind /= JSON_Boolean_Type then
                     Abort_Parsing
                       ("invalid ""independent_lines"" entry for "
                        & Debug_Name (Node) & ": boolean expected");
                  end if;

                  Node_Config.List_Config.Independent_Lines := Value.Get;
               end;
            elsif Base_Config /= null then
               Node_Config.List_Config.Independent_Lines :=
                 Base_Config.List_Config.Independent_Lines;
            else
               Node_Config.List_Config.Independent_Lines := False;
            end if;
         end;
      end loop;

      --  It is only now that all templates are known and that inheritance
      --  been applied, that it becomes possible to expand "recurse" templates
      --  for regular nodes into the corresponding "recurse_field" templates.
      --  This will remove the need to do this expansion over and over when
      --  instantiating templates.

      for Cur in Result.Node_Configs.Iterate loop
         declare
            Node        : constant Type_Ref :=
              From_Index (Language, Node_Config_Maps.Key (Cur));
            Node_Config : constant Node_Config_Access :=
              Node_Config_Maps.Element (Cur);
         begin
            Expand_Regular_Node_Template
              (Pool, Node, Node_Config.Node_Template);
         end;
      end loop;

      --  Process the optional "max_empty_lines" entry

      declare
         Max_Empty_Lines : Integer := -1;
      begin
         if JSON.Has_Field ("max_empty_lines") then
            declare
               Value : constant JSON_Value := JSON.Get ("max_empty_lines");
            begin
               if Value.Kind /= JSON_Int_Type or else Integer'(Value.Get) < 0
               then
                  Abort_Parsing
                    ("invalid ""max_empty lines"" entry: natural integer"
                     & " expected");
               end if;
               Max_Empty_Lines := Value.Get;
            end;
         end if;
         Result.Max_Empty_Lines := Max_Empty_Lines;
      end;

      --  If non-fatal errors were emitted, still fail to load the config

      if not Diagnostics.Is_Empty then
         Release (Result);
         return null;
      end if;

      return Result;

   exception
      when Invalid_Input =>
         pragma Assert (not Diagnostics.Is_Empty);
         Release (Result);
         return null;
   end Load_Unparsing_Config_From_Buffer;

end Langkit_Support.Unparsing_Config;
