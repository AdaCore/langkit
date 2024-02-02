--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Prettier_Ada.Document_Vectors; use Prettier_Ada.Document_Vectors;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Names;  use Langkit_Support.Names;

package body Langkit_Support.Prettier_Utils is

   ------------------
   -- Node_Matches --
   ------------------

   function Node_Matches
     (Node : Lk_Node; Types : Type_Vectors.Vector) return Boolean
   is
      N : constant Value_Ref := From_Node (Node.Language, Node);
   begin
      for T of Types loop
         if Type_Matches (N, T) then
            return True;
         end if;
      end loop;
      return False;
   end Node_Matches;

   --------------------------
   -- To_Prettier_Document --
   --------------------------

   function To_Prettier_Document
     (Document : Document_Type) return Prettier.Document_Type
   is
      function "+" (Text : Unbounded_Text_Type) return Unbounded_String
      is (To_Unbounded_String (To_UTF8 (To_Text (Text))));

      function Text_For (Document : Document_Type) return Unbounded_String
      is (case Document.Kind is
          when Token      => +Document.Token_Text,
          when Whitespace => Document.Whitespace_Length * ' ',
          when others     => raise Program_Error);
   begin
      case Document.Kind is
         when Align =>
            return Align
              (Data     => Document.Align_Data,
               Contents => To_Prettier_Document (Document.Align_Contents));

         when Break_Parent =>
            return Break_Parent;

         when Fill =>
            return Fill (To_Prettier_Document (Document.Fill_Document));

         when Group =>
            return Group
              (To_Prettier_Document (Document.Group_Document),
               Document.Group_Options);

         when Hard_Line =>
            return Hard_Line;

         when Hard_Line_Without_Break_Parent =>
            return Hard_Line_Without_Break_Parent;

         when If_Break =>
            return If_Break
              (To_Prettier_Document (Document.If_Break_Contents),
               To_Prettier_Document (Document.If_Break_Flat_Contents),
               (Group_Id => Document.If_Break_Group_Id));

         when Indent =>
            return Indent (To_Prettier_Document (Document.Indent_Document));

         when Line =>
            return Line;

         when List =>

            --  Flatten nested lists, to avoid document bloat, and merge
            --  consecutive tokens.

            declare
               Items : Document_Vector;
               Text  : Unbounded_String;

               procedure Process_List (Document : Document_Type);
               procedure Flush_Text;

               ------------------
               -- Process_List --
               ------------------

               procedure Process_List (Document : Document_Type) is
                  D : Document_Type;
               begin
                  for I in 1 .. Document.List_Documents.Last_Index loop
                     D := Document.List_Documents.Element (I);
                     if D.Kind = List then
                        Process_List (D);
                     elsif D.Kind in Token | Whitespace then
                        Append (Text, Text_For (D));
                     else
                        Flush_Text;
                        Items.Append (To_Prettier_Document (D));
                     end if;
                  end loop;
               end Process_List;

               ----------------
               -- Flush_Text --
               ----------------

               procedure Flush_Text is
               begin
                  if Length (Text) > 0 then
                     Items.Append (Prettier.Builders.Text (Text));
                     Text := Null_Unbounded_String;
                  end if;
               end Flush_Text;

            begin
               Process_List (Document);
               Flush_Text;
               return List (Items);
            end;

         when Literal_Line =>
            return Literal_Line;

         when Recurse | Recurse_Flatten =>
            raise Program_Error with "uninstantiated template";

         when Soft_Line =>
            return Soft_Line;

         when Token | Whitespace =>
            return Prettier.Builders.Text (Text_For (Document));

         when Trim =>
            return Trim;
      end case;
   end To_Prettier_Document;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Document_Pool) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Document_Record, Document_Type);
   begin
      for Document of Self loop
         Free (Document);
      end loop;
      Self.Clear;
   end Release;

   --------------
   -- Register --
   --------------

   procedure Register (Self : in out Document_Pool; Document : Document_Type)
   is
   begin
      Self.Append (Document);
   end Register;

   ------------------
   -- Create_Align --
   ------------------

   function Create_Align
     (Self     : in out Document_Pool;
      Data     : Prettier.Alignment_Data_Type;
      Contents : Document_Type;
      Node     : Lk_Node := No_Lk_Node) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind           => Align,
           Node           => Node,
           Align_Data     => Data,
           Align_Contents => Contents)
      do
         Self.Register (Result);
      end return;
   end Create_Align;

   -------------------------
   -- Create_Break_Parent --
   -------------------------

   function Create_Break_Parent
     (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Break_Parent)
      do
         Result.Node := No_Lk_Node;
         Self.Register (Result);
      end return;
   end Create_Break_Parent;

   -----------------
   -- Create_Fill --
   -----------------

   function Create_Fill
     (Self     : in out Document_Pool;
      Document : Document_Type;
      Node     : Lk_Node := No_Lk_Node) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind          => Fill,
           Node          => Node,
           Fill_Document => Document)
      do
         Self.Register (Result);
      end return;
   end Create_Fill;

   ------------------
   -- Create_Group --
   ------------------

   function Create_Group
     (Self     : in out Document_Pool;
      Document : Document_Type;
      Options  : Group_Options_Type;
      Node     : Lk_Node := No_Lk_Node) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind           => Group,
           Node           => Node,
           Group_Document => Document,
           Group_Options  => Options)
      do
         Self.Register (Result);
      end return;
   end Create_Group;

   ----------------------
   -- Create_Hard_Line --
   ----------------------

   function Create_Hard_Line (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Hard_Line)
      do
         Result.Node := No_Lk_Node;
         Self.Register (Result);
      end return;
   end Create_Hard_Line;

   -------------------------------------------
   -- Create_Hard_Line_Without_Break_Parent --
   -------------------------------------------

   function Create_Hard_Line_Without_Break_Parent
     (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Hard_Line_Without_Break_Parent)
      do
         Self.Register (Result);
      end return;
   end Create_Hard_Line_Without_Break_Parent;

   ---------------------
   -- Create_If_Break --
   ---------------------

   function Create_If_Break
     (Self          : in out Document_Pool;
      Contents      : Document_Type;
      Flat_Contents : Document_Type := null;
      Group_Id      : Prettier.Symbol_Type :=
        Prettier.No_Symbol) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind                   => If_Break,
           If_Break_Contents      => Contents,
           If_Break_Flat_Contents =>
             (if Flat_Contents = null
              then Self.Create_Empty_List
              else Flat_Contents),
           If_Break_Group_Id      => Group_Id,
           Node                   => No_Lk_Node)
      do
         Self.Register (Result);
      end return;
   end Create_If_Break;

   -------------------
   -- Create_Indent --
   -------------------

   function Create_Indent
     (Self     : in out Document_Pool;
      Document : Document_Type;
      Node     : Lk_Node := No_Lk_Node) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind            => Indent,
           Node            => Node,
           Indent_Document => Document)
      do
         Self.Register (Result);
      end return;
   end Create_Indent;

   -----------------
   -- Create_Line --
   -----------------

   function Create_Line (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Line)
      do
         Result.Node := No_Lk_Node;
         Self.Register (Result);
      end return;
   end Create_Line;

   -----------------
   -- Create_List --
   -----------------

   function Create_List
     (Self      : in out Document_Pool;
      Documents : in out Document_Vectors.Vector;
      Node      : Lk_Node := No_Lk_Node) return Document_Type
   is
      use type Ada.Containers.Count_Type;
   begin
      if Documents.Length = 1 then
         return Documents.Element (1);
      end if;

      return Result : constant Document_Type :=
        new Document_Record (Kind => List)
      do
         Result.Node := Node;
         Result.List_Documents.Move (Documents);
         Self.Register (Result);
      end return;
   end Create_List;

   -------------------------
   -- Create_Literal_Line --
   -------------------------

   function Create_Literal_Line
     (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Literal_Line)
      do
         Result.Node := No_Lk_Node;
         Self.Register (Result);
      end return;
   end Create_Literal_Line;

   -----------------------
   -- Create_Empty_List --
   -----------------------

   function Create_Empty_List
     (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => List)
      do
         Result.Node := No_Lk_Node;
         Self.Register (Result);
      end return;
   end Create_Empty_List;

   --------------------
   -- Create_Recurse --
   --------------------

   function Create_Recurse (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type := new Document_Record (Recurse)
      do
         Result.Node := No_Lk_Node;
         Self.Register (Result);
      end return;
   end Create_Recurse;

   ----------------------------
   -- Create_Recurse_Flatten --
   ----------------------------

   function Create_Recurse_Flatten
     (Self  : in out Document_Pool;
      Types : in out Type_Vectors.Vector) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Recurse_Flatten)
      do
         Result.Node := No_Lk_Node;
         Result.Recurse_Flatten_Types.Move (Types);
         Self.Register (Result);
      end return;
   end Create_Recurse_Flatten;

   ----------------------
   -- Create_Soft_Line --
   ----------------------

   function Create_Soft_Line (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Soft_Line)
      do
         Result.Node := No_Lk_Node;
         Self.Register (Result);
      end return;
   end Create_Soft_Line;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token
     (Self : in out Document_Pool;
      Kind : Token_Kind_Ref;
      Text : Unbounded_Text_Type) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind       => Token,
           Node       => No_Lk_Node,
           Token_Kind => Kind,
           Token_Text => Text)
      do
         Self.Register (Result);
      end return;
   end Create_Token;

   -----------------
   -- Create_Trim --
   -----------------

   function Create_Trim (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Trim)
      do
         Result.Node := No_Lk_Node;
         Self.Register (Result);
      end return;
   end Create_Trim;

   -----------------------
   -- Create_Whitespace --
   -----------------------

   function Create_Whitespace
     (Self : in out Document_Pool; Length : Positive := 1) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind              => Whitespace,
           Node              => No_Lk_Node,
           Whitespace_Length => Length)
      do
         Self.Register (Result);
      end return;
   end Create_Whitespace;

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

   -----------------------------
   -- Insert_Required_Spacing --
   -----------------------------

   procedure Insert_Required_Spacing
     (Pool : in out Document_Pool; Document : in out Document_Type)
   is
      procedure Process
        (Document     : in out Document_Type;
         Last_Token   : in out Token_Kind_Ref;
         Last_Spacing : in out Spacing_Kind);
      --  Assuming that the last token unparsed before Document is Last_Token,
      --  and that Last_Spacing was unparsed since then, insert required
      --  spacing inside Document itself.
      --
      --  Update Last_Token/Last_Spacing to reflect the last token/spacing
      --  emitted once Document itself has been unparsed.

      procedure Extend_Spacing (Self : in out Spacing_Kind; To : Spacing_Kind);
      --  If To is includes more spacing than Self, set Self to To

      -------------
      -- Process --
      -------------

      procedure Process
        (Document     : in out Document_Type;
         Last_Token   : in out Token_Kind_Ref;
         Last_Spacing : in out Spacing_Kind) is
      begin
         case Document.Kind is
            when Align =>

               --  Align does not emit any spacing before processing its inner
               --  document.

               Extend_Spacing (Last_Spacing, None);
               Process (Document.Align_Contents, Last_Token, Last_Spacing);

            when Break_Parent =>
               null;

            when Fill =>
               Process (Document.Fill_Document, Last_Token, Last_Spacing);

            when Group =>
               Process (Document.Group_Document, Last_Token, Last_Spacing);

            when Hard_Line =>
               Extend_Spacing (Last_Spacing, Newline);

            when Hard_Line_Without_Break_Parent =>
               Extend_Spacing (Last_Spacing, Newline);

            when If_Break =>
               declare
                  LT_Break : Token_Kind_Ref := Last_Token;
                  LS_Break : Spacing_Kind := Last_Spacing;

                  LT_Flat : Token_Kind_Ref := Last_Token;
                  LS_Flat : Spacing_Kind := Last_Spacing;
               begin
                  Process (Document.If_Break_Contents, LT_Break, LS_Break);
                  Process (Document.If_Break_Flat_Contents, LT_Flat, LS_Flat);

                  --  Our "recurse" sanitization should guarantee us that all
                  --  alternatives output the same sequence of tokens, so this
                  --  should not happen.

                  if LT_Break /= LT_Flat then
                     raise Program_Error;
                  end if;

                  Last_Token := LT_Break;
                  Last_Spacing := Spacing_Kind'Min (LS_Break, LS_Flat);
               end;

            when Indent =>

               --  Indent does not emit any spacing before processing its inner
               --  document.

               Extend_Spacing (Last_Spacing, None);
               Process (Document.Indent_Document, Last_Token, Last_Spacing);

            when Line =>

               --  A Line command can be replaced by line breaks or a space: be
               --  conservative and consider its weakest form: a space.

               Extend_Spacing (Last_Spacing, Whitespace);

            when List =>
               for I in 1 .. Document.List_Documents.Last_Index loop
                  declare
                     D : Document_Type := Document.List_Documents.Element (I);
                  begin
                     Process (D, Last_Token, Last_Spacing);
                     Document.List_Documents.Replace_Element (I, D);
                  end;
               end loop;

            when Literal_Line =>
               Extend_Spacing (Last_Spacing, Newline);

            when Recurse | Recurse_Flatten =>
               raise Program_Error;

            when Soft_Line =>

               --  A Soft_Line command can be replaced by a line break or
               --  nothing: be conservative and consider its weakest form:
               --  nothing.

               Extend_Spacing (Last_Spacing, None);

            when Token =>
               declare
                  Saved_Last_Spacing : constant Spacing_Kind := Last_Spacing;
                  Required           : constant Spacing_Kind :=
                    Required_Spacing (Last_Token, Document.Token_Kind);
               begin
                  Last_Token := Document.Token_Kind;
                  Last_Spacing := None;
                  if Required <= Saved_Last_Spacing then
                     return;
                  end if;

                  --  If we reach this point, we noticed that there is no
                  --  guarantee that this token has the required spacing with
                  --  the previous token unparsed: insert the spacing that is
                  --  missing.

                  declare
                     Token_Document : constant Document_Type := Document;
                     Items          : Document_Vectors.Vector;
                  begin
                     case Required is
                        when None => raise Program_Error;
                        when Whitespace =>
                           Items.Append (Pool.Create_Whitespace);
                        when Newline =>
                           Items.Append (Pool.Create_Hard_Line);
                     end case;
                     Items.Append (Token_Document);
                     Document := Pool.Create_List (Items);
                  end;
               end;

            when Trim =>
               null;

            when Whitespace =>
               Extend_Spacing (Last_Spacing, Whitespace);
         end case;
      end Process;

      --------------------
      -- Extend_Spacing --
      --------------------

      procedure Extend_Spacing (Self : in out Spacing_Kind; To : Spacing_Kind)
      is
      begin
         Self := Spacing_Kind'Max (Self, To);
      end Extend_Spacing;

      Last_Token   : Token_Kind_Ref := No_Token_Kind_Ref;
      Last_Spacing : Spacing_Kind := None;
   begin
      Process (Document, Last_Token, Last_Spacing);
   end Insert_Required_Spacing;

   ----------
   -- Dump --
   ----------

   procedure Dump (Document : Document_Type) is
      Simple_Indent : constant String := "  ";
      List_Indent   : constant String := "| ";

      procedure Process (Document : Document_Type; Prefix : String);

      -------------
      -- Process --
      -------------

      procedure Process (Document : Document_Type; Prefix : String) is
      begin
         Put (Prefix);
         if Document = null then
            Put_Line ("<none>");
            return;
         end if;
         case Document.Kind is
            when Align =>
               Put_Line ("align:");
               Put (Prefix & Simple_Indent & "data: ");
               case Document.Align_Data.Kind is
                  when Prettier.Width =>
                     Put_Line
                       ("width (number):" & Document.Align_Data.N'Image);
                  when Prettier.Text =>
                     Put_Line
                       ("width (text):" & To_String (Document.Align_Data.T));
                  when Prettier.Dedent =>
                     Put_Line ("dedent");
                  when Prettier.Dedent_To_Root =>
                     Put_Line ("dedentToRoot");
                  when Prettier.Root =>
                     Put_Line ("markAsRoot");
                  when Prettier.None =>
                     Put_Line ("none");
               end case;
               Process (Document.Align_Contents, Prefix & Simple_Indent);

            when Break_Parent =>
               Put_Line ("breakParent");

            when Fill =>
               Put_Line ("fill:");
               Process (Document.Fill_Document, Prefix & Simple_Indent);

            when Group =>
               Put_Line ("group:");
               Put_Line
                 (Prefix & Simple_Indent & "shouldBreak: "
                  & Document.Group_Options.Should_Break'Image);
               Put_Line
                 (Prefix & Simple_Indent & "id: "
                  & Prettier.Image (Document.Group_Options.Id));
               Process (Document.Group_Document, Prefix & Simple_Indent);

            when Hard_Line =>
               Put_Line ("hardline");

            when Hard_Line_Without_Break_Parent =>
               Put_Line ("hardlineWithoutBreakParent");

            when If_Break =>
               Put_Line ("ifBreak:");
               if Document.If_Break_Group_Id /= No_Symbol then
                  Put_Line
                    (Prefix & Simple_Indent & "groupId: "
                     & Prettier.Image (Document.If_Break_Group_Id));
               end if;
               Process (Document.If_Break_Contents, Prefix & List_Indent);
               Process (Document.If_Break_Flat_Contents, Prefix & List_Indent);

            when Indent =>
               Put_Line ("indent:");
               Process (Document.Indent_Document, Prefix & Simple_Indent);

            when Line =>
               Put_Line ("line");

            when List =>
               Put_Line ("list:");
               for I in 1 .. Document.List_Documents.Last_Index loop
                  Process
                    (Document.List_Documents.Element (I),
                     Prefix & List_Indent);
               end loop;

            when Literal_Line =>
               Put_Line ("literalline");

            when Recurse =>
               Put_Line ("recurse");

            when Recurse_Flatten =>
               Put_Line ("recurse_flatten:");
               for I in 1 .. Document.Recurse_Flatten_Types.Last_Index loop
                  declare
                     T : constant Type_Ref :=
                       Document.Recurse_Flatten_Types.Element (I);
                  begin
                     Put_Line (Prefix & Simple_Indent & Debug_Name (T));
                  end;
               end loop;

            when Soft_Line =>
               Put_Line ("softline");

            when Token =>
               declare
                  Token_Name : constant Name_Type :=
                    Token_Kind_Name (Document.Token_Kind);
               begin
                  Put_Line
                    ("token[" & Image (Format_Name (Token_Name, Camel)) & "]: "
                     & Image (To_Text (Document.Token_Text)));
               end;

            when Trim =>
               Put_Line ("trim");

            when Whitespace =>
               Put_Line
                 ("whitespace(" & Document.Whitespace_Length'Image & ")");
         end case;
      end Process;
   begin
      Process (Document, "");
   end Dump;

end Langkit_Support.Prettier_Utils;
