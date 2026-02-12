with Ada.Assertions; use Ada.Assertions;
with Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;    use Ada.Text_IO;

with Interfaces; use Interfaces;

with GNAT.Regexp;
with GNATCOLL.Traces;

with Liblktlang.Analysis;          use Liblktlang.Analysis;
with Liblktlang.Prelude;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;
with Liblktlang_Support.Adalog.Debug;
with Liblktlang_Support.Text;      use Liblktlang_Support.Text;

package body Liblktlang.Implementation.Extensions is

   Lkt_Filename_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile ("([a-z](_?[a-z0-9]+)*)\.lkt");

   package WWS renames Ada.Strings.Wide_Wide_Unbounded;

   function Common_Denoted_String
     (Node : Bare_Lkt_Node) return Internal_Decoded_String_Value;
   --  Common implementation for the ``p_denoted_string`` property of all
   --  string/pattern literal token nodes.

   function Block_Denoted_String
     (Lines : Bare_Lkt_Node_Base_List) return Internal_Decoded_String_Value;
   --  Common implementation for the ``p_denoted_string`` property of all block
   --  string literals.

   procedure Read_Denoted_Char
     (Buffer       : Text_Type;
      For_Char_Lit : Boolean;
      Cursor       : in out Positive;
      Cursor_Sloc  : in out Source_Location;
      Result       : out Internal_Decoded_Char_Value);
   --  Read the next denoted character starting at ``Buffer (Cursor)``.
   --
   --  The location of the character at ``Buffer (Cursor)`` must be passed to
   --  ``Cursor_Sloc``, which is updated to follow the evolution of ``Cursor``.
   --
   --  Upon return, ``Cursor`` points to the first item in ``Buffer`` for the
   --  next character to read (or to the closing single/double quote if the
   --  character read was the last one) and ``Result`` is set to the character
   --  that was just read, or to an error message if reading one character was
   --  unsuccessful.

   ---------------------------
   -- Common_Denoted_String --
   ---------------------------

   function Common_Denoted_String
     (Node : Bare_Lkt_Node) return Internal_Decoded_String_Value
   is
      Tab_Stop : constant Positive := Node.Unit.Context.Tab_Stop;

      N_Text : constant Text_Type := Text (Node);
      pragma Assert (N_Text (N_Text'Last) = '"');

      Cursor      : Natural := N_Text'First + 1;
      Cursor_Sloc : Source_Location := Start_Sloc (Sloc_Range (Node));

      Result      : Text_Type (1 .. N_Text'Length);
      Result_Last : Natural := Result'First - 1;
      Char_Value  : Internal_Decoded_Char_Value;
   begin
      --  Make sure that the slice starts at the first denoted character in the
      --  presence of string literal prefix.

      if N_Text (N_Text'First) /= '"' then
         pragma Assert (N_Text (Cursor) = '"');
         Cursor := Cursor + 1;
      end if;

      --  Update Cursor_Sloc so that it reflects the location of N_Text's item
      --  at index Cursor.

      Cursor_Sloc.Column :=
        Cursor_Sloc.Column
        + Column_Count (N_Text (N_Text'First .. Cursor - 1), Tab_Stop);

      while Cursor /= N_Text'Last loop
         Result_Last := Result_Last + 1;
         Read_Denoted_Char (N_Text, False, Cursor, Cursor_Sloc, Char_Value);
         if Char_Value.Has_Error then
            return
              (Value         => Empty_String,
               Has_Error     => True,
               Error_Sloc    => Char_Value.Error_Sloc,
               Error_Message => Char_Value.Error_Message);
         end if;
         Result (Result_Last) := Char_Value.Value;
      end loop;

      return
        (Value         => Create_String (Result (Result'First .. Result_Last)),
         Has_Error     => False,
         Error_Sloc    => No_Source_Location,
         Error_Message => Empty_String);
   end Common_Denoted_String;

   --------------------------
   -- Block_Denoted_String --
   --------------------------

   function Block_Denoted_String
     (Lines : Bare_Lkt_Node_Base_List) return Internal_Decoded_String_Value
   is
      Line_Prefix : constant Text_Type :=
        (if Kind (Lines) in Lkt_Module_Doc_String_Line_List_Range
         then "|"""""
         else "|""");
      --  The line prefix depends on the kind of block string (module docstring
      --  of regular string literals).

      Result : Unbounded_Text_Type;
   begin
      for I in 1 .. Lines.Count loop
         declare
            Item   : constant Bare_Lkt_Node := Lines.Nodes.all (I);
            N_Text : constant Text_Type := Text (Item);

            Cursor      : Positive := N_Text'First;
            Cursor_Sloc : Source_Location := Start_Sloc (Sloc_Range (Item));

            Char_Value : Internal_Decoded_Char_Value;
         begin
            --  There are two legal cases:
            --
            --  * '|"' (or '|""'), which designates an empty line
            --  * '|" ...' (or '|" ...'), which designates a non-empty line:
            --    the space after '|"' (or '|""') is mandatory. Decode escape
            --    sequences in the line content.
            --
            --  The only thing guaranteed here (by the lexer) is that N_Next
            --  starts with ``|"`` (or ``|""``): the rest is to be checked
            --  here, as semantic checks.

            pragma Assert (N_Text'Length >= Line_Prefix'Length);
            pragma Assert
              (N_Text (N_Text'First .. N_Text'First + Line_Prefix'Length - 1)
               = Line_Prefix);
            Cursor := Cursor + Line_Prefix'Length;
            Cursor_Sloc.Column := Cursor_Sloc.Column + Line_Prefix'Length;

            if N_Text'Length > Line_Prefix'Length then
               if N_Text (Cursor) /= ' ' then
                  return
                    (Value         => Empty_String,
                     Has_Error     => True,
                     Error_Sloc    => Cursor_Sloc,
                     Error_Message => Create_String ("space missing"));
               elsif N_Text'Length = Line_Prefix'Length + 1 then
                  return
                    (Value         => Empty_String,
                     Has_Error     => True,
                     Error_Sloc    => Cursor_Sloc,
                     Error_Message => Create_String
                       ("empty line must not end with a space"));
               end if;

               Cursor := Cursor + 1;
               Cursor_Sloc.Column := Cursor_Sloc.Column + 1;

               while Cursor <= N_Text'Last loop
                  Read_Denoted_Char
                    (N_Text, False, Cursor, Cursor_Sloc, Char_Value);
                  if Char_Value.Has_Error then
                     return
                       (Value         => Empty_String,
                        Has_Error     => True,
                        Error_Sloc    => Char_Value.Error_Sloc,
                        Error_Message => Char_Value.Error_Message);
                  end if;
                  WWS.Append (Result, Char_Value.Value);
               end loop;
            end if;
            WWS.Append (Result, Chars.LF);
         end;
      end loop;
      return
        (Value         => Create_String (To_Text (Result)),
         Has_Error     => False,
         Error_Sloc    => No_Source_Location,
         Error_Message => Empty_String);
   end Block_Denoted_String;

   -----------------------
   -- Read_Denoted_Char --
   -----------------------

   procedure Read_Denoted_Char
     (Buffer       : Text_Type;
      For_Char_Lit : Boolean;
      Cursor       : in out Positive;
      Cursor_Sloc  : in out Source_Location;
      Result       : out Internal_Decoded_Char_Value) is
   begin
      Result :=
        (Value         => ' ',
         Has_Error     => False,
         Error_Sloc    => Cursor_Sloc,
         Error_Message => Empty_String);

      if Buffer (Cursor) = '\' then
         Cursor := Cursor + 1;
         Cursor_Sloc.Column := Cursor_Sloc.Column + 1;
         declare
            function Read_Digits (N : Positive) return Character_Type;
            --  Read N hexadecimal digits (encoding a codepoint number) and
            --  return the corresponding character.

            function Short_Escape_Sequence
              (Codepoint : Character) return Character_Type;
            --  Helper to read a short escape sequence: return ``Codepoint``
            --  converted to ``Character_Type`` and increment ``Cursor``, to
            --  make it go from the short escape sequence character ("n" in
            --  "\n", for instance) to the next item in ``Buffer``.

            -----------------
            -- Read_Digits --
            -----------------

            function Read_Digits (N : Positive) return Character_Type is
               Codepoint   : Unsigned_32 := 0;
               Digit_Char  : Character_Type;
               Digit_Value : Unsigned_32;
            begin
               for I in 1 .. N loop
                  Cursor := Cursor + 1;
                  Cursor_Sloc.Column := Cursor_Sloc.Column + 1;

                  Digit_Char := Buffer (Cursor);
                  case Digit_Char is
                     when '0' .. '9' =>
                        Digit_Value :=
                           Character_Type'Pos (Digit_Char)
                           - Character_Type'Pos ('0');
                     when 'a' .. 'f' =>
                        Digit_Value :=
                           Character_Type'Pos (Digit_Char)
                           - Character_Type'Pos ('a') + 10;
                     when 'A' .. 'F' =>
                        Digit_Value :=
                           Character_Type'Pos (Digit_Char)
                           - Character_Type'Pos ('A') + 10;
                     when others =>
                        Result.Has_Error := True;
                        Result.Error_Message :=
                          Create_String ("invalid escape sequence");
                        return ' ';
                  end case;
                  Codepoint := 16 * Codepoint + Digit_Value;
               end loop;

               --  Move past the last digit of the escape sequence

               Cursor := Cursor + 1;
               Cursor_Sloc.Column := Cursor_Sloc.Column + 1;

               return Character_Type'Val (Codepoint);
            end Read_Digits;

            ---------------------------
            -- Short_Escape_Sequence --
            ---------------------------

            function Short_Escape_Sequence
              (Codepoint : Character) return Character_Type is
            begin
               Cursor := Cursor + 1;
               Cursor_Sloc.Column := Cursor_Sloc.Column + 1;
               return Character_Type'Val (Character'Pos (Codepoint));
            end Short_Escape_Sequence;

         begin
            case Buffer (Cursor) is

               --  Escape sequences for codepoint numbers

               when 'x' =>
                  Result.Value := Read_Digits (2);
               when 'u' =>
                  Result.Value := Read_Digits (4);
               when 'U' =>
                  Result.Value := Read_Digits (8);

               --  Short escape sequences

               when '0' =>
                  Result.Value := Short_Escape_Sequence (ASCII.NUL);
               when 'a' =>
                  Result.Value := Short_Escape_Sequence (ASCII.BEL);
               when 'b' =>
                  Result.Value := Short_Escape_Sequence (ASCII.BS);
               when 't' =>
                  Result.Value := Short_Escape_Sequence (ASCII.HT);
               when 'n' =>
                  Result.Value := Short_Escape_Sequence (ASCII.LF);
               when 'v' =>
                  Result.Value := Short_Escape_Sequence (ASCII.VT);
               when 'f' =>
                  Result.Value := Short_Escape_Sequence (ASCII.FF);
               when 'r' =>
                  Result.Value := Short_Escape_Sequence (ASCII.CR);
               when '\' =>
                  Result.Value := Short_Escape_Sequence ('\');
               when '"' =>
                  if For_Char_Lit then
                     Result.Has_Error := True;
                     Result.Error_Message :=
                       Create_String ("invalid escape sequence");
                  else
                     Result.Value := Short_Escape_Sequence ('"');
                  end if;
               when ''' =>
                  if For_Char_Lit then
                     Result.Value := Short_Escape_Sequence (''');
                  else
                     Result.Has_Error := True;
                     Result.Error_Message :=
                       Create_String ("invalid escape sequence");
                  end if;

               when others =>
                  Result.Has_Error := True;
                  Result.Error_Message :=
                    Create_String ("invalid escape sequence");
            end case;

            if Result.Has_Error then
               return;
            end if;
         end;
      else
         Result.Value := Buffer (Cursor);
         Cursor_Sloc.Column :=
           Cursor_Sloc.Column + Column_Count (Buffer (Cursor .. Cursor));
         Cursor := Cursor + 1;
      end if;
   end Read_Denoted_Char;

   --------------------------------
   -- Langkit_Root_P_Module_Name --
   --------------------------------

   function Langkit_Root_P_Module_Name
     (Node : Bare_Langkit_Root) return Symbol_Type
   is
      Filename : constant String :=
        Ada.Directories.Simple_Name (Node.Unit.Get_Filename);
   begin
      --  If ``Node`` belongs to the prelude, which is not a regular module,
      --  return the empty symbol, as documented.

      if Lkt_Node_P_Is_From_Prelude (Node) then
         return No_Symbol;

      --  Otherwise, make sure the unit has a valid Lkt filename

      elsif not GNAT.Regexp.Match (Filename, Lkt_Filename_Regexp) then
         Raise_Property_Exception
           (Node, Property_Error'Identity, "invalid Lkt filename");
      end if;

      --  From there, just strip the extension and we have the module name

      declare
         Module_Name : String renames
           Filename (Filename'First .. Filename'Last - 4);
      begin
         return Find (Node.Unit.Context.Symbols, To_Text (Module_Name));
      end;
   end Langkit_Root_P_Module_Name;

   -----------------------------
   -- Lkt_Node_P_Prelude_Unit --
   -----------------------------

   function Lkt_Node_P_Prelude_Unit (Node : Bare_Lkt_Node) return Internal_Unit
   is
      Ctx     : constant Analysis_Context := Wrap_Context (Node.Unit.Context);
      Prelude : Analysis_Unit;
   begin
      Prelude := Ctx.Get_From_File ("__prelude");
      if Prelude.Root = No_Lkt_Node then
         Prelude := Ctx.Get_From_Buffer
           ("__prelude", "ascii", Liblktlang.Prelude.Content);

         --  Check if we have syntactic errors in the prelude. If we do, raise
         --  an assertion error.

         if Prelude.Diagnostics'Length > 0 then
            for Diagnostic of Prelude.Diagnostics loop
               Put_Line (To_Pretty_String (Diagnostic));
            end loop;
            raise Assertion_Error with "Errors in prelude";
         end if;

         Populate_Lexical_Env (Prelude);
      end if;
      return Unwrap_Unit (Prelude);
   end Lkt_Node_P_Prelude_Unit;

   --------------------------------------
   -- Lkt_Node_P_Set_Solver_Debug_Mode --
   --------------------------------------

   function Lkt_Node_P_Set_Solver_Debug_Mode
     (Node   : Bare_Lkt_Node;
      Enable : Boolean) return Boolean
   is
      pragma Unreferenced (Node);
      use Liblktlang_Support.Adalog;
   begin
      GNATCOLL.Traces.Parse_Config_File;
      Debug.Set_Debug_State
        (if Enable then Debug.Trace else Debug.None);
      Solver_Trace.Set_Active (Enable);
      Solv_Trace.Set_Active (Enable);
      Sol_Trace.Set_Active (Enable);
      return True;
   end Lkt_Node_P_Set_Solver_Debug_Mode;

   -----------------------
   -- Id_P_Custom_Image --
   -----------------------

   function Id_P_Custom_Image
     (Node : Bare_Id; E_Info : Entity_Info) return String_Type
   is
      pragma Unreferenced (E_Info);
   begin
      return
        Create_String
          ("<" & To_Text (Node.Kind_Name) & " """ & Node.Text & """ "
           & To_Text (Ada.Directories.Simple_Name (Node.Unit.Get_Filename))
           & ":" & To_Text (Node.Sloc_Range.Image) & ">");
   end Id_P_Custom_Image;

   -------------------------
   -- Decl_P_Custom_Image --
   -------------------------

   function Decl_P_Custom_Image
      (Node : Bare_Decl; E_Info : Entity_Info) return String_Type
   is
      Full_Name_Acc : String_Type :=
         Dispatcher_Decl_P_Full_Name_Internal (Node, E_Info);
      Full_Name     : constant Text_Type := Full_Name_Acc.Content;
      File_Name     : constant Text_Type :=
         To_Text (Ada.Directories.Simple_Name (Get_Filename (Unit (Node))));
   begin
      Dec_Ref (Full_Name_Acc);
      if File_Name = "__prelude" then
         return Create_String ("<" & To_Text (Kind_Name (Node))
              & " prelude: """ & Full_Name & """>");
      else
         return Create_String
           ("<" & To_Text (Kind_Name (Node)) & " """ & Full_Name & """ "
            & File_Name
            --  Don't show the sloc for function types, because it will be
            --  the root node's sloc, and thus will always change when we add
            --  stuff to the file, which is not helpful nor practical for
            --  tests.
            & (if Node.Kind = Lkt_Function_Type
                then ""
                else ":" & To_Text (Image (Sloc_Range (Node)))) & ">");
      end if;
   end Decl_P_Custom_Image;

   --------------------
   -- Id_Short_Image --
   --------------------

   function Id_Short_Image (Node : Bare_Id) return Text_Type is
      Image : String_Type := Id_P_Custom_Image (Node, No_Entity_Info);
   begin
      return Result : constant Text_Type := Image.Content do
         Dec_Ref (Image);
      end return;
   end Id_Short_Image;

   ----------------------
   -- Decl_Short_Image --
   ----------------------

   function Decl_Short_Image (Node : Bare_Decl) return Text_Type is
      Image : String_Type := Decl_P_Custom_Image (Node, No_Entity_Info);
   begin
      return Result : constant Text_Type := Image.Content do
         Dec_Ref (Image);
      end return;
   end Decl_Short_Image;

   -----------------------
   -- Id_P_Is_Type_Name --
   -----------------------

   function Id_P_Is_Type_Name (Node : Bare_Id) return Boolean is
      N_Text : constant Text_Type := Text (Node);
   begin
      --  Here is not the time to perform casing validation. Type names are
      --  supposed to have lower case, so we can just check if the first
      --  character has upper case.

      return N_Text'Length > 0 and then N_Text (N_Text'First) in 'A' .. 'Z';
   end Id_P_Is_Type_Name;

   -----------------------------------------------
   -- Lkt_Node_P_Internal_Fetch_Referenced_Unit --
   -----------------------------------------------

   function Lkt_Node_P_Internal_Fetch_Referenced_Unit
     (Node : Bare_Lkt_Node; Name : String_Type) return Internal_Unit
   is
      I : constant Internal_Unit := Get_From_Provider
        (Context => Node.Unit.Context,
         Name    => Name.Content,
         Kind    => Unit_Body,
         Charset => "ascii",
         Reparse => False);
   begin
      Populate_Lexical_Env (Wrap_Unit (I));
      return I;
   end Lkt_Node_P_Internal_Fetch_Referenced_Unit;

   -------------------------------------------------
   -- Single_Line_String_Lit_P_Is_Prefixed_String --
   -------------------------------------------------

   function Single_Line_String_Lit_P_Is_Prefixed_String
     (Node : Bare_Single_Line_String_Lit) return Boolean
   is
      Tok_Kind : constant Token_Kind :=
         Kind (Data (Token (Node, Node.Token_Start_Index)));
   begin
      return Tok_Kind = Lkt_P_String;
   end Single_Line_String_Lit_P_Is_Prefixed_String;

   -------------------------------------
   -- Single_Line_String_Lit_P_Prefix --
   -------------------------------------

   function Single_Line_String_Lit_P_Prefix
     (Node : Bare_Single_Line_String_Lit) return Character_Type
   is
      N_Text : constant Text_Type := Text (Node);
   begin
      return
        (if Single_Line_String_Lit_P_Is_Prefixed_String (Node) then
            N_Text (N_Text'First)
         else
            Character_Type'Val (0));
   end Single_Line_String_Lit_P_Prefix;

   ------------------------------
   -- Char_Lit_P_Denoted_Value --
   ------------------------------

   function Char_Lit_P_Denoted_Value
     (Node : Bare_Char_Lit) return Internal_Decoded_Char_Value
   is
      N_Text : constant Text_Type := Text (Node);
      pragma Assert (N_Text (N_Text'First) = ''');
      pragma Assert (N_Text (N_Text'Last) = ''');

      Cursor      : Positive := N_Text'First + 1;
      Cursor_Sloc : Source_Location := Start_Sloc (Sloc_Range (Node));
      Result      : Internal_Decoded_Char_Value;
   begin
      --  Before reading the denoted character, update Cursor_Sloc so that it
      --  corresponds to the character right after the opening single quote.

      Cursor_Sloc.Column := Cursor_Sloc.Column + 1;
      Read_Denoted_Char (N_Text, True, Cursor, Cursor_Sloc, Result);

      --  Ensure that reading one character has moved the cursor to the closing
      --  quote. If it is not the case, there are too many characters in this
      --  literal.

      if not Result.Has_Error and then Cursor /= N_Text'Last then
         Result.Has_Error := True;
         Result.Error_Sloc := Cursor_Sloc;
         Result.Error_Message :=
           Create_String ("exactly one character expected");
      end if;
      return Result;
   end Char_Lit_P_Denoted_Value;

   --------------------------------------
   -- Block_String_Lit_P_Denoted_Value --
   --------------------------------------

   function Block_String_Lit_P_Denoted_Value
     (Node : Bare_Block_String_Lit) return Internal_Decoded_String_Value is
   begin
      return Block_Denoted_String (Node.Block_String_Lit_F_Lines);
   end Block_String_Lit_P_Denoted_Value;

   -------------------------------------------
   -- Module_Doc_String_Lit_P_Denoted_Value --
   -------------------------------------------

   function Module_Doc_String_Lit_P_Denoted_Value
     (Node : Bare_Module_Doc_String_Lit) return Internal_Decoded_String_Value is
   begin
      return Block_Denoted_String (Node.Module_Doc_String_Lit_F_Lines);
   end Module_Doc_String_Lit_P_Denoted_Value;

   --------------------------------------------
   -- Single_Line_String_Lit_P_Denoted_Value --
   --------------------------------------------

   function Single_Line_String_Lit_P_Denoted_Value
     (Node : Bare_Single_Line_String_Lit) return Internal_Decoded_String_Value
   is
   begin
      return Common_Denoted_String (Node);
   end Single_Line_String_Lit_P_Denoted_Value;

   -------------------------------
   -- Token_Lit_P_Denoted_Value --
   -------------------------------

   function Token_Lit_P_Denoted_Value
     (Node : Bare_Token_Lit) return Internal_Decoded_String_Value is
   begin
      return Common_Denoted_String (Node);
   end Token_Lit_P_Denoted_Value;

   ---------------------------------------
   -- Token_Pattern_Lit_P_Denoted_Value --
   ---------------------------------------

   function Token_Pattern_Lit_P_Denoted_Value
     (Node : Bare_Token_Pattern_Lit) return Internal_Decoded_String_Value is
   begin
      return Common_Denoted_String (Node);
   end Token_Pattern_Lit_P_Denoted_Value;

end Liblktlang.Implementation.Extensions;
