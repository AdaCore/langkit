with Ada.Assertions; use Ada.Assertions;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces; use Interfaces;

with Liblktlang.Analysis;          use Liblktlang.Analysis;
with Liblktlang.Prelude;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;

package body Liblktlang.Implementation.Extensions is

   function Common_Denoted_String
     (Node : Bare_Lkt_Node) return Internal_Decoded_String_Value;
   --  Common implementation for the ``p_denoted_string`` property of all
   --  string/pattern literal nodes.

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

      --  Update Cursor_Sloc so that it reflects the location of N_Text
      --  (Cursor).

      Cursor_Sloc.Column :=
        Cursor_Sloc.Column
        + Column_Count (N_Text (N_Text'First .. Cursor), Tab_Stop);

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

   ----------------------------------
   -- Langkit_Root_P_Fetch_Prelude --
   ----------------------------------

   function Langkit_Root_P_Fetch_Prelude
     (Node : Bare_Langkit_Root) return Boolean
   is
      Ctx     : constant Analysis_Context := Wrap_Context (Node.Unit.Context);
      Prelude : Analysis_Unit;
   begin
      Prelude := Ctx.Get_From_File ("__prelude");
      if Prelude.Root = No_Lkt_Node then
         Prelude := Ctx.Get_From_Buffer
           ("__prelude", "ascii", Liblktlang.Prelude.Content);

         --  Check if we have syntactic or semantic errors in the prelude. If
         --  we do, raise an assertion error.

         if Prelude.Diagnostics'Length > 0 then
            for Diagnostic of Prelude.Diagnostics loop
               Put_Line (To_Pretty_String (Diagnostic));
            end loop;
            raise Assertion_Error with "Errors in prelude";
         end if;

         declare
            Sem_Results : constant Tree_Semantic_Result :=
              Prelude.Root.As_Langkit_Root.P_Check_Semantic;
         begin
            if Analysis.Has_Error (Sem_Results) then
               for R of Analysis.Results (Sem_Results) loop
                  if Analysis.Error_Message (R) /= "" then
                     Put_Line
                       (Image (Analysis.Node (R).Full_Sloc_Image
                               & Error_Message (R)));
                  end if;
               end loop;
               raise Assertion_Error with "Errors in prelude";
            end if;
         end;

         Populate_Lexical_Env (Prelude);
         return True;
      else
         return False;
      end if;
   end Langkit_Root_P_Fetch_Prelude;

   ------------------------
   -- Ref_Id_Short_Image --
   ------------------------

   function Ref_Id_Short_Image (Node : Bare_Ref_Id) return Text_Type is
   begin
      return
        "<" & To_Text (Kind_Name (Node))
        & " """ & Text (Node) & """ "
        & To_Text (Ada.Directories.Simple_Name (Get_Filename (Unit (Node))))
        & ":" & To_Text (Image (Sloc_Range (Node))) & ">";
   end Ref_Id_Short_Image;

   ----------------------
   -- Decl_Short_Image --
   ----------------------

   function Decl_Short_Image (Node : Bare_Decl) return Text_Type is
      Full_Name_Acc : String_Type := Dispatcher_Decl_P_Full_Name (Node);
      Full_Name     : constant Text_Type := Full_Name_Acc.Content;
      File_Name     : constant Text_Type :=
         To_Text (Ada.Directories.Simple_Name (Get_Filename (Unit (Node))));
   begin
      Dec_Ref (Full_Name_Acc);
      if File_Name = "__prelude" then
         return "<" & To_Text (Kind_Name (Node))
           & " prelude: """ & Full_Name & """>";
      else
         return "<" & To_Text (Kind_Name (Node)) & " """ & Full_Name & """ "
           & File_Name

           --  Don't show the sloc for function types, because it will be the
           --  root node's sloc, and thus will always change when we add stuff
           --  to the file, which is not helpful nor practical for tests.
           & (if Node.Kind = Lkt_Function_Type
              then ""
              else ":" & To_Text (Image (Sloc_Range (Node)))) & ">";
      end if;
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

   ---------------------------------------
   -- Lkt_Node_P_Env_From_Vals_Internal --
   ---------------------------------------

   function Lkt_Node_P_Env_From_Vals_Internal
     (Node : Bare_Lkt_Node;
      Vals : Internal_Env_Kv_Array_Access) return Lexical_Env
   is
      Ret : constant Lexical_Env :=
         Create_Static_Lexical_Env (Null_Lexical_Env, Node);
   begin

      for El of Vals.Items loop
         AST_Envs.Add (Ret, El.Key, El.Value);
      end loop;

      return Ret;
   end Lkt_Node_P_Env_From_Vals_Internal;

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

   -------------------------------------
   -- String_Lit_P_Is_Prefixed_String --
   -------------------------------------

   function String_Lit_P_Is_Prefixed_String
     (Node : Bare_String_Lit) return Boolean
   is
      Tok_Kind : constant Token_Kind :=
         Kind (Data (Token (Node, Node.Token_Start_Index)));
   begin
      return Tok_Kind = Lkt_P_String;
   end String_Lit_P_Is_Prefixed_String;

   -------------------------
   -- String_Lit_P_Prefix --
   -------------------------

   function String_Lit_P_Prefix
     (Node : Bare_String_Lit) return Character_Type
   is
      N_Text : constant Text_Type := Text (Node);
   begin
      return
        (if String_Lit_P_Is_Prefixed_String (Node) then
            N_Text (N_Text'First)
         else
            Character_Type'Val (0));
   end String_Lit_P_Prefix;

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

   --------------------------------
   -- String_Lit_P_Denoted_Value --
   --------------------------------

   function String_Lit_P_Denoted_Value
     (Node : Bare_String_Lit) return Internal_Decoded_String_Value is
   begin
      return Common_Denoted_String (Node);
   end String_Lit_P_Denoted_Value;

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
