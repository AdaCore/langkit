------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

package body Langkit_Support.Token_Data_Handlers is

   function Internal_Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array;

   procedure Compute_Lines_Starts (TDH : in out Token_Data_Handler)
     with Pre => TDH.Source_Buffer /= null;
   --  Compute the table of lines starting indices, that will allow us to
   --  go between offsets and line/columns.

   generic
      type Key_Type is private;
      --  Type of the value used to sort vector elements

      with package Element_Vectors is new Langkit_Support.Vectors (<>);

      with function Compare
        (K       : Key_Type;
         E_Index : Positive;
         E       : Element_Vectors.Element_Type)
          return Relative_Position is <>;
      --  Tell where K is with respect to E (E_Index is the index of E in the
      --  vector).
   function Floor
     (K : Key_Type; Vector : Element_Vectors.Vector) return Natural;
   --  Assuming that Vector contains a sorted sequence of elements, return the
   --  element index for which K is either inside or right after. If K actuals
   --  points right before the first Vector element, return the index of the
   --  first element.

   -----------
   -- Floor --
   -----------

   function Floor
     (K : Key_Type; Vector : Element_Vectors.Vector) return Natural
   is
      Before, After : Natural;
   begin
      if Vector.Is_Empty then
         return 0;
      end if;

      Before := Vector.First_Index;
      After  := Vector.Last_Index;
      while Before < After loop
         declare
            Last_Step : constant Boolean := Before + 1 = After;
            Middle    : constant Natural := (After + Before) / 2;
         begin
            case Compare (K, Middle, Vector.Get (Middle)) is
               when Slocs.Before =>
                  After := Middle;
               when Slocs.Inside =>
                  return Middle;
               when Slocs.After =>
                  Before := Middle;
            end case;

            if Last_Step then
               case Compare (K, After, Vector.Get (After)) is
                  when Slocs.Before =>
                     return Before;
                  when Slocs.Inside | Slocs.After =>
                     return After;
               end case;
            end if;
         end;
      end loop;

      return Before;
   end Floor;

   -----------------
   -- Initialized --
   -----------------

   function Initialized (TDH : Token_Data_Handler) return Boolean is
   begin
      return TDH.Symbols /= No_Symbol_Table;
   end Initialized;

   -----------------------
   -- Has_Source_Buffer --
   -----------------------

   function Has_Source_Buffer (TDH : Token_Data_Handler) return Boolean is
   begin
      return TDH.Source_Buffer /= null;
   end Has_Source_Buffer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (TDH      : out Token_Data_Handler;
      Symbols  : Symbol_Table;
      Tab_Stop : Positive := Default_Tab_Stop) is
   begin
      TDH := (Source_Buffer      => null,
              Source_First       => <>,
              Source_Last        => <>,
              Filename           => <>,
              Charset            => <>,
              Tokens             => <>,
              Symbols            => Symbols,
              Tokens_To_Trivias  => <>,
              Trivias            => <>,
              Lines_Starts       => <>,
              Tab_Stop           => Tab_Stop);
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (TDH           : in out Token_Data_Handler;
      Source_Buffer : Text_Access;
      Source_First  : Positive;
      Source_Last   : Natural) is
   begin
      Free (TDH.Source_Buffer);
      TDH.Source_Buffer := Source_Buffer;
      TDH.Source_First := Source_First;
      TDH.Source_Last := Source_Last;
      TDH.Lines_Starts.Clear;

      Compute_Lines_Starts (TDH);

      Clear (TDH.Tokens);
      Clear (TDH.Trivias);
      Clear (TDH.Tokens_To_Trivias);
   end Reset;

   --------------------------
   -- Compute_Lines_Starts --
   --------------------------

   procedure Compute_Lines_Starts (TDH : in out Token_Data_Handler) is
      T : Text_Type
      renames TDH.Source_Buffer (TDH.Source_First .. TDH.Source_Last);

      Idx : Natural := T'First - 1;
      --  Index in T of the newline character that ends the currently processed
      --  line.

      function Index
        (Buffer    : Text_Type;
         Char      : Wide_Wide_Character;
         Start_Pos : Positive) return Natural;
      --  Helper function. Return the index of the first occurence of ``Char``
      --  in ``Buffer (Start_Pos .. End_Pos)``, or ``0`` if not found.

      -----------
      -- Index --
      -----------

      function Index
        (Buffer    : Text_Type;
         Char      : Wide_Wide_Character;
         Start_Pos : Positive) return Natural
      is
      begin
         for I in Start_Pos .. T'Last loop
            if Buffer (I) = Char then
               return I;
            end if;
         end loop;
         return 0;
      end Index;

   begin
      TDH.Lines_Starts.Append (1);
      loop
         --  Search the index of the newline char that follows the current line
         Idx := Index (T, Chars.LF, Idx + 1);

         --  Append the index of the first character of line N+1 to
         --  Self.Line_Starts. This is the character at Idx+1.
         --
         --  For regular cases, this is Idx + 1. If no next newline found,
         --  emulate the presence of this trailing LF (at T'Last+1) and return.
         if Idx = 0 then
            Idx := T'Last + 1;
            TDH.Lines_Starts.Append (Idx + 1);
            return;
         end if;

         TDH.Lines_Starts.Append (Idx + 1);
      end loop;

   end Compute_Lines_Starts;

   ----------
   -- Free --
   ----------

   procedure Free (TDH : in out Token_Data_Handler) is
   begin
      Free (TDH.Source_Buffer);
      TDH.Tokens.Destroy;
      TDH.Trivias.Destroy;
      TDH.Tokens_To_Trivias.Destroy;
      TDH.Lines_Starts.Destroy;
      TDH.Symbols := No_Symbol_Table;
   end Free;

   ----------
   -- Move --
   ----------

   procedure Move (Destination, Source : in out Token_Data_Handler) is
   begin
      Destination := Source;
      Source := (Source_Buffer     => null,
                 Source_First      => <>,
                 Source_Last       => <>,
                 Filename          => <>,
                 Charset           => <>,
                 Tokens            => <>,
                 Symbols           => No_Symbol_Table,
                 Tokens_To_Trivias => <>,
                 Trivias           => <>,
                 Lines_Starts      => <>,
                 Tab_Stop          => <>);
   end Move;

   --------------------------
   -- Internal_Get_Trivias --
   --------------------------

   function Internal_Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array
   is
      subtype Index_Type is Trivia_Vectors.Index_Type;

      First_Trivia_Index : constant Token_Index :=
        (if Length (TDH.Tokens_To_Trivias) = 0
         then No_Token_Index
         else Token_Index (Get (TDH.Tokens_To_Trivias,
                                Index_Type (Index + 1))));
      Last_Trivia_Index  : Token_Index := First_Trivia_Index;

   begin
      if First_Trivia_Index /= No_Token_Index then
         while Get (TDH.Trivias, Index_Type (Last_Trivia_Index)).Has_Next loop
            Last_Trivia_Index := Last_Trivia_Index + 1;
         end loop;

         declare
            Trivia_Count : constant Natural :=
               Natural (Last_Trivia_Index) - Natural (First_Trivia_Index) + 1;
            Result       : Token_Index_Vectors.Elements_Array
              (1 .. Trivia_Count);
         begin
            for Index in First_Trivia_Index .. Last_Trivia_Index loop
               Result (Index_Type (Index - First_Trivia_Index + 1)) := Index;
            end loop;
            return Result;
         end;
      end if;

      return Token_Index_Vectors.Empty_Array;
   end Internal_Get_Trivias;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Stored_Token_Data is
   begin
      return Token_Vectors.Get (TDH.Tokens, Natural (Index));
   end Get_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (TDH : Token_Data_Handler) return Token_Index is
   begin
      return Token_Index (Token_Vectors.Last_Index (TDH.Tokens));
   end Last_Token;

   ---------------------------
   -- First_Token_Or_Trivia --
   ---------------------------

   function First_Token_Or_Trivia
     (TDH : Token_Data_Handler) return Token_Or_Trivia_Index is
   begin
      if TDH.Tokens_To_Trivias.Is_Empty
         or else TDH.Tokens_To_Trivias.First_Element
                 = Integer (No_Token_Index)
      then
         --  There is no leading trivia: return the first token

         return (if TDH.Tokens.Is_Empty
                 then No_Token_Or_Trivia_Index
                 else (Token_Index (TDH.Tokens.First_Index), No_Token_Index));

      else
         return (No_Token_Index, Token_Index (TDH.Tokens.First_Index));
      end if;
   end First_Token_Or_Trivia;

   --------------------------
   -- Last_Token_Or_Trivia --
   --------------------------

   function Last_Token_Or_Trivia
     (TDH : Token_Data_Handler) return Token_Or_Trivia_Index is
   begin
      if TDH.Tokens_To_Trivias.Is_Empty
         or else TDH.Tokens_To_Trivias.Last_Element = Integer (No_Token_Index)
      then
         --  There is no trailing trivia: return the last token

         return (if TDH.Tokens.Is_Empty
                 then No_Token_Or_Trivia_Index
                 else (Token_Index (TDH.Tokens.Last_Index), No_Token_Index));

      else
         return (No_Token_Index, Token_Index (TDH.Trivias.First_Index));
      end if;
   end Last_Token_Or_Trivia;

   ----------
   -- Next --
   ----------

   function Next
     (Token          : Token_Or_Trivia_Index;
      TDH            : Token_Data_Handler;
      Exclude_Trivia : Boolean := False) return Token_Or_Trivia_Index
   is
      function Next_Token
        (Current : Token_Or_Trivia_Index) return Token_Or_Trivia_Index
      is
        (if Current.Token < Token_Index (TDH.Tokens.Last_Index)
         then (Current.Token + 1, No_Token_Index)
         else No_Token_Or_Trivia_Index);
      --  Return a reference to the next token (not trivia) or no token if
      --  Token was the last one.

      function Next_Step
        (Current : Token_Or_Trivia_Index) return Token_Or_Trivia_Index;
      --  Compute what Next must return when called with Exclude_Trivia left
      --  to False.

      ---------------
      -- Next_Step --
      ---------------

      function Next_Step
        (Current : Token_Or_Trivia_Index) return Token_Or_Trivia_Index is
      begin
         if Current = No_Token_Or_Trivia_Index then
            return Current;
         end if;

         if Current.Trivia /= No_Token_Index then
            --  Current is a reference to a trivia: take the next trivia if it
            --  exists, or escalate to the next token otherwise.

            declare
               Tr : constant Trivia_Node :=
                  TDH.Trivias.Get (Natural (Current.Trivia));
            begin
               return (if Tr.Has_Next
                       then (Current.Token, Current.Trivia + 1)
                       else Next_Token (Current));
            end;

         else
            --  Thanks to the guard above, we cannot get to the declare block
            --  for the No_Token case, so if Token does not refers to a trivia,
            --  it must be a token.

            pragma Assert (Current.Token /= No_Token_Index);

            --  If there is no trivia, just go to the next token

            if TDH.Tokens_To_Trivias.Is_Empty then
               return Next_Token (Current);
            end if;

            --  If this token has trivia, return a reference to the first one,
            --  otherwise get the next token.

            declare
               Tr_Index : constant Token_Index := Token_Index
                 (TDH.Tokens_To_Trivias.Get (Natural (Current.Token) + 1));
            begin
               return (if Tr_Index = No_Token_Index
                       then Next_Token (Current)
                       else (Current.Token, Tr_Index));
            end;
         end if;
      end Next_Step;

      Result : Token_Or_Trivia_Index := Next_Step (Token);
   begin
      if not Exclude_Trivia then
         return Result;
      end if;
      while Result /= No_Token_Or_Trivia_Index
            and then Result.Trivia /= No_Token_Index
      loop
         Result := Next_Step (Result);
      end loop;
      return Result;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Token          : Token_Or_Trivia_Index;
      TDH            : Token_Data_Handler;
      Exclude_Trivia : Boolean := False) return Token_Or_Trivia_Index
   is
      function Next_Step
        (Current : Token_Or_Trivia_Index) return Token_Or_Trivia_Index;
      --  Compute what Previous must return when called with Exclude_Trivia
      --  left to False.

      ---------------
      -- Next_Step --
      ---------------

      function Next_Step
        (Current : Token_Or_Trivia_Index) return Token_Or_Trivia_Index is
      begin
         if Current = No_Token_Or_Trivia_Index then
            return Current;
         end if;

         if Current.Trivia = No_Token_Index then
            --  Current is a regular token, so the previous token is either the
            --  last trivia of the previous regular token, either the previous
            --  regular token itself.
            declare
               Prev_Trivia : Token_Index;
            begin
               --  Get the index of the trivia that is right before Current (if
               --  any).
               if TDH.Tokens_To_Trivias.Length = 0 then
                  Prev_Trivia := No_Token_Index;

               else
                  Prev_Trivia := Token_Index
                    (TDH.Tokens_To_Trivias.Get (Natural (Current.Token)));
                  while Prev_Trivia /= No_Token_Index
                           and then
                        TDH.Trivias.Get (Natural (Prev_Trivia)).Has_Next
                  loop
                     Prev_Trivia := Prev_Trivia + 1;
                  end loop;
               end if;

               --  If there is no such trivia and Current was the first one,
               --  then this was the start of the token stream: no previous
               --  token.
               if Prev_Trivia = No_Token_Index
                  and then Current.Token <= First_Token_Index
               then
                  return No_Token_Or_Trivia_Index;
               else
                  return (Current.Token - 1, Prev_Trivia);
               end if;
            end;

         --  Past this point: Current is known to be a trivia

         elsif Current.Trivia = First_Token_Index then
            --  This is the first trivia for some token, so the previous token
            --  cannot be a trivia.
            return (if Current.Token = No_Token_Index
                    then No_Token_Or_Trivia_Index
                    else (Current.Token, No_Token_Index));

         elsif Current.Token = No_Token_Index then
            --  This is a leading trivia and not the first one, so the previous
            --  token has to be a trivia.
            return (No_Token_Index, Current.Trivia - 1);

         --  Past this point: Current is known to be a trivia *and* it is not a
         --  leading trivia.

         else
            return (Current.Token,
                    (if TDH.Trivias.Get (Natural (Current.Trivia - 1)).Has_Next
                     then Current.Trivia - 1
                     else No_Token_Index));
         end if;
      end Next_Step;

      Result : Token_Or_Trivia_Index := Next_Step (Token);
   begin
      if not Exclude_Trivia then
         return Result;
      end if;
      while Result /= No_Token_Or_Trivia_Index
            and then Result.Trivia /= No_Token_Index
      loop
         Result := Next_Step (Result);
      end loop;
      return Result;
   end Previous;

   --------------------
   -- Previous_Token --
   --------------------

   function Previous_Token
     (Trivia : Token_Index; TDH : Token_Data_Handler) return Token_Index
   is

      function Compare
        (Key_Trivia    : Token_Index;
         Element_Index : Positive;
         Element       : Integer) return Relative_Position;

      -------------
      -- Compare --
      -------------

      function Compare
        (Key_Trivia    : Token_Index;
         Element_Index : Positive;
         Element       : Integer) return Relative_Position
      is
         Triv_Index : constant Natural := Natural (Key_Trivia);
      begin
         --  Index can be zero if the corresponding token is not followed by
         --  any trivia. In this case, rely on the sloc to compare them.
         if Element = 0 then
            declare
               Triv_Index     : constant Natural := Natural (Key_Trivia);
               Tok_Index      : constant Natural := Element_Index - 1;
               Key_Start_Sloc : constant Source_Location := Sloc_Start
                 (TDH, TDH.Trivias.Get (Triv_Index).T);
            begin
               return Compare
                 (Sloc_Range (TDH, TDH.Tokens.Get (Tok_Index)),
                  Key_Start_Sloc);
            end;
         end if;

         if Element < Triv_Index then
            return After;
         elsif Element > Triv_Index then
            return Before;
         else
            return Inside;
         end if;
      end Compare;

      function Token_Floor is new Floor
        (Key_Type        => Token_Index,
         Element_Vectors => Integer_Vectors);

   begin
      --  Perform a binary search over Tokens_To_Trivias to find the index of
      --  the Token that precedes Trivia.

      return Token_Index (Token_Floor (Trivia, TDH.Tokens_To_Trivias) - 1);
      --  Token_Floor deals with indexes for the Tokens_To_Trivia vector, so
      --  their corresponding to token indexes, but off by one (token index 1
      --  corresponding to Tokens_To_Trivia index 2, ...). Hence the X-1
      --  operation when returning.
   end Previous_Token;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (TDH : Token_Data_Handler; Sloc : Source_Location)
      return Token_Or_Trivia_Index
   is

      function Compare
        (Sloc        : Source_Location;
         Dummy_Index : Positive;
         Token       : Stored_Token_Data) return Relative_Position
      is (Compare (Sloc_Range (TDH, Token), Sloc));

      function Compare
        (Sloc        : Source_Location;
         Dummy_Index : Positive;
         Trivia      : Trivia_Node) return Relative_Position
      is (Compare (Sloc_Range (TDH, Trivia.T), Sloc));

      function Token_Floor is new Floor
        (Key_Type        => Source_Location,
         Element_Vectors => Token_Vectors);
      function Trivia_Floor is new Floor
        (Key_Type        => Source_Location,
         Element_Vectors => Trivia_Vectors);

      --  Look for a candidate token and a candidate trivia, then take the one
      --  that is the closest to Sloc.

      Token  : constant Natural := Token_Floor (Sloc, TDH.Tokens);
      Trivia : constant Natural := Trivia_Floor (Sloc, TDH.Trivias);

      function Result_From_Token return Token_Or_Trivia_Index is
        ((Token_Index (Token), No_Token_Index));
      function Result_From_Trivia return Token_Or_Trivia_Index is
        ((Previous_Token (Token_Index (Trivia), TDH), Token_Index (Trivia)));

   begin
      if Trivia = 0 then
         return Result_From_Token;
      elsif Token = 0 then
         return Result_From_Trivia;
      end if;

      declare
         function SS (Token : Stored_Token_Data) return Source_Location is
           (Sloc_Start (TDH, Token));

         Tok_Sloc  : constant Source_Location := SS (TDH.Tokens.Get (Token));
         Triv_Sloc : constant Source_Location :=
            SS (TDH.Trivias.Get (Trivia).T);
      begin
         if Tok_Sloc < Triv_Sloc then
            if Sloc < Triv_Sloc then
               return Result_From_Token;
            end if;
         elsif Tok_Sloc < Sloc or else Tok_Sloc = Sloc then
            return Result_From_Token;
         end if;

         return Result_From_Trivia;
      end;
   end Lookup_Token;

   ----------
   -- Data --
   ----------

   function Data
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Stored_Token_Data is
   begin
      return (if Token.Trivia = No_Token_Index
              then TDH.Tokens.Get (Natural (Token.Token))
              else TDH.Trivias.Get (Natural (Token.Trivia)).T);
   end Data;

   -----------------
   -- Get_Trivias --
   -----------------

   function Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array is
   begin
      if Index = No_Token_Index then
         return Token_Index_Vectors.Empty_Array;
      end if;
      return Internal_Get_Trivias (TDH, Index);
   end Get_Trivias;

   -------------------------
   -- Get_Leading_Trivias --
   -------------------------

   function Get_Leading_Trivias
     (TDH : Token_Data_Handler) return Token_Index_Vectors.Elements_Array is
   begin
      return Internal_Get_Trivias (TDH, No_Token_Index);
   end Get_Leading_Trivias;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (TDH : Token_Data_Handler; Line_Number : Positive) return Text_Type is
   begin
      --  Return slice from...
      return
        TDH.Source_Buffer
          (
           --  The first character in the requested line
           TDH.Lines_Starts.Get (Line_Number)

           ..

           --  The character before the LF that precedes the first character of
           --  the next line.
             TDH.Lines_Starts.Get (Line_Number + 1) - 2
          );

   end Get_Line;

   --------------
   -- Get_Sloc --
   --------------

   function Get_Sloc
     (TDH : Token_Data_Handler; Index : Natural) return Source_Location
   is
      function Compare
        (Sought      : Positive;
         Dummy_Index : Positive;
         Line_Start  : Positive) return Relative_Position
      is
        (if Sought > Line_Start then After
         elsif Sought = Line_Start then Inside
         else Before);

      function Get_Line_Index is new Floor (Positive, Index_Vectors);
      --  Return the index of the first character of Line `N` in a given
      --  `TDH.Line_Starts` vector.

      Column : Natural := 0;
      --  0 based column number

      Tab_Stop : Positive renames TDH.Tab_Stop;
   begin
      --  Allow 0 as an offset because it's a common value when the text buffer
      --  is empty: in that case just return a null source location.
      if Index = 0 then
         return No_Source_Location;
      end if;

      declare
         Line_Index  : constant Positive :=
           Get_Line_Index (Index, TDH.Lines_Starts);
         Line_Offset : constant Positive := TDH.Lines_Starts.Get (Line_Index);
      begin
         --  Allow a sloc pointing at the EOL char (hence the + 1)
         if Index > TDH.Source_Buffer'Last + 1 then
            raise Constraint_Error with "out of bound access";
         end if;

         --  Make horizontal tabulations move by stride of Tab_Stop columns, as
         --  usually implemented in code editors.
         for I in Line_Offset .. Natural'Min (Index, TDH.Source_Last) - 1 loop
            if TDH.Source_Buffer (I) = Chars.HT then
               Column := (Column + Tab_Stop) / Tab_Stop * Tab_Stop;
            else
               Column := Column + 1;
            end if;
         end loop;

         return Source_Location'
           (Line   => Line_Number (Line_Index),
            Column =>
              Column_Number
                (Natural'Max (Column + 1, Index - Line_Offset + 1)));
      end;
   end Get_Sloc;

   ----------------
   -- Sloc_Start --
   ----------------

   function Sloc_Start
     (TDH   : Token_Data_Handler;
      Token : Stored_Token_Data) return Source_Location is
   begin
      return Get_Sloc (TDH, Token.Source_First);
   end Sloc_Start;

   --------------
   -- Sloc_End --
   --------------

   function Sloc_End
     (TDH   : Token_Data_Handler;
      Token : Stored_Token_Data) return Source_Location is
   begin
      return Get_Sloc
        (TDH, Token.Source_Last + 1);
   end Sloc_End;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (TDH   : Token_Data_Handler;
      Token : Stored_Token_Data) return Source_Location_Range is
   begin
      return Make_Range (Sloc_Start (TDH, Token), Sloc_End (TDH, Token));
   end Sloc_Range;

end Langkit_Support.Token_Data_Handlers;
