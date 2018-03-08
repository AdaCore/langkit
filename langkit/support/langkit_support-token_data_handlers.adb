package body Langkit_Support.Token_Data_Handlers is

   function Internal_Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array;

   generic
      type Key_Type is private;
      --  Type of the value used to sort vector elements

      type Element_Type is private;
      --  Type for vector elements

      Small_Vector_Capacity : Natural := 0;
      with package Element_Vectors is new Langkit_Support.Vectors
        (Element_Type, Small_Vector_Capacity);

      with function Compare
         (K : Key_Type; E_Index : Positive; E : Element_Type)
          return Relative_Position is <>;
      --  Tell where K is with respect to E (E_Index is the index of E in the
      --  vector).
   function Floor
     (K : Key_Type; Vector : Element_Vectors.Vector) return Natural;
   --  Assuming that Vector contains a sorted sequence of elements, return the
   --  element index for which K is either inside or right after. If K actualls
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (TDH : out Token_Data_Handler; Symbols : Symbol_Table)
   is
   begin
      TDH := (Source_Buffer     => null,
              Source_First      => <>,
              Source_Last       => <>,
              Tokens            => <>,
              Symbols           => Symbols,
              Tokens_To_Trivias => <>,
              Trivias           => <>);
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (TDH           : out Token_Data_Handler;
      Source_Buffer : Text_Access;
      Source_First  : Positive;
      Source_Last   : Natural)
   is
   begin
      Free (TDH.Source_Buffer);
      TDH.Source_Buffer := Source_Buffer;
      TDH.Source_First := Source_First;
      TDH.Source_Last := Source_Last;

      Clear (TDH.Tokens);
      Clear (TDH.Trivias);
      Clear (TDH.Tokens_To_Trivias);
   end Reset;

   ----------
   -- Free --
   ----------

   procedure Free (TDH : in out Token_Data_Handler) is
   begin
      Free (TDH.Source_Buffer);
      Destroy (TDH.Tokens);
      Destroy (TDH.Trivias);
      Destroy (TDH.Tokens_To_Trivias);
      TDH.Symbols := No_Symbol_Table;
   end Free;

   ----------
   -- Move --
   ----------

   procedure Move (Destination, Source : in out Token_Data_Handler) is
   begin
      Destination := Source;
      Initialize (Source, No_Symbol_Table);
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
      Index : Token_Index) return Token_Data_Type is
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
      begin
         --  Index can be zero if the corresponding token is not followed by
         --  any trivia. In this case, rely on the sloc to compare them.
         if Element = 0 then
            declare
               Key_Start_Sloc : constant Source_Location := Start_Sloc
                 (Sloc_Range (TDH.Trivias.Get (Natural (Key_Trivia)).T));
            begin
               return Compare
                 (Sloc_Range (TDH.Tokens.Get (Element_Index - 1)),
                  Key_Start_Sloc);
            end;
         end if;

         declare
            Designated_Trivia : constant Natural :=
               TDH.Tokens_To_Trivias.Get (Element);
         begin
            if Designated_Trivia < Natural (Key_Trivia) then
               return After;
            elsif Designated_Trivia > Natural (Key_Trivia) then
               return Before;
            else
               return Inside;
            end if;
         end;
      end Compare;

      function Token_Floor is new Floor
        (Key_Type        => Token_Index,
         Element_Type    => Integer,
         Element_Vectors => Integer_Vectors);

   begin
      --  Perform a binary search over Tokens_To_Trivias to find the index of
      --  the Token that precedes Trivia.

      return Token_Index (Token_Floor (Trivia, TDH.Tokens_To_Trivias) - 1);
      --  Token_Floor deals with indexes for the Tokens_To_Trivia vector, so
      --  they corresponding to token indexes, but off by one (token index 1
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
        (Sloc  : Source_Location;
         Index : Positive;
         Token : Token_Data_Type) return Relative_Position
      is (Compare (Sloc_Range (Token), Sloc));

      function Compare
        (Sloc   : Source_Location;
         Index  : Positive;
         Trivia : Trivia_Node) return Relative_Position
      is (Compare (Sloc_Range (Trivia.T), Sloc));

      function Token_Floor is new Floor
        (Key_Type        => Source_Location,
         Element_Type    => Token_Data_Type,
         Element_Vectors => Token_Vectors);
      function Trivia_Floor is new Floor
        (Key_Type        => Source_Location,
         Element_Type    => Trivia_Node,
         Element_Vectors => Trivia_Vectors);

      --  Look for a candidate token and a candidate trivia, then take the one
      --  that is the closest to Sloc.

      Token  : constant Natural := Token_Floor (Sloc, TDH.Tokens);
      Trivia : constant Natural := Trivia_Floor (Sloc, TDH.Trivias);

      function Result_From_Token return Token_Or_Trivia_Index is
        ((Token_Index (Token), False));
      function Result_From_Trivia return Token_Or_Trivia_Index is
        ((Token_Index (Trivia), True));

   begin
      if Trivia = 0 then
         return Result_From_Token;
      elsif Token = 0 then
         return Result_From_Trivia;
      end if;

      declare
         function SS (Token : Token_Data_Type) return Source_Location is
           (Start_Sloc (Sloc_Range (Token)));

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

end Langkit_Support.Token_Data_Handlers;
