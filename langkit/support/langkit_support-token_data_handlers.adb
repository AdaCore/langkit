package body Langkit_Support.Token_Data_Handlers is

   function Internal_Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Vectors.Elements_Array;

   procedure Free_String_Literals (TDH : in out Token_Data_Handler);
   --  Helper for Initialize and Free. Free all the string literals in TDH.
   --  This preserve the vector itself, though.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (TDH     : out Token_Data_Handler;
      Symbols : Symbol_Table) is
   begin
      TDH := (Tokens            => <>,
              Symbols           => Symbols,
              String_Literals   => <>,
              Tokens_To_Trivias => <>,
              Trivias           => <>);
   end Initialize;

   ----------------
   -- Add_String --
   ----------------

   function Add_String (TDH : in out Token_Data_Handler;
                        S   : Text_Type) return Text_Access is
      S_Access : constant Text_Access := new Text_Type'(S);
   begin
      Append (TDH.String_Literals, S_Access);
      return S_Access;
   end Add_String;

   -----------
   -- Reset --
   -----------

   procedure Reset (TDH : out Token_Data_Handler) is
   begin
      Free_String_Literals (TDH);
      Clear (TDH.Tokens);
      Clear (TDH.String_Literals);
      Clear (TDH.Trivias);
      Clear (TDH.Tokens_To_Trivias);
   end Reset;

   ----------
   -- Free --
   ----------

   procedure Free (TDH : in out Token_Data_Handler) is
   begin
      Free_String_Literals (TDH);
      Destroy (TDH.Tokens);
      Destroy (TDH.String_Literals);
      Destroy (TDH.Trivias);
      Destroy (TDH.Tokens_To_Trivias);
      TDH.Symbols := No_Symbol_Table;
   end Free;

   --------------------------
   -- Internal_Get_Trivias --
   --------------------------

   function Internal_Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Vectors.Elements_Arrays.Array_Type
   is
      subtype Index_Type is Trivia_Vectors.Index_Type;

      First_Trivia_Index : constant Token_Index :=
        (if Length (TDH.Tokens_To_Trivias) = 0
         then No_Token_Index
         else Token_Index (Get (TDH.Tokens_To_Trivias,
                                Index_Type (Index + 1))));

      function Extract (T : Trivia_Node) return Token_Data_Type
      is (T.T);

      function Map_Extract is new Trivia_Vectors.Elements_Arrays.Map_Gen
        (Token_Data_Type,
         Token_Vectors.Elements_Arrays.Array_Type, Extract);

      J : Token_Index;

   begin
      if First_Trivia_Index /= No_Token_Index then
         J := First_Trivia_Index;

         while Get (TDH.Trivias, Index_Type (J)).Has_Next loop
            J := J + 1;
         end loop;

         declare
            First : constant Index_Type := Index_Type (First_Trivia_Index);
            Last  : constant Index_Type := Index_Type (J);
         begin
            return Map_Extract (Slice (TDH.Trivias, First, Last));
         end;
      end if;

      return Token_Vectors.Elements_Arrays.Empty_Array;
   end Internal_Get_Trivias;

   -----------------
   -- Get_Trivias --
   -----------------

   function Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Vectors.Elements_Arrays.Array_Type
   is
   begin
      if Index = No_Token_Index then
         return Token_Vectors.Elements_Arrays.Empty_Array;
      end if;
      return Internal_Get_Trivias (TDH, Index);
   end Get_Trivias;

   -------------------------
   -- Get_Leading_Trivias --
   -------------------------

   function Get_Leading_Trivias
     (TDH : Token_Data_Handler) return Token_Vectors.Elements_Array is
   begin
      return Internal_Get_Trivias (TDH, No_Token_Index);
   end Get_Leading_Trivias;

   --------------------------
   -- Free_String_Literals --
   --------------------------

   procedure Free_String_Literals (TDH : in out Token_Data_Handler) is
   begin
      --  Iterate explicitely on indices rather than using the high-level
      --  iteration interface for performance.

      for J in First_Index (TDH.String_Literals)
               .. Last_Index (TDH.String_Literals)
      loop
         declare
            SL : Text_Access := Get (TDH.String_Literals, J);
         begin
            Free (SL);
         end;
      end loop;
   end Free_String_Literals;

end Langkit_Support.Token_Data_Handlers;
