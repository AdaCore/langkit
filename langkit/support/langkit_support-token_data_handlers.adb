package body Langkit_Support.Token_Data_Handlers is

   function Internal_Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (TDH : out Token_Data_Handler; Symbols : Symbol_Table)
   is
   begin
      TDH := (Source_Buffer     => null,
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
      Source_Buffer : Text_Access) is
   begin
      Free (TDH.Source_Buffer);
      TDH.Source_Buffer := Source_Buffer;

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

      return Token_Index_Vectors.Elements_Arrays.Empty_Array;
   end Internal_Get_Trivias;

   -----------------
   -- Get_Trivias --
   -----------------

   function Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array is
   begin
      if Index = No_Token_Index then
         return Token_Index_Vectors.Elements_Arrays.Empty_Array;
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
