with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;
with Langkit_Support.Vectors;

generic
   type Token_Data_Type is private;
package Langkit_Support.Token_Data_Handlers is

   --  Trivias are tokens that are not to be taken into account during parsing,
   --  and are marked as so in the lexer definition. Conceptually, we want
   --  to keep a (potentially empty) list of trivias for each token, which
   --  is every trivia that is between the current token and the next token.

   type Trivia_Node is record
      T        : Token_Data_Type;
      Has_Next : Boolean;
   end record;
   --  This defines a node in a trivia linked list

   package Token_Vectors is new Langkit_Support.Vectors
     (Element_Type => Token_Data_Type);
   package Text_Vectors is new Langkit_Support.Vectors
     (Element_Type => Text_Access);
   package Trivia_Vectors is new Langkit_Support.Vectors
     (Element_Type => Trivia_Node);
   package Integer_Vectors is new Langkit_Support.Vectors
     (Element_Type => Integer);

   use Token_Vectors, Text_Vectors, Trivia_Vectors, Integer_Vectors;

   type Token_Index is new Integer range
      Token_Vectors.Index_Type'First - 1
      .. Token_Vectors.Index_Type'Last;
   --  Although we cannot use anything else than Natural as Token_Vectors
   --  indexes, this type will be used outside this package so that typing
   --  helps us fining index misuses.

   No_Token_Index    : constant Token_Index := Token_Index'First;
   First_Token_Index : constant Token_Index := Token_Index'First + 1;

   package Token_Index_Vectors is new Langkit_Support.Vectors
     (Element_Type => Token_Index);

   type Token_Data_Handler is record
      Source_Buffer     : Text_Access;
      --  The whole source buffer. It belongs to this token data handler, and
      --  will be deallocated along with it.

      Tokens            : Token_Vectors.Vector;
      --  Sequence of tokens in the same order as found in the source file

      Trivias           : Trivia_Vectors.Vector;
      --  Sequence of trivia in the same order as found in the source file.
      --  Trivia are stored in a way that is related to the neighbor tokens:
      --
      --  * If a token T0 at index I0 is followed by trivias T1, T2, ..., TN,
      --    then the (I0+1)'th entry in Tokens_To_Trivia will contain an index
      --    I1. T1 is then to be found in Trivia at index I1.
      --
      --    If it's the only trivia before the next token, then Has_Next is
      --    False for it, otherwise it is true and T2 is present at index I1+1.
      --    The same goes on for T2, ..., until TN, the last trivia before the
      --    next token, for which Has_Next is False.
      --
      --  * If T0 is not followed by any trivia before the next token, then
      --    the (I0+1)'th entry in Tokens_To_Trivia is No_Token_Index.

      Tokens_To_Trivias : Integer_Vectors.Vector;
      --  This is the correspondence map between regular tokens and trivias:
      --  see documentation for the Trivias field. Note that the first entry
      --  stands for the leading trivia, i.e. trivia that come before the first
      --  token, then the second entry stands for the trivia that come after
      --  the first token, and so on.

      Symbols           : Symbol_Table;
   end record;

   type Token_Data_Handler_Access is access all Token_Data_Handler;

   procedure Initialize
     (TDH           : out Token_Data_Handler;
      Symbols       : Symbol_Table;
      Source_Buffer : Text_Access := null);
   --  Create a token data handler that is associated with Symbols

   procedure Reset
     (TDH           : out Token_Data_Handler;
      Source_Buffer : Text_Access);
   --  Free TDH's source buffer, remove all its tokens and associate another
   --  source buffer to it. Unlike Free, this does not deallocate the vectors.
   --
   --  This is equivalent to calling Free and then Initialize on TDH except
   --  from the performance point of view: this re-uses allocated resources.

   procedure Free (TDH : in out Token_Data_Handler);
   --  Free all the resources allocated to TDH. After then, one must call
   --  Initialize again in order to use the TDH.

   function Get_Token
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Data_Type
   is
     (Token_Vectors.Get (TDH.Tokens, Natural (Index)));

   function Last_Token (TDH : Token_Data_Handler) return Token_Index
   is
     (Token_Index (Token_Vectors.Last_Index (TDH.Tokens)));

   function Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array;

   function Get_Leading_Trivias
     (TDH : Token_Data_Handler) return Token_Index_Vectors.Elements_Array;

end Langkit_Support.Token_Data_Handlers;
