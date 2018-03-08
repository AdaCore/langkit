with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;
with Langkit_Support.Vectors;

generic
   type Token_Data_Type is private;
   with function Sloc_Range
     (Token : Token_Data_Type) return Source_Location_Range is <>;
package Langkit_Support.Token_Data_Handlers is

   --  Trivias are tokens that are not to be taken into account during parsing,
   --  and are marked as so in the lexer definition. Conceptually, we want
   --  to keep a (potentially empty) list of trivias for each token, which
   --  is every trivia that is between the current token and the next token.

   type Trivia_Node is record
      T        : aliased Token_Data_Type;
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

   use Token_Vectors, Trivia_Vectors, Integer_Vectors;

   type Token_Index is new Integer range
      Token_Vectors.Index_Type'First - 1
      .. Token_Vectors.Index_Type'Last;
   --  Although we cannot use anything else than Natural as Token_Vectors
   --  indexes, this type will be used outside this package so that typing
   --  helps us finding index misuses.

   No_Token_Index    : constant Token_Index := Token_Index'First;
   First_Token_Index : constant Token_Index := Token_Index'First + 1;

   package Token_Index_Vectors is new Langkit_Support.Vectors
     (Element_Type => Token_Index);

   type Token_Data_Handler is record
      Source_Buffer : Text_Access;
      --  The whole source buffer. It belongs to this token data handler, and
      --  will be deallocated along with it.

      Source_First : Positive;
      Source_Last  : Natural;
      --  Actual bounds in Source_Buffer for the source text.
      --
      --  Because of Quex's hackish way of working, Source_Buffer actually has
      --  extra elements allocated for the lexer to work properly. These
      --  elements, at the beginning and at the end of Source_Buffer don't
      --  actually belong to the sources.

      Tokens : Token_Vectors.Vector;
      --  Sequence of tokens in the same order as found in the source file

      Trivias : Trivia_Vectors.Vector;
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

      Symbols : Symbol_Table;
   end record;

   type Token_Data_Handler_Access is access all Token_Data_Handler;

   procedure Initialize (TDH : out Token_Data_Handler; Symbols : Symbol_Table);
   --  Create a token data handler that is associated with Symbols

   procedure Reset
     (TDH           : out Token_Data_Handler;
      Source_Buffer : Text_Access;
      Source_First  : Positive;
      Source_Last   : Natural);
   --  Free TDH's source buffer, remove all its tokens and associate another
   --  source buffer to it. Unlike Free, this does not deallocate the vectors.
   --
   --  This is equivalent to calling Free and then Initialize on TDH except
   --  from the performance point of view: this re-uses allocated resources.

   procedure Free (TDH : in out Token_Data_Handler);
   --  Free all the resources allocated to TDH. After then, one must call
   --  Initialize again in order to use the TDH.

   procedure Move (Destination, Source : in out Token_Data_Handler);
   --  Move data from the Source handler to the Destination one. All data in
   --  Destination is overriden, so call Free on it first. Source is reset to
   --  null.

   function Get_Token
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Data_Type;
   --  Return data for the token at the given Index in TDH

   function Last_Token (TDH : Token_Data_Handler) return Token_Index;
   --  Return the index of the last token in TDH

   function Previous_Token
     (Trivia : Token_Index; TDH : Token_Data_Handler) return Token_Index;
   --  Given a trivia index in TDH, return the index of the token that precedes
   --  it. Return No_Token_Index for a leading trivia.

   function Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array;

   function Get_Leading_Trivias
     (TDH : Token_Data_Handler) return Token_Index_Vectors.Elements_Array;

end Langkit_Support.Token_Data_Handlers;
