


with Liblktlang.Common; use Liblktlang.Common;

private package Liblktlang.Lexer_State_Machine is

   use Support.Text;

   type Lexer_State is limited private;

   type Lexed_Token is record
      Kind : Token_Kind;
      --  Kind for the scanned token

      Text_First : Positive;
      Text_Last  : Natural;
      --  Index range in the lexer input for the text covered by this token
   end record;

   procedure Initialize
     (Self        : out Lexer_State;
      Input       : Text_Access;
      Input_First : Positive;
      Input_Last  : Natural);
   --  Create a lexer state to scan the given input. Self will keep a reference
   --  to Input to be used for each call to Next_Token, so the caller must keep
   --  it point to allocated memory.

   function Last_Token (Self : Lexer_State) return Lexed_Token;
   --  Return the last token that Self scanned. This is the termination token
   --  with the Input'First - 1 .. Input'Last index range when Next_Token
   --  wasn't called yet.

   function Has_Next (Self : Lexer_State) return Boolean;
   --  Return whether Self scanned the whole input buffer

   procedure Next_Token
     (Self : in out Lexer_State; Token : out Lexed_Token)
      with Pre => Has_Next (Self);
   --  Scan for the next token in Self. Store its kind and index range in the
   --  Input respectively in Kind, Text_First and Text_Last.

private

   type Lexer_State is limited record
      Input       : Text_Access;
      Input_First : Positive;
      Input_Last  : Natural;
      --  Input buffer and buffer bounds for the content to scan

      Has_Next   : Boolean;
      Last_Token : Lexed_Token;

      Last_Token_Kind : Token_Kind;
      --  Kind of the last actual token (not trivia) emitted
   end record;

end Liblktlang.Lexer_State_Machine;
