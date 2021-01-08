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

with Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;
with Langkit_Support.Vectors;

package Langkit_Support.Token_Data_Handlers is

   type Raw_Token_Kind is new Natural;
   --  Kind for a token, stored as a mere number

   type Stored_Token_Data is record
      Kind : Raw_Token_Kind;

      Source_First : Positive;
      Source_Last  : Natural;
      --  Bounds in the source buffer corresponding to this token

      Symbol : Thin_Symbol;
      --  Depending on the token kind (according to the lexer specification),
      --  this is either null or the symbolization of the token text.
      --
      --  For instance: null for keywords but actual text for identifiers.
   end record with Pack;
   --  Holder for per-token data to be stored in the token data handler

   --  Trivias are tokens that are not to be taken into account during parsing,
   --  and are marked as so in the lexer definition. Conceptually, we want
   --  to keep a (potentially empty) list of trivias for each token, which
   --  is every trivia that is between the current token and the next token.

   type Trivia_Node is record
      T        : aliased Stored_Token_Data;
      Has_Next : Boolean;
   end record with Pack;
   --  This defines a node in a trivia linked list

   package Token_Vectors is new Langkit_Support.Vectors
     (Element_Type => Stored_Token_Data);
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

   type Token_Or_Trivia_Index is record
      Token, Trivia : Token_Index;
      --  Indices that identify what this refers to.
      --
      --  * If this references a token, then Token is the corresponding index
      --    in TDH.Tokens and Trivia is No_Token_Index.
      --
      --  * If this references a trivia that comes before the first token,
      --    Token is No_Token_Index while Trivia is the corresponding index in
      --    TDH.Trivias.
      --
      --  * If this references a trivia that comes after some token, Token is
      --    the index for this token and Trivia is the corresponding index for
      --    this trivia.
      --
      --  * If this references no token, both Token and Trivia are
      --    No_Token_Index.
   end record;

   No_Token_Or_Trivia_Index : constant Token_Or_Trivia_Index :=
     (No_Token_Index, No_Token_Index);

   package Token_Index_Vectors is new Langkit_Support.Vectors
     (Element_Type => Token_Index);

   package Index_Vectors is new Langkit_Support.Vectors (Positive);

   type Token_Data_Handler is record
      Source_Buffer : Text_Access;
      --  The whole source buffer. It belongs to this token data handler,
      --  and will be deallocated along with it. WARNING: this buffer might
      --  actually be *larger* than the real source, which is why we have the
      --  ``Source_First``/``Source_Last`` fields below. We allocate a bigger
      --  buffer pessimistically so we don't have to have a growable buffer.

      Source_First : Positive;
      Source_Last  : Natural;
      --  Actual bounds in Source_Buffer for the source text

      Filename : GNATCOLL.VFS.Virtual_File;
      --  If the source buffer comes from a file, Filename contains the name of
      --  that file. No_File otherwise.

      Charset : Ada.Strings.Unbounded.Unbounded_String;
      --  If the source buffer was decoded, charset that was used to do so.
      --  Empty string otherwise.

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
      --  Symbol table for this handler. Note that this can be shared accross
      --  multiple Token_Data_Handlers.

      Lines_Starts : Index_Vectors.Vector;
      --  Table keeping count of line starts and line endings. The index of the
      --  starting character for line N is at the Nth position in the vector.

      Tab_Stop : Positive;
   end record;

   Default_Tab_Stop : constant Positive := 8;
   --  Value that will be used for the default tab stop if none is passed
   --  during the initialization of a ``Token_Data_Handler``.

   type Token_Data_Handler_Access is access all Token_Data_Handler;

   function Initialized (TDH : Token_Data_Handler) return Boolean;
   --  Return whether TDH has been initialized (see the Initialize procedure)

   function Has_Source_Buffer (TDH : Token_Data_Handler) return Boolean
      with Pre => Initialized (TDH);
   --  Return whether TDH was used to lex some input source

   procedure Initialize
     (TDH      : out Token_Data_Handler;
      Symbols  : Symbol_Table;
      Tab_Stop : Positive := Default_Tab_Stop)
      with Pre  => Symbols /= No_Symbol_Table,
           Post => Initialized (TDH) and then not Has_Source_Buffer (TDH);
   --  Create a token data handler that is associated with the ``Symbols``
   --  symbol table, and takes its value for the tabulation in the ``Tab_Stop``
   --  access.

   procedure Reset
     (TDH           : in out Token_Data_Handler;
      Source_Buffer : Text_Access;
      Source_First  : Positive;
      Source_Last   : Natural)
      with Pre => Initialized (TDH);
   --  Free TDH's source buffer, remove all its tokens and associate another
   --  source buffer to it. Unlike Free, this does not deallocate the vectors.
   --
   --  This is equivalent to calling Free and then Initialize on TDH except
   --  from the performance point of view: this re-uses allocated resources.

   procedure Free (TDH : in out Token_Data_Handler)
      with Post => not Initialized (TDH);
   --  Free all the resources allocated to TDH. After then, one must call
   --  Initialize again in order to use the TDH.

   procedure Move (Destination, Source : in out Token_Data_Handler)
      with Pre  => Initialized (Source) and then not Initialized (Destination),
           Post => Initialized (Destination) and then not Initialized (Source);
   --  Move data from the Source handler to the Destination one. All data in
   --  Destination is overriden, so call Free on it first. Source is reset to
   --  null.

   function Get_Token
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Stored_Token_Data;
   --  Return data for the token at the given Index in TDH

   function Last_Token (TDH : Token_Data_Handler) return Token_Index;
   --  Return the index of the last token in TDH

   function First_Token_Or_Trivia
     (TDH : Token_Data_Handler) return Token_Or_Trivia_Index;
   --  Return the first element in the logical sequence of tokens/trivias in
   --  TDH.

   function Last_Token_Or_Trivia
     (TDH : Token_Data_Handler) return Token_Or_Trivia_Index;
   --  Return the last element in the logical sequence of tokens/trivias in
   --  TDH.

   function Next
     (Token          : Token_Or_Trivia_Index;
      TDH            : Token_Data_Handler;
      Exclude_Trivia : Boolean := False) return Token_Or_Trivia_Index;
   --  Return the element that follows Token in the logical sequence of
   --  tokens/trivias that TDH holds. If Exclude_Trivia is true, look for
   --  tokens only. This just returns No_Token_Or_Trivia_Index if Token is
   --  No_Token_Or_Trivia_Index or if it is the last sequence item.

   function Previous
     (Token          : Token_Or_Trivia_Index;
      TDH            : Token_Data_Handler;
      Exclude_Trivia : Boolean := False) return Token_Or_Trivia_Index;
   --  Return the element that precedes Token in the logical sequence of
   --  tokens/trivias that TDH holds. If Exclude_Trivia is true, look for
   --  tokens only. This just returns No_Token_Or_Trivia_Index if Token is
   --  No_Token_Or_Trivia_Index or if it is the first sequence item.

   function Previous_Token
     (Trivia : Token_Index; TDH : Token_Data_Handler) return Token_Index;
   --  Given a trivia index in TDH, return the index of the token that precedes
   --  it. Return No_Token_Index for a leading trivia.

   function Lookup_Token
     (TDH : Token_Data_Handler; Sloc : Source_Location)
      return Token_Or_Trivia_Index;
   --  Look for a token in TDH that contains the given source location.
   --
   --  We consider here both regular tokens and trivia as "tokens", both making
   --  a logically continuous stream of token/trivia.
   --
   --  If Sloc falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in TDH, return No_Token_Or_Trivia_Index.

   function Data
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Stored_Token_Data;
   --  Return the data associated to Token in TDH

   function Get_Trivias
     (TDH   : Token_Data_Handler;
      Index : Token_Index) return Token_Index_Vectors.Elements_Array;

   function Get_Leading_Trivias
     (TDH : Token_Data_Handler) return Token_Index_Vectors.Elements_Array;

   function Text
     (TDH : Token_Data_Handler;
      T   : Stored_Token_Data) return Text_Type
   is (TDH.Source_Buffer.all (T.Source_First .. T.Source_Last));
   --  Return the text associated to T, a token that belongs to TDH

   function Image
     (TDH : Token_Data_Handler;
      T   : Stored_Token_Data) return String
   is (Image (Text (TDH, T)));
   --  Debug helper: return a human-readable representation of T, a token that
   --  belongs to TDH.

   function Get_Line
     (TDH : Token_Data_Handler; Line_Number : Positive) return Text_Type;
   --  Get the source text of line at index ``Line_Number``

   function Get_Sloc
     (TDH : Token_Data_Handler; Index : Natural) return Source_Location;
   --  Return the sloc for given ``Index`` in ``TDH``. If:
   --
   --  - ``Index`` is ``0``, return ``No_Source_Location``.
   --
   --  - ``Index`` is in range ``1 .. TDH.Source_Buffer'Last + 1``, return a
   --    corresponding sloc (``TDH.Source_Buffer'Last + 1`` being the EOF
   --    sloc).
   --
   --  - ``Index`` is bigger than ``TDH.Source_Buffer'Last + 1``: raise a
   --    ``Constraint_Error``.

   function Sloc_Start
     (TDH   : Token_Data_Handler;
      Token : Stored_Token_Data) return Source_Location;
   --  Get the starting sloc for given ``Token`` in ``TDH``

   function Sloc_End
     (TDH   : Token_Data_Handler;
      Token : Stored_Token_Data) return Source_Location;
   --  Get the end sloc for given ``Token`` in ``TDH``

   function Sloc_Range
     (TDH   : Token_Data_Handler;
      Token : Stored_Token_Data) return Source_Location_Range;
   --  Get the sloc range for given ``Token`` in ``TDH``

end Langkit_Support.Token_Data_Handlers;
