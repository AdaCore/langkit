--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package implement memoization tables for the langkit generated
--  parsers, which allows them to implement the packrat method of parsing.
--
--  See https://en.wikipedia.org/wiki/Parsing_expression_grammar for more
--  details.

generic
   type T is private;
   type Token_Index is range <>;
   Memo_Size : Positive := 16;
package Langkit_Support.Packrat is

   --  Those memo tables have a limited size, and use basic modulo to fit any
   --  offset in the limited size, so that an entry at index N will be put at
   --  index N mod Memo_Size.
   --
   --  If there was already an entry at this spot, it will simply be removed.
   --  When querying for the entry at a given offset, we check whether there
   --  is an entry corresponding to Offset mod Memo_Size, and then if the entry
   --  exists, whether is corresponds to the same offset.

   type Memo_State is (No_Result, Failure, Success);
   --  State of a memo entry. Whether we have a result or not.

   type Memo_Entry is record
      State             : Memo_State := No_Result;
      --  State of the memo entry

      Instance          : T;
      --  Parsed object

      Offset            : Token_Index := Token_Index'First;
      --  Real offset of this memo entry. Used to verify that it corresponds to
      --  the queried offset.

      Final_Pos         : Token_Index := Token_Index'First;
      --  Last token position for the given parsed object. Used to tell the
      --  parser where to start back parsing after getting the memoized object.
   end record;

   type Memo_Type is private;

   procedure Clear (Memo : in out Memo_Type);
   --  Clear the memo table, eg. reset it to a blank state for a new parsing
   --  session.

   function Get (Memo : Memo_Type; Offset : Token_Index) return Memo_Entry
     with Inline;
   --  Get the element at given offset in the memo table, if it exists

   procedure Set (Memo              : in out Memo_Type;
                  Is_Success        : Boolean;
                  Instance          : T;
                  Offset, Final_Pos : Token_Index)
     with Inline;
   --  Set the memo entry at given offset

private

   type Memo_Type is array (0 .. Memo_Size - 1) of Memo_Entry;

end Langkit_Support.Packrat;
