--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.
--
--  This package implement memoization tables for the langkit generated
--  parsers, which allows them to implement the packrat method of parsing.
--
--  See https://en.wikipedia.org/wiki/Parsing_expression_grammar for more
--  details.

with Ada.Containers.Vectors;

with Liblktlang_Support.Diagnostics; use Liblktlang_Support.Diagnostics;
with Liblktlang_Support.Slocs;       use Liblktlang_Support.Slocs;
with Liblktlang_Support.Text;        use Liblktlang_Support.Text;
with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;

package Liblktlang_Support.Packrat is

   ---------------------
   -- Diagnostic pool --
   ---------------------

   --  Because of the memoization of parsing results in Packrat tables, it is
   --  common to have different memoization entries (parsing results) that have
   --  a common prefix list of diagnostics. For instance::
   --
   --     Parsing at token index T1:
   --       [expr_parser] <ExprA>
   --         error: 10:4: cannot parse <expr>
   --         error: 10:14: expected Identifier, got '('
   --       [stmt_parser] <StmtA>
   --         error: 10:4: cannot parse <stmt>
   --         error: 10:14: expected Identifier, got '('
   --         error: 10:28: missing expected ';'
   --
   --  The obvious way to represent this is to store each vector of diagnostic
   --  along with the result node in each memoization entry, but this is
   --  inefficient, as a lot of diagnostics are duplicated.
   --
   --  To avoid duplication, the diagnostic pool data structure stores (as much
   --  as possible) deduplicated chains of diagnostics, with "previous" links:
   --  it is enough to have a diagnostic mark (a reference to a specific
   --  diagnostic in the pool) to recover the vector of diagnostics: just
   --  following the "previous" links yields a list of diagnostics in reverse
   --  order.
   --
   --  For the above example, we get instead::
   --
   --     Diagnostic pool:
   --       [I1, previous:none] 10:4: cannot parse <stmt>
   --       ...
   --       [I2, previous:I1]   10:14: expected Identifier, got '('
   --       ...
   --       [I3, previous:I3]   10:28: missing expected ';'
   --
   --     Parsing at token index T1:
   --       [expr_parser] <ExprA> [diagnostic I2]
   --       [stmt_parser] <StmtA> [diagnostic I3]

   type Diagnostic_Pool is private;
   --  Storage for chains of diagnostics

   type Diagnostic_Mark is private;
   --  Nullable reference to a single diagnostic in a ``Diagnostic_Pool``

   No_Diagnostic : constant Diagnostic_Mark;
   --  Special diagnostic mark to denote the absence of diagnostic

   procedure Append
     (Self     : in out Diagnostic_Pool;
      Mark     : in out Diagnostic_Mark;
      Location : Source_Location_Range;
      Message  : Unbounded_Text_Type);
   --  Allocate a new diagnostic to the pool ``Self`` with ``Mark`` being its
   --  previous diagnostic. ``Mark`` is updated to refer to the new diagnostic.
   --
   --  ``Location`` and ``Message`` are the diagnostic details.

   procedure Append_Group
     (Self       : in out Diagnostic_Pool;
      Mark       : in out Diagnostic_Mark;
      Group_Last : Diagnostic_Mark);
   --  Append the full chain of diagnostics referenced by ``Group_Last`` at the
   --  end of the chain of diagnostics referenced by ``Mark``, and update
   --  ``Mark`` to denote the updated chain of diagnostics.
   --
   --  Since diagnostics are represented using linked lists, this creates a
   --  "group" entry in the diagnostic pool so that this operation has constant
   --  time.

   procedure Iterate
     (Self    : Diagnostic_Pool;
      Mark    : Diagnostic_Mark;
      Process : access procedure (D : Diagnostic));
   --  Call ``Process`` on all diagnostics in the chain that ``Mark``
   --  designates. The callback is invoked in the natural order, i.e. ``Mark``
   --  comes last.

   ------------------------
   -- Memoization tables --
   ------------------------

   type Memo_State is (No_Result, Failure, Success);
   --  State of a memo entry: whether we have a result or not

   type Fail_Info_Kind is (Token_Fail, Predicate_Fail);
   --  Kind of parsing failure:
   --
   --  * ``Token_Fail``: actual/expected token kind mismatch.
   --  * ``Predicate_Fail``: miscellaneous failure.

   generic
      type T is private;
      No_Instance : T;
      type Token_Kind is (<>);
      Memo_Size : Positive := 16;
   package Tables is

      --  Those memo tables have a limited size, and use basic modulo to fit
      --  any offset in the limited size, so that an entry at index N will be
      --  put at index N mod Memo_Size.
      --
      --  If there was already an entry at this spot, it will simply be
      --  removed.  When querying for the entry at a given offset, we check
      --  whether there is an entry corresponding to Offset mod Memo_Size, and
      --  then if the entry exists, whether is corresponds to the same offset.

      type Memo_Entry is record
         State : Memo_State := No_Result;
         --  State of the memo entry

         Fail_Kind : Fail_Info_Kind := Token_Fail;
         --  If ``State`` is ``Failure``, specifies the kind of parsing failure
         --  involved. Unused otherwise.

         Instance : T := No_Instance;
         --  Parsed object

         Mark : Diagnostic_Mark := No_Diagnostic;

         Offset : Token_Index := No_Token_Index;
         --  Real offset of this memo entry. Used to verify that it corresponds
         --  to the queried offset.

         Final_Pos : Token_Index := No_Token_Index;
         --  If ``State`` is ``Success``, This is used to tell the parser where
         --  to start back parsing after getting the memoized object.
         --
         --  If ``State`` is ``Failure``, index for the first token on which
         --  parsing failed.

         Expected_Token_Id, Found_Token_Id : Token_Kind := Token_Kind'First;
         --  If ``Fail_Kind`` is ``Token_Fail``, token kinds for
         --  expected/actual tokens.
      end record;

      type Memo_Type is private;

      procedure Clear (Memo : in out Memo_Type);
      --  Clear the memo table, eg. reset it to a blank state for a new parsing
      --  session.

      function Get (Memo : Memo_Type; Offset : Token_Index) return Memo_Entry
        with Inline;
      --  Get the element at given offset in the memo table, if it exists

      procedure Set_Success
        (Memo      : in out Memo_Type;
         Offset    : Token_Index;
         Instance  : T;
         Mark      : Diagnostic_Mark;
         Final_Pos : Token_Index)
        with Inline;
      --  Associate a successful parsing result to the token index at
      --  ``Offset``.

      procedure Set_Failure
        (Memo              : in out Memo_Type;
         Offset            : Token_Index;
         Mark              : Diagnostic_Mark;
         Fail_Kind         : Fail_Info_Kind;
         Final_Pos         : Token_Index;
         Expected_Token_Id : Token_Kind;
         Found_Token_Id    : Token_Kind)
        with Inline;
      --  Associate a failed parsing result to the token index at ``Offset``

      procedure Iterate
        (Memo : Memo_Type; Process : access procedure (E : Memo_Entry));
      --  Call ``Process`` for all ``Failure`` or ``Success`` entries in
      --  ``Memo``.

   private
      type Memo_Type is array (0 .. Memo_Size - 1) of Memo_Entry;
   end Tables;

private

   type Diagnostic_Entry_Content (Is_Group : Boolean := False) is record
      case Is_Group is
         when False =>
            Location : Source_Location_Range;
            Message  : Unbounded_Text_Type;
            --  Source location and message for the diagnostic

         when True =>
            Target : Positive;
            --  Index of the "last" diagnostic for this group (the group also
            --  includes all its predecessors in the chain). Note that we do
            --  not allow empty groups, hence the ``Positive`` index instead of
            --  ``Natural``.
      end case;
   end record;

   type Diagnostic_Entry is record
      Previous : Natural;
      --  Index of the previous diagnostic in the chain

      Content : Diagnostic_Entry_Content;
   end record;

   package Diagnostic_Vectors is new Ada.Containers.Vectors
     (Positive, Diagnostic_Entry);

   type Diagnostic_Pool is record
      Entries : Diagnostic_Vectors.Vector;
   end record;

   type Diagnostic_Mark is record
      Index : Natural;
   end record;

   No_Diagnostic : constant Diagnostic_Mark := (Index => 0);

end Liblktlang_Support.Packrat;
