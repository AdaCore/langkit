--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.

generic
   type Item_Type is private;
   --  Type for objects to get

   type Sequence_Type (<>) is private;
   --  Sequence of such items

   with function Length (S : Sequence_Type) return Natural;
   --  Return the length of a sequence, i.e. its element count

   with function Get (S : Sequence_Type; Index : Integer) return Item_Type;
   --  Return the Index'th item in a sequence. Index is zero-based. Behavior
   --  for out-of-bounds access is left unspecified.

function Liblktlang_Support.Relative_Get
  (S     : Sequence_Type;
   Index : Integer;
   Item  : out Item_Type)
   return Boolean;
--  If Index is positive, return the Index'th item in S into Item. Return the
--  item at (Length - Index - 1) element if it is negative. In any case, return
--  False when this performs an out-of-bound access (Item is not set, then).
--  Return True otherwise.
