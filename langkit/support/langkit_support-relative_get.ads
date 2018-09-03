------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

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

function Langkit_Support.Relative_Get
  (S     : Sequence_Type;
   Index : Integer;
   Item  : out Item_Type)
   return Boolean;
--  If Index is positive, return the Index'th item in S into Item. Return the
--  item at (Length - Index - 1) element if it is negative. In any case, return
--  False when this performs an out-of-bound access (Item is not set, then).
--  Return True otherwise.
