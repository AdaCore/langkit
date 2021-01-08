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

package body Langkit_Support.Packrat is

   function Entry_Index (Offset : Token_Index) return Natural is
     (Integer (Offset) mod Memo_Size);

   -----------
   -- Clear --
   -----------

   procedure Clear (Memo : in out Memo_Type) is
   begin
      for E of Memo loop
         E.State := No_Result;
      end loop;
   end Clear;

   ---------
   -- Get --
   ---------

   function Get (Memo : Memo_Type; Offset : Token_Index) return Memo_Entry is
      E : Memo_Entry renames Memo (Entry_Index (Offset));
   begin
      if E.Offset = Offset then
         return E;
      else
         return (State => No_Result, others => <>);
      end if;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Memo              : in out Memo_Type;
                  Is_Success        : Boolean;
                  Instance          : T;
                  Offset, Final_Pos : Token_Index)
   is
      E : Memo_Entry renames Memo (Entry_Index (Offset));
   begin
      E := (State     => (if Is_Success then Success else Failure),
            Instance  => Instance,
            Offset    => Offset,
            Final_Pos => Final_Pos);
   end Set;

end Langkit_Support.Packrat;
