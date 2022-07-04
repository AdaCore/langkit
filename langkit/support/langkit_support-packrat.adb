--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
