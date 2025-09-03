--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

package body Langkit_Support.Packrat is

   package Integer_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   ------------
   -- Append --
   ------------

   procedure Append
     (Self     : in out Diagnostic_Pool;
      Mark     : in out Diagnostic_Mark;
      Location : Source_Location_Range;
      Message  : Unbounded_Text_Type) is
   begin
      Self.Entries.Append (Diagnostic_Entry'(Mark.Index, Location, Message));
      Mark.Index := Self.Entries.Last_Index;
   end Append;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Self    : Diagnostic_Pool;
      Mark    : Diagnostic_Mark;
      Process : access procedure (D : Diagnostic))
   is
      Stack : Integer_Vectors.Vector;
   begin
      declare
         Cursor : Natural := Mark.Index;
      begin
         while Cursor > 0 loop
            Stack.Append (Cursor);
            Cursor := Self.Entries (Cursor).Previous;
         end loop;
      end;

      for Cursor of reverse Stack loop
         declare
            E : Diagnostic_Entry renames Self.Entries (Cursor);
         begin
            Process.all ((E.Location, E.Message));
         end;
      end loop;
   end Iterate;

   package body Tables is
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

      function Get (Memo : Memo_Type; Offset : Token_Index) return Memo_Entry
      is
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

      -------------
      -- Iterate --
      -------------

      procedure Iterate
        (Memo : Memo_Type; Process : access procedure (E : Memo_Entry)) is
      begin
         for E of Memo loop
            if E.State /= No_Result then
               Process.all (E);
            end if;
         end loop;
      end Iterate;

   end Tables;

end Langkit_Support.Packrat;
