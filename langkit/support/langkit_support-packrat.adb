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
      Self.Entries.Append
        (Diagnostic_Entry'
           (Previous => Mark.Index,
            Content  => (Is_Group => False,
                         Location => Location,
                         Message  => Message)));
      Mark.Index := Self.Entries.Last_Index;
   end Append;

   ------------------
   -- Append_Group --
   ------------------

   procedure Append_Group
     (Self       : in out Diagnostic_Pool;
      Mark       : in out Diagnostic_Mark;
      Group_Last : Diagnostic_Mark) is
   begin
      --  Do not bother creating a group if that would be an empty chain of
      --  diagnostics.

      if Group_Last = No_Diagnostic then
         return;
      end if;

      Self.Entries.Append
        (Diagnostic_Entry'
           (Previous => Mark.Index,
            Content  => (Is_Group => True, Target => Group_Last.Index)));
      Mark.Index := Self.Entries.Last_Index;
   end Append_Group;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Self    : Diagnostic_Pool;
      Mark    : Diagnostic_Mark;
      Process : access procedure (D : Diagnostic))
   is
      Stack : Integer_Vectors.Vector;

      procedure Visit (Index : Natural);

      -----------
      -- Visit --
      -----------

      procedure Visit (Index : Natural) is
         Cursor : Natural := Index;
      begin
         while Cursor > 0 loop
            declare
               E : Diagnostic_Entry renames Self.Entries (Cursor);
            begin
               if E.Content.Is_Group then
                  Visit (E.Content.Target);
               else
                  Stack.Append (Cursor);
               end if;
               Cursor := E.Previous;
            end;
         end loop;
      end Visit;
   begin
      Visit (Mark.Index);

      for Cursor of reverse Stack loop
         declare
            E : Diagnostic_Entry renames Self.Entries (Cursor);
         begin
            Process.all ((E.Content.Location, E.Content.Message));
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

      -----------------
      -- Set_Success --
      -----------------

      procedure Set_Success
        (Memo      : in out Memo_Type;
         Offset    : Token_Index;
         Instance  : T;
         Mark      : Diagnostic_Mark;
         Final_Pos : Token_Index)
      is
         E : Memo_Entry renames Memo (Entry_Index (Offset));
      begin
         E.State := Success;
         E.Instance := Instance;
         E.Mark := Mark;
         E.Offset := Offset;
         E.Final_Pos := Final_Pos;
      end Set_Success;

      -----------------
      -- Set_Failure --
      -----------------

      procedure Set_Failure
        (Memo              : in out Memo_Type;
         Offset            : Token_Index;
         Mark              : Diagnostic_Mark;
         Fail_Kind         : Fail_Info_Kind;
         Final_Pos         : Token_Index;
         Expected_Token_Id : Token_Kind;
         Found_Token_Id    : Token_Kind)
      is
         E : Memo_Entry renames Memo (Entry_Index (Offset));
      begin
         E.State := Failure;
         E.Fail_Kind := Fail_Kind;
         E.Instance := No_Instance;
         E.Mark := Mark;
         E.Offset := Offset;
         E.Final_Pos := Final_Pos;
         E.Expected_Token_Id := Expected_Token_Id;
         E.Found_Token_Id := Found_Token_Id;
      end Set_Failure;

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
