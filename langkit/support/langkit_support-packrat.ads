generic
   type T is private;
   type Token_Index is range <>;
   Memo_Size : Positive := 16;
package Langkit_Support.Packrat is

   type Memo_State is (No_Result, Failure, Success);

   type Memo_Entry is record
      State             : Memo_State := No_Result;
      Instance          : T;
      Offset, Final_Pos : Token_Index := Token_Index'First;
   end record;

   type Memo_Type is private;

   procedure Clear (Memo : in out Memo_Type);

   function Get (Memo : Memo_Type; Offset : Token_Index) return Memo_Entry
     with Inline;

   procedure Set (Memo              : in out Memo_Type;
                  Is_Success        : Boolean;
                  Instance          : T;
                  Offset, Final_Pos : Token_Index)
     with Inline;

private

   type Memo_Type is array (0 .. Memo_Size - 1) of Memo_Entry;

end Langkit_Support.Packrat;
