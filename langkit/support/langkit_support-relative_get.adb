function Langkit_Support.Relative_Get
  (S     : Sequence_Type;
   Index : Integer;
   Item  : out Item_Type)
   return Boolean
is
   L : constant Natural := Length (S);
begin
   if Index < -L or else L <= Index then
      return False;
   elsif Index < 0 then
      Item := Get (S, L - Index - 1);
   else
      Item := Get (S, Index);
   end if;
   return True;
end Langkit_Support.Relative_Get;
