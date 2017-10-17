package body Langkit_Support.Hashes is

   -------------
   -- Combine --
   -------------

   function Combine (L, R : Hash_Type) return Hash_Type is
   begin
      --  TODO??? Look for a more clever way to combine hashes
      return L xor R;
   end Combine;

   -------------
   -- Combine --
   -------------

   function Combine (Hashes : Hash_Array) return Hash_Type is
      Result : Hash_Type := Initial_Hash;
   begin
      for H of Hashes loop
         Result := Combine (Result, H);
      end loop;
      return Result;
   end Combine;

end Langkit_Support.Hashes;
