package body Libfoolang.Implementation.Extensions is

   -----------------
   -- Name_P_Pred --
   -----------------

   function Name_P_Pred (Node : Bare_Name) return Boolean is
      T : constant Text_Type := Text (Node);
   begin
      return T /= "" and then T (T'First) in 'A' .. 'Z';
   end Name_P_Pred;

end Libfoolang.Implementation.Extensions;
