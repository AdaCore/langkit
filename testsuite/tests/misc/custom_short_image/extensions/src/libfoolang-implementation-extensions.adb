package body Libfoolang.Implementation.Extensions is

   ----------------------
   -- Name_Short_Image --
   ----------------------

   function Name_Short_Image (Node : Bare_Name) return Text_Type is
   begin
      return "<Name """ & Text (Node) & """>";
   end Name_Short_Image;

end Libfoolang.Implementation.Extensions;
