with Ada.Directories; use Ada.Directories;

package body Libfoolang.Implementation.Extensions is

   function Token_Node_Short_Image (Node : Bare_Foo_Node) return Text_Type
   is ("<"
       & To_Text (Kind_Name (Node))
       & " """ & Text (Node) & """ "
       & To_Text (Simple_Name (Get_Filename (Unit (Node))))
       & ":" & To_Text (Image (Sloc_Range (Node)))
       & ">");

   ----------------------
   -- Name_Short_Image --
   ----------------------

   function Name_Short_Image (Node : Bare_Name) return Text_Type is
   begin
      return Token_Node_Short_Image (Node);
   end Name_Short_Image;

   -------------------------
   -- Literal_Short_Image --
   -------------------------

   function Literal_Short_Image (Node : Bare_Literal) return Text_Type is
   begin
      return Token_Node_Short_Image (Node);
   end Literal_Short_Image;

end Libfoolang.Implementation.Extensions;
