package Liblktlang.Implementation.Extensions is

   function Langkit_Root_P_Fetch_Prelude
     (Node : Bare_Langkit_Root) return Boolean;

   function Decl_Short_Image (Node : Bare_Decl) return Text_Type;
   --  Custom version of Short_Image for declarations. Include
   --  the names of the entities it declares.

end Liblktlang.Implementation.Extensions;
