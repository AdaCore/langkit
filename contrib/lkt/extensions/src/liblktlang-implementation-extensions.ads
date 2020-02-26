package Liblktlang.Implementation.Extensions is

   function Langkit_Root_P_Fetch_Prelude
     (Node : Bare_Langkit_Root) return Boolean;

   function Decl_Short_Image (Node : Bare_Decl) return Text_Type;
   --  Custom version of Short_Image for declarations. Include
   --  the names of the entities it declares.

   function LK_Node_P_Env_From_Vals_Internal
     (Node : Bare_LK_Node;
      Vals : Internal_EnvKV_Array_Access) return Lexical_Env;

end Liblktlang.Implementation.Extensions;
