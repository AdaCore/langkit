package Liblktlang.Implementation.Extensions is

   function Langkit_Root_P_Fetch_Prelude
     (Node : Bare_Langkit_Root) return Boolean;

   function Decl_Short_Image (Node : Bare_Decl) return Text_Type;
   --  Custom version of Short_Image for declarations. Include
   --  the names of the entities it declares.

   function Ref_Id_Short_Image (Node : Bare_Ref_Id) return Text_Type;
   --  Custom version of Short_Image for referencing identifiers. Include
   --  the identifier.

   function Lkt_Node_P_Env_From_Vals_Internal
     (Node : Bare_Lkt_Node;
      Vals : Internal_EnvKV_Array_Access) return Lexical_Env;

   function Lkt_Node_P_Internal_Fetch_Referenced_Unit
     (Node : Bare_Lkt_Node; Name : String_Type) return Internal_Unit;
   --  Return the unit that this name designates. Load it if needed.

   function String_Lit_P_Is_Prefixed_String
     (Node : Bare_String_Lit) return Boolean;
   --  Return whether this string is prefixed or not

   function String_Lit_P_Prefix
     (Node : Bare_String_Lit) return Character_Type;
   --  Return the prefix of this string

   function String_Lit_P_Denoted_Value
     (Node : Bare_String_Lit) return String_Type;
   --  Return the content of the given string literal node

end Liblktlang.Implementation.Extensions;
