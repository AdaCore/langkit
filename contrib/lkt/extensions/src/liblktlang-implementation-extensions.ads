package Liblktlang.Implementation.Extensions is

   function Langkit_Root_P_Fetch_Prelude
     (Node : Bare_Langkit_Root) return Boolean;

   function Decl_Short_Image (Node : Bare_Decl) return Text_Type;
   --  Custom version of Short_Image for declarations. Include
   --  the names of the entities it declares.

   function Ref_Id_Short_Image (Node : Bare_Ref_Id) return Text_Type;
   --  Custom version of Short_Image for referencing identifiers. Include
   --  the identifier.

   function Id_P_Is_Type_Name (Node : Bare_Id) return Boolean;
   --  Return whether this identifier refers to a type name

   function Lkt_Node_P_Env_From_Vals_Internal
     (Node : Bare_Lkt_Node;
      Vals : Internal_Env_Kv_Array_Access) return Lexical_Env;

   function Lkt_Node_P_Internal_Fetch_Referenced_Unit
     (Node : Bare_Lkt_Node; Name : String_Type) return Internal_Unit;
   --  Return the unit that this name designates. Load it if needed.

   function Single_Line_String_Lit_P_Is_Prefixed_String
     (Node : Bare_Single_Line_String_Lit) return Boolean;
   --  Return whether this single line string literal is prefixed

   function Single_Line_String_Lit_P_Prefix
     (Node : Bare_Single_Line_String_Lit) return Character_Type;
   --  Return the prefix of this single line string literal

   function Char_Lit_P_Denoted_Value
     (Node : Bare_Char_Lit) return Internal_Decoded_Char_Value;
   --  Return the content of the given character literal node

   function Block_String_Lit_P_Denoted_Value
     (Node : Bare_Block_String_Lit) return Internal_Decoded_String_Value;
   --  Return the content of the given block string literal node

   function Single_Line_String_Lit_P_Denoted_Value
     (Node : Bare_Single_Line_String_Lit) return Internal_Decoded_String_Value;
   --  Return the content of the given single line string literal node

   function Token_Lit_P_Denoted_Value
     (Node : Bare_Token_Lit) return Internal_Decoded_String_Value;
   --  Return the content of the given token literal node

   function Token_Pattern_Lit_P_Denoted_Value
     (Node : Bare_Token_Pattern_Lit) return Internal_Decoded_String_Value;
   --  Return the content of the given token pattern literal node

end Liblktlang.Implementation.Extensions;
