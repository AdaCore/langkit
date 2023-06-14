--  Test that the Equivalence function for lexical envs works properly

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Symbols;      use Langkit_Support.Symbols;

with Support; use Support;

procedure Main is
   use Envs;

   function "+" (S : String) return String_Access is (new String'(S));

   Name_Old_Env_1 : String_Access := +"Old_Env_1";
   Name_New_Env_1 : String_Access := +"New_Env_1";
   Name_Old_Env_2 : String_Access := +"Old_Env_2";
   Name_New_Env_2 : String_Access := +"New_Env_2";
   Name_Prim_A    : String_Access := +"Prim_A";
   Name_Prim_B    : String_Access := +"Prim_B";
   Name_Item_X1   : String_Access := +"Item(X1)";
   Name_Item_X2   : String_Access := +"Item(X2)";
   Name_Item_Y1   : String_Access := +"Item(Y1)";
   Name_Item_Y2   : String_Access := +"Item(Y2)";

   Symbols : Symbol_Table := Create_Symbol_Table;
   Key_X   : constant Thin_Symbol := Thin (Find (Symbols, "X"));
   Key_Y   : constant Thin_Symbol := Thin (Find (Symbols, "Y"));

   Old_Env_1 : Lexical_Env := Create_Lexical_Env
     (Null_Lexical_Env, Name_Old_Env_1,
      Owner => No_Generic_Unit,
      Sym_Table => Symbols);
   New_Env_1 : Lexical_Env := Create_Lexical_Env
     (Null_Lexical_Env, Name_New_Env_1,
      Owner => No_Generic_Unit,
      Sym_Table => Symbols);
   Old_Env_2 : Lexical_Env := Create_Lexical_Env
     (Null_Lexical_Env, Name_Old_Env_2,
      Owner => No_Generic_Unit,
      Sym_Table => Symbols);
   New_Env_2 : Lexical_Env := Create_Lexical_Env
     (Null_Lexical_Env, Name_New_Env_2,
      Owner => No_Generic_Unit,
      Sym_Table => Symbols);

   R1 : Env_Rebindings := Append (null, Old_Env_1, New_Env_1);
   R2 : Env_Rebindings := Append (R1, Old_Env_2, New_Env_2);

   Prim_A : Lexical_Env := Create_Lexical_Env
     (Null_Lexical_Env, Name_Prim_A,
      Owner => No_Generic_Unit,
      Sym_Table => Symbols);
   Prim_B : Lexical_Env := Create_Lexical_Env
     (Prim_A,
      Name_Prim_B,
      Owner => No_Generic_Unit,
      Sym_Table => Symbols);

   Orphaned_1 : Lexical_Env := Orphan (Prim_B);
   Orphaned_2 : Lexical_Env := Orphan (Orphaned_1);

   Grouped: Lexical_Env := Group ((Orphaned_1, Orphaned_2), (I => 1));

   Rebound : Lexical_Env := Rebind_Env (Prim_B, R2);

begin
   Add (Prim_A, Key_X, Name_Item_X1);
   Add (Prim_A, Key_X, Name_Item_X2);
   Add (Prim_A, Key_Y, Name_Item_Y1);
   Add (Prim_B, Key_Y, Name_Item_Y2);

   Put (Lexical_Env_Image (Prim_A));
   Put (Lexical_Env_Image (Prim_B));
   New_Line;
   Put (Lexical_Env_Image (Orphaned_1));
   Put (Lexical_Env_Image (Orphaned_2));
   New_Line;
   Put (Lexical_Env_Image (Grouped));
   New_Line;
   Put (Lexical_Env_Image (Rebound));

   Dec_Ref (Orphaned_1);
   Dec_Ref (Orphaned_2);
   Dec_Ref (Grouped);
   Dec_Ref (Rebound);

   Destroy (Old_Env_1);
   Destroy (New_Env_1);
   Destroy (Old_Env_2);
   Destroy (New_Env_2);

   Destroy (Prim_A);
   Destroy (Prim_B);

   R1.Children.Destroy;
   Destroy (R1);
   Destroy (R2);

   Destroy (Symbols);

   Destroy (Name_Old_Env_1);
   Destroy (Name_New_Env_1);
   Destroy (Name_Old_Env_2);
   Destroy (Name_New_Env_2);
   Destroy (Name_Prim_A);
   Destroy (Name_Prim_B);
   Destroy (Name_Item_X1);
   Destroy (Name_Item_X2);
   Destroy (Name_Item_Y1);
   Destroy (Name_Item_Y2);

   New_Line;
   Put_Line ("Done.");
end Main;
