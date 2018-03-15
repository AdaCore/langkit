--  Test that the Equivalence function for lexical envs works properly

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

with Support; use Support;

procedure Main is
   use Envs;

   function "+" (S : String) return String_Access is (new String'(S));

   Symbols : Symbol_Table := Create;
   Key_X   : constant Symbol_Type := Find (Symbols, "X");
   Key_Y   : constant Symbol_Type := Find (Symbols, "Y");

   Old_Env_1 : Lexical_Env := Create
     (No_Env_Getter, +"Old_Env_1", Owner => True);
   New_Env_1 : Lexical_Env := Create
     (No_Env_Getter, +"New_Env_1", Owner => True);
   Old_Env_2 : Lexical_Env := Create
     (No_Env_Getter, +"Old_Env_2", Owner => True);
   New_Env_2 : Lexical_Env := Create
     (No_Env_Getter, +"New_Env_2", Owner => True);

   R1 : constant Env_Rebindings := Append (null, Old_Env_1, New_Env_1);
   R2 : constant Env_Rebindings := Append (R1, Old_Env_2, New_Env_2);

   Prim_A : Lexical_Env := Create
     (No_Env_Getter, +"Prim_A", Owner => True);
   Prim_B : Lexical_Env := Create
     (Simple_Env_Getter (Prim_A), +"Prim_B", Owner => True);

   Orphaned_1 : constant Lexical_Env := Orphan (Prim_B);
   Orphaned_2 : constant Lexical_Env := Orphan (Orphaned_1);

   Grouped: constant Lexical_Env :=
      Group ((Orphaned_1, Orphaned_2), (I => 1));

   Rebound : constant Lexical_Env := Rebind_Env (Prim_B, R2);

begin
   Add (Prim_A, Key_X, +"Item(X1)");
   Add (Prim_A, Key_X, +"Item(X2)");
   Add (Prim_A, Key_Y, +"Item(Y1)");
   Add (Prim_B, Key_Y, +"Item(Y2)");

   Put (Lexical_Env_Image (Prim_A));
   Put (Lexical_Env_Image (Prim_B));
   New_Line;
   Put (Lexical_Env_Image (Orphaned_1));
   Put (Lexical_Env_Image (Orphaned_2));
   New_Line;
   Put (Lexical_Env_Image (Grouped));
   New_Line;
   Put (Lexical_Env_Image (Rebound));

   New_Line;
   Put_Line ("Done.");
end Main;
