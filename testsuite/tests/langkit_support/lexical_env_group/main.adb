--  Test that lookups on grouped lexical envs works correctly

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Text;    use Langkit_Support.Text;

with Support; use Support;
use Support.Envs;
use Support.Symbols;

procedure Main is
   Symbols : Symbol_Table := Create_Symbol_Table;
   Key_X   : constant Symbol_Type := Find (Symbols, "X");

   A_Parent : Lexical_Env := Create_Lexical_Env
     (No_Env_Getter, 'P', Owner => True);
   A        : Lexical_Env := Create_Lexical_Env
     (Simple_Env_Getter (A_Parent), 'A', Owner => True);
   B        : Lexical_Env := Create_Lexical_Env
     (No_Env_Getter, 'B', Owner => True);

   Grouped : Lexical_Env := Group ((A, B));
begin
   Add (A_Parent, Key_X, '1');
   Add (A, Key_X, '2');
   Add (B, Key_X, '3');

   Put_Line ("Looking in Grouped (Lookup_Kind => Recursive):");
   Put_Line (Get (Grouped, Key_X, Lookup_Kind => Recursive));

   Put_Line ("Looking in Grouped (Lookup_Kind => Flat):");
   Put_Line (Get (Grouped, Key_X, Lookup_Kind => Flat));

   Dec_Ref (Grouped);
   Destroy (A_Parent);
   Destroy (A);
   Destroy (B);
end Main;
