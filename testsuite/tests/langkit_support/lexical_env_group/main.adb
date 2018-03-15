--  Test that lookups on grouped lexical envs works correctly

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

with Support; use Support;
use Support.Envs;

procedure Main is
   Symbols : Symbol_Table := Create;
   Key_X   : constant Symbol_Type := Find (Symbols, "X");

   A_Parent : constant Lexical_Env :=
      Create (No_Env_Getter, 'P', Owner => True);
   A        : constant Lexical_Env :=
      Create (Simple_Env_Getter (A_Parent), 'A', Owner => True);
   B        : constant Lexical_Env :=
      Create (No_Env_Getter, 'B', Owner => True);

   Grouped : constant Lexical_Env := Group ((A, B));
begin
   Add (A_Parent, Key_X, '1');
   Add (A, Key_X, '2');
   Add (B, Key_X, '3');

   Put_Line ("Looking in Grouped (Recursive => True):");
   Put_Line (Get (Grouped, Key_X, Recursive => True));

   Put_Line ("Looking in Grouped (Recursive => False):");
   Put_Line (Get (Grouped, Key_X, Recursive => False));
end Main;
