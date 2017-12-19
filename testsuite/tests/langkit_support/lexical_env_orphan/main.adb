--  Test that lookups on orphaned lexical envs works correctly

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

with Support; use Support;
use Support.Envs;

procedure Main is
   Symbols : Symbol_Table := Create;
   Key_A   : constant Symbol_Type := Find (Symbols, "A");
   Key_B   : constant Symbol_Type := Find (Symbols, "B");

   Root     : constant Lexical_Env :=
      Create (No_Env_Getter, 'R', Owner => True);
   Child    : constant Lexical_Env :=
      Create (Simple_Env_Getter (Root), 'C', Owner => True);
   Orphaned : constant Lexical_Env := Orphan (Child);
begin
   Add (Root, Key_A, '1');
   Add (Child, Key_B, '2');

   Put_Line ("Looking for A in Child:");
   Put_Line (Get (Child, Key_A));

   Put_Line ("Looking for B in Child:");
   Put_Line (Get (Child, Key_B));

   Put_Line ("Looking for A in Orphaned:");
   Put_Line (Get (Orphaned, Key_A));

   Put_Line ("Looking for B in Orphaned:");
   Put_Line (Get (Orphaned, Key_B));
end Main;
