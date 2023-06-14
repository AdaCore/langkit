--  Test that lookups on grouped lexical envs works correctly

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Symbols;      use Langkit_Support.Symbols;

with Support; use Support;
use Support.Envs;

procedure Main is
   Symbols  : Symbol_Table := Create_Symbol_Table;
   Key_X    : constant Symbol_Type := Find (Symbols, "X");
   Key_Thin : constant Thin_Symbol := Thin (Key_X);

   A_Parent : Lexical_Env := Create_Lexical_Env
     (Null_Lexical_Env, 'P',
      Owner => No_Generic_Unit,
      Sym_Table => Symbols);

   A        : Lexical_Env := Create_Lexical_Env
     (A_Parent, 'A',
      Owner => No_Generic_Unit,
      Sym_Table => Symbols);

   B        : Lexical_Env := Create_Lexical_Env
     (Null_Lexical_Env, 'B', Owner => No_Generic_Unit, Sym_Table => Symbols);

   Grouped : Lexical_Env := Group ((A, B));
begin
   Add (A_Parent, Key_Thin, '1');
   Add (A, Key_Thin, '2');
   Add (B, Key_Thin, '3');

   Put_Line ("Looking in Grouped (Lookup_Kind => Recursive):");
   Put_Line (Get (Grouped, Key_Thin, Lookup_Kind => Recursive));

   Put_Line ("Looking in Grouped (Lookup_Kind => Flat):");
   Put_Line (Get (Grouped, Key_Thin, Lookup_Kind => Flat));

   Dec_Ref (Grouped);
   Destroy (A_Parent);
   Destroy (A);
   Destroy (B);
   Destroy (Symbols);
end Main;
