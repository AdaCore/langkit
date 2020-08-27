--  Test that lookups on grouped lexical envs works correctly

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Support; use Support;
use Support.Envs;

procedure Main is
   Symbols : Symbol_Table := Create_Symbol_Table;
   Key_X   : constant Symbol_Type := Find (Symbols, "X");

   New_Env : Lexical_Env := Create_Lexical_Env
     (No_Env_Getter, 'N', Owner => True);

   Root  : Lexical_Env := Create_Lexical_Env
     (No_Env_Getter, 'R', Owner => True);
   Child : Lexical_Env := Create_Lexical_Env
     (Simple_Env_Getter (Root), 'R', Owner => True);
   Grandchild : Lexical_Env := Create_Lexical_Env
     (Simple_Env_Getter (Child), 'O', Owner => True);

   Rebindings : Env_Rebindings := Append (null, Child, New_Env);
   Rebound    : Lexical_Env := Rebind_Env (Grandchild, Rebindings);
begin
   Add (Root, Key_X, '1');
   Add (New_Env, Key_X, '2');
   Add (Grandchild, Key_X, '3');

   Put_Line ("Looking in Rebound:");
   Put_Line (Get (Rebound, Key_X));

   declare
      Dummy : Env_Rebindings;
   begin
      Dummy := Append (Rebindings, Child, New_Env);
      Put_Line ("Double rebinding: no error raised...");
   exception
      when Exc : Property_Error =>
         Put_Line ("Got a Property_Error:");
         Put_Line (Ada.Exceptions.Exception_Message (Exc));
   end;

   Dec_Ref (Rebound);

   Destroy (Grandchild);
   Destroy (New_Env);

   Destroy (Root);
   Destroy (Child);

   for Child of Rebindings.Children loop
      declare
         C : Env_Rebindings := Child;
      begin
         Destroy (C);
      end;
   end loop;
   Rebindings.Children.Destroy;
   Destroy (Rebindings);

   Destroy (Symbols);
end Main;
