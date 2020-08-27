--  Test that lookups on orphaned lexical envs works correctly

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Support; use Support;
use Support.Envs;

procedure Main is
   Symbols : Symbol_Table := Create_Symbol_Table;
   Key_A   : constant Symbol_Type := Find (Symbols, "A");
   Key_B   : constant Symbol_Type := Find (Symbols, "B");

   Old_Env : Lexical_Env := Create_Lexical_Env
     (No_Env_Getter, 'O', Owner => True);
   New_Env : Lexical_Env := Create_Lexical_Env
     (No_Env_Getter, 'N', Owner => True);
   Rebindings : Env_Rebindings := Append (null, Old_Env, New_Env);

   Root     : Lexical_Env := Create_Lexical_Env
     (No_Env_Getter, 'R', Owner => True);
   Child    : Lexical_Env := Create_Lexical_Env
     (Simple_Env_Getter (Root), 'C', Owner => True);
   Orphaned : Lexical_Env := Orphan (Child);
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

   declare
      Transitive_Child : Lexical_Env :=
         Create_Lexical_Env (Simple_Env_Getter (Root), 'C', True, True);

      Grouped    : Lexical_Env := Group ((Root, Child));
      Rebound_TC : Lexical_Env := Rebind_Env (Transitive_Child, Rebindings);
      Rebound_OK : Lexical_Env := Rebind_Env (Child, Rebindings);

      E : Lexical_Env;
   begin

      Put_Line ("Trying to build an orphan with transitive parent...");
      begin
         E := Orphan (Transitive_Child);
         Put_Line ("  No error raised");
         Dec_Ref (E);
      exception
         when Exc : Property_Error =>
            Put_Line ("  Got a property error: " & Exception_Message (Exc));
      end;

      Put_Line ("Trying to build an orphan with grouped env...");
      begin
         E := Orphan (Grouped);
         Put_Line ("  No error raised");
         Dec_Ref (E);
      exception
         when Exc : Property_Error =>
            Put_Line ("  Got a property error: " & Exception_Message (Exc));
      end;

      Put_Line ("Trying to build an orphan with rebound transitive parent...");
      begin
         E := Orphan (Rebound_TC);
         Put_Line ("  No error raised");
         Dec_Ref (E);
      exception
         when Exc : Property_Error =>
            Put_Line ("  Got a property error: " & Exception_Message (Exc));
      end;

      Put_Line
        ("Trying to build an orphan with rebound non-transitive parent...");
      begin
         E := Orphan (Rebound_OK);
         Put_Line ("  No error raised");
         Dec_Ref (E);
      exception
         when Exc : Property_Error =>
            Put_Line ("  Got a property error: " & Exception_Message (Exc));
      end;

      Dec_Ref (Grouped);
      Dec_Ref (Rebound_TC);
      Dec_Ref (Rebound_OK);

      Destroy (Transitive_Child);
   end;

   Dec_Ref (Orphaned);

   Destroy (Old_Env);
   Destroy (New_Env);
   Destroy (Rebindings);

   Destroy (Root);
   Destroy (Child);

   Destroy (Symbols);
end Main;
