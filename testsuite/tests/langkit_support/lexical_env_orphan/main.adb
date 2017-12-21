--  Test that lookups on orphaned lexical envs works correctly

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

with Support; use Support;
use Support.Envs;

procedure Main is
   Symbols : Symbol_Table := Create;
   Key_A   : constant Symbol_Type := Find (Symbols, "A");
   Key_B   : constant Symbol_Type := Find (Symbols, "B");

   Old_Env : constant Lexical_Env :=
      Create (No_Env_Getter, 'O', Owner => True);
   New_Env : constant Lexical_Env :=
      Create (No_Env_Getter, 'N', Owner => True);
   Rebindings : constant Env_Rebindings := Append (null, Old_Env, New_Env);

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

   declare
      type Env_Array is array (Positive range <>) of Lexical_Env;
      function Group is new Support.Envs.Group (Positive, Env_Array);

      Transitive_Child : constant Lexical_Env :=
         Create (Simple_Env_Getter (Root), 'C', True, True);

      Grouped : constant Lexical_Env :=
         Group ((Root, Child));

      Rebound_TC : constant Lexical_Env :=
         Rebind_Env (Transitive_Child, Rebindings);

      Rebound_OK : constant Lexical_Env :=
         Rebind_Env (Child, Rebindings);

      E : Lexical_Env;
   begin

      Put_Line ("Trying to build an orphan with transitive parent...");
      begin
         E := Orphan (Transitive_Child);
         Put_Line ("  No error raised");
      exception
         when Exc : Property_Error =>
            Put_Line ("  Got a property error: " & Exception_Message (Exc));
      end;

      Put_Line ("Trying to build an orphan with grouped env...");
      begin
         E := Orphan (Grouped);
         Put_Line ("  No error raised");
      exception
         when Exc : Property_Error =>
            Put_Line ("  Got a property error: " & Exception_Message (Exc));
      end;

      Put_Line ("Trying to build an orphan with rebound transitive parent...");
      begin
         E := Orphan (Rebound_TC);
         Put_Line ("  No error raised");
      exception
         when Exc : Property_Error =>
            Put_Line ("  Got a property error: " & Exception_Message (Exc));
      end;

      Put_Line
        ("Trying to build an orphan with rebound non-transitive parent...");
      begin
         E := Orphan (Rebound_OK);
         Put_Line ("  No error raised");
      exception
         when Exc : Property_Error =>
            Put_Line ("  Got a property error: " & Exception_Message (Exc));
      end;

   end;
end Main;
