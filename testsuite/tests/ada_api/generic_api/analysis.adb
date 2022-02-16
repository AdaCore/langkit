with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors;      use Langkit_Support.Errors;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Names;       use Langkit_Support.Names;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Libfoolang.Analysis;
with Libfoolang.Common;
with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Analysis is
   Id : Language_Id renames Libfoolang.Generic_API.Foo_Lang_Id;

   Ctx : Lk_Context;
   U   : Lk_Unit;
   N   : Lk_Node;
begin
   New_Line;

   Put_Line
     ("Language name: "
      & Image (Format_Name (Language_Name (Id), Camel_With_Underscores)));
   New_Line;

   Put_Line ("Grammar rules:");
   for I in 1 .. Last_Grammar_Rule (Id) loop
      declare
         Rule : constant Grammar_Rule_Ref := From_Index (Id, I);
         Doc  : constant Text_Type := Grammar_Rule_Doc (Rule);
      begin
         Put ("  " & Image (Format_Name (Grammar_Rule_Name (Rule),
                                         Camel_With_Underscores)));
         if Rule = Default_Grammar_Rule (Id) then
            Put (" (default)");
         end if;
         if Is_Public (Rule) then
            Put (" (public)");
         end if;

         Put_Line
           (": "
            & Image (Format_Name (Node_Type_Name (Grammar_Rule_Type (Rule)),
                                  Camel_With_Underscores)));
         if Doc /= "" then
            Put_Line ("    doc: " & Image (Doc, With_Quotes => True));
         end if;
      end;
   end loop;
   New_Line;

   Put_Line ("Token kinds:");
   for I in 1 .. Last_Token_Kind (Id) loop
      declare
         Kind : constant Token_Kind_Ref := From_Index (Id, I);
      begin
         Put_Line ("  " & Image (Format_Name (Token_Kind_Name (Kind),
                                              Camel_With_Underscores)));
      end;
   end loop;
   New_Line;

   Put_Line ("Use of null context:");
   declare
      Dummy : Boolean;
   begin
      Dummy := Ctx.Has_Unit ("foo.txt");
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Use of null unit:");
   begin
      --  Disable warnings about reading U before it is initialized: we have
      --  special provision to handle that case in the API, and we want to
      --  check that the behavior is deterministic here.
      pragma Warnings (Off);
      N := U.Root;
      pragma Warnings (On);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Use of null node:");
   begin
      N := N.Parent;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Ctx := Create_Context (Id);

   Put_Line ("Parsing example.txt...");
   U := Ctx.Get_From_File ("example.txt");
   N := U.Root;

   if U.Context /= Ctx then
      raise Program_Error with "wrong unit->context backlink";
   elsif N.Unit /= U then
      raise Program_Error with "wrong node->unit backlink";
   end if;

   declare
      Has_1 : constant Boolean := Ctx.Has_Unit ("example.txt");
      Has_2 : constant Boolean := Ctx.Has_Unit ("foo.txt");
   begin
      Put_Line ("Has example.txt? -> " & Has_1'Image);
      Put_Line ("Has foo.txt? -> " & Has_2'Image);
   end;

   Put_Line ("Line 2:");
   Put_Line ("  " & Image (U.Get_Line (2), With_Quotes => True));

   Put_Line ("Traversing its parsing tree...");
   declare
      function Visit (N : Lk_Node) return Visit_Status;

      -----------
      -- Visit --
      -----------

      function Visit (N : Lk_Node) return Visit_Status is
      begin
         Put_Line (N.Image);
         return Into;
      end Visit;

   begin
      N.Traverse (Visit'Access);
   end;
   New_Line;

   Put_Line ("Unit.Text -> " & Image (U.Text, With_Quotes => True));
   Put ("No_Lk_Unit.Text -> ");
   declare
      Dummy : Integer;
   begin
      Dummy := No_Lk_Node.Text'Length;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Testing various node operations:");
   Put_Line ("Root.Is_Null -> " & N.Is_Null'Image);

   N := N.Next_Sibling;
   Put_Line ("Root.Next_Sibling.Image -> " & N.Image);
   Put_Line ("Root.Next_Sibling.Is_Null -> " & N.Is_Null'Image);

   N := U.Root.Child (2);
   Put_Line ("Root.Child (2).Image -> " & N.Image);

   declare
      Prev : constant Lk_Node := N.Previous_Sibling;
      Equal_1 : constant Boolean := Prev.Next_Sibling = N;
      Equal_2 : constant Boolean := Prev = N;
   begin
      Put_Line ("Root.Child (2).Previous_Sibling.Image -> " & Prev.Image);
      Put_Line
        ("[...].Previous_Sibling = [...] -> " & Equal_1'Image);
      Put_Line
        ("[...].Previous_Sibling = [...].Previous_Sibling.Next_Sibling -> "
         & Equal_2'Image);
   end;
   Put_Line ("Root.Children:");
   for C of U.Root.Children loop
      Put_Line ("  -> " & C.Image);
   end loop;
   New_Line;

   N := U.Root.Child (2).Child (1);
   for B in Boolean'Range loop
      Put_Line ("Parents (" & Image (N) & ", With_Self => " & B'Image & "):");
      for P of N.Parents (With_Self => B) loop
         Put_Line ("  " & Image (P));
      end loop;
   end loop;

   Put ("Parents (No_Lk_Node) = ");
   declare
      Length : Natural;
   begin
      Length := No_Lk_Node.Parents'Length;
      Put_Line (Length'Image);
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Is_Ghost (" & Image (U.Root) & ") = " & U.Root.Is_Ghost'Image);
   Put_Line ("Is_Ghost (" & Image (N) & ") = " & N.Is_Ghost'Image);
   Put ("Is_Ghost (No_Lk_Node) = ");
   declare
      B : Boolean;
   begin
      B := No_Lk_Node.Is_Ghost;
      Put_Line (B'Image);
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Root.Text -> " & Image (U.Root.Text, With_Quotes => True));
   Put ("No_Lk_Node.Text -> ");
   declare
      Dummy : Integer;
   begin
      Dummy := No_Lk_Node.Text'Length;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Root.Sloc_Range -> " & Image (U.Root.Sloc_Range));
   Put ("No_Lk_Node.Sloc_Range -> ");
   declare
      Dummy : Source_Location_Range;
   begin
      Dummy := No_Lk_Node.Sloc_Range;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Root.Is_Incomplete -> " & U.Root.Is_Incomplete'Image);
   Put ("No_Lk_Node.Is_Incomplete -> ");
   declare
      Dummy : Boolean;
   begin
      Dummy := No_Lk_Node.Is_Incomplete;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Root.Print (without slocs)");
   U.Root.Print (Show_Slocs => False, Line_Prefix => "... ");
   Put_Line ("Root.Print (with slocs)");
   U.Root.Print (Show_Slocs => True, Line_Prefix => "... ");
   Put ("No_Lk_Node.Print -> ");
   begin
      No_Lk_Node.Print;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Testing various token operations:");
   Put_Line ("No_Lk_Token.Is_Null -> " & No_Lk_Token.Is_Null'Image);
   Put_Line ("First_Token.Is_Null -> " & U.First_Token.Is_Null'Image);
   Put_Line ("Checking the Token->Unit backlink...");
   if U.First_Token.Unit /= U then
      raise Program_Error with "wrong token->unit backlink";
   end if;
   Put ("No_Lk_Token.Unit -> ");
   begin
      if No_Lk_Token.Unit /= U then
         raise Program_Error;
      end if;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   Put_Line ("First_Token.Kind -> "
             & Image (Format_Name (Token_Kind_Name (U.First_Token.Kind),
                                   Camel_With_Underscores)));
   Put_Line ("Last_Token.Kind -> "
             & Image (Format_Name (Token_Kind_Name (U.Last_Token.Kind),
                                   Camel_With_Underscores)));
   Put_Line ("First_Token.Image -> " & U.First_Token.Image);
   Put_Line ("Last_Token.Image -> " & U.Last_Token.Image);
   Put_Line ("No_Lk_Token.Image -> " & No_Lk_Token.Image);
   Put_Line ("First_Token.Text -> "
             & Image (U.First_Token.Text, With_Quotes => True));
   Put_Line ("Last_Token.Text -> "
             & Image (U.Last_Token.Text, With_Quotes => True));
   Put ("No_Lk_Token.Text -> ");
   begin
      Put_Line (Image (No_Lk_Token.Text, With_Quotes => True));
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   Put_Line ("No_Lk_Token.Next -> " & No_Lk_Token.Next.Image);
   Put_Line ("First_Token.Next -> " & U.First_Token.Next.Image);
   Put_Line ("Last_Token.Next -> " & U.Last_Token.Next.Image);
   Put_Line ("No_Lk_Token.Previous -> " & No_Lk_Token.Previous.Image);
   Put_Line ("First_Token.Previous -> " & U.First_Token.Previous.Image);
   Put_Line ("Last_Token.Previous -> " & U.Last_Token.Previous.Image);
   Put_Line ("First_Token.Is_Trivia -> " & U.First_Token.Is_Trivia'Image);
   Put_Line ("Last_Token.Is_Trivia -> " & U.Last_Token.Is_Trivia'Image);
   Put_Line ("Last_Token.Previous.Is_Trivia -> "
             & U.Last_Token.Previous.Is_Trivia'Image);
   Put_Line ("First_Token.Index ->" & U.First_Token.Index'Image);
   New_Line;

   Put_Line ("Testing ordering predicate for various cases:");
   declare
      FT : constant Lk_Token := U.First_Token;
      LT : constant Lk_Token := U.Last_Token;

      U2 : constant Lk_Unit := Ctx.Get_From_File ("example2.txt");

      procedure Check (Label : String; Left, Right : Lk_Token);

      -----------
      -- Check --
      -----------

      procedure Check (Label : String; Left, Right : Lk_Token) is
         Result : Boolean;
      begin
         Put (Label & " -> ");
         Result := Left < Right;
         Put_Line (Result'Image);
      exception
         when Exc : Precondition_Failure =>
            Put_Line ("Got a Precondition_Failure exception: "
                      & Exception_Message (Exc));
         when Exc : Stale_Reference_Error =>
            Put_Line ("Got a Stale_Reference_Error exception: "
                      & Exception_Message (Exc));
      end Check;
   begin
      Check ("First_Token < Last_Token:", FT, LT);
      Check ("First_Token < No_Lk_Token:", FT, No_Lk_Token);
      Check ("No_Lk_Token < Last_Token:", No_Lk_Token, LT);
      Check ("First_Token < Other_Unit", FT, U2.Last_Token);

      U := Ctx.Get_From_File ("example.txt", Reparse => True);
      Check ("First_Token < Stale", U.First_Token, LT);
      Check ("Stale < Last_Token", FT, U.Last_Token);
   end;
   New_Line;

   Put_Line ("Testing text range for various cases:");
   declare
      FT : constant Lk_Token := U.First_Token;
      LT : constant Lk_Token := U.Last_Token;

      U2 : constant Lk_Unit := Ctx.Get_From_File ("example2.txt");

      procedure Check (Label : String; Left, Right : Lk_Token);

      -----------
      -- Check --
      -----------

      procedure Check (Label : String; Left, Right : Lk_Token) is
      begin
         Put (Label & " -> ");
         Put_Line (Image (Text (Left, Right), With_Quotes => True));
      exception
         when Exc : Precondition_Failure =>
            Put_Line ("Got a Precondition_Failure exception: "
                      & Exception_Message (Exc));
         when Exc : Stale_Reference_Error =>
            Put_Line ("Got a Stale_Reference_Error exception: "
                      & Exception_Message (Exc));
      end Check;
   begin
      Check ("First_Token .. Last_Token:", FT, LT);
      Check ("First_Token .. No_Lk_Token:", FT, No_Lk_Token);
      Check ("No_Lk_Token .. Last_Token:", No_Lk_Token, LT);
      Check ("First_Token .. Other_Unit", FT, U2.Last_Token);

      U := Ctx.Get_From_File ("example.txt", Reparse => True);
      Check ("First_Token .. Stale", U.First_Token, LT);
      Check ("Stale .. Last_Token", FT, U.Last_Token);
   end;
   New_Line;

   Put_Line ("Testing token equivalence for various cases:");
   declare
      U2 : constant Lk_Unit := Ctx.Get_From_File ("example2.txt");

      U_Example_Tok : constant Lk_Token := U.First_Token;
      U_Var_Tok     : constant Lk_Token := U.First_Token.Next.Next.Next.Next;
      U2_Var_Tok    : constant Lk_Token := U2.First_Token;

      procedure Check (Left, Right : Lk_Token);

      -----------
      -- Check --
      -----------

      procedure Check (Left, Right : Lk_Token) is
      begin
         Put ("  ");
         Put ("Is_Equivalent (" & Image (Left) & ", " & Image (Right)
              & ") = ");
         Put_Line (Left.Is_Equivalent (Right)'Image);
      exception
         when Exc : Precondition_Failure =>
            Put_Line ("Got a Precondition_Failure exception: "
                      & Exception_Message (Exc));
         when Exc : Stale_Reference_Error =>
            Put_Line ("Got a Stale_Reference_Error exception: "
                      & Exception_Message (Exc));
      end Check;
   begin
      Put_Line ("  Non stale references...");
      Check (U_Example_Tok, U_Var_Tok);
      Check (U_Var_Tok, U2_Var_Tok);
      Check (No_Lk_Token, U_Example_Tok);
      Check (U_Example_Tok, No_Lk_Token);
      New_Line;

      Put_Line ("  Stale references...");
      U := Ctx.Get_From_File ("example.txt", Reparse => True);
      Check (U_Example_Tok, U2_Var_Tok);
      Check (U2_Var_Tok, U_Example_Tok);
   end;
   New_Line;

   Put_Line ("Use of stale node reference:");
   U := Ctx.Get_From_File ("example.txt", Reparse => True);
   begin
      Put_Line ("--> " & N.Image);
      raise Program_Error;
   exception
      when Exc : Stale_Reference_Error =>
         Put_Line ("Got a Stale_Reference_Error exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Check generic/specific context type converters");
   declare
      Gen_Ctx : Lk_Context := Create_Context (Id);
      Spe_Ctx : Libfoolang.Analysis.Analysis_Context :=
        From_Generic_Context (Gen_Ctx);

      Dummy_Unit : Libfoolang.Analysis.Analysis_Unit;
   begin
      --  Create an analysis unit while the context can only be referenced
      --  through language-specific types.

      Gen_Ctx := No_Lk_Context;
      Dummy_Unit := Spe_Ctx.Get_From_Buffer
        (Filename => "foo.txt", Buffer => "example bar");
      Dummy_Unit := Libfoolang.Analysis.No_Analysis_Unit;

      --  Now switch back to the generic type and make sure the context is
      --  functional.

      Gen_Ctx := To_Generic_Context (Spe_Ctx);
      Spe_Ctx := Libfoolang.Analysis.No_Analysis_Context;
      Put_Line (Gen_Ctx.Get_From_File ("foo.txt").Root.Image);
   end;
   New_Line;

   Put_Line ("Check generic/specific unit type converters");
   declare
      Gen_Ctx  : Lk_Context := Create_Context (Id);
      Gen_Unit : Lk_Unit := Gen_Ctx.Get_From_File ("example.txt");
      Spe_Unit : constant Libfoolang.Analysis.Analysis_Unit :=
        From_Generic_Unit (Gen_Unit);
   begin
      --  Create an analysis unit while the context can only be referenced
      --  through language-specific types.

      Gen_Unit := No_Lk_Unit;
      Gen_Ctx := No_Lk_Context;

      --  At this point, the context/units, created through public APIs, are
      --  live only through the language-specific unit type in Spe_Unit.  Now
      --  switch back to the generic type and make sure both are functional.

      Gen_Unit := To_Generic_Unit (Spe_Unit);
      Gen_Ctx := Gen_Unit.Context;
      Put_Line (Gen_Ctx.Get_From_File ("example.txt").Root.Image);
   end;
   New_Line;

   Put_Line ("Check generic/specific grammar rule converters");
   declare
      use Libfoolang.Common;

      procedure Check (Rule : Grammar_Rule);
      --  Check the specific/generic conversion back and forth for Rule

      -----------
      -- Check --
      -----------

      procedure Check (Rule : Grammar_Rule) is
         R : constant Grammar_Rule_Ref := To_Generic_Grammar_Rule (Rule);
      begin
         Put_Line
           (Rule'Image & " -> "
            & Image (Format_Name (Grammar_Rule_Name (R),
                                  Camel_With_Underscores)));
         if From_Generic_Grammar_Rule (R) /= Rule then
            raise Program_Error;
         end if;
      end Check;

   begin
      for Rule in Grammar_Rule loop
         Check (Rule);
      end loop;
   end;

   Put ("Check error case... ");
   declare
      use Libfoolang.Common;

      Dummy : Grammar_Rule;
   begin
      Dummy := From_Generic_Grammar_Rule (No_Grammar_Rule_Ref);
      raise Program_Error;
   exception
      when Exc : Langkit_Support.Errors.Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;
end Analysis;
