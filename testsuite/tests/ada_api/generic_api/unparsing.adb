with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Names; use Langkit_Support.Names;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Unparsing is
   Last_Token : Token_Kind_Ref := No_Token_Kind_Ref;
   Buffer     : Unbounded_Text_Type;

   function "+" (Name : Name_Type) return String
   is (Image (Format_Name (Name, Camel_With_Underscores)));

   procedure Unparse (N : Lk_Node);
   --  Unparse ``N`` to fragments, dump the fragments on the standard output,
   --  then recursively unparse the fragments to ``Buffer``.

   -------------
   -- Unparse --
   -------------

   procedure Unparse (N : Lk_Node) is
      Fragments : Unparsing_Fragment_Vectors.Vector;
   begin
      Put_Line ("Fragments for " & N.Image & ":");
      Unparse_To_Fragments (N, Fragments);

      --  Dump the fragments

      for F of Fragments loop
         case F.Kind is
            when Token_Fragment =>
               Put_Line
                 ("  Token_Fragment: "
                  & Image (To_Text (F.Token_Text), With_Quotes => True)
                  & " (" & (+Token_Kind_Name (F.Token_Kind)) & ")");

            when Field_Fragment =>
               Put_Line
                 ("  Field_Fragment: "
                  & F.Node.Image
                  & " (" & (+Member_Name (F.Field)) & ")");

            when List_Child_Fragment =>
               Put_Line
                 ("  List_Child_Fragment: "
                  & F.Node.Image
                  & " (" & F.Child_Index'Image & ")");
         end case;
      end loop;

      --  Do the actual unparsing

      for F of Fragments loop
         case F.Kind is
            when Token_Fragment =>
               case Required_Spacing (Last_Token, F.Token_Kind) is
                  when None =>
                     null;
                  when Newline =>
                     Append (Buffer, Chars.LF);
                  when Whitespace =>
                     Append (Buffer, ' ');
               end case;
               Last_Token := F.Token_Kind;
               Append (Buffer, F.Token_Text);

            when Field_Fragment | List_Child_Fragment =>
               Unparse (F.Node);
         end case;
      end loop;
   end Unparse;

   Ctx : constant Lk_Context :=
     Create_Context (Libfoolang.Generic_API.Self_Id);
   U   : constant Lk_Unit := Ctx.Get_From_File ("example3.txt");
begin
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;
   Unparse (U.Root);
   New_Line;
   Put_Line ("Unparsed source:");
   Put (Image (To_Text (Buffer), With_Quotes => True));
end Unparsing;
