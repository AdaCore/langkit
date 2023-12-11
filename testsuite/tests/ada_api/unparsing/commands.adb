with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with Prettier_Ada.Documents;
with Prettier_Ada.Documents.Json;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Commands is

   Context : constant Lk_Context := Create_Context (Self_Id);
   Unit    : constant Lk_Unit := Context.Get_From_Buffer
    (Filename => "foo.txt", Buffer => "var i: Int = 0;");

   procedure Check (Filename : String);
   procedure Remove_Ids (Value : JSON_Value);

   -----------
   -- Check --
   -----------

   procedure Check (Filename : String) is
      Config    : Unparsing_Configuration;
      Doc       : Prettier_Ada.Documents.Document_Type;
      JSON      : JSON_Value;
      JSON_Text : Unbounded_String;
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;

      Config := Load_Unparsing_Config (Self_Id, Filename);
      Doc := Unparse_To_Prettier (Unit.Root, Config);
      JSON_Text := Prettier_Ada.Documents.Json.Serialize (Doc);

      --  Remove "id" fields from the JSON representation, for output stability

      JSON := GNATCOLL.JSON.Read (JSON_Text);
      Remove_Ids (JSON);

      JSON_Text := JSON.Write (Compact => False);
      Put_Line (JSON_Text);
      New_Line;
   end Check;

   ----------------
   -- Remove_Ids --
   ----------------

   procedure Remove_Ids (Value : JSON_Value) is
      procedure Process (Name : String; Value : JSON_Value);

      -------------
      -- Process --
      -------------

      procedure Process (Name : String; Value : JSON_Value) is
         pragma Unreferenced (Name);
      begin
         Remove_Ids (Value);
      end Process;
   begin
      case Value.Kind is
         when JSON_Object_Type =>
            if Value.Has_Field ("id") then
               Value.Unset_Field ("id");
            end if;
            Value.Map_JSON_Object (Process'Access);

         when JSON_Array_Type =>
            for V of JSON_Array'(Value.Get) loop
               Remove_Ids (V);
            end loop;

         when others =>
            return;
      end case;
   end Remove_Ids;

begin
   if Unit.Has_Diagnostics then
      raise Program_Error;
   end if;

   Check ("cmd_align.json");
   Check ("cmd_align2.json");
   Check ("cmd_breakparent.json");
   Check ("cmd_dedent.json");
   Check ("cmd_dedenttoroot.json");
   Check ("cmd_fill.json");
   Check ("cmd_group.json");
   Check ("cmd_hardline.json");
   Check ("cmd_ifbreak.json");
   Check ("cmd_indent.json");
   Check ("cmd_line.json");
   Check ("cmd_list.json");
   Check ("cmd_literalline.json");
   Check ("cmd_markasroot.json");
   Check ("cmd_recurse.json");
   Check ("cmd_softline.json");
   Check ("cmd_trim.json");
   Check ("cmd_whitespace_3.json");
   Check ("cmd_whitespace_default.json");

   Put_Line ("Done.");
end Commands;
