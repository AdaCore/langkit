with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with Prettier_Ada.Documents;
with Prettier_Ada.Documents.Json;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Commands is

   Context : constant Lk_Context := Create_Context (Self_Id);

   procedure Check (Filename : String; Buffer : String := "var i: Int = 0;");

   procedure Reset_Ids (Value : JSON_Value);
   --  Ids in prettier are generated with a process-wide counter. To avoid
   --  interference between tests, we use this procedure to renumber Ids for a
   --  given document.

   -----------
   -- Check --
   -----------

   procedure Check (Filename : String; Buffer : String := "var i: Int = 0;") is
      Unit      : constant Lk_Unit := Context.Get_From_Buffer
       (Filename => "foo.txt", Buffer => Buffer);
      Config    : Unparsing_Configuration;
      Doc       : Prettier_Ada.Documents.Document_Type;
      JSON      : JSON_Value;
      JSON_Text : Unbounded_String;
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;

      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         raise Program_Error;
      end if;

      declare
         Diagnostics : Diagnostics_Vectors.Vector;
      begin
         Config := Load_Unparsing_Config (Self_Id, Filename, Diagnostics);
         if Config = No_Unparsing_Configuration then
            Print (Diagnostics);
            raise Program_Error;
         end if;
      end;
      Doc := Unparse_To_Prettier (Unit.Root, Config);
      JSON_Text := Prettier_Ada.Documents.Json.Serialize (Doc);

      --  Remove "id" fields from the JSON representation, for output stability

      JSON := GNATCOLL.JSON.Read (JSON_Text);
      Reset_Ids (JSON);

      JSON_Text := JSON.Write (Compact => False);
      Put_Line (JSON_Text);
      New_Line;
   end Check;

   ---------------
   -- Reset_Ids --
   ---------------

   procedure Reset_Ids (Value : JSON_Value) is

      package Id_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Integer, Element_Type => Integer);
      Id_Map : Id_Maps.Map;

      procedure Process (Name : String; Value : JSON_Value);
      procedure Renumber (Object : JSON_Value; Name : String);
      procedure Recurse (Value : JSON_Value);

      -------------
      -- Process --
      -------------

      procedure Process (Name : String; Value : JSON_Value) is
         pragma Unreferenced (Name);
      begin
         Recurse (Value);
      end Process;

      --------------
      -- Renumber --
      --------------

      procedure Renumber (Object : JSON_Value; Name : String) is
      begin
         if Object.Has_Field (Name) then
            declare
               Old_Id : constant Integer := Object.Get (Name);
               New_Id : Integer;
            begin
               if Old_Id = 0 then
                  Object.Unset_Field (Name);
               else
                  if Id_Map.Contains (Old_Id) then
                     New_Id := Id_Map.Element (Old_Id);
                  else
                     New_Id := Integer (Id_Map.Length) + 1;
                     Id_Map.Insert (Old_Id, New_Id);
                  end if;
                  Object.Set_Field (Name, New_Id);
               end if;
            end;
         end if;
      end Renumber;

      -------------
      -- Recurse --
      -------------

      procedure Recurse (Value : JSON_Value) is
      begin
         case Value.Kind is
            when JSON_Object_Type =>
               Renumber (Value, "id");
               Renumber (Value, "ifBreakGroupId");
               Value.Map_JSON_Object (Process'Access);

            when JSON_Array_Type =>
               for V of JSON_Array'(Value.Get) loop
                  Recurse (V);
               end loop;

            when others =>
               return;
         end case;
      end Recurse;

   begin
      Recurse (Value);
   end Reset_Ids;

begin
   Check ("cmd_align.json");
   Check ("cmd_align2.json");
   Check ("cmd_breakparent.json");
   Check ("cmd_dedent.json");
   Check ("cmd_dedenttoroot.json");
   Check ("cmd_fill.json");
   Check ("cmd_group.json");
   Check ("cmd_group_id.json");
   Check ("cmd_hardline.json");
   Check ("cmd_hardlinewithoutbreakparent.json");
   Check ("cmd_ifbreak.json");
   Check ("cmd_indent.json");
   Check ("cmd_line.json");
   Check ("cmd_list.json");
   Check ("cmd_literalline.json");
   Check ("cmd_markasroot.json");
   Check ("cmd_innerroot.json");
   Check ("cmd_recurse.json");
   Check
     ("cmd_recurse_field.json",
      "var i: Int = 0;" & ASCII.LF
      & "def f(i: Int): Int {i;}");
   Check
     ("cmd_recurse_flatten.json",
      "var i: Int = AAAAAAAAAAAAAAAAAA"
      & "(XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      & ".YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY"
      & ".ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ)"
      & ".DDDDDDDDDDDDDDDD"
      & ".EEEEEEEEEEEEEEEE"
      & ".FFFFFFFFFFFFFFFF"
      & ".GGGGGGGGGGGGGGGG;");
   Check ("cmd_softline.json");
   Check ("cmd_trim.json");
   Check ("cmd_whitespace_3.json");
   Check ("cmd_whitespace_default.json");

   Put_Line ("Done.");
end Commands;
