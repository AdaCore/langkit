## vim: filetype=makoada

with Ada.Calendar;              use Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off, "internal");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;

with Langkit_Support.Diagnostics;        use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;              use Langkit_Support.Slocs;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Init;     use ${ada_lib_name}.Init;
${(
   # This is necessary to avoid generating multiple lines, which avoids style
   # errors.
   'with Libadalang.Analysis.Properties; use Libadalang.Analysis.Properties;\n'
   if ctx.separate_properties else ''
)}
procedure Parse is

   function "+" (S : String) return Unbounded_String renames
      To_Unbounded_String;

   package String_Vectors is new Ada.Containers.Vectors
     (Natural, Unbounded_String);

   Config      : Command_Line_Configuration;
   Silent      : aliased Boolean;
   Measure_Time, Do_Print_Trivia : aliased Boolean;
   Rule_Name   : aliased GNAT.Strings.String_Access :=
      new String'("${ctx.main_rule_name}");
   Charset     : aliased GNAT.Strings.String_Access :=
      new String'("iso-8859-1");
   File_Name   : aliased GNAT.Strings.String_Access;
   File_List   : aliased GNAT.Strings.String_Access;
   Print_Envs  : aliased Boolean;
   Count_Nodes : aliased Boolean;
   Pretty_Print : aliased Boolean;

   Input_Str : Unbounded_String;
   Lookups   : String_Vectors.Vector;

   procedure Register_Lookups;
   procedure Process_Lookups (Node : ${root_entity.api_name}'Class);
   procedure Process_Node (Res : ${root_entity.api_name}'Class);
   procedure Parse_Input;
   procedure Process_File (File_Name : String; Ctx : in out Analysis_Context);

   ----------------------
   -- Register_Lookups --
   ----------------------

   procedure Register_Lookups is
   begin
      loop
         declare
            Arg : constant String := Get_Argument;
         begin
            exit when Arg'Length = 0;
            Lookups.Append (+Arg);
         end;
      end loop;
   end Register_Lookups;

   ---------------------
   -- Process_Lookups --
   ---------------------

   procedure Process_Lookups (Node : ${root_entity.api_name}'Class) is
   begin
      for Lookup_Str of Lookups loop
         New_Line;

         declare
            Sep : constant Natural := Index (Lookup_Str, ":");

            Line   : constant Line_Number := Line_Number'Value
              (Slice (Lookup_Str, 1, Sep - 1));
            Column : constant Column_Number := Column_Number'Value
              (Slice (Lookup_Str, Sep + 1, Length (Lookup_Str)));

            Sloc        : constant Source_Location := (Line, Column);
            Lookup_Node : constant ${root_entity.api_name} :=
               Lookup (Node, (Line, Column));
         begin
            Put_Line ("Lookup " & Image (Sloc) & ":");
            Print (Lookup_Node);
         end;
      end loop;
   end Process_Lookups;

   ------------------
   -- Process_Node --
   ------------------

   procedure Process_Node (Res : ${root_entity.api_name}'Class) is
   begin
      if Is_Null (Res) then
         Put_Line ("<null node>");
         return;
      end if;

      if not Silent then
         if Do_Print_Trivia then
            PP_Trivia (Res);
         else
            Print (Res);
         end if;
      end if;

      Process_Lookups (Res);
   end Process_Node;

   -----------------
   -- Parse_Input --
   -----------------

   procedure Parse_Input is
      Input_Str_Ptr    : Big_String_Access;
      Input_Str_Length : Natural;

      Ctx  : Analysis_Context := Create (With_Trivia => Do_Print_Trivia);
      Unit : Analysis_Unit;
      Rule : Grammar_Rule;
   begin
      begin
         Rule := Grammar_Rule'Value (Rule_Name.all & "_Rule");
      exception
         when Constraint_Error =>
            raise Program_Error with "Unsupported rule: " & Rule_Name.all;
      end;

      Get_String (Input_Str, Input_Str_Ptr, Input_Str_Length);
      Unit := Get_From_Buffer
        (Context  => Ctx,
         Filename => "<input>",
         Buffer   => Input_Str_Ptr (1 .. Input_Str_Length),
         Rule     => Rule);

      if Has_Diagnostics (Unit) then
         Put_Line ("Parsing failed:");
         for D of Diagnostics (Unit) loop
            Put_Line (To_Pretty_String (D));
         end loop;
      end if;

      --  Error recovery may make the parser return something even on error:
      --  process it anyway.
      Process_Node (Root (Unit));
      Destroy (Ctx);
   end Parse_Input;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (File_Name : String; Ctx : in out Analysis_Context)
   is
      Unit         : Analysis_Unit;
      Time_Before  : constant Time := Clock;
      Time_After   : Time;
      AST          : ${root_entity.api_name};
   begin
      Unit := Get_From_File (Ctx, File_Name, "", True);
      AST := Root (Unit);
      Time_After := Clock;

      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Put (File_Name & ": ");
            Put_Line (To_Pretty_String (D));
         end loop;
      end if;

      if (not Silent) and then not Is_Null (AST) then
         if Do_Print_Trivia then
            PP_Trivia (Unit);
         else
            Print (AST);
         end if;

         Process_Lookups (AST);
      end if;

      if Print_Envs then
         Populate_Lexical_Env (Unit);
         Put_Line ("");
         Put_Line ("==== Dumping lexical environments ====");
         Dump_Lexical_Env (Unit);
      end if;

      if Count_Nodes then
         declare
            Count : Natural := 0;

            function Visit
              (Node : ${root_entity.api_name}'Class) return Visit_Status;

            -----------
            -- Visit --
            -----------

            function Visit
              (Node : ${root_entity.api_name}'Class) return Visit_Status is
            begin
               if Is_Null (Node) then
                  Count := Count + 1;
               end if;
               return Into;
            end Visit;

         begin
            if Is_Null (AST) then
               Traverse (AST, Visit'Access);
            end if;
            if not Silent then
               Put_Line
                 ("The tree contains" & Natural'Image (Count) & " nodes.");
            end if;
         end;
      end if;

      if Pretty_Print then
         Put_Line (PP (AST));
      end if;

      if Measure_Time then
         Put_Line
           ("Time elapsed: " & Duration'Image (Time_After - Time_Before));
      end if;

   end Process_File;

begin
   Initialize;

   Set_Usage
     (Config,
      Usage    => "[switches] [input] [lookups]");
   Define_Switch
     (Config, Silent'Access, "-s", "--silent",
      Help   => "Do not print the representation of the resulting tree");
   Define_Switch
     (Config, Count_Nodes'Access, "-C", "--count",
      Help   => "Count the number of nodes in the resulting tree. This is"
                & " handy to measure the performance of tree traversal.");
   Define_Switch
     (Config, Print_Envs'Access, "-E", "--print-envs",
      Help   => "Print lexical environments computed");
   Define_Switch
     (Config, Measure_Time'Access, "-t", "--time",
      Help   => "Time the execution of parsing");
   Define_Switch
     (Config, Rule_Name'Access, "-r:", "--rule-name:",
      Help   => "Rule name to parse");
   Define_Switch
     (Config, Charset'Access, "-c:", "--charset:",
      Help   => "Charset to use to decode the source code");
   Define_Switch
     (Config, Do_Print_Trivia'Access, "-P", "--print-with-trivia",
      Help   => "Print a simplified tree with trivia included");
   Define_Switch
     (Config, File_Name'Access, "-f:", "--file-name:",
      Help   => "Parse file");
   Define_Switch
     (Config, File_List'Access, "-F:", "--file-list:",
      Help   => ("Parse files listed in the provided filename with the regular"
                 & " analysis circuitry (useful for timing measurements)"));
   Define_Switch
     (Config, Pretty_Print'Access, "-P", "--pp",
      Help   => "Pretty print the code with the built in pretty printer");
   begin
      Getopt (Config);
   exception
      when GNAT.Command_Line.Exit_From_Command_Line =>
         return;
   end;

   if File_List.all'Length /= 0 then
      declare
         F : File_Type;
         Ctx : Analysis_Context :=
           Create (Charset.all, With_Trivia => Do_Print_Trivia);
      begin
         Open (F, In_File, File_List.all);
         while not End_Of_File (F) loop
            declare
               Filename : constant String := Get_Line (F);
            begin
               Process_File (Filename, Ctx);
               Remove (Ctx, Filename);
            end;
         end loop;
         Close (F);
         Destroy (Ctx);
      end;

   elsif File_Name.all'Length /= 0 then
      declare
         Ctx : Analysis_Context :=
           Create (Charset.all, With_Trivia => Do_Print_Trivia);
      begin
         Register_Lookups;
         Process_File (File_Name.all, Ctx);
         Destroy (Ctx);
      end;

   else
      Input_Str := +Get_Argument;
      Register_Lookups;

      declare
         Time_Before : constant Time := Clock;
         Time_After  : Time;
      begin
         Parse_Input;
         Time_After := Clock;
         if Measure_Time then
            Put_Line
              ("Time elapsed: " & Duration'Image (Time_After - Time_Before));
         end if;
      end;

   end if;

   GNAT.Strings.Free (Rule_Name);
   GNAT.Strings.Free (Charset);
   GNAT.Strings.Free (File_List);
   GNAT.Strings.Free (File_Name);
   Free (Config);
end Parse;
