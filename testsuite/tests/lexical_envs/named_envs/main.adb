with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Images; use Langkit_Support.Images;
with Langkit_Support.Text;   use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;

procedure Main is

   Ctx : constant Analysis_Context := Create_Context;

   type Unit_Ref is (Foo_Spec, Foo_Body);
   function Filename (U : Unit_Ref) return String is
     (case U is
      when Foo_Spec => "foo.ads",
      when Foo_Body => "foo.adb");
   type Unit_Ref_Array is array (Positive range <>) of Unit_Ref;

   procedure Run (Label : String; Loading_Sequence : Unit_Ref_Array);
   procedure Put_Node (N : Foo_Node'Class);

   --------------
   -- Put_Node --
   --------------

   procedure Put_Node (N : Foo_Node'Class) is
   begin
      if N.Is_Null then
         Put ("<none>");
         return;
      end if;

      declare
         Filename    : constant String :=
            +Create (+N.Unit.Get_Filename).Base_Name;
         Line_Number : constant Positive :=
            Positive (N.Sloc_Range.Start_Line);
         Line        : constant String :=
            Image (N.Unit.Get_Line (Line_Number));
      begin
         Put (Filename & ":" & Stripped_Image (Line_Number) & ": "
              & Trim (Line, Ada.Strings.Both));
      end;
   end Put_Node;

   ---------
   -- Run --
   ---------

   procedure Run (Label : String; Loading_Sequence : Unit_Ref_Array) is
      Units : array (Unit_Ref) of Analysis_Unit;
   begin
      Put_Line ("== " & Label & " ==");

      --  Load units in the requested order
      for UR of Loading_Sequence loop
         declare
            U : Analysis_Unit renames Units (UR);
         begin
            U := Ctx.Get_From_File (Filename (UR), Reparse => True);
            if U.Has_Diagnostics then
               for D of U.Diagnostics loop
                  Put_Line (U.Format_GNU_Diagnostic (D));
               end loop;
               raise Program_Error;
            end if;
            U.Populate_Lexical_Env;
         end;
      end loop;

      --  Then perform resolution
      for UR in Unit_Ref loop
         New_Line;
         declare
            U : Analysis_Unit renames Units (UR);
            use Libfoolang.Common;

            function Process (N : Foo_Node'Class) return Visit_Status;

            -------------
            -- Process --
            -------------

            function Process (N : Foo_Node'Class) return Visit_Status is
               Result : Foo_Node;
            begin
               begin
                  case N.Kind is
                     when Foo_Ref =>
                        Put_Node (N);
                        Put_Line (": P_Resolve:");
                        Result := N.As_Ref.P_Resolve;
                     when Foo_Package_Decl =>
                        Put_Node (N);
                        Put_Line (": P_Body_Part:");
                        Result := N.As_Package_Decl.P_Body_Part.As_Foo_Node;
                     when Foo_Package_Body =>
                        Put_Node (N);
                        Put_Line (": P_Decl_Part:");
                        Result := N.As_Package_Body.P_Decl_Part.As_Foo_Node;
                     when others =>
                        return Into;
                  end case;
               exception
                  when Exc : Property_Error =>
                     Put_Line ("  <error: " & Exception_Message (Exc) & ">");
                     return Into;
               end;
               Put ("  ");
               Put_Node (Result);
               New_Line;
               return Into;
            end Process;

         begin
            U.Root.Traverse (Process'Access);
         end;
      end loop;

      --  Clear both analysis units for the next run. This is necessary to
      --  exercize the full relocation cycle.
      for UR in Unit_Ref loop
         Units (UR).Reparse (Buffer => "");
      end loop;

      New_Line;
   end Run;

begin
   Ctx.Discard_Errors_In_Populate_Lexical_Env (Discard => False);
   Run ("spec, body", (Foo_Spec, Foo_Body));
   Run ("body, spec", (Foo_Body, Foo_Spec));
end Main;
