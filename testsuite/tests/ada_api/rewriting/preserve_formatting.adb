with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;

procedure Preserve_Formatting is

   procedure Traverse
     (Handle : Node_Rewriting_Handle;
      Callback : not null access procedure (Handle : Node_Rewriting_Handle));
   procedure Double_Text (Handle : Node_Rewriting_Handle);

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Handle : Node_Rewriting_Handle;
      Callback : not null access procedure (Handle : Node_Rewriting_Handle))
   is
   begin
      if Handle = No_Node_Rewriting_Handle then
         return;
      end if;

      Callback (Handle);
      for C of Children (Handle) loop
         Traverse (C, Callback);
      end loop;
   end Traverse;

   -----------------
   -- Double_Text --
   -----------------

   procedure Double_Text (Handle : Node_Rewriting_Handle) is
   begin
      if Is_Token_Node (Kind (Handle)) then
         Set_Text (Handle, Text (Handle) & Text (Handle));
      end if;
   end Double_Text;

   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit :=
      Get_From_File (Ctx, "preserve_formatting.txt");
   RH  : Rewriting_Handle;
begin
   if Has_Diagnostics (U) then
      Put_Line ("Errors:");
      for D of Diagnostics (U) loop
         Put_Line (Format_GNU_Diagnostic (U, D));
      end loop;
      return;
   end if;

   RH := Start_Rewriting (Ctx);

   Put_Line ("Running the double text substitution...");
   Traverse (Handle (Root (U)), Double_Text'Access);

   New_Line;
   Put_Line ("Running the unit's Unparse...");
   Put_Line (Encode (To_Wide_Wide_String (Unparse (Handle (U))), "ASCII"));

   New_Line;
   Put_Line ("Applying the diff...");
   Process_Apply (RH);

   New_Line;
   Put_Line ("Quoting source buffer for rewritten unit...");
   Put_Line (Encode (Text (U), "ASCII"));

   Put_Line ("preserve_formatting.adb: Done.");
end Preserve_Formatting;
