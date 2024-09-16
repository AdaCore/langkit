with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit :=
     Ctx.Get_From_Buffer ("main.txt", Buffer => "example +example");
   N   : Examples;

   procedure Check
     (Label    : String;
      Property : access function (N : Examples'Class) return Boolean);

   -----------
   -- Check --
   -----------

   procedure Check
     (Label    : String;
      Property : access function (N : Examples'Class) return Boolean)
   is
      Result : Boolean;
   begin
      Put_Line ("Calling Examples." & Label & "...");
      begin
         Result := Property.all (N);
         Put_Line ("Result: " & Result'Image);
      exception
         when Exc : Property_Error =>
            Put_Line ("Got a Property_Error: " & Exception_Message (Exc));
      end;
      New_Line;
   end Check;

begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;
   N := U.Root.As_Examples;

   Check ("p_fail_bind_conv_prop", P_Fail_Bind_Conv_Prop'Access);
   Check ("p_fail_nprop_single", P_Fail_Nprop_Single'Access);
   Check ("p_ok_nprop_multi", P_Ok_Nprop_Multi'Access);
   Check ("p_fail_nprop_multi_1", P_Fail_Nprop_Multi_1'Access);
   Check ("p_fail_nprop_multi_2", P_Fail_Nprop_Multi_2'Access);
   Check ("p_fail_nprop_multi_3", P_Fail_Nprop_Multi_3'Access);
   Check ("p_ok_nprop_varargs", P_Ok_Nprop_Varargs'Access);
   Check ("p_fail_nprop_varargs", P_Fail_Nprop_Varargs'Access);
   Check ("p_ok_pred", P_Ok_Pred'Access);
   Check ("p_fail_pred_1", P_Fail_Pred_1'Access);
   Check ("p_fail_pred_2", P_Fail_Pred_2'Access);
   Check ("p_fail_pred_3", P_Fail_Pred_3'Access);

   Put_Line ("main.adb: Done.");
end Main;
