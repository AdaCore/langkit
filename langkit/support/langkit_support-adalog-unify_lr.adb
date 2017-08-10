with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Unify_LR is
   use Left_Var; use Right_Var;

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out Unify_LR) return Solving_State is
      Result : Solving_State;
      L      : L_Type;
      R      : R_Type;
      LC     : R_Type;
   begin
      Trace ("In Unify_LR");

      if Is_Defined (Self.Left) then

         L := Get_Value (Self.Left);
         LC := Convert (Self.L_Data, L);

         if Is_Defined (Self.Right) then

            --  Both values are defined, return true if they are equal

            R := Get_Value (Self.Right);
            Result := +Equals (Self.Eq_Data, LC, R);
            Trace ("In Unify_LR, Left value is : " & Element_Image (L));
            Trace ("In Unify_LR, Right value is : " & Element_Image (R));
            Trace ("In Unify_LR, both defined, returning " & Result'Image);

         else
            --  Left is defined, right is not, give right the value of left and
            --  return true.

            Set_Value (Self.Right, LC);
            Result := Satisfied;
            Self.State := Right_Changed;
            Trace ("In Unify_LR, propagating right, from "
                   & Image (Self.Left) & " to "
                   & Image (Self.Right));
            Trace ("In Unify_LR, From value is : " & Element_Image (L));
            Trace ("In Unify_LR, New to value is : " & Element_Image (LC));
         end if;

         L_Dec_Ref (L);
         R_Dec_Ref (R);
         R_Dec_Ref (LC);
         return Result;
      end if;

      if not Is_Defined (Self.Right) then
         return No_Progress;
      end if;

      --  Right is defined, left is not: give left the value of right and
      --  return true.

      R := Get_Value (Self.Right);
      declare
         RC : L_Type := Convert (Self.R_Data, R);
      begin
         Set_Value (Self.Left, RC);
         R_Dec_Ref (R);
         L_Dec_Ref (RC);
      end;

      Self.State := Left_Changed;
      Trace ("In Unify_LR, propagating left,"
             & " from " & Image (Self.Right) & " to " & Image (Self.Left));
      return Satisfied;
   end Apply;

   ------------
   -- Revert --
   ------------

   procedure Revert (Self : in out Unify_LR) is
   begin
      case Self.State is
         when Left_Changed => Reset (Self.Left);
         when Right_Changed => Reset (Self.Right);
         when others => null;
      end case;
      Self.State := No_Change;
   end Revert;

   ------------------
   -- Custom_Image --
   ------------------

   function Custom_Image (Self : Unify_LR) return String is
      C : constant String :=
        (if Convert_Image = "" then ""
         else " (convert: " & Convert_Image & ")");
      E : constant String :=
        (if Equals_Image = "" then ""
         else " (equals: " & Equals_Image & ")");
   begin
      return ("Bind " & Left_Var.Image (Self.Left)
              & " <=> " & Right_Var.Image (Self.Right)
              & C & E);
   end Custom_Image;

end Langkit_Support.Adalog.Unify_LR;
