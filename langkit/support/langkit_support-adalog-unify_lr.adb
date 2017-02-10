with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Unify_LR is
   use Left_Var; use Right_Var;

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out Unify_LR) return Boolean is
   begin
      Trace ("In Unify_LR");

      if Is_Defined (Self.Left) then

         --  Both values are defined, return true if they are equal
         if Is_Defined (Self.Right) then
            return C : Boolean do
               C :=
                 Equals (Convert (Self.L_Data, GetL (Self.Left)),
                         GetL (Self.Right));
               Trace ("In Unify_LR, both defined, returning " & C'Image);
            end return;

         end if;

         --  Left is defined, right is not, give right the value of left and
         --  return true.
         if
           Set_Value (Self.Right, Convert (Self.L_Data, Get_Value (Self.Left)))
         then
            Self.State := Right_Changed;
            Trace ("In Unify_LR, propagating right, from "
                   & Image (Self.Left) & " to "
                   & Image (Self.Right));
            Trace ("In Unify_LR, From value is : "
                   & Element_Image (GetL (Self.Left)));
            Trace ("In Unify_LR, To value is : "
                   & Element_Image (GetL (Self.Right)));
            return True;
         else
            Trace ("In Unify_LR, propagating right failed ! ");
            return False;
         end if;
      end if;

      if not Is_Defined (Self.Right) then
         --  TODO??? Another solution, rather than force the users to define
         --  domains for variables before binding them together, would be to
         --  defer unification, like it is done for predicates.
         --
         --  It is not sure it would be better: The outcome would probably
         --  be potentially slower solving. However, it might be that some
         --  equations cannot be expressed easily if we don't have unification
         --  defer.

         raise Early_Binding_Error
           with "Variable " & Image (Self.Right) & " and " & Image (Self.Left)
           & " are not defined yet.";
      end if;

      --  Right is defined, left is not, give left the value of right and
      --  return true.
      if
         Set_Value (Self.Left, Convert (Self.R_Data, Get_Value (Self.Right)))
      then
         Self.State := Left_Changed;
         Trace ("In Unify_LR, propagating left, from "
                   & Image (Self.Right) & " to "
                   & Image (Self.Left));
         return True;
      else
         Trace ("In Unify_LR, propagating left failed ! ");
         return False;
      end if;
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
end Langkit_Support.Adalog.Unify_LR;
