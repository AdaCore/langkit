--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

package body Liblktlang_Support.Rewriting.Types is

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count
     (Handle : Node_Rewriting_Handle_Access) return Natural is
   begin
      return
        (case Handle.Children.Kind is
         when Unexpanded          => Children_Count (Handle.Node),
         when Expanded_Regular    => Natural (Handle.Children.Vector.Length),
         when Expanded_List       => Handle.Children.Count,
         when Expanded_Token_Node => 0);
   end Children_Count;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle_Access) return Text_Type is
   begin
      case Handle.Children.Kind is
         when Unexpanded =>
            if Handle.Kind.Is_Token_Node then
               return Handle.Node.Text;
            else
               raise Program_Error;
            end if;
         when Expanded_Regular | Expanded_List =>
            return (raise Program_Error);
         when Expanded_Token_Node =>
            return To_Text (Handle.Children.Text);
      end case;
   end Text;

end Liblktlang_Support.Rewriting.Types;
