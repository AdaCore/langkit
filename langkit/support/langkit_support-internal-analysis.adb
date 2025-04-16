--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;

package body Langkit_Support.Internal.Analysis is

   ----------------------------
   -- Create_Node_Safety_Net --
   ----------------------------

   function Create_Node_Safety_Net
     (Id         : Language_Id;
      Context    : Internal_Context;
      Unit       : Internal_Unit;
      Rebindings : Env_Rebindings) return Node_Safety_Net
   is
      Desc : constant Language_Descriptor_Access := +Id;
   begin
      return
        (Context            => Context,
         Context_Version    => Version (Context),
         Unit               => Unit,
         Unit_Version       => Desc.Unit_Version (Unit),
         Rebindings_Version => (if Rebindings = null
                                then 0
                                else Rebindings.Version));
   end Create_Node_Safety_Net;

   -------------
   -- Version --
   -------------

   function Version (Context : Internal_Context) return Version_Number is
      C : Internal_Context_Stable_ABI
        with Import, Address => System.Address (Context);
   begin
      return C.Version;
   end Version;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Reparsed : in out Reparsed_Unit) is
   begin
      Free (Reparsed.TDH);
      Reparsed.Diagnostics := Diagnostics_Vectors.Empty_Vector;
      Free (Reparsed.Ast_Mem_Pool);
      Reparsed.Ast_Root := Langkit_Support.Internal.Analysis.No_Internal_Node;
   end Destroy;

end Langkit_Support.Internal.Analysis;
