--
--  Copyright (C) 2014-2022, AdaCore
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
         Context_Version    => Desc.Context_Version (Context),
         Unit               => Unit,
         Unit_Version       => Desc.Unit_Version (Unit),
         Rebindings_Version => (if Rebindings = null
                                then 0
                                else Rebindings.Version));
   end Create_Node_Safety_Net;

end Langkit_Support.Internal.Analysis;
