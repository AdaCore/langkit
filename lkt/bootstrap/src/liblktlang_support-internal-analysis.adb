--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Liblktlang_Support.Internal.Descriptor;
use Liblktlang_Support.Internal.Descriptor;

package body Liblktlang_Support.Internal.Analysis is

   use type System.Address;

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

   --------------------------
   -- Get_Rewriting_Handle --
   --------------------------

   function Get_Rewriting_Handle
     (Context : Internal_Context) return System.Address
   is
      C : Internal_Context_Stable_ABI
        with Import, Address => System.Address (Context);
   begin
      return C.Rewriting_Handle;
   end Get_Rewriting_Handle;

   --------------------------
   -- Set_Rewriting_Handle --
   --------------------------

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Pointer : System.Address)
   is
      C : Internal_Context_Stable_ABI
        with Import, Address => System.Address (Context);
   begin
      C.Rewriting_Handle := Pointer;

      --  If we have deallocated a rewriting session, bump the rewriting
      --  version number to mark existing references to handles as stale.

      if Pointer = System.Null_Address then
         C.Rewriting_Version := C.Rewriting_Version + 1;
      end if;
   end Set_Rewriting_Handle;

   -----------------------
   -- Rewriting_Version --
   -----------------------

   function Rewriting_Version
     (Context : Internal_Context) return Version_Number
   is
      C : Internal_Context_Stable_ABI
        with Import, Address => System.Address (Context);
   begin
      return C.Rewriting_Version;
   end Rewriting_Version;

   ------------------------------
   -- Normalized_Unit_Filename --
   ------------------------------

   function Normalized_Unit_Filename
     (Cache : in out Virtual_File_Cache; Filename : String)
      return GNATCOLL.VFS.Virtual_File
   is
      use GNATCOLL.VFS;
      use Virtual_File_Maps;
      Key      : constant Unbounded_String := To_Unbounded_String (Filename);
      Cur      : Cursor;
      Inserted : Boolean;
   begin
      Cache.Insert (Key, Cur, Inserted);
      if Inserted then
         declare
            F : constant Virtual_File :=
              Create
                (Create_From_Base (+Filename).Full_Name, Normalize => True);
         begin
            Cache.Replace_Element (Cur, F);
            return F;
         end;
      else
         return Element (Cur);
      end if;
   end Normalized_Unit_Filename;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Reparsed : in out Reparsed_Unit) is
   begin
      Free (Reparsed.TDH);
      Reparsed.Diagnostics := Diagnostics_Vectors.Empty_Vector;
      Free (Reparsed.Ast_Mem_Pool);
      Reparsed.Ast_Root := Liblktlang_Support.Internal.Analysis.No_Internal_Node;
   end Destroy;

end Liblktlang_Support.Internal.Analysis;
