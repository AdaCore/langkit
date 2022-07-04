--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;

package body Langkit_Support.Adalog.Debug is

   Runtime_Debug_State : Debug_State_Type := None;

   -----------
   -- Trace --
   -----------

   procedure Trace (Str : String) is
   begin
      if Debug then
         Put_Line (Str);
      end if;
   end Trace;

   -----------
   -- Debug --
   -----------

   function Debug return Boolean is
   begin
      return Debug_Enabled
        and then Runtime_Debug_State in Trace .. Step_At_First_Unsat;
   end Debug;

   ---------------------
   -- Set_Debug_State --
   ---------------------

   procedure Set_Debug_State (Val : Debug_State_Type) is
   begin
      Runtime_Debug_State := Val;
   end Set_Debug_State;

   ---------------------
   -- Is_Step_Mode_On --
   ---------------------

   function Debug_State return Debug_State_Type
   is
     (if Debug_Enabled then Runtime_Debug_State else None);

end Langkit_Support.Adalog.Debug;
