------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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
        and then Runtime_Debug_State in Trace | Step | Step_At_First_Unsat;
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
