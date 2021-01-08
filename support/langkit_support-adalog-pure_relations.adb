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

package body Langkit_Support.Adalog.Pure_Relations is

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out False_Relation_Rec) return Solving_State is
      pragma Unreferenced (Self);
   begin
      return Unsatisfied;
   end Apply;

   ------------------
   -- Custom_Image --
   ------------------

   function Custom_Image (Self : False_Relation_Rec) return String is
      pragma Unreferenced (Self);
   begin
      return "<False>";
   end Custom_Image;

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out True_Relation_Rec) return Solving_State is
      pragma Unreferenced (Self);
   begin
      return Satisfied;
   end Apply;

   ------------------
   -- Custom_Image --
   ------------------

   function Custom_Image (Self : True_Relation_Rec) return String is
      pragma Unreferenced (Self);
   begin
      return "<True>";
   end Custom_Image;

end Langkit_Support.Adalog.Pure_Relations;
