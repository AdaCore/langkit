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

with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   package body Pure_Relation is

      ----------------
      -- Solve_Impl --
      ----------------

      function Solve_Impl
        (Self    : in out Rel;
         Context : in out Solving_Context) return Solving_State
      is
         pragma Unreferenced (Context);
      begin
         if Self.Done then
            Trace ("In Pure_Relation: already done, returning UNSATISFIED");
            return Unsatisfied;
         end if;
         Self.Done := True;
         Trace ("In Pure_Relation: not done yet, evaluating...");
         return Apply (Self.Rel);
      end Solve_Impl;

      -----------
      -- Reset --
      -----------

      overriding procedure Reset (Self : in out Rel) is
      begin
         Self.Done := False;
      end Reset;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup (Self : in out Rel) is
      begin
         Free (Self.Rel);
      end Cleanup;

      ------------------
      -- Custom_Image --
      ------------------

      overriding function Custom_Image (Self : Rel) return String
      is
      begin
         return Custom_Image (Self.Rel);
      end Custom_Image;

   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   package body Stateful_Relation is

      ----------------
      -- Solve_Impl --
      ----------------

      function Solve_Impl
        (Self    : in out Rel;
         Context : in out Solving_Context) return Solving_State
      is
         pragma Unreferenced (Context);
      begin
         case Self.State is
            when Start =>
               Trace ("In Stateful_Relation: Start state, evaluating...");
               case Apply (Self.Rel) is
                  when No_Progress => return No_Progress;
                  when Progress    => return Progress;

                  when Satisfied =>
                     Trace ("In Stateful_Relation: moving to Success state");
                     Self.State := Success;
                     return Satisfied;

                  when Unsatisfied =>
                     Trace ("In Stateful_Relation: moving to Finish state");
                     Self.State := Finish;
                     return Unsatisfied;
               end case;

            when Success =>
               Trace ("In Stateful_Relation: Success state, reverting, moving"
                      & " to Finish state and returning UNSATISFIED");
               Revert (Self.Rel);
               Self.State := Finish;
               return Unsatisfied;

            when Finish =>
               Trace ("In Stateful_Relation: Finish state, returning"
                      & " UNSATISFIED");
               return Unsatisfied;
         end case;
      end Solve_Impl;

      -----------
      -- Reset --
      -----------

      procedure Reset (Self : in out Rel) is
      begin
         Self.State := Start;
         Revert (Self.Rel);
      end Reset;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup (Self : in out Rel) is
      begin
         Free (Self.Rel);
      end Cleanup;

   end Stateful_Relation;

end Langkit_Support.Adalog.Relations;
