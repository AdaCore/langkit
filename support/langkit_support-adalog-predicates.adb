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

package body Langkit_Support.Adalog.Predicates is

   ---------------
   -- Predicate --
   ---------------

   package body Predicate is

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Predicate_Logic) is
      begin
         Free (Self.Pred);
      end Free;

      -----------
      -- Apply --
      -----------

      pragma Warnings (Off);
      --  Hide complains that Self could be IN, as we are forced to make it IN
      --  OUT for generic instantiation.
      function Apply (Self : in out Predicate_Logic) return Solving_State is
      pragma Warnings (On);
      begin
         if not Is_Defined (Self.Ref) then
            Trace ("In Predicate apply, var " & Image (Self.Ref)
                   & " not defined, no progress");
            return No_Progress;
         end if;

         Trace ("In Predicate apply, calling predicate");
         declare
            R : constant Boolean := Call (Self.Pred, Get_Value (Self.Ref));
         begin
            Trace (R'Img);
            return +R;
         end;
      end Apply;

      ------------
      -- Revert --
      ------------

      procedure Revert (Self : in out Predicate_Logic) is
      begin
         null;
      end Revert;

   end Predicate;

   -----------------
   -- N_Predicate --
   -----------------

   package body N_Predicate is

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Predicate_Logic) is
      begin
         Free (Self.Pred);
      end Free;

      -----------
      -- Apply --
      -----------

      pragma Warnings (Off);
      --  Hide complains that Self could be IN, as we are forced to make it IN
      --  OUT for generic instantiation.
      function Apply (Self : in out Predicate_Logic) return Solving_State is
      pragma Warnings (On);
      begin
         for Ref of Self.Refs loop
            if not Is_Defined (Ref) then
               Trace ("In N_Predicate apply, var " & Image (Ref)
                      & " not defined, deferring application");
               return No_Progress;
            end if;
         end loop;

         Trace ("In N_Predicate apply, calling predicate");
         declare
            Vals : Val_Array (1 .. Arity);
         begin
            for I in Self.Refs'Range loop
               Vals (I) := Get_Value (Self.Refs (I));
            end loop;

            return +Call (Self.Pred, Vals);
         end;
      end Apply;

      ------------
      -- Revert --
      ------------

      procedure Revert (Self : in out Predicate_Logic) is
      begin
         null;
      end Revert;

   end N_Predicate;

   package body Predicate_2 is

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Predicate_Wrapper) is
      begin
         Free (Self.T);
      end Free;

      ------------
      -- Create --
      ------------

      function Create
        (L, R : Var.Var; Pred : Predicate_Type) return Relation
      is
      begin
         return Predicate_2_Internal.Create
           ((L, R), Predicate_Wrapper'(Pred, L, R));
      end Create;

   end Predicate_2;

end Langkit_Support.Adalog.Predicates;
