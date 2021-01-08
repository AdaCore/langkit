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

with Langkit_Support.Array_Utils;

with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;
with Langkit_Support.Adalog.Pure_Relations;
use Langkit_Support.Adalog.Pure_Relations;

package body Langkit_Support.Adalog.Operations is

   package Rel_Arrays_Utils is new Langkit_Support.Array_Utils
     (Relation, Positive, Relation_Array);

   procedure Initialize_Working_Queue (Self : in out Base_Aggregate_Rel'Class);
   --  Initialize Self's working queue with starting indexes

   function Get_From_Queue
     (Self : Base_Aggregate_Rel'Class; Index : Positive) return Relation
   is (Self.Sub_Rels (Self.Working_Queue (Index)));
   --  Return the relation corresponding to Index in the working queue

   procedure Set_Completed
     (Self : in out Base_Aggregate_Rel'Class; Index : Positive);
   --  Tag the Index'th element in the working queue as completed: it will not
   --  be evaluated anymore.

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Base_Aggregate_Rel) is
   begin
      for R of Self.Sub_Rels loop
         R.Reset;
      end loop;
      Self.Next := 1;
   end Reset;

   -------------
   -- Cleanup --
   -------------

   overriding procedure Cleanup (Self : in out Base_Aggregate_Rel) is
   begin
      for R of Self.Sub_Rels loop
         Dec_Ref (R);
      end loop;
   end Cleanup;

   ------------------
   -- Custom_Image --
   ------------------

   overriding function Custom_Image (Self : Any_Rel) return String
   is ("<Any>");

   ------------------
   -- Custom_Image --
   ------------------

   overriding function Custom_Image (Self : All_Rel) return String
   is ("<All>");

   ----------------
   -- Solve_Impl --
   ----------------

   overriding function Solve_Impl
     (Self    : in out Any_Rel;
      Context : in out Solving_Context) return Solving_State
   is
      Has_Progressed : Boolean := False;
      --  Whether there was progress at all during this call

      Stalled : Boolean := False;
      --  We want to stop trying to evaluate sub-expressions if there was no
      --  progress made during one whole iteration. This boolean is clear iff
      --  whether the current iteration (FOR loop below) made progress.

      procedure Tag_Progress;
      --  Update the above flags to indicate progress has been made

      ------------------
      -- Tag_Progress --
      ------------------

      procedure Tag_Progress is
      begin
         Has_Progressed := True;
         Stalled := False;
      end Tag_Progress;

   begin
      Trace ("In Any_Rel:");

      while not Stalled loop
         Stalled := True;
         for I in Self.Next .. Self.Count loop
            Tick (Context);

            case Get_From_Queue (Self, I).Solve (Context) is
               when No_Progress =>
                  --  TODO: Previous implementation was wrong: it was just a
                  --  null statement, so resolution would skip to the next one
                  --  here, and if the next one was SAT then return SAT, but
                  --  without setting the current relation to the next one, so
                  --  when executing resolution again, then Any would return
                  --  No_Progress again, rather than calling the previously SAT
                  --  relation which would have been the proper behavior.
                  --
                  --  I didn't figure out how to do that, rather implementing
                  --  another behavior: on no progress, we reset self and
                  --  return no progress. This is correct but probably not the
                  --  most efficient behavior.

                  Self.Reset;
                  return No_Progress;

               when Progress =>
                  Tag_Progress;

               when Satisfied =>
                  return Satisfied;

               when Unsatisfied =>
                  Tag_Progress;
                  Set_Completed (Self, I);
                  Trace ("In Any_Rel: relation unsatisfied, get the next one");
            end case;
         end loop;
      end loop;

      --  If we completed the evaluation of all sub-relations without finding
      --  one Satisfied, then we know this aggregate relation is Unsatisfied.

      if Self.Next > Self.Count then
         Trace ("In Any_Rel: no relation satisfied");
         return Unsatisfied;

      elsif Has_Progressed then
         Trace ("In Any_Rel: some progress made, now returning");
         return Progress;

      else
         Trace ("In Any_Rel: no progress made, now returning");
         return No_Progress;
      end if;
   end Solve_Impl;

   ----------------
   -- Solve_Impl --
   ----------------

   overriding function Solve_Impl
     (Self    : in out All_Rel;
      Context : in out Solving_Context) return Solving_State
   is
      Has_Progressed : Boolean := False;
      --  Whether there was progress at all during this call

      Stalled : Boolean := False;
      --  We want to stop trying to evaluate sub-expressions if there was no
      --  progress made during one whole iteration. This boolean is clear iff
      --  whether the current iteration (FOR loop below) made progress.

      procedure Tag_Progress;
      --  Update the above flags to indicate progress has been made

      ------------------
      -- Tag_Progress --
      ------------------

      procedure Tag_Progress is
      begin
         Has_Progressed := True;
         Stalled := False;
      end Tag_Progress;

      I : Positive;
   begin
      Trace ("In All_Rel:");

      if Self.Next > Self.Count then
         Trace ("In All_Rel: last relation was evaluated: getting back to it");
         Self.Next := Self.Count;
      end if;

      while not Stalled loop
         Stalled := True;
         I := Self.Next;
         Inner_Loop : while I <= Self.Count loop
            Tick (Context);

            case Get_From_Queue (Self, I).Solve (Context) is
               when No_Progress =>
                  I := I + 1;

               when Progress =>
                  Tag_Progress;
                  I := I + 1;

               when Satisfied =>
                  Tag_Progress;
                  Trace ("In All_Rel: relation satisfied");
                  Set_Completed (Self, I);
                  I := I + 1;

               when Unsatisfied =>
                  if Self.Next = 1 then
                     Trace ("In All_Rel: first evaluated relation"
                            & " unsatisfied");
                     return Unsatisfied;
                  end if;

                  Tag_Progress;
                  Trace ("In All_Rel: relation unsatisfied, resetting it and"
                         & " getting back to the previous one");
                  Get_From_Queue (Self, I).Reset;

                  --  We managed to evaluate this sub-relation, so we must move
                  --  it to the list of completed sub-relations. This
                  --  increments Self.Next, and we must get back to the
                  --  previous sub-relation, hence the -2.
                  Set_Completed (Self, I);
                  Self.Next := Self.Next - 2;
                  exit Inner_Loop;
            end case;
         end loop Inner_Loop;
      end loop;

      --  If we completed the evaluation of all sub-relations without finding
      --  one Unsatisfied, then we know this aggregate relation is Satisfied.

      if Self.Next > Self.Count then
         Trace ("In All_Rel: all relations satisfied");
         return Satisfied;

      elsif Has_Progressed then
         Trace ("In All_Rel: some progress made, now returning");
         return Progress;

      else
         Trace ("In All_Rel: no progress made, now returning");
         return No_Progress;
      end if;
   end Solve_Impl;

   --------------
   -- Logic_Or --
   --------------

   function Logic_Or
     (L, R      : Relation;
      Sloc_Info : String_Access := null) return Relation is
   begin
      return Logic_Any ((L, R), Sloc_Info);
   end Logic_Or;

   ---------------
   -- Logic_And --
   ---------------

   function Logic_And
     (L, R      : Relation;
      Sloc_Info : String_Access := null) return Relation is
   begin
      return Logic_All ((L, R), Sloc_Info);
   end Logic_And;

   ---------------
   -- Logic_Any --
   ---------------

   function Logic_Any
     (Rels : Relation_Array;
      Sloc_Info : String_Access := null) return Relation
   is
      function Process (Rel : Relation) return Relation_Array
      is
        (if Rel.all in False_Relation.Rel'Class then Empty_Array
         elsif Rel.all in Any_Rel'Class then Any_Rel (Rel.all).Sub_Rels
         else (1 => Rel));

      function Process_Rels is new Rel_Arrays_Utils.Id_Flat_Map_Gen (Process);

      Keep_Rels : constant Relation_Array := Process_Rels (Rels);
   begin

      if Keep_Rels'Length = 0 then
         return False_Rel (Sloc_Info => Sloc_Info);
      end if;

      for Rel of Keep_Rels loop
         Inc_Ref (Rel);
      end loop;

      if Keep_Rels'Length = 1 then
         return Keep_Rels (1);
      end if;

      declare
         Result : constant Base_Aggregate_Rel_Access := new Any_Rel'
           (Ref_Count     => 1,
            Count         => Keep_Rels'Length,
            Next          => <>,
            Sub_Rels      => Keep_Rels,
            Sloc_Info     => Sloc_Info,
            Working_Queue => <>);
      begin
         Initialize_Working_Queue (Result.all);
         return Relation (Result);
      end;
   end Logic_Any;

   ---------------
   -- Logic_All --
   ---------------

   function Logic_All
     (Rels : Relation_Array; Sloc_Info : String_Access := null) return Relation
   is

      function Process (Rel : Relation) return Relation_Array
      is
        (if Rel.all in True_Relation.Rel'Class then Empty_Array
         elsif Rel.all in All_Rel'Class then All_Rel (Rel.all).Sub_Rels
         else (1 => Rel));

      function Process_Rels is new Rel_Arrays_Utils.Id_Flat_Map_Gen (Process);

      Keep_Rels : constant Relation_Array := Process_Rels (Rels);

   begin
      if Keep_Rels'Length = 0 then
         return True_Rel (Sloc_Info => Sloc_Info);
      end if;

      for Rel of Keep_Rels loop
         Inc_Ref (Rel);
      end loop;

      if Keep_Rels'Length = 1 then
         return Keep_Rels (1);
      end if;

      declare
         Result : constant Base_Aggregate_Rel_Access := new All_Rel'
           (Ref_Count     => 1,
            Count         => Keep_Rels'Length,
            Next          => <>,
            Sub_Rels      => Keep_Rels,
            Sloc_Info     => Sloc_Info,
            Working_Queue => <>);
      begin
         Initialize_Working_Queue (Result.all);
         return Relation (Result);
      end;
   end Logic_All;

   ------------------------------
   -- Initialize_Working_Queue --
   ------------------------------

   procedure Initialize_Working_Queue (Self : in out Base_Aggregate_Rel'Class)
   is
   begin
      for I in Self.Working_Queue'Range loop
         Self.Working_Queue (I) := I;
      end loop;
   end Initialize_Working_Queue;

   -------------------
   -- Set_Completed --
   -------------------

   procedure Set_Completed
     (Self : in out Base_Aggregate_Rel'Class; Index : Positive)
   is
      Saved : constant Positive := Self.Working_Queue (Self.Next);
   begin
      Self.Working_Queue (Self.Next) := Self.Working_Queue (Index);
      Self.Working_Queue (Index) := Saved;
      Self.Next := Self.Next + 1;
   end Set_Completed;

end Langkit_Support.Adalog.Operations;
