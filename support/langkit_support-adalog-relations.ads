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

--  Internal implementation package, not to be used directly by users a-priori.
--  TODO??? document the inner workings a bit more.

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;

package Langkit_Support.Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   generic
      type Ty is private;
      with function Apply (Self : in out Ty) return Solving_State is <>;
      with procedure Free (Self : in out Ty) is <>;
      with function Custom_Image (Self : Ty) return String is <>;
   package Pure_Relation is
      --  This generic package represents a relation that will always
      --  yield the same result, and does not produce any side effects.

      type Rel is new Base_Relation with record
         Done : Boolean := False;
         Rel  : Ty;
      end record;

      overriding function Solve_Impl
        (Self    : in out Rel;
         Context : in out Solving_Context) return Solving_State;
      overriding procedure Reset (Self : in out Rel);
      overriding procedure Cleanup (Self : in out Rel);
      overriding function Custom_Image (Self : Rel) return String;
   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   generic
      type Ty is private;
      with function Apply (Self : in out Ty) return Solving_State is <>;
      with procedure Revert (Self : in out Ty) is <>;
      with procedure Free (Self : in out Ty) is <>;
      with function Custom_Image (Self : Ty) return String is <>;
   package Stateful_Relation is

      --  This package represents a relation that has state,
      --  and that needs to be reset to be reused.

      type State_Type is (Start, Success, Finish);

      type Rel is new Base_Relation with record
         State : State_Type := Start;
         Rel   : Ty;
      end record;

      overriding function Solve_Impl
        (Self    : in out Rel;
         Context : in out Solving_Context) return Solving_State;
      overriding procedure Reset (Self : in out Rel);
      overriding procedure Cleanup (Self : in out Rel);

      overriding function Custom_Image (Self : Rel) return String
      is (Custom_Image (Self.Rel));

   end Stateful_Relation;

end Langkit_Support.Adalog.Relations;
