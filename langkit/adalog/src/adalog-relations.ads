------------------------------------------------------------------------------
--                               A D A L O G                                --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Internal implementation package, not to be used directly by users a-priori.
--  TODO??? document the inner workings a bit more.

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;

package Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   generic
      type Ty is private;
      with function Apply (Inst : in out Ty) return Boolean is <>;
      with procedure Free (Inst : in out Ty) is <>;
   package Pure_Relation is
      --  This generic package represents a relation that will always
      --  yield the same result, and does not produce any side effects.

      type Rel is new I_Relation with record
         Done : Boolean := False;
         Rel  : Ty;
      end record;

      overriding function Solve_Impl (Inst : in out Rel) return Boolean;
      overriding procedure Reset (Inst : in out Rel) is null;
      overriding procedure Cleanup (Inst : in out Rel);
   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   generic
      type Ty is private;
      with function Apply (Inst : in out Ty) return Boolean is <>;
      with procedure Revert (Inst : in out Ty) is <>;
      with procedure Free (Inst : in out Ty) is <>;
      with function Custom_Image (Inst : Ty) return String is <>;
   package Stateful_Relation is

      --  This package represents a relation that has state,
      --  and that needs to be reset to be reused.

      type State_Type is (Start, Success, Finish);

      type Rel is new I_Relation with record
         State : State_Type := Start;
         Rel   : Ty;
      end record;

      overriding function Solve_Impl (Inst : in out Rel) return Boolean;
      overriding procedure Reset (Inst : in out Rel);
      overriding procedure Cleanup (Inst : in out Rel);

      overriding function Custom_Image (Inst : Rel) return String
      is (Custom_Image (Inst.Rel));

   end Stateful_Relation;

end Adalog.Relations;
