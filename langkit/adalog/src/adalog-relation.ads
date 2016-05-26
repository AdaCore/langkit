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

with Adalog.Relation_Interface;

--  Internal implementation package, not to be used directly by users a-priori.
--  TODO??? document the inner workings a bit more.

package Adalog.Relation is

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

      type Rel is record
         Done : Boolean := False;
         Rel  : Ty;
      end record;

      function Call (Inst : in out Rel) return Boolean;
      procedure Reset (Inst : in out Rel) is null;
      procedure Free (Inst : in out Rel);

      package Impl is new Relation_Interface (Ty => Rel);
   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   generic
      type Ty is private;
      with function Apply (Inst : in out Ty) return Boolean is <>;
      with procedure Revert (Inst : in out Ty) is <>;
      with procedure Free (Inst : in out Ty) is <>;
   package Stateful_Relation is

      --  This package represents a relation that has state,
      --  and that needs to be reset to be reused.

      type State_Type is (Start, Success, Finish);

      type Rel is record
         State : State_Type := Start;
         Rel   : Ty;
      end record;

      function Call (Inst : in out Rel) return Boolean;
      procedure Reset (Inst : in out Rel);
      procedure Free (Inst : in out Rel);

      package Impl is new Relation_Interface (Ty => Rel);
   end Stateful_Relation;

end Adalog.Relation;
