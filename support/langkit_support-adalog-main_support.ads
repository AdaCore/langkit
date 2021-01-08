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

--  Provide common support material for unit tests

with Ada.Containers.Vectors;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Eq_Same;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;

package Langkit_Support.Adalog.Main_Support is

   function Element_Image (I : Integer) return String is (I'Image);
   package Eq_Int is new Eq_Same (Integer);
   package Pred_Int is new Dyn_Predicate (Integer, Eq_Int.Refs.Raw_Logic_Var);

   function "+" (R : Relation) return Relation;
   --  Register R and return it. This is used to keep track of allocated
   --  relations in testcases.

   procedure Release_Relations;
   --  Decrement the ref-count of all relations registered with "+"

private

   package Relation_Vectors is new Ada.Containers.Vectors
     (Positive, Relation);
   Relations : Relation_Vectors.Vector;

end Langkit_Support.Adalog.Main_Support;
