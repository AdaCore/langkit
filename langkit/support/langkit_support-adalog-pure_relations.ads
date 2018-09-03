------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Relations;
use Langkit_Support.Adalog.Relations;

package Langkit_Support.Adalog.Pure_Relations is

   --------------------
   -- False_Relation --
   --------------------

   type False_Relation_Rec is null record;
   function Apply (Self : in out False_Relation_Rec) return Solving_State;
   procedure Free (Self : in out False_Relation_Rec) is null;
   function Custom_Image (Self : False_Relation_Rec) return String;
   package False_Relation is new Pure_Relation (Ty => False_Relation_Rec);

   function False_Rel return Relation
   is (new False_Relation.Rel'(others => <>));

   -------------------
   -- True_Relation --
   -------------------

   type True_Relation_Rec is null record;
   function Apply (Self : in out True_Relation_Rec) return Solving_State;
   procedure Free (Self : in out True_Relation_Rec) is null;
   function Custom_Image (Self : True_Relation_Rec) return String;

   package True_Relation is new Pure_Relation (Ty => True_Relation_Rec);

   function True_Rel return Relation is (new True_Relation.Rel'(others => <>));

end Langkit_Support.Adalog.Pure_Relations;
