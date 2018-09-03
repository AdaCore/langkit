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

--  Provide miscellaneous types to Langkit-generated libraries

with Interfaces;

package Langkit_Support.Types is

   type Version_Number is new Interfaces.Unsigned_64;
   --  Number associated to a resource. This number is supposed to be unique
   --  for some class of resource. For instance unique in all analysis contexts
   --  a process creates.

   type Comparison_Relation is
     (Less_Than, Less_Or_Equal, Greater_Than, Greater_Or_Equal);

end Langkit_Support.Types;
