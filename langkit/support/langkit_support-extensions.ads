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

--  This package helps to deal with AST_Node extensions

package Langkit_Support.Extensions is

   type Extension_ID is new Natural;
   --  Unique identifier for a class of extensions.
   --
   --  AST_Node can hold many extensions, so we need a way to perform lookups
   --  on a specific class of extension (for instance: Python bindings
   --  extensions). Having a unique identifier is the chosen way to achieve
   --  this.

   function Register_Extension (Name : String) return Extension_ID;
   --  Register an extension to the engine and return the corresponding ID.
   --
   --  Extension_ID is a scalar in order to make comparison as fast as
   --  possible. On the other hand, it is more convenient for plug-ins to use
   --  human-readable strings to identify extensions. So this function makes
   --  the conversion between unique human-readable strings and unique IDs
   --  (i.e. it returns always the same Extension_ID for the same string and
   --  always returns different IDs for different strings).

   function Has_Extensions return Boolean;
   --  Return whether at least one extension was registered

end Langkit_Support.Extensions;
