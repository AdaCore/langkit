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

--  This package contains definitions for exceptions common to all
--  Langkit-generated libraries.

package Langkit_Support.Errors with Preelaborate is

   Native_Exception        : exception;
   Precondition_Failure    : exception;
   Property_Error          : exception;
   Invalid_Unit_Name_Error : exception;
   Invalid_Symbol_Error    : exception;
   Stale_Reference_Error   : exception;
   Unknown_Charset         : exception;
   Invalid_Input           : exception;

   package Introspection is
      Bad_Type_Error      : exception;
      Out_Of_Bounds_Error : exception;
   end Introspection;

   package Rewriting is
      Template_Format_Error        : exception;
      Template_Args_Error          : exception;
      Template_Instantiation_Error : exception;
   end Rewriting;

end Langkit_Support.Errors;
