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

package body Langkit_Support.Slocs is

   -------------
   -- Compare --
   -------------

   function Compare
     (Reference, Compared : Source_Location) return Relative_Position
   is
   begin
      --  First compare line numbers...

      if Compared.Line < Reference.Line then
         return Before;
      elsif Reference.Line < Compared.Line then
         return After;

      --  Past this point, we know that both are on the same line, so now
      --  compare column numbers.

      elsif Compared.Column < Reference.Column then
         return Before;
      elsif Reference.Column < Compared.Column then
         return After;
      else
         return Inside;
      end if;
   end Compare;

   -------------
   -- Compare --
   -------------

   function Compare
     (Sloc_Range : Source_Location_Range;
      Sloc       : Source_Location) return Relative_Position
   is
      Inclusive_End_Sloc : Source_Location := End_Sloc (Sloc_Range);
   begin
      --  End_Sloc returns an exclusive end sloc. Switch to an inclusive
      --  representation for computation.

      Inclusive_End_Sloc.Column := Inclusive_End_Sloc.Column - 1;

      return (case Compare (Start_Sloc (Sloc_Range), Sloc) is
                 when Before => Before,
                 when Inside | After =>
                   (if Compare (Inclusive_End_Sloc, Sloc) = After
                    then After
                    else Inside));
   end Compare;

end Langkit_Support.Slocs;
