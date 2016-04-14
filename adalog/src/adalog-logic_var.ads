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

pragma Warnings (Off);

generic
   type Logic_Var_Type is private;
   type Element_Type is private;

   with procedure Reset (Self : in out Logic_Var_Type) is <>;
   with function Is_Defined (Self : Logic_Var_Type) return Boolean is <>;
   with procedure SetL (Self : in out Logic_Var_Type; Data : Element_Type)
     is <>
       with Inline => True;
   with function GetL (Self : Logic_Var_Type) return Element_Type
      is <>
       with Inline => True;
   with function Create return Logic_Var_Type is <>;
package Adalog.Logic_Var is
   subtype Var is Logic_Var_Type;
end Adalog.Logic_Var;
