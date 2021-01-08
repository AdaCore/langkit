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

with Ada.Containers.Vectors;

package body Langkit_Support.Iterators is

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (I    : in out Iterator'Class;
      Proc : access procedure (Element : Element_Type))
   is
      Element : Element_Type;
   begin
      while I.Next (Element) loop
         Proc (Element);
      end loop;
   end Iterate;

   -------------
   -- Consume --
   -------------

   function Consume (I : Iterator'Class) return Element_Array is
      package Element_Vectors is new Ada.Containers.Vectors
        (Positive, Element_Type);

      Element : Element_Type;
      V       : Element_Vectors.Vector;
   begin
      --  Note: This is bad design in Ada: We're hiding mutation of the
      --  Iterator object, because if we make it mutable, then you can no
      --  longer call consume on an expression that returns an Iterator, which
      --  in user APIs is not very friendly, because it means you cannot write
      --  write::
      --
      --      for Element of Node.Find (...).Consume loop
      --         ...
      --      end loop;
      --
      --  You have to declare the iterator explicitly.

      while I'Unrestricted_Access.Next (Element) loop
         V.Append (Element);
      end loop;

      declare
         Result : Element_Array (1 .. Natural (V.Length));
      begin
         for I in Result'Range loop
            Result (I) := V.Element (I);
         end loop;
         return Result;
      end;
   end Consume;

end Langkit_Support.Iterators;
