--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
