with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Adalog.Logic_Var;

--  This package contains the implementation of logic variables. This is done
--  by implementing base simple types, and instantiating the Adalog.Logic_Var
--  formal package.
--
--  There are two different implementations that vary only in terms of memory
--  management:
--
--  - One is a refcounted, controlled-object based logic variable, so its
--  memory management is automatic.
--
--  - The other is a naked access, so memory management is left to the user.
--

generic
   type Element_Type is private;
   with procedure Inc_Ref (E : Element_Type);
   with procedure Dec_Ref (E : in out Element_Type);
   with function Element_Image (E : Element_Type) return String;
package Langkit_Support.Adalog.Logic_Ref is

   ------------------------------
   -- Base logic variable type --
   ------------------------------

   --  This type implements the common logic for a logic variable, and the
   --  common needed operations. See Adalog.Logic_Var for the documentation
   --  of those operations.

   --  This type, however, has by-value semantics, where we want the end
   --  implementations to have by-reference semantics.

   type Var is record
      Reset             : Boolean := True;
      --  Whether this variable is set or not. Reset is True when the variable
      --  has no value.

      Value             : Element_Type;
      --  The value of this logic variable, when it is set

      Dbg_Name          : String_Access;
      --  Access to a string representing the name of this variable. Using
      --  this, you can name your variable with human readable names, and
      --  the debugging facilities of Adalog will use it to display it in
      --  equations.
   end record;

   -------------------------------
   -- Base primitive operations --
   -------------------------------

   procedure Reset (Self : in out Var);
   function Is_Defined (Self : Var) return Boolean;
   function Set_Value (Self : in out Var; Data : Element_Type) return Boolean;
   function Get_Value (Self : Var) return Element_Type;
   procedure Destroy (Self : in out Var);
   function Image (Self : Var) return String is
     (if Self.Dbg_Name /= null then Self.Dbg_Name.all else "None");

   -----------------------
   -- Raw variable type --
   -----------------------

   --  This type is a reference to a logic variable implemented with a simple
   --  unsafe access. To use if you want maximum performance and are ready
   --  to manage your memory manually.

   type Raw_Var is access all Var;
   procedure Reset (Self : in out Raw_Var);
   function Is_Defined (Self : Raw_Var) return Boolean;
   function Set_Value
     (Self : in out Raw_Var; Data : Element_Type) return Boolean;
   function Get_Value (Self : Raw_Var) return Element_Type;
   function Create return Raw_Var;

   function Image (Self : Raw_Var) return String is
     (Image (Self.all));

   ------------------------------------
   -- Formal packages instantiations --
   ------------------------------------

   package Raw_Logic_Var is new Adalog.Logic_Var
     (Raw_Var, Element_Type, Inc_Ref, Dec_Ref);

end Langkit_Support.Adalog.Logic_Ref;
