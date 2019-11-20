------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates;
with Langkit_Support.Adalog.Pure_Relations;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Langkit_Support.Adalog.Solver is

   procedure Append_Var (Self : in out Relation; V : Var);

   type Default_Converter is null record;
   function Convert
     (Dummy : Default_Converter; From : Value_Type) return Value_Type
   is (From);
   function Image (Dummy : Default_Converter) return String is ("");

   type Default_Eq is null record;
   function Equals
     (Dummy : Default_Eq; L, R : Value_Type) return Boolean
   is (L = R);
   function Image (Dummy : Default_Eq) return String is ("");

   package SSM_Unify is new SSM_Solve.Raw_Custom_Bind
     (Converter        => Default_Converter,
      No_Data          => (null record),
      Equals_Data      => Default_Eq,
      No_Equals_Data   => (null record),
      Convert          => Convert,
      Equals           => Equals,
      One_Side_Convert => False);

   type Comparer_Access is access all Solver_Ifc.Comparer_Type'Class;
   function Equals (Self : Comparer_Access; L, R : Value_Type) return Boolean
   is
     (if Self = null
      then L = R
      else Self.Compare (L, R));

   function Image (Self : Comparer_Access) return String is
     (if Self = null then "" else Self.Image);

   type Converter_Access is access all Solver_Ifc.Converter_Type'Class;
   function Convert
     (Self : Converter_Access; From : Value_Type) return Value_Type
   is
     (if Self = null
      then From
      else Self.Convert (From));

   function Image (Self : Converter_Access) return String is
     (if Self = null then "" else Self.Image);

   package SSM_Bind_One_Side is new SSM_Solve.Raw_Custom_Bind
     (Converter        => Converter_Access,
      No_Data          => null,
      Equals_Data      => Comparer_Access,
      No_Equals_Data   => null,
      Convert          => Convert,
      Equals           => Equals,
      One_Side_Convert => True);

   package SSM_Bind is new SSM_Solve.Raw_Custom_Bind
     (Converter        => Default_Converter,
      No_Data          => (null record),
      Equals_Data      => Comparer_Access,
      No_Equals_Data   => null,
      Convert          => Convert,
      Equals           => Equals,
      One_Side_Convert => False);

   ---------------------------
   -- SSM Predicate wrapper --
   ---------------------------

   type Predicate_Access is access all Solver_Ifc.Predicate_Type'Class;
   function Call (Self : Predicate_Access; Val : Value_Type) return Boolean
   is
     (Self.Call (Val));

   function Image (Self : Predicate_Access) return String
   is
     (Self.Image);

   procedure Free
   is new Ada.Unchecked_Deallocation
     (Solver_Ifc.Predicate_Type'Class, Predicate_Access);

   package SSM_Predicate is new Predicates.Predicate
     (El_Type        => Value_Type,
      Var            => Solver_Ifc.Logic_Vars,
      Predicate_Type => Predicate_Access,
      Call           => Call,
      Free           => Free,
      Image          => Image);

   type N_Predicate_Access is access all Solver_Ifc.N_Predicate_Type'Class;
   function Call (Self : N_Predicate_Access; Vals : Val_Array) return Boolean
   is
     (Self.Call (Vals));

   function Image (Self : N_Predicate_Access) return String
   is
     (Self.Image);

   procedure Free
   is new Ada.Unchecked_Deallocation
     (Solver_Ifc.N_Predicate_Type'Class, N_Predicate_Access);

   package SSM_N_Predicate is new Predicates.N_Predicate
     (El_Type        => Value_Type,
      Var            => Solver_Ifc.Logic_Vars,
      Predicate_Type => N_Predicate_Access,
      Call           => Call,
      Free           => Free,
      Image          => Image);

   ----------------
   -- Append_Var --
   ----------------

   procedure Append_Var (Self : in out Relation; V : Var) is
   begin
      if Debug then
         if Self.Vars = null then
            Self.Vars := new Var_Sets.Map;
         end if;
         Self.Vars.Include (Image (V), V);
      end if;
   end Append_Var;

   Global_Kind : Valid_Solver_Kind := Default_Solver_Kind;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (Kind : Solver_Kind) is
   begin
      Global_Kind := Kind;
   end Set_Kind;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Relation) is
   begin
      case Self.Kind is
         when Symbolic => Sym_Solve.Inc_Ref (Self.Symbolic_Relation);
         when State_Machine => Abstract_Relation.Inc_Ref (Self.SSM_Relation);
         when None => null;
      end case;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Relation) is
   begin
      case Self.Kind is
         when Symbolic => Sym_Solve.Dec_Ref (Self.Symbolic_Relation);
         when State_Machine => Abstract_Relation.Dec_Ref (Self.SSM_Relation);
         when None => null;
      end case;
   end Dec_Ref;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function return Boolean;
      Solve_Options     : Solve_Options_Type := Default_Options)
   is
   begin
      case Self.Kind is
         when Symbolic => Sym_Solve.Solve
              (Self.Symbolic_Relation, Solution_Callback, Solve_Options);
         when State_Machine =>
            while Abstract_Relation.Solve (Self.SSM_Relation) loop
               declare
                  Ignore : Boolean := Solution_Callback.all;
               begin
                  null;
               end;
            end loop;
         when None => raise Constraint_Error with "Cannot solve No_Relation";
      end case;
   end Solve;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function
        (Vars : Logic_Var_Array) return Boolean;
      Solve_Options     : Solve_Options_Type := Default_Options)
   is
   begin
      case Self.Kind is
         when Symbolic => Sym_Solve.Solve
              (Self.Symbolic_Relation, Solution_Callback, Solve_Options);
         when State_Machine =>
            while Abstract_Relation.Solve (Self.SSM_Relation) loop
               declare
                  Vars   : Logic_Var_Array (1 .. Positive (Self.Vars.Length));
                  Ignore : Boolean;
                  --  Call the callback with the vars that we accumulated in
                  --  the wrapper during the building of the relation.
                  use Var_Sets;
                  I      : Positive := 1;
               begin
                  for El in Self.Vars.Iterate loop
                     Vars (I) := Element (El);
                     I := I + 1;
                  end loop;

                  Ignore := Solution_Callback (Vars);
               end;
            end loop;
         when None => raise Constraint_Error with "Cannot solve No_Relation";
      end case;
   end Solve;

   -----------------
   -- Solve_First --
   -----------------

   function Solve_First
     (Self : Relation; Solve_Options : Solve_Options_Type := Default_Options)
      return Boolean
   is
   begin
      case Self.Kind is
         when Symbolic =>
            return Sym_Solve.Solve_First
              (Self.Symbolic_Relation, Solve_Options);
         when State_Machine =>
            return Abstract_Relation.Solve (Self.SSM_Relation);
         when None => raise Constraint_Error with "Cannot solve No_Relation";
      end case;
   end Solve_First;

   ----------------------
   -- Create_Predicate --
   ----------------------

   function Create_Predicate
     (Logic_Var    : Var;
      Pred         : Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation
   is
   begin
      case Global_Kind is
         when Symbolic =>
            return Relation'
              (Symbolic,
               Sym_Solve.Create_Predicate (Logic_Var, Pred, Debug_String));
         when State_Machine =>
            return Rel : Relation :=
              (State_Machine,
               SSM_Predicate.Create
                 (Logic_Var, new Predicate_Type'Class'(Pred)),
               Vars => <>)
            do
               Append_Var (Rel, Logic_Var);
            end return;
      end case;
   end Create_Predicate;

   ------------------------
   -- Create_N_Predicate --
   ------------------------

   function Create_N_Predicate
     (Logic_Vars   : Variable_Array;
      Pred         : N_Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation
   is
   begin
      case Global_Kind is
         when Symbolic =>
            return Relation'
              (Symbolic,
               Sym_Solve.Create_N_Predicate (Logic_Vars, Pred, Debug_String));
         when State_Machine =>
            return Rel : Relation :=
              (State_Machine,
               SSM_N_Predicate.Create
                 (Logic_Vars, new N_Predicate_Type'Class'(Pred)),
               Vars => <>)
            do
               for Var of Logic_Vars loop
                  Append_Var (Rel, Var);
               end loop;
            end return;
      end case;
   end Create_N_Predicate;

   -------------------
   -- Create_Assign --
   -------------------

   function Create_Assign
     (Logic_Var    : Var;
      Value        : Value_Type;
      Conv         : Converter_Type'Class := No_Converter;
      Eq           : Comparer_Type'Class  := No_Comparer;
      Debug_String : String_Access        := null) return Relation
   is
   begin
      case Global_Kind is
         when Symbolic =>
            return Relation'
              (Symbolic,
               Sym_Solve.Create_Assign
                 (Logic_Var, Value, Conv, Eq, Debug_String));
         when State_Machine =>
            declare
               C : Converter_Access := null;
               E : Comparer_Access := null;
            begin
               if Conv /= No_Converter then
                  C := new Converter_Type'Class'(Conv);
               end if;

               if Eq /= No_Comparer then
                  E := new Comparer_Type'Class'(Eq);
               end if;

               return Rel : Relation :=
                 (State_Machine,
                  SSM_Bind_One_Side.Create
                    (Logic_Var,
                     Value,
                     C, E,
                     Debug_String),
                  Vars => <>)
               do
                  Append_Var (Rel, Logic_Var);
               end return;
            end;
      end case;
   end Create_Assign;

   ------------------
   -- Create_Unify --
   ------------------

   function Create_Unify
     (From, To : Var; Debug_String : String_Access := null) return Relation
   is
   begin
      case Global_Kind is
         when Symbolic =>
            return Relation'
              (Symbolic, Sym_Solve.Create_Unify (From, To, Debug_String));
         when State_Machine =>
            return Rel : Relation :=
              (State_Machine,
               SSM_Unify.Create
                 (From, To, (null record), (null record), Debug_String),
               Vars => <>)
            do
               Append_Var (Rel, From);
               Append_Var (Rel, To);
            end return;
      end case;
   end Create_Unify;

   ----------------------
   -- Create_Propagate --
   ----------------------

   function Create_Propagate
     (From, To     : Var;
      Conv         : Converter_Type'Class := No_Converter;
      Eq           : Comparer_Type'Class := No_Comparer;
      Debug_String : String_Access       := null) return Relation
   is
   begin
      case Global_Kind is
         when Symbolic =>
            return Relation'
              (Symbolic,
               Sym_Solve.Create_Propagate (From, To, Conv, Eq, Debug_String));
         when State_Machine =>
            declare
               C : Converter_Access := null;
               E : Comparer_Access := null;
            begin
               if Conv /= No_Converter then
                  C := new Converter_Type'Class'(Conv);
               end if;

               if Eq /= No_Comparer then
                  E := new Comparer_Type'Class'(Eq);
               end if;

               return Rel : Relation
               do
                  if C = null then
                     Rel :=
                       (State_Machine,
                        SSM_Bind.Create
                          (From,
                           To,
                           (null record), E,
                           Debug_String),
                        Vars => <>);
                  else
                     Rel :=
                       (State_Machine,
                        SSM_Bind_One_Side.Create
                          (From,
                           To,
                           C, E,
                           Debug_String),
                        Vars => <>);
                  end if;

                  Append_Var (Rel, From);
                  Append_Var (Rel, To);
               end return;
            end;
      end case;
   end Create_Propagate;

   -------------------
   -- Create_Domain --
   -------------------

   function Create_Domain
     (Logic_Var    : Var; Domain : Value_Array;
      Debug_String : String_Access := null) return Relation
   is
   begin
      case Global_Kind is
         when Symbolic =>
            return Relation'
              (Symbolic,
               Sym_Solve.Create_Domain (Logic_Var, Domain, Debug_String));
         when State_Machine =>
            return Rel : Relation :=
              (State_Machine,
               SSM_Unify.Impl.Member
                 (Logic_Var,
                  SSM_Unify.Impl.Unify_Left.R_Type_Array (Domain)),
               Vars => <>)
            do
               Append_Var (Rel, Logic_Var);
            end return;

      end case;
   end Create_Domain;

   ----------------
   -- Create_Any --
   ----------------

   function Create_Any
     (Relations : Relation_Array; Debug_String : String_Access := null)
      return Relation
   is

   begin

      case Global_Kind is
         when Symbolic =>
            declare
               Internal_Rels : Sym_Solve.Relation_Array (Relations'Range);
            begin
               for J in Relations'Range loop
                  Internal_Rels (J) := Relations (J).Symbolic_Relation;
               end loop;
               return Relation'
                 (Symbolic,
                  Sym_Solve.Create_Any (Internal_Rels, Debug_String));
            end;
         when State_Machine =>
            declare
               Internal_Rels : Abstract_Relation.Relation_Array
                 (Relations'Range);
               use Var_Sets;
            begin
               return Rel : Relation (State_Machine) do

                  for J in Relations'Range loop
                     Internal_Rels (J) := Relations (J).SSM_Relation;
                     if Relations (J).Vars /= null then
                        for El in Relations (J).Vars.Iterate loop
                           Append_Var (Rel, Element (El));
                        end loop;
                     end if;
                  end loop;

                  Rel.SSM_Relation := Operations.Logic_Any
                    (Internal_Rels, Debug_String);
               end return;
            end;
      end case;
   end Create_Any;

   ----------------
   -- Create_All --
   ----------------

   function Create_All
     (Relations : Relation_Array; Debug_String : String_Access := null)
      return Relation
   is
   begin

      case Global_Kind is
         when Symbolic =>
            declare
               Internal_Rels : Sym_Solve.Relation_Array (Relations'Range);
            begin
               for J in Relations'Range loop
                  Internal_Rels (J) := Relations (J).Symbolic_Relation;
               end loop;

               return Relation'
                 (Symbolic,
                  Sym_Solve.Create_All (Internal_Rels, Debug_String));
            end;
         when State_Machine =>
            declare
               Internal_Rels : Abstract_Relation.Relation_Array
                 (Relations'Range);
               use Var_Sets;
            begin
               return Rel : Relation (State_Machine) do

                  for J in Relations'Range loop
                     Internal_Rels (J) := Relations (J).SSM_Relation;
                     if Relations (J).Vars /= null then
                        for El in Relations (J).Vars.Iterate loop
                           Append_Var (Rel, Element (El));
                        end loop;
                     end if;
                  end loop;

                  Rel.SSM_Relation := Operations.Logic_All
                    (Internal_Rels, Debug_String);
               end return;
            end;
      end case;
   end Create_All;

   -----------------
   -- Create_True --
   -----------------

   function Create_True (Debug_String : String_Access := null) return Relation
   is
   begin
      case Global_Kind is
         when Symbolic =>
            return Relation'
              (Symbolic,
               Sym_Solve.Create_True (Debug_String));
         when State_Machine =>
            return Relation'
              (State_Machine,
               Pure_Relations.True_Rel (Debug_String),
               Vars => <>);
      end case;
   end Create_True;

   ------------------
   -- Create_False --
   ------------------

   function Create_False (Debug_String : String_Access := null) return Relation
   is
   begin
      case Global_Kind is
         when Symbolic =>
            return Relation'
              (Symbolic,
               Sym_Solve.Create_False (Debug_String));
         when State_Machine =>
            return Relation'
              (State_Machine,
               Pure_Relations.False_Rel (Debug_String),
               Vars => <>);
      end case;
   end Create_False;

   -----------
   -- Image --
   -----------

   function Image (Self : Relation) return String is
   begin
      case Self.Kind is
         when Symbolic =>
            return Sym_Solve.Image (Self.Symbolic_Relation);
         when State_Machine =>
            return "<not implemented>";
         when None => return "<No relation>";
      end case;
   end Image;

   --------------------
   -- Print_Relation --
   --------------------

   procedure Print_Relation (Self : Relation) is
   begin
      case Self.Kind is
         when Symbolic =>
            Put_Line (Sym_Solve.Image (Self.Symbolic_Relation));
         when State_Machine =>
            Abstract_Relation.Print_Relation (Self.SSM_Relation);
         when None => null;
      end case;
   end Print_Relation;

end Langkit_Support.Adalog.Solver;
