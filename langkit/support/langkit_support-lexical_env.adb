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

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with System.Address_Image;

with Langkit_Support.Images; use Langkit_Support.Images;
with System.Assertions;

package body Langkit_Support.Lexical_Env is

   No_Entity_Info : constant Entity_Info := (Empty_Metadata, null);

   function Has_Lookup_Cache (Dummy : Lexical_Env) return Boolean
   is
     (Activate_Lookup_Cache);
   --  Whether lookup cache is enabled for the given lexical environment.
   --  Note that for now, this is only a global setting (not per env).

   function Is_Lookup_Cache_Valid (Env : Lexical_Env) return Boolean
      with Pre => Env.Kind = Primary;
   --  Return whether Env's lookup cache is valid. This will check every
   --  Lookup_Cache_Valid flag up Env's parent chain.

   function Wrap
     (Env   : Lexical_Env_Access;
      Owner : Unit_T := No_Unit) return Lexical_Env
   is
     ((Env     => Env,
       Hash    => Hash (Env),
       Kind    => (if Env = null then Primary else Env.Kind),
       Owner   => Owner,
       Version => (if Owner /= No_Unit
                   then Get_Version (Owner) else 0)));

   function OK_For_Rebindings (Self : Lexical_Env) return Boolean is
     (Self.Kind = Primary and then Env_Node (Self) /= No_Node);

   function Extract_Rebinding
     (Rebindings  : in out Env_Rebindings;
      Rebound_Env : Lexical_Env) return Lexical_Env;
   --  Look for a pair in Rebindings whose Old_Env field is "Rebound_Env".
   --
   --  If there is one, return the env it is associated to, and put the
   --  remaining set of rebindings in rebindings. Otherwise, return
   --  "Rebound_Env".
   --
   --  If Rebindings is bound to a new set of rebindings upon return, the
   --  ownership share of the old rebinding set will have been forfeited.

   function Shed_Rebindings
     (From_Env   : Lexical_Env;
      Rebindings : Env_Rebindings) return Env_Rebindings
     with Inline;
   --  Shed env rebindings that are not in the parent chain for From_Env

   procedure Check_Rebindings_Unicity (Self : Env_Rebindings);
   --  Perform a unicity check on the various rebindings in Self. In
   --  particular, check that there are no two identical Old_Env and no two
   --  identical New_Env in the set of rebindings. If there are, raise a
   --  property error.

   function Get_Internal
     (Self          : Lexical_Env;
      Key           : Symbol_Type;
      Lookup_Kind   : Lookup_Kind_Type := Recursive;
      Rebindings    : Env_Rebindings := null;
      Metadata      : Node_Metadata := Empty_Metadata;
      Categories    : Ref_Categories)
      return Lookup_Result_Array;

   procedure Reset_Lookup_Cache (Self : Lexical_Env);
   --  Reset Self's lexical environment lookup cache

   -----------
   -- Image --
   -----------

   function Image (Cats : Ref_Categories) return Text_Type is
      Ret : Unbounded_Text_Type;
   begin
      Append (Ret, "(");

      for Cat in Ref_Category'Range loop
         Append (Ret, To_Text (Cat'Image & " => " & Cats (Cat)'Image & ", "));
      end loop;
      Append (Ret, ")");
      return To_Text (Ret);
   end Image;

   ---------------------------
   -- Is_Lookup_Cache_Valid --
   ---------------------------

   function Is_Lookup_Cache_Valid (Env : Lexical_Env) return Boolean is
      P : Lexical_Env;
   begin
      if Env.Env.Lookup_Cache_Valid then
         P := Parent (Env);
         return (P in Null_Lexical_Env | Empty_Env
                 or else Is_Lookup_Cache_Valid (P));
      else
         return False;
      end if;
   end Is_Lookup_Cache_Valid;

   ------------------------
   -- Reset_Lookup_Cache --
   ------------------------

   procedure Reset_Lookup_Cache (Self : Lexical_Env) is
   begin
      for C of Self.Env.Lookup_Cache loop
         C.Elements.Destroy;
      end loop;

      Self.Env.Lookup_Cache.Clear;
      Self.Env.Lookup_Cache_Valid := True;
   end Reset_Lookup_Cache;

   -----------------------
   -- Simple_Env_Getter --
   -----------------------

   function Simple_Env_Getter (E : Lexical_Env) return Env_Getter is
   begin
      Inc_Ref (E);
      return Env_Getter'(Dynamic => False,
                         Env     => E);
   end Simple_Env_Getter;

   --------------------
   -- Dyn_Env_Getter --
   --------------------

   function Dyn_Env_Getter
     (Resolver : Lexical_Env_Resolver; Node : Node_Type) return Env_Getter is
   begin
      return Env_Getter'(True, Null_Lexical_Env, Node, Resolver);
   end Dyn_Env_Getter;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (Self : in out Env_Getter) is
      Env : Lexical_Env;
   begin
      case Self.Dynamic is
         when True =>
            Env := Get_Env (Self);

            --  Get_Env returns an ownership share for the returned reference,
            --  but we don't use it here, so dec ref.
            Dec_Ref (Env);
         when False =>
            null;
      end case;
   end Resolve;

   -------------
   -- Get_Env --
   -------------

   function Get_Env (Self : in out Env_Getter) return Lexical_Env is
   begin
      if Self.Dynamic then

         if Self.Env /= Null_Lexical_Env then

            if not Is_Stale (Self.Env) then
               Inc_Ref (Self.Env);
               return Self.Env;
            end if;

            Dec_Ref (Self.Env);
         end if;

         declare
            R : constant Lexical_Env_Resolver := Self.Resolver;
            E : constant Entity :=
              (Node => Self.Node, Info => No_Entity_Info);
         begin

            --  We use the share returned by the resolver, so no need for
            --  inc ref here.
            Self.Env := R.all (E);

         end;
      end if;

      --  Inc ref for the returned value
      Inc_Ref (Self.Env);
      return Self.Env;
   end Get_Env;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Env_Getter) return Boolean is
   begin
      if L.Dynamic or else R.Dynamic then
         raise Constraint_Error with "trying to compare dynamic env getters";
      else
         return L.Env = R.Env;
      end if;
   end Equivalent;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Env_Getter) is
   begin
      Inc_Ref (Self.Env);
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Env_Getter) is
   begin
      Dec_Ref (Self.Env);
   end Dec_Ref;

   -------------
   -- Combine --
   -------------

   function Combine (L, R : Env_Rebindings) return Env_Rebindings is
      Result, Cur : Env_Rebindings;
   begin
      if L = null then
         return R;
      elsif R = null then
         return L;
      end if;

      Result := L;
      Cur := R;
      while Cur /= null loop
         Result := Append (Result, Cur.Old_Env, Cur.New_Env);
         Cur := Cur.Parent;
      end loop;

      Check_Rebindings_Unicity (Result);
      return Result;
   end Combine;

   ------------
   -- Append --
   ------------

   function Append
     (Self             : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings is
   begin
      --  Look for an existing rebinding for the result: in the Old_Env's pool
      --  if there is no parent, otherwise in the parent's children.
      if Self = null then
         if Old_Env.Env.Rebindings_Pool /= null then
            declare
               use Env_Rebindings_Pools;
               Cur : constant Cursor := Old_Env.Env.Rebindings_Pool.Find
                 (New_Env);
            begin
               if Cur /= Env_Rebindings_Pools.No_Element then
                  return Element (Cur);
               end if;
            end;
         end if;

      else
         for C of Self.Children loop
            if C.Old_Env = Old_Env and then C.New_Env = New_Env then
               return C;
            end if;
         end loop;
      end if;

      --  No luck? then create a new rebinding and register it where required
      declare
         Result : constant Env_Rebindings := new Env_Rebindings_Type'
           (Parent   => Self,
            Old_Env  => Old_Env,
            New_Env  => New_Env,
            Children => Env_Rebindings_Vectors.Empty_Vector);
      begin
         if Self /= null then
            Self.Children.Append (Result);
         else
            if Old_Env.Env.Rebindings_Pool = null then
               Old_Env.Env.Rebindings_Pool := new Env_Rebindings_Pools.Map;
            end if;
            Old_Env.Env.Rebindings_Pool.Insert (New_Env, Result);
         end if;

         Register_Rebinding (Env_Node (Old_Env), Result.all'Address);
         Register_Rebinding (Env_Node (New_Env), Result.all'Address);
         Check_Rebindings_Unicity (Result);
         return Result;
      end;
   end Append;

   ----------------------
   -- Append_Rebinding --
   ----------------------

   function Append_Rebinding
     (Self    : Env_Rebindings;
      Old_Env : Lexical_Env;
      New_Env : Lexical_Env) return Env_Rebindings is
   begin
      if not Is_Rebindable (Env_Node (Old_Env)) then
         Raise_Property_Error ("Illegal lexical environment rebinding");
      end if;

      return Append (Self, Old_Env, New_Env);
   end Append_Rebinding;

   -------------------
   -- Create_Entity --
   -------------------

   function Create_Entity (Node : Node_Type; MD : Node_Metadata) return Entity
   is
   begin
      return Entity'(Node => Node,
                     Info => (MD => MD, Rebindings => null));
   end Create_Entity;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Entity) return Boolean is
   begin
      if L.Node /= R.Node then
         return False;
      end if;

      --  All null nodes are equals, regardless of their entity info
      if L.Node = No_Node then
         return True;
      end if;

      --  For all other cases, make sure the entity info is equivalent
      return L.Info = R.Info;
   end Equivalent;

   ------------------------
   -- Create_Lexical_Env --
   ------------------------

   function Create_Lexical_Env
     (Parent            : Env_Getter;
      Node              : Node_Type;
      Transitive_Parent : Boolean := False;
      Owner             : Unit_T) return Lexical_Env is
   begin
      if Parent /= No_Env_Getter then
         Inc_Ref (Parent);
      end if;
      return Wrap
        (new Lexical_Env_Type'
           (Kind               => Primary,
            Parent             => Parent,
            Transitive_Parent  => Transitive_Parent,
            Node               => Node,
            Referenced_Envs    => <>,
            Map                => new Internal_Envs.Map,
            Rebindings_Pool    => null,
            Lookup_Cache_Valid => True,
            Lookup_Cache       => Lookup_Cache_Maps.Empty_Map),
         Owner => Owner);
   end Create_Lexical_Env;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self     : Lexical_Env;
      Key      : Symbol_Type;
      Value    : Node_Type;
      MD       : Node_Metadata := Empty_Metadata;
      Resolver : Entity_Resolver := null)
   is
      use Internal_Envs;

      Node  : constant Internal_Map_Node := (Value, MD, Resolver);
      C     : Cursor;
      Dummy : Boolean;
      Map   : Internal_Envs.Map renames Self.Env.Map.all;
   begin
      --  See Empty_Env's documentation

      if Self = Empty_Env then
         return;
      end if;

      Self.Env.Lookup_Cache_Valid := False;
      Map.Insert (Key, Internal_Map_Node_Vectors.Empty_Vector, C, Dummy);
      Reference (Map, C).Element.Append (Node);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (Self : Lexical_Env; Key : Symbol_Type; Value : Node_Type)
   is
      V : constant Internal_Envs.Reference_Type :=
         Self.Env.Map.Reference (Key);
   begin
      if Self = Empty_Env then
         return;
      end if;

      --  Get rid of element. Do this in reverse order so that removing one
      --  element does not make us "step over" the next item. Also don't do
      --  this in place (using V.Pop) as we need to preserve the order of
      --  elements.
      for I in reverse 1 .. V.Length loop
         if V.Get (I).Node = Value then
            V.Remove_At (I);
            exit;
         end if;
      end loop;

      Self.Env.Lookup_Cache_Valid := False;
   end Remove;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self            : Lexical_Env;
      Referenced_From : Node_Type;
      Resolver        : Lexical_Env_Resolver;
      Kind            : Ref_Kind := Normal;
      Categories      : Ref_Categories := All_Cats)
   is
      Refd_Env : Referenced_Env :=
        (Kind,
         Dyn_Env_Getter (Resolver, Referenced_From),
         False, Inactive, Categories);
   begin
      if Self = Empty_Env then
         return;
      end if;
      Resolve (Refd_Env.Getter);
      Refd_Env.State := Active;
      Referenced_Envs_Vectors.Append (Self.Env.Referenced_Envs, Refd_Env);
      Self.Env.Lookup_Cache_Valid := False;
   end Reference;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self         : Lexical_Env;
      To_Reference : Lexical_Env;
      Kind         : Ref_Kind := Normal;
      Categories   : Ref_Categories := All_Cats)
   is
      Ref : constant Referenced_Env :=
        (Kind, Simple_Env_Getter (To_Reference), False, Active, Categories);
   begin
      if Self = Empty_Env then
         return;
      end if;
      Referenced_Envs_Vectors.Append (Self.Env.Referenced_Envs, Ref);
      Self.Env.Lookup_Cache_Valid := False;
   end Reference;

   ---------
   -- Get --
   ---------

   function Get_Internal
     (Self          : Lexical_Env;
      Key           : Symbol_Type;
      Lookup_Kind   : Lookup_Kind_Type := Recursive;
      Rebindings    : Env_Rebindings := null;
      Metadata      : Node_Metadata := Empty_Metadata;
      Categories    : Ref_Categories)
      return Lookup_Result_Array
   is

      Local_Results      : Lookup_Result_Vector;
      Current_Rebindings : Env_Rebindings;

      procedure Get_Refd_Nodes (Self : in out Referenced_Env);

      procedure Append_Result
        (Node       : Internal_Map_Node;
         MD         : Node_Metadata;
         Rebindings : Env_Rebindings);
      --  Add E to results, if it passes the Can_Reach filter. Return whether
      --  result was appended or not.

      use Internal_Envs;

      function Get_Nodes (Env : Lexical_Env) return Boolean;
      --  Lookup for matching nodes in Env's internal map and append them to
      --  Local_Results. Return whether we found some.

      ---------------
      -- Get_Nodes --
      ---------------

      function Get_Nodes (Env : Lexical_Env) return Boolean is
         C     : Cursor := Internal_Envs.No_Element;
         Nodes : Internal_Map_Node_Vectors.Vector;
      begin
         if Env.Env.Map /= null then
            C := Env.Env.Map.Find (Key);
         end if;

         if Has_Element (C) then
            Nodes := Element (C);

            --  We iterate in reverse, so that last inserted results are
            --  returned first.

            --  TODO??? Use "for ... of reverse" next GPL release
            for I in reverse Nodes.First_Index .. Nodes.Last_Index loop
               Append_Result (Nodes.Get (I), Metadata, Current_Rebindings);
            end loop;
            return True;
         end if;

         return False;
      end Get_Nodes;

      -------------------
      -- Append_Result --
      -------------------

      procedure Append_Result
        (Node       : Internal_Map_Node;
         MD         : Node_Metadata;
         Rebindings : Env_Rebindings)
      is
         E : constant Entity :=
           (Node => Node.Node,
            Info => (MD         => Combine (Node.MD, MD),
                     Rebindings => Rebindings));
      begin

         if Has_Trace then
            Traces.Trace
              (Me, "Found " & Image (Node_Image (E.Node, False)));
         end if;

         declare
            Resolved_Entity : constant Entity :=
              (if Node.Resolver = null
               then E
               else Node.Resolver.all (E));
         begin
            Local_Results.Append
              (Lookup_Result_Item'
                 (E                    => Resolved_Entity,
                  Filter_From          => Node.Resolver = null,
                  Override_Filter_Node => No_Node));
         end;
      end Append_Result;

      --------------------
      -- Get_Refd_Nodes --
      --------------------

      procedure Get_Refd_Nodes (Self : in out Referenced_Env) is
         Env : Lexical_Env := Empty_Env;
         --  Make sure this holds a valid environment at all times so that the
         --  exception handler below can always call Dec_Ref on it.
      begin
         --  Don't follow the referenced environment if either:
         --   * the node from which this reference starts cannot reach From;
         --   * the node that created this environment reference is a parent of
         --     From.

         if (Lookup_Kind /= Recursive and then Self.Kind /= Transitive)
           or else Self.Being_Visited
           or else Self.State = Inactive
         then
            return;
         end if;

         if Self.Categories /= All_Cats
            and then Categories /= All_Cats
            and then not (for some C
                          of Ref_Categories'(Categories and Self.Categories)
                          => C)
         then
            return;
         end if;

         Self.Being_Visited := True;
         Env := Get_Env (Self.Getter);

         declare
            Refd_Results : constant Lookup_Result_Array :=
              Get_Internal
                (Env, Key,
                 Lookup_Kind =>
                   (if Lookup_Kind = Recursive and then Self.Kind = Transitive
                    then Recursive
                    elsif Lookup_Kind = Minimal
                    then raise System.Assertions.Assert_Failure
                      with "Should not happen"
                    else Flat),
                 Rebindings =>
                   (if Self.Kind = Transitive
                    then Current_Rebindings
                    else Shed_Rebindings (Env, Current_Rebindings)),
                 Metadata    => Metadata,
                 Categories  => Categories);
         begin
            if Self.Getter.Dynamic then
               for Res of Refd_Results loop
                  declare
                     New_Res : Lookup_Result_Item := Res;
                  begin
                     New_Res.Override_Filter_Node := Self.Getter.Node;
                     Local_Results.Append (New_Res);
                  end;
               end loop;
            else
               Local_Results.Concat (Refd_Results);
            end if;
         end;

         Self.Being_Visited := False;
         Dec_Ref (Env);

      exception
         when others =>
            --  Make sure that we always Dec_Ref the returned environment so we
            --  don't leak in case of error.
            Dec_Ref (Env);
            Self.Being_Visited := False;
            raise;
      end Get_Refd_Nodes;

      Env : Lexical_Env;

      Res_Key           : constant Lookup_Cache_Key :=
        (Key, Rebindings, Metadata, Categories);
      Cached_Res_Cursor : Lookup_Cache_Maps.Cursor;
      Res_Val           : Lookup_Cache_Entry;
      Inserted, Dummy   : Boolean;
      use Lookup_Cache_Maps;

   begin
      if Self in Empty_Env then
         return Empty_Lookup_Result_Array;
      end if;

      if Has_Trace then
         Traces.Trace
           (Me, "Get_Internal env="
            & Lexical_Env_Image (Self, Dump_Content => False)
            & " key = " & Image (Key.all)
            & " lookup kind = " & Lookup_Kind_Type'Image (Lookup_Kind));
      end if;

      case Self.Kind is
         when Orphaned => null;
            return Get_Internal
              (Self.Env.Orphaned_Env, Key, Flat, Rebindings, Metadata,
               Categories);

         when Grouped =>
            --  Just concatenate lookups for all grouped environments
            Traces.Increase_Indent (Me);
            declare
               MD : constant Node_Metadata :=
                  Combine (Self.Env.Default_MD, Metadata);
            begin
               for E of Self.Env.Grouped_Envs.all loop
                  Local_Results.Concat
                    (Get_Internal (E, Key, Lookup_Kind, Rebindings, MD,
                     Categories));
               end loop;
            end;
            Traces.Decrease_Indent (Me);

            return R : constant Lookup_Result_Array := Local_Results.To_Array
            do
               Local_Results.Destroy;
            end return;

         when Rebound =>
            return Get_Internal
              (Self.Env.Rebound_Env, Key, Lookup_Kind,
               Combine (Self.Env.Rebindings, Rebindings),
               Metadata, Categories);

         when Primary => null; --  Handled below to avoid extra nesting levels
      end case;

      --  At this point, we know that Self is a primary lexical environment

      if Has_Lookup_Cache (Self) and then Lookup_Kind = Recursive then

         if not Is_Lookup_Cache_Valid (Self) then
            Reset_Lookup_Cache (Self);
         end if;

         declare
            Val : constant Lookup_Cache_Entry :=
              (Computing, Empty_Lookup_Result_Vector);
         begin
            Self.Env.Lookup_Cache.Insert
              (Res_Key, Val, Cached_Res_Cursor, Inserted);
         end;

         if not Inserted then
            Res_Val := Element (Cached_Res_Cursor);

            if Has_Trace then
               Traces.Trace
                 (Me, "Found a cache entry: "
                       & Lookup_Cache_Entry_State'Image (Res_Val.State));
            end if;

            case Res_Val.State is
               when Computing =>
                  return Empty_Lookup_Result_Array;
               when Computed =>
                  return Res_Val.Elements.To_Array;
               when None => null;
            end case;
         end if;
      end if;

      --  If there is an environment corresponding to Self in env rebindings,
      --  we'll get it here. We'll also shed it from the set of current
      --  rebindings.

      Current_Rebindings := Rebindings;

      --  Phase 1: Get nodes in own env if there are any

      Env := Extract_Rebinding (Current_Rebindings, Self);
      if not Get_Nodes (Env) and then Env /= Self then
         --  Getting the nodes in Self (env before extract rebinding) should
         --  still have the proper env rebindings, so we do Get_Nodes in the
         --  context of Old rebindings. TODO??? This code is kludgy ugly. There
         --  might be a better way, for example, passing rebindings to
         --  Get_Nodes explicitly.
         declare
            Tmp : Env_Rebindings;
         begin
            Tmp := Current_Rebindings;
            Current_Rebindings := Rebindings;
            Dummy := Get_Nodes (Self);
            Current_Rebindings := Tmp;
         end;
      end if;

      if Lookup_Kind /= Minimal then

         --  Phase 2: Get nodes in transitive and prioritary referenced envs

         for I in Self.Env.Referenced_Envs.First_Index
           .. Self.Env.Referenced_Envs.Last_Index
         loop
            if Self.Env.Referenced_Envs.Get_Access (I).Kind
            in Transitive | Prioritary
            then
               Get_Refd_Nodes (Self.Env.Referenced_Envs.Get_Access (I).all);
            end if;
         end loop;

         --  Phase 3: Get nodes in parent envs

         if Lookup_Kind = Recursive or else Self.Env.Transitive_Parent
         then
            declare
               Parent_Env        : Lexical_Env := Parent (Self);
               Parent_Rebindings : constant Env_Rebindings :=
                 (if Env /= Self
                  then Shed_Rebindings (Parent_Env, Current_Rebindings)
                  else Current_Rebindings);
            begin
               if Has_Trace then
                  Traces.Trace
                    (Me, "Recursing on parent environments");
                  Traces.Increase_Indent (Me);
               end if;
               Local_Results.Concat
                 (Get_Internal
                    (Parent_Env, Key, Lookup_Kind,
                     Parent_Rebindings,
                     Metadata, Categories));
               if Has_Trace then
                  Traces.Decrease_Indent (Me);
               end if;

               Dec_Ref (Parent_Env);
            end;
         end if;

         --  Phase 4: Get nodes in normal referenced envs

         if Lookup_Kind = Recursive then
            if Has_Trace then
               Traces.Trace
                 (Me, "Recursing on non transitive referenced environments");
               Traces.Increase_Indent (Me);
            end if;

            for I in Self.Env.Referenced_Envs.First_Index
              .. Self.Env.Referenced_Envs.Last_Index
            loop
               if Self.Env.Referenced_Envs.Get_Access (I).Kind
               not in Transitive | Prioritary
               then
                  Get_Refd_Nodes (Self.Env.Referenced_Envs.Get_Access (I).all);
               end if;
            end loop;

            if Has_Trace then
               Traces.Decrease_Indent (Me);
            end if;
         end if;
      end if;

      Dec_Ref (Env);

      if Has_Lookup_Cache (Self) and then Lookup_Kind = Recursive then
         declare
            Val : constant Lookup_Cache_Entry := (Computed, Local_Results);
         begin
            Self.Env.Lookup_Cache.Include (Res_Key, Val);
         end;

         return Local_Results.To_Array;
      else
         return R : constant Lookup_Result_Array := Local_Results.To_Array
         do
            Local_Results.Destroy;
         end return;
      end if;

   end Get_Internal;

   ---------
   -- Get --
   ---------

   function Get
     (Self        : Lexical_Env;
      Key         : Symbol_Type;
      From        : Node_Type := No_Node;
      Lookup_Kind : Lookup_Kind_Type := Recursive;
      Categories  : Ref_Categories := All_Cats)
      return Entity_Array
   is
      FV : Entity_Vectors.Vector;
   begin

      if Has_Trace then
         Traces.Trace
           (Me, "===== In Env get, key=" & Image (Key.all) & " =====");
         Traces.Increase_Indent (Me);
      end if;

      declare
         Results : constant Lookup_Result_Array :=
           Get_Internal
             (Self, Key, Lookup_Kind, null, Empty_Metadata, Categories);
      begin
         for El of Results loop
            if From = No_Node
              or else (if El.Override_Filter_Node /= No_Node
                       then Can_Reach (El.Override_Filter_Node, From)
                       else Can_Reach (El.E.Node, From))
              or else not El.Filter_From
            then
               FV.Append (El.E);
            end if;
         end loop;

         if Has_Trace then
            Traces.Trace
              (Me, "Returning vector with length " & FV.Length'Image);
         end if;

         return Ret : constant Entity_Array := Entity_Vectors.To_Array (FV) do
            FV.Destroy;
            if Has_Trace then
               Traces.Decrease_Indent (Me);
               Traces.Trace (Me, "===== Out Env get =====");
            end if;
         end return;
      end;
   end Get;

   ---------
   -- Get --
   ---------

   function Get_First
     (Self        : Lexical_Env;
      Key         : Symbol_Type;
      From        : Node_Type := No_Node;
      Lookup_Kind : Lookup_Kind_Type := Recursive;
      Categories  : Ref_Categories := All_Cats) return Entity
   is
      FV : Entity_Vectors.Vector;
   begin

      if Has_Trace then
         Traces.Trace
           (Me, "==== In Env Get_First, key=" & Image (Key.all) & " ====");
         Traces.Increase_Indent (Me);
      end if;

      declare
         V : constant Lookup_Result_Array :=
           Get_Internal
             (Self, Key, Lookup_Kind, null, Empty_Metadata, Categories);
      begin

         for El of V loop
            if From = No_Node
              or else (if El.Override_Filter_Node /= No_Node
                       then Can_Reach (El.Override_Filter_Node, From)
                       else Can_Reach (El.E.Node, From))
              or else not El.Filter_From
            then
               FV.Append (El.E);
            end if;
         end loop;

         if Has_Trace then
            Traces.Trace
              (Me, "Returning vector with length " & FV.Length'Image);
         end if;

         return Ret : constant Entity :=
           (if FV.Length > 0 then FV.First_Element
            else (No_Node, No_Entity_Info))
         do
            FV.Destroy;
            if Has_Trace then
               Traces.Decrease_Indent (Me);
               Traces.Trace (Me, "===== Out Env Get_First =====");
            end if;
         end return;
      end;
   end Get_First;

   ------------
   -- Orphan --
   ------------

   function Orphan (Self : Lexical_Env) return Lexical_Env is

      procedure Check_Valid (Env : Lexical_Env);
      --  Raise a property error if we can't create an orphan for Env. Do
      --  nothing otherwise.

      -----------------
      -- Check_Valid --
      -----------------

      procedure Check_Valid (Env : Lexical_Env) is
      begin
         case Env.Kind is
            when Primary =>
               if Env.Env.Transitive_Parent then
                  Raise_Property_Error
                     ("Cannot create an orphan for an environment with a"
                      & " transitive parent");
               end if;

            when Orphaned => null;

            when Grouped =>
               Raise_Property_Error
                 ("Cannot create an orphan for a grouped environment");

            when Rebound =>
               Check_Valid (Env.Env.Rebound_Env);
         end case;
      end Check_Valid;

   begin
      Check_Valid (Self);

      --  If Self is already an orphan, don't create yet another lexical env
      --  wrapper: just return Self itself.
      Inc_Ref (Self);
      return (if Self.Kind = Orphaned
              then Self
              else Wrap (new Lexical_Env_Type'
                          (Kind         => Orphaned,
                           Ref_Count    => <>,
                           Orphaned_Env => Self),
                         Owner => Self.Owner));
   end Orphan;

   -----------
   -- Group --
   -----------

   function Group
     (Envs    : Lexical_Env_Array;
      With_Md : Node_Metadata := Empty_Metadata) return Lexical_Env
   is
      V : Lexical_Env_Vectors.Vector;

      procedure Append_Envs (E : Lexical_Env);
      --  Append envs from E to the envs vector. This takes care of flattening
      --  potential grouped envs.

      function Already_Has (E : Lexical_Env) return Boolean;
      --  Returns whether the results already contain E

      -----------------
      -- Already_Has --
      -----------------

      function Already_Has (E : Lexical_Env) return Boolean is
      begin
         for El of V loop
            if Equivalent (El, E) then
               return True;
            end if;
         end loop;
         return False;
      end Already_Has;

      -----------------
      -- Append_Envs --
      -----------------

      procedure Append_Envs (E : Lexical_Env) is
      begin
         case E.Kind is
            --  Flatten grouped envs
            when Grouped =>
               for C of E.Env.Grouped_Envs.all loop
                  Append_Envs (C);
               end loop;
            when others =>
               if not Already_Has (E) then
                  Inc_Ref (E);
                  V.Append (E);
               end if;
         end case;
      end Append_Envs;
   begin
      if Envs'Length = 0 then
         return Empty_Env;
      end if;

      for E of Envs loop
         Append_Envs (E);
      end loop;

      return L : constant Lexical_Env := Wrap
        (new Lexical_Env_Type'
           (Kind         => Grouped,
            Ref_Count    => <>,
            Grouped_Envs =>
               new Lexical_Env_Array'(Lexical_Env_Array (V.To_Array)),
            Default_MD   => With_Md))
      do
         Lexical_Env_Vectors.Destroy (V);
      end return;
   end Group;

   ----------------
   -- Rebind_Env --
   ----------------

   function Rebind_Env
     (Base_Env : Lexical_Env; Rebindings : Env_Rebindings) return Lexical_Env
   is
   begin
      --  If no rebindings were passed, just return the original base env
      Inc_Ref (Base_Env);
      return (if Rebindings = null
              then Base_Env
              else Wrap (new Lexical_Env_Type'
                           (Kind        => Rebound,
                            Ref_Count   => <>,
                            Rebound_Env => Base_Env,
                            Rebindings  => Rebindings),
                         Owner => Base_Env.Owner));
   end Rebind_Env;

   ----------------
   -- Rebind_Env --
   ----------------

   function Rebind_Env
     (Base_Env : Lexical_Env; E_Info : Entity_Info) return Lexical_Env is
   begin
      return Rebind_Env (Base_Env, E_Info.Rebindings);
   end Rebind_Env;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Lexical_Env) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Lexical_Env_Type, Lexical_Env_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Lexical_Env_Array, Lexical_Env_Array_Access);
   begin
      if Self in Null_Lexical_Env | Empty_Env then
         return;
      end if;

      case Self.Kind is
         when Primary =>
            --  Release the reference to the parent environment
            Dec_Ref (Self.Env.Parent);

            --  Release referenced environments
            for Ref_Env of Self.Env.Referenced_Envs loop
               declare
                  Getter : Env_Getter := Ref_Env.Getter;
               begin
                  Dec_Ref (Getter);
               end;
            end loop;
            Self.Env.Referenced_Envs.Destroy;

            --  Release the internal map. Don't assume it was allocated, as
            --  it's convenient for testing not to allocate it.
            if Self.Env.Map /= null then
               for Elts of Self.Env.Map.all loop
                  Internal_Map_Node_Vectors.Destroy (Elts);
               end loop;
               Destroy (Self.Env.Map);
            end if;

            --  Release the lookup cache
            Reset_Lookup_Cache (Self);

            --  Release the pool of rebindings
            Destroy (Self.Env.Rebindings_Pool);

         when Orphaned =>
            Dec_Ref (Self.Env.Orphaned_Env);

         when Grouped =>
            for E of Self.Env.Grouped_Envs.all loop
               Dec_Ref (E);
            end loop;
            Free (Self.Env.Grouped_Envs);

         when Rebound =>
            Dec_Ref (Self.Env.Rebound_Env);
      end case;

      Free (Self.Env);
      Self := Null_Lexical_Env;
   end Destroy;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Lexical_Env) is
   begin
      if Self.Kind /= Primary then
         Self.Env.Ref_Count := Self.Env.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Lexical_Env) is
   begin
      if Self.Kind = Primary then
         return;
      end if;

      Self.Env.Ref_Count := Self.Env.Ref_Count - 1;
      if Self.Env.Ref_Count = 0 then
         Destroy (Self);
      end if;
      Self := Null_Lexical_Env;
   end Dec_Ref;

   function Pop (Rebindings : Env_Rebindings) return Env_Rebindings is
     (if Rebindings = null then null else Rebindings.Parent);

   -----------------------
   -- Extract_Rebinding --
   -----------------------

   function Extract_Rebinding
     (Rebindings  : in out Env_Rebindings;
      Rebound_Env : Lexical_Env) return Lexical_Env
   is
      Return_Env : Lexical_Env := Rebound_Env;
   begin
      if Rebindings /= null then
         --  Look for a rebinding in the chain whose Old_Env is Rebound_Env.
         --  The correct behavior is to extract the *last* rebinding, because
         --  due to the stacked nature of rebindings, the user is never
         --  supposed to extract a rebinding other than the last one that was
         --  added.
         --
         --  TODO: We're still doing a full for-loop to check if this invariant
         --  is consistently respected. This means that the case where the env
         --  is not found is linear, which is probably the most common case.
         --  Ideally we would have the loop only in debug builds.

         declare
            R : Env_Rebindings := Rebindings;
         begin
            while R /= null loop
               declare
                  R_Old_Env : constant Lexical_Env := R.Old_Env;
               begin
                  if Rebound_Env = R_Old_Env then
                     Return_Env := R.New_Env;

                     --  Extracted rebinding *must* be the last one
                     if R /= Rebindings then
                        Raise_Property_Error ("Incorrect rebindings");
                     end if;
                     exit;
                  end if;
               end;
               R := R.Parent;
            end loop;
         end;
      end if;

      Inc_Ref (Return_Env);

      --  If we found the rebinding to extract, create the new rebindings set
      if Return_Env /= Rebound_Env then
         Rebindings := Pop (Rebindings);
      end if;

      return Return_Env;
   end Extract_Rebinding;

   ---------------------
   -- Shed_Rebindings --
   ---------------------

   function Shed_Rebindings
     (From_Env   : Lexical_Env;
      Rebindings : Env_Rebindings) return Env_Rebindings
   is
      First_Rebindable_Parent : Lexical_Env;
      Result                  : Env_Rebindings := Rebindings;
   begin
      --  If there is no bindings, nothing to do here
      if Rebindings = null then
         return null;
      end if;

      --  Look for the first environment in From_Env's parent chain whose Node
      --  is rebindable. Use null if there is no such env.
      First_Rebindable_Parent := From_Env;
      Inc_Ref (First_Rebindable_Parent);
      while
         First_Rebindable_Parent /= Empty_Env
         and then
           (Env_Node (First_Rebindable_Parent) = No_Node
            or else not Is_Rebindable (Env_Node (First_Rebindable_Parent)))
      loop
         declare
            Next : constant Lexical_Env := Parent (First_Rebindable_Parent);
         begin
            Dec_Ref (First_Rebindable_Parent);
            First_Rebindable_Parent := Next;
         end;
      end loop;

      --  If there is no rebindable parent anywhere, it means we cannot have
      --  rebindings. In that case, shed them all, i.e. return null rebindings.
      if First_Rebindable_Parent = Empty_Env then
         return null;
      end if;

      --  If we fond a rebindable parent, then we will shed all rebindings
      --  between the top of the rebinding stack and the corresponding
      --  rebinding.
      while Result /= null and then Result.Old_Env /= First_Rebindable_Parent
      loop
         Result := Result.Parent;
      end loop;

      Dec_Ref (First_Rebindable_Parent);
      return Result;
   end Shed_Rebindings;

   ---------------------
   -- Shed_Rebindings --
   ---------------------

   function Shed_Rebindings
     (E_Info : Entity_Info; Env : Lexical_Env) return Entity_Info is
   begin
      return (MD         => E_Info.MD,
              Rebindings => Shed_Rebindings (Env, E_Info.Rebindings));
   end Shed_Rebindings;

   ------------------------------
   -- Check_Rebindings_Unicity --
   ------------------------------

   procedure Check_Rebindings_Unicity (Self : Env_Rebindings) is
      L, R : Env_Rebindings := Self;
   begin
      while L /= null loop
         R := L.Parent;
         while R /= null loop
            if L.Old_Env = R.Old_Env then
               Raise_Property_Error ("Old_Env present twice in rebindings");
            elsif L.New_Env = R.New_Env then
               Raise_Property_Error ("New_Env present twice in rebindings");
            end if;
            R := R.Parent;
         end loop;
         L := L.Parent;
      end loop;
   end Check_Rebindings_Unicity;

   -----------
   -- Image --
   -----------

   function Image (Self : Env_Rebindings) return Text_Type is

      function Image (Self : Lexical_Env) return Text_Type is
        (Node_Image (Env_Node (Self)));

      function Rebinding_Image (Self : Env_Rebindings) return Text_Type is
        (Image (Self.New_Env));

   begin
      if Self = null then
         return "<null>";
      end if;
      declare
         use Env_Rebindings_Vectors;
         Rebindings_Vector : Vector := Empty_Vector;
         Cur               : Env_Rebindings := Self;
         Buffer            : Unbounded_Wide_Wide_String;
      begin
         --  Get rebindings in reverse order: older first, most recent last
         while Cur /= null loop
            Rebindings_Vector.Append (Cur);
            Cur := Cur.Parent;
         end loop;

         Append (Buffer, "[");
         for I in reverse 1 .. Rebindings_Vector.Last_Index loop
            if I < Rebindings_Vector.Last_Index then
               Append (Buffer, ", ");
            end if;
            Append (Buffer, Rebinding_Image (Rebindings_Vector.Get (I)));
         end loop;
         Append (Buffer, "]");

         Rebindings_Vector.Destroy;
         return To_Wide_Wide_String (Buffer);
      end;
   end Image;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Lexical_Env) return Boolean is
   begin
      if L = R then
         return True;
      elsif L.Kind /= R.Kind then
         return False;
      end if;

      case L.Kind is
         when Primary =>
            --  If we have two primary environments, don't go through
            --  structural comparison: we can use pointer equality as each
            --  instance has its own identity.
            return False;

         when Orphaned =>
            return Equivalent (L.Env.Orphaned_Env, R.Env.Orphaned_Env);

         when Grouped =>
            return (L.Env.Grouped_Envs'Length = R.Env.Grouped_Envs'Length
                    and then L.Env.Default_MD = R.Env.Default_MD
                    and then (for all I in L.Env.Grouped_Envs'Range =>
                              Equivalent (L.Env.Grouped_Envs (I),
                                          R.Env.Grouped_Envs (I))));

         when Rebound =>
            return (L.Env.Rebindings = R.Env.Rebindings
                    and then Equivalent (L.Env.Rebound_Env,
                                         R.Env.Rebound_Env));
      end case;
   end Equivalent;

   ----------
   -- Hash --
   ----------

   function Hash (Env : Lexical_Env_Access) return Hash_Type is

      function Convert is new Ada.Unchecked_Conversion
        (Lexical_Env_Resolver, System.Address);
      function Hash is new Hash_Address
        (System.Word_Size / System.Storage_Unit);
      function Hash is new Hash_Access (Internal_Envs.Map, Internal_Map);
      function Hash (Getter : Env_Getter) return Hash_Type;
      function Hash (Ref : Referenced_Env) return Hash_Type;
      function Hash (Refs : Referenced_Envs_Vectors.Vector) return Hash_Type;

      ----------
      -- Hash --
      ----------

      function Hash (Getter : Env_Getter) return Hash_Type is
      begin
         case Getter.Dynamic is
            when True =>
               return Combine
                 ((1,
                   Node_Hash (Getter.Node),
                   Hash (Convert (Getter.Resolver))));
            when False =>
               return Combine (0, Hash (Getter.Env));
         end case;
      end Hash;

      ----------
      -- Hash --
      ----------

      function Hash (Ref : Referenced_Env) return Hash_Type is
      begin
         return Combine ((Ref_Kind'Pos (Ref.Kind), Hash (Ref.Getter)));
      end Hash;

      ----------
      -- Hash --
      ----------

      function Hash (Refs : Referenced_Envs_Vectors.Vector) return Hash_Type is
         Hashes : Hash_Array (1 .. Refs.Length);
      begin
         for I in Hashes'Range loop
            Hashes (I) := Hash (Refs.Get (I));
         end loop;
         return Combine (Hashes);
      end Hash;

      Base_Hash : constant Hash_Type :=
        (if Env = null
         then Initial_Hash
         else Lexical_Env_Kind'Pos (Env.Kind));
   begin
      if Env = null then
         return Base_Hash;
      end if;

      case Env.Kind is
         when Primary =>
            return Combine
              ((Base_Hash,
                Hash (Env.Parent),
                (if Env.Transitive_Parent then 1 else 0),
                Node_Hash (Env.Node),
                Hash (Env.Referenced_Envs),
                Hash (Env.Map)));

         when Orphaned =>
            return Combine (Base_Hash, Hash (Env.Orphaned_Env));

         when Grouped =>
            declare
               Result : Hash_Type := Base_Hash;
            begin
               for E of Env.Grouped_Envs.all loop
                  Result := Combine (Result, Hash (E));
               end loop;
               return Combine (Result, Metadata_Hash (Env.Default_MD));
            end;

         when Rebound =>
            return Combine
              ((Base_Hash, Hash (Env.Rebound_Env), Hash (Env.Rebindings)));
      end case;
   end Hash;

   -----------------
   -- Sorted_Envs --
   -----------------

   --  Those ordered maps are used to have a stable representation of internal
   --  lexical environments, which is not the case with hashed maps.

   function "<" (L, R : Symbol_Type) return Boolean
   is
     (L.all < R.all);

   package Sorted_Envs is new Ada.Containers.Ordered_Maps
     (Key_Type     => Symbol_Type,
      Element_Type => Internal_Map_Node_Vectors.Vector,
      "<"          => "<",
      "="          => Internal_Map_Node_Vectors."=");

   function To_Sorted_Env (Env : Internal_Envs.Map) return Sorted_Envs.Map;

   -------------------
   -- To_Sorted_Env --
   -------------------

   function To_Sorted_Env (Env : Internal_Envs.Map) return Sorted_Envs.Map is
      Ret_Env : Sorted_Envs.Map;
      use Internal_Envs;
   begin
      for El in Env.Iterate loop
         Ret_Env.Include (Key (El), Element (El));
      end loop;
      return Ret_Env;
   end To_Sorted_Env;

   ----------
   -- Dump --
   ----------

   function Lexical_Env_Image
     (Self           : Lexical_Env;
      Env_Id         : String := "";
      Parent_Env_Id  : String := "";
      Dump_Addresses : Boolean := False;
      Dump_Content   : Boolean := True;
      Prefix         : String := "") return String
   is
      use Sorted_Envs;

      Result : Unbounded_String;

      Sub_Prefix : constant String := Prefix & "  ";

      function Short_Image
        (N : Node_Type) return String
      is (if N = No_Node then "<null>"
          else Image (Node_Image (N, False)));
      --  Wrapper around Node_Image to handle null nodes.
      --
      --  TODO??? This is slightly hackish, because we're converting a wide
      --  string back to string. But since we're using this solely for
      --  test/debug purposes, it should not matter. Still, would be good to
      --  have Text_Type everywhere at some point.

      function Image (Node : Internal_Map_Node) return String is
        (Short_Image (Node.Node));
      --  Wrapper around Node_Image to format a lexical env map node

      function Image is new Internal_Map_Node_Vectors.Image (Image);

      procedure New_Arg;
      --  Helper to be called before emitting a new lexical env. "argument".
      --  Used to separate each argument with a comma in Result.

      -------------
      -- New_Arg --
      -------------

      procedure New_Arg is
      begin
         Append (Result, ", ");
      end New_Arg;

   begin
      if Self = Null_Lexical_Env then
         return "";
      end if;

      --  No matter what, emit a short description of this environment: kind,
      --  whether it's empty, parent/node/... if asked.

      if Env_Id'Length /= 0 then
         Append (Result, Env_Id & " = ");
      end if;
      Append (Result, "LexEnv(" & (case Self.Kind is
                                   when Primary => "Primary",
                                   when Orphaned => "Orphaned",
                                   when Grouped => "Grouped",
                                   when Rebound => "Rebound"));

      if Self = Empty_Env then
         New_Arg;
         Append (Result, "Empty");
      end if;

      if Self.Kind = Primary then
         if Parent_Env_Id'Length > 0 then
            New_Arg;
            Append (Result,
                    "Parent="
                    & (if Parent (Self) /= Empty_Env
                       then Parent_Env_Id else "null"));
         end if;

         if Env_Node (Self) /= No_Node then
            New_Arg;
            Append (Result, "Node="
                    & Image (Node_Image (Env_Node (Self), False)));
         end if;
      end if;

      if Dump_Addresses then
         New_Arg;
         Append (Result, "0x"
                         & System.Address_Image (Self.Env.all'Address));
      end if;
      Append (Result, ")");

      --  If that was all that was asked, stop here
      if not Dump_Content then
         return To_String (Result);
      end if;

      --  Otherwise, go to details...
      Append (Result, ":" & ASCII.LF);

      case Self.Kind is
         when Primary =>
            declare
               Refs : Referenced_Envs_Vectors.Vector
                  renames Self.Env.Referenced_Envs;
            begin
               for I in Refs.First_Index .. Refs.Last_Index loop
                  declare
                     G   : Env_Getter renames Refs.Get_Access (I).Getter;
                     Env : Lexical_Env := Get_Env (G);
                  begin
                     if Env /= Empty_Env then
                        Append (Result, Sub_Prefix & "Referenced: ");
                        if G.Dynamic then
                           Append (Result, Short_Image (G.Node) & ": ");
                        end if;

                        Append
                          (Result,
                           Lexical_Env_Image (Self           => Env,
                                              Dump_Addresses => Dump_Addresses,
                                              Dump_Content   => False));
                        Append (Result, ASCII.LF);
                        Dec_Ref (Env);
                     end if;
                  end;
               end loop;
            end;

            if Self.Env.Map.Is_Empty then
               Append (Result, Sub_Prefix & "  <empty>" & ASCII.LF);
            else
               for El in To_Sorted_Env (Self.Env.Map.all).Iterate loop
                  Append
                    (Result,
                     Sub_Prefix & "  "
                     & Langkit_Support.Text.Image (Key (El).all) & ": "
                     & Image (Element (El))
                     & ASCII.LF);
               end loop;
            end if;

         when Orphaned =>
            Append
              (Result, Sub_Prefix & "Orphaned: " & Lexical_Env_Image
                 (Self           => Self.Env.Orphaned_Env,
                  Dump_Addresses => Dump_Addresses,
                  Dump_Content   => Dump_Content,
                  Prefix         => Sub_Prefix));

         when Grouped =>
            for E of Self.Env.Grouped_Envs.all loop
               Append
                 (Result, Sub_Prefix & "Grouped: " & Lexical_Env_Image
                    (Self           => E,
                     Dump_Addresses => Dump_Addresses,
                     Dump_Content   => Dump_Content,
                     Prefix         => Sub_Prefix));
            end loop;

         when Rebound =>
            Append
              (Result, Sub_Prefix & "Rebindings: "
                       & Image (Image (Self.Env.Rebindings))
                       & ASCII.LF);
            Append
              (Result, Sub_Prefix & "Rebound: " & Lexical_Env_Image
                 (Self           => Self.Env.Rebound_Env,
                  Dump_Addresses => Dump_Addresses,
                  Dump_Content   => Dump_Content,
                  Prefix         => Sub_Prefix));
      end case;

      return To_String (Result);
   end Lexical_Env_Image;

   ------------------------------
   -- Lexical_Env_Parent_Chain --
   ------------------------------

   function Lexical_Env_Parent_Chain (Env : Lexical_Env) return String is
      Id     : Positive := 1;
      E      : Lexical_Env := Env;
      Result : Unbounded_String;
   begin

      if E = Null_Lexical_Env then
         Append (Result, "<null>" & ASCII.LF);
      end if;

      while E /= Empty_Env loop
         declare
            Id_Str : constant String := '@' & Stripped_Image (Id);
         begin
            Append
              (Result,
               Lexical_Env_Image
                 (Self           => E,
                  Env_Id         => Id_Str,
                  Parent_Env_Id  => '@' & Stripped_Image (Id + 1),
                  Dump_Addresses => True));
         end;
         Id := Id + 1;
         E := Parent (E);
      end loop;
      return To_String (Result);
   end Lexical_Env_Parent_Chain;

   --------------------------
   -- Dump_One_Lexical_Env --
   --------------------------

   procedure Dump_One_Lexical_Env
     (Self           : Lexical_Env;
      Env_Id         : String := "";
      Parent_Env_Id  : String := "";
      Dump_Addresses : Boolean := False;
      Dump_Content   : Boolean := True)
   is
   begin
      Put_Line
        (Lexical_Env_Image
           (Self, Env_Id, Parent_Env_Id, Dump_Addresses, Dump_Content));
   end Dump_One_Lexical_Env;

   -----------------------------------
   -- Dump_Lexical_Env_Parent_Chain --
   -----------------------------------

   procedure Dump_Lexical_Env_Parent_Chain (Env : Lexical_Env) is
   begin
      Put_Line (Lexical_Env_Parent_Chain (Env));
   end Dump_Lexical_Env_Parent_Chain;

   ------------
   -- Parent --
   ------------

   function Parent (Self : Lexical_Env) return Lexical_Env is
   begin
      case Self.Kind is
         when Primary =>
            declare
               Ret : constant Lexical_Env := Get_Env (Self.Env.Parent);
            begin
               return (if Ret = Null_Lexical_Env then Empty_Env else Ret);
            end;
         when Orphaned =>
            return Parent (Self.Env.Orphaned_Env);
         when Grouped =>
            return Empty_Env;
         when Rebound =>
            return Parent (Self.Env.Rebound_Env);
      end case;
   end Parent;

   --------------
   -- Env_Node --
   --------------

   function Env_Node (Self : Lexical_Env) return Node_Type is
   begin
      return (case Self.Kind is
              when Primary  => Self.Env.Node,
              when Orphaned => Env_Node (Self.Env.Orphaned_Env),
              when Grouped  => No_Node,
              when Rebound  => Env_Node (Self.Env.Rebound_Env));
   end Env_Node;

   --------------------------------
   -- Deactivate_Referenced_Envs --
   --------------------------------

   procedure Deactivate_Referenced_Envs (Self : Lexical_Env) is
      R : access Referenced_Env;
   begin
      if Self in Null_Lexical_Env | Empty_Env then
         return;
      end if;

      Self.Env.Lookup_Cache_Valid := False;

      for I in Self.Env.Referenced_Envs.First_Index
            .. Self.Env.Referenced_Envs.Last_Index
      loop
         R := Self.Env.Referenced_Envs.Get_Access (I);
         R.State := Inactive;
      end loop;
   end Deactivate_Referenced_Envs;

   -------------------------------
   -- Recompute_Referenced_Envs --
   -------------------------------

   procedure Recompute_Referenced_Envs (Self : Lexical_Env) is
      R : access Referenced_Env;
   begin
      if Self in Null_Lexical_Env | Empty_Env then
         return;
      end if;

      for I in Self.Env.Referenced_Envs.First_Index
            .. Self.Env.Referenced_Envs.Last_Index
      loop
         R := Self.Env.Referenced_Envs.Get_Access (I);
         Resolve (R.Getter);
         R.State := Active;
      end loop;
   end Recompute_Referenced_Envs;

   --------------
   -- Is_Stale --
   --------------

   function Is_Stale (Self : Lexical_Env) return Boolean is
      L : Lexical_Env;
   begin
      if Self.Owner /= No_Unit then
         return Get_Version (Self.Owner) > Self.Version;
      elsif Self = Empty_Env then
         --  Empty_Env is always stale, because since it is not linked to any
         --  unit, we have no way to know if it is stale or not. TODO: Maybe we
         --  should forbid its use in referenced envs.
         return True;
      end if;

      case Self.Kind is
         when Primary =>
            for I in Self.Env.Referenced_Envs.First_Index
                  .. Self.Env.Referenced_Envs.Last_Index
            loop
               L := Get_Env (Self.Env.Referenced_Envs.Get_Access (I).Getter);
               if Is_Stale (L) then
                  return True;
               end if;
               Dec_Ref (L);
            end loop;
            return False;

         when Orphaned =>
            return Is_Stale (Self.Env.Orphaned_Env);

         when Grouped =>
            return (for some E of Self.Env.Grouped_Envs.all => Is_Stale (E));

         when Rebound =>
            return Is_Stale (Self.Env.Rebound_Env);
      end case;
   end Is_Stale;

end Langkit_Support.Lexical_Env;
