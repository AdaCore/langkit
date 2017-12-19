with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with System.Address_Image;

with Langkit_Support.Images; use Langkit_Support.Images;

package body Langkit_Support.Lexical_Env is

   function Is_Lookup_Cache_Valid (Env : Lexical_Env) return Boolean;
   --  Return whether Env's lookup cache is valid. This will check every
   --  Lookup_Cache_Valid flag up Env's parent chain.

   function Wrap
     (Env   : Lexical_Env_Access;
      Owner : Unit_T := No_Unit) return Lexical_Env
   is
     ((Env           => Env,
       Hash          => Hash (Env),
       Is_Refcounted => Env /= null and then Env.Ref_Count /= No_Refcount,
       Owner         => Owner,
       Version       => (if Owner /= No_Unit
                         then Get_Version (Owner) else 0)));

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
   --  identical New_Env in the set of rebindings.

   function Get_Internal
     (Self          : Lexical_Env;
      Key           : Symbol_Type;
      Recursive     : Boolean := True;
      Rebindings    : Env_Rebindings := null;
      Metadata      : Element_Metadata := Empty_Metadata)
      return Lookup_Result_Array;

   procedure Reset_Lookup_Cache (Self : Lexical_Env);
   --  Reset Self's lexical environment lookup cache

   ---------------------------
   -- Is_Lookup_Cache_Valid --
   ---------------------------

   function Is_Lookup_Cache_Valid (Env : Lexical_Env) return Boolean is
      P : Lexical_Env;
   begin
      if Env.Env.Lookup_Cache_Valid then
         P := Get_Env (Env.Env.Parent);
         if P /= Null_Lexical_Env then
            return Is_Lookup_Cache_Valid (P);
         else
            return True;
         end if;
      end if;
      return False;
   end Is_Lookup_Cache_Valid;

   ------------------------
   -- Reset_Lookup_Cache --
   ------------------------

   procedure Reset_Lookup_Cache (Self : Lexical_Env) is
   begin
      --  Don't destroy the map itself: preserve entries but clear their
      --  contents. This is an optimization that will save time during
      --  deallocation (here) and reallocation (when filling the cache again
      --  with the lookups to come).

      for C in Self.Env.Lookup_Cache.Iterate loop
         Self.Env.Lookup_Cache.Reference (C).Elements.Destroy;

         Self.Env.Lookup_Cache.Replace_Element
           (C, No_Lookup_Cache_Entry);
      end loop;

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
     (Resolver : Lexical_Env_Resolver; Node : Element_T) return Env_Getter is
   begin
      return Env_Getter'(True, Null_Lexical_Env, Node, Resolver, False);
   end Dyn_Env_Getter;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (Self : in out Env_Getter) is
      Env : Lexical_Env;
   begin
      case Self.Dynamic is
         when True =>
            Self.Computed := False;
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
         if not Self.Computed then
            declare
               R : constant Lexical_Env_Resolver := Self.Resolver;
               E : constant Entity :=
                 (El => Self.Node, Info => No_Entity_Info);
            begin

               if Self.Env /= Null_Lexical_Env then

                  if not Is_Stale (Self.Env) then
                     Inc_Ref (Self.Env);
                     return Self.Env;
                  end if;

                  Dec_Ref (Self.Env);
               end if;

               --  We use the share returned by the resolver, so no need for
               --  inc ref here.
               Self.Env := R.all (E);

               Self.Computed := True;
            end;
         end if;
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

         Register_Rebinding (Old_Env.Env.Node, Result.all'Address);
         Register_Rebinding (New_Env.Env.Node, Result.all'Address);
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
      if not Is_Rebindable (Old_Env.Env.Node) then
         Raise_Property_Error ("Illegal lexical environment rebinding");
      end if;

      return Append (Self, Old_Env, New_Env);
   end Append_Rebinding;

   ------------
   -- Create --
   ------------

   function Create (El : Element_T; MD : Element_Metadata) return Entity is
   begin
      return Entity'
        (El      => El,
         Info    => (MD => MD, Rebindings => null));
   end Create;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Entity) return Boolean is
   begin
      if L.El /= R.El then
         return False;
      end if;

      --  All null element are equals, regardless of their entity info
      if L.El = No_Element then
         return True;
      end if;

      --  For all other cases, make sure the entity info is equivalent
      return L.Info = R.Info;
   end Equivalent;

   ------------
   -- Create --
   ------------

   function Create
     (Parent            : Env_Getter;
      Node              : Element_T;
      Is_Refcounted     : Boolean;
      Default_MD        : Element_Metadata := Empty_Metadata;
      Transitive_Parent : Boolean := False;
      Owner             : Unit_T) return Lexical_Env is
   begin
      if Parent /= No_Env_Getter then
         Inc_Ref (Parent);
      end if;
      return Wrap
        (new Lexical_Env_Type'
           (Parent              => Parent,
            Transitive_Parent   => Transitive_Parent,
            Node                => Node,
            Referenced_Envs     => <>,
            Map                 => new Internal_Envs.Map,
            Default_MD          => Default_MD,
            Rebindings          => null,
            Rebindings_Pool     => null,
            Ref_Count           => (if Is_Refcounted then 1
                                    else No_Refcount),
            Lookup_Cache_Active => True,
            Lookup_Cache_Valid  => True,
            Lookup_Cache        => Lookup_Cache_Maps.Empty_Map),
         Owner => Owner);
   end Create;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self     : Lexical_Env;
      Key      : Symbol_Type;
      Value    : Element_T;
      MD       : Element_Metadata := Empty_Metadata;
      Resolver : Entity_Resolver := null)
   is
      use Internal_Envs;

      Element : constant Internal_Map_Element := (Value, MD, Resolver);
      C       : Cursor;
      Dummy   : Boolean;
      Map     : Internal_Envs.Map renames Self.Env.Map.all;
   begin
      --  See Empty_Env's documentation

      if Self = Empty_Env then
         return;
      end if;

      if Self.Env.Lookup_Cache_Active then
         Self.Env.Lookup_Cache_Valid := False;
      end if;

      Map.Insert (Key, Internal_Map_Element_Vectors.Empty_Vector, C, Dummy);
      Reference (Map, C).Element.Append (Element);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self  : Lexical_Env;
      Key   : Symbol_Type;
      Value : Element_T)
   is
      V : constant Internal_Envs.Reference_Type :=
         Self.Env.Map.Reference (Key);
   begin
      --  Get rid of element. Do this in reverse order so that removing one
      --  element does not make us "step over" the next item. Also don't do
      --  this in place (using V.Pop) as we need to preserve the order of
      --  elements.
      for I in reverse 1 .. V.Length loop
         if V.Get (I).Element = Value then
            V.Remove_At (I);
            exit;
         end if;
      end loop;

      if Self.Env.Lookup_Cache_Active then
         Self.Env.Lookup_Cache_Valid := False;
      end if;
   end Remove;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self            : Lexical_Env;
      Referenced_From : Element_T;
      Resolver        : Lexical_Env_Resolver;
      Transitive      : Boolean := False)
   is
      Refd_Env : Referenced_Env :=
        (Transitive,
         Dyn_Env_Getter (Resolver, Referenced_From),
         False, Inactive);
   begin
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
      Transitive   : Boolean := False)
   is
      Ref : constant Referenced_Env :=
        (Transitive, Simple_Env_Getter (To_Reference), False, Active);
   begin
      Referenced_Envs_Vectors.Append (Self.Env.Referenced_Envs, Ref);
      Self.Env.Lookup_Cache_Valid := False;
   end Reference;

   ---------
   -- Get --
   ---------

   function Get_Internal
     (Self          : Lexical_Env;
      Key           : Symbol_Type;
      Recursive     : Boolean := True;
      Rebindings    : Env_Rebindings := null;
      Metadata      : Element_Metadata := Empty_Metadata)
      return Lookup_Result_Array
   is

      Local_Results      : Lookup_Result_Vector;
      Current_Rebindings : Env_Rebindings;
      Current_Metadata   : Element_Metadata;

      procedure Get_Refd_Elements (Self : in out Referenced_Env);

      procedure Append_Result
        (El         : Internal_Map_Element;
         MD         : Element_Metadata;
         Rebindings : Env_Rebindings);
      --  Add E to results, if it passes the Can_Reach filter. Return whether
      --  result was appended or not.

      use Internal_Envs;

      function Get_Elements (Env : Lexical_Env) return Boolean;

      function Get_Elements (Env : Lexical_Env) return Boolean is
         C        : Cursor := Internal_Envs.No_Element;
         Elements : Internal_Map_Element_Vectors.Vector;
      begin
         if Env.Env.Map /= null then
            C := Env.Env.Map.Find (Key);
         end if;

         if Has_Element (C) then
            Elements := Element (C);

            --  We iterate in reverse, so that last inserted results are
            --  returned first.

            --  TODO??? Use "for ... of reverse" next GPL release
            for I in reverse Elements.First_Index .. Elements.Last_Index loop
               Append_Result
                 (Elements.Get (I),
                  Current_Metadata,
                  Current_Rebindings);
            end loop;
            return True;
         end if;

         return False;
      end Get_Elements;

      -------------------
      -- Append_Result --
      -------------------

      procedure Append_Result
        (El         : Internal_Map_Element;
         MD         : Element_Metadata;
         Rebindings : Env_Rebindings)
      is
         E : constant Entity :=
           (El   => El.Element,
            Info => (MD         => Combine (El.MD, MD),
                     Rebindings => Rebindings));
      begin

         if Has_Trace then
            Traces.Trace
              (Me, "Found " & Image (Element_Image (E.El, False)));
         end if;

         declare
            Resolved_Entity : constant Entity :=
              (if El.Resolver = null
               then E
               else El.Resolver.all (E));
         begin
            Local_Results.Append
              (Lookup_Result_Item'(E                    => Resolved_Entity,
                                   Filter_From          => El.Resolver = null,
                                   Override_Filter_Node => No_Element));
         end;
      end Append_Result;

      -----------------------
      -- Get_Refd_Elements --
      -----------------------

      procedure Get_Refd_Elements (Self : in out Referenced_Env) is
         Env        : Lexical_Env;
      begin
         --  Don't follow the reference environment if either:
         --   * the node from which this reference starts cannot reach From;
         --   * the node that created this environment reference is a parent of
         --     From.

         if (not Recursive
             and then not Self.Is_Transitive)
           or else Self.Being_Visited
           or else Self.State = Inactive
         then
            return;
         end if;

         Self.Being_Visited := True;
         Env := Get_Env (Self.Getter);

         declare
            Refd_Results : constant Lookup_Result_Array :=
              Get_Internal
                (Env, Key,
                 Recursive  => Recursive and Self.Is_Transitive,
                 Rebindings =>
                   (if Self.Is_Transitive
                    then Current_Rebindings
                    else Shed_Rebindings (Env, Current_Rebindings)),
                 Metadata   => Current_Metadata);
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
      end Get_Refd_Elements;

      Env               : Lexical_Env;
      Parent_Env        : Lexical_Env;
      Parent_Rebindings : Env_Rebindings;

      Res_Key           : constant Lookup_Cache_Key :=
        (Key, Rebindings, Metadata);
      Cached_Res_Cursor : Lookup_Cache_Maps.Cursor;
      Res_Val           : Lookup_Cache_Entry;
      Inserted, Dummy   : Boolean;
      use Lookup_Cache_Maps;

      function Do_Cache return Boolean is
        (Recursive and then Self.Env.Lookup_Cache_Active);

   begin
      if Self in Null_Lexical_Env | Empty_Env then
         return Empty_Lookup_Result_Array;
      end if;

      if Has_Trace then
         Traces.Trace
           (Me, "Get_Internal env="
            & Lexical_Env_Image (Self, Dump_Content => False)
            & " key = " & Image (Key.all));
      end if;

      if Do_Cache then

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

            case Res_Val.State is
            when Computing => return Empty_Lookup_Result_Array;
            when Computed => return Res_Val.Elements.To_Array;
            when None => null;
            end case;
         end if;
      end if;

      Current_Rebindings := Combine (Self.Env.Rebindings, Rebindings);
      Current_Metadata := Combine (Self.Env.Default_MD, Metadata);

      --  If there is an environment corresponding to Self in env rebindings,
      --  we'll get it here. We'll also shed it from the set of current
      --  rebindings.

      Env := Extract_Rebinding (Current_Rebindings, Self);

      --  Phase 1: Get elements in own env if there are any

      if not Get_Elements (Env) and then Env /= Self then
         Dummy := Get_Elements (Self);
      end if;

      --  Phase 2: Get elements in transitive referenced envs

      for I in
        Self.Env.Referenced_Envs.First_Index
          .. Self.Env.Referenced_Envs.Last_Index
      loop
         if Self.Env.Referenced_Envs.Get_Access (I).Is_Transitive then
            Get_Refd_Elements (Self.Env.Referenced_Envs.Get_Access (I).all);
         end if;
      end loop;

      --  Phase 3: Get elements in parent envs

      if Recursive or Self.Env.Transitive_Parent then
         Parent_Env := Get_Parent_Env (Self);

         Parent_Rebindings :=
           (if Env /= Self
            then Shed_Rebindings (Parent_Env, Current_Rebindings)
            else Current_Rebindings);

         Local_Results.Concat
           (Get_Internal
              (Parent_Env, Key, True,
               Parent_Rebindings,
               Current_Metadata));
      end if;

      --  Phase 4: Get elements in non transitive referenced envs

      for I in
        Self.Env.Referenced_Envs.First_Index
          .. Self.Env.Referenced_Envs.Last_Index
      loop
         if not Self.Env.Referenced_Envs.Get_Access (I).Is_Transitive then
            Get_Refd_Elements (Self.Env.Referenced_Envs.Get_Access (I).all);
         end if;
      end loop;

      Dec_Ref (Env);

      if Do_Cache then
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
     (Self       : Lexical_Env;
      Key        : Symbol_Type;
      From       : Element_T := No_Element;
      Recursive  : Boolean := True)
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
            Get_Internal (Self, Key, Recursive, null, Empty_Metadata);
      begin
         for El of Results loop
            if From = No_Element
              or else (if El.Override_Filter_Node /= No_Element
                       then Can_Reach (El.Override_Filter_Node, From)
                       else Can_Reach (El.E.El, From))
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
     (Self       : Lexical_Env;
      Key        : Symbol_Type;
      From       : Element_T := No_Element;
      Recursive  : Boolean := True) return Entity
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
           Get_Internal (Self, Key, Recursive, null, Empty_Metadata);
      begin

         for El of V loop
            if From = No_Element
              or else (if El.Override_Filter_Node /= No_Element
                       then Can_Reach (El.Override_Filter_Node, From)
                       else Can_Reach (El.E.El, From))
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
            else (No_Element, No_Entity_Info))
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
      Env : Lexical_Env_Type renames Self.Env.all;
   begin
      return Wrap
        (new Lexical_Env_Type'
           (Parent              => No_Env_Getter,
            Transitive_Parent   => False,
            Node                => Env.Node,
            Referenced_Envs     => Env.Referenced_Envs.Copy,
            Map                 => Env.Map,
            Default_MD          => Env.Default_MD,
            Rebindings          => Env.Rebindings,
            Rebindings_Pool     => null,
            Ref_Count           => <>,
            Lookup_Cache_Active => False,
            Lookup_Cache_Valid  => False,
            Lookup_Cache        => Lookup_Cache_Maps.Empty_Map),
         Owner => Self.Owner);
   end Orphan;

   -----------
   -- Group --
   -----------

   function Group
     (Envs    : Lexical_Env_Array;
      With_Md : Element_Metadata := Empty_Metadata) return Lexical_Env
   is
      N : Lexical_Env;
   begin
      case Envs'Length is
         when 0 =>
            return Empty_Env;
         when others =>
            N := Wrap (new Lexical_Env_Type'
                         (Parent              => No_Env_Getter,
                          Transitive_Parent   => False,
                          Node                => No_Element,
                          Referenced_Envs     => <>,
                          Map                 => null,
                          Default_MD          => With_Md,
                          Rebindings          => null,
                          Rebindings_Pool     => null,
                          Ref_Count           => <>,
                          Lookup_Cache_Active => False,
                          Lookup_Cache_Valid  => False,
                          Lookup_Cache        => Lookup_Cache_Maps.Empty_Map));

            for Env of Envs loop
               Reference (N, Env, Transitive => True);
            end loop;
            return N;
      end case;
   end Group;

   ----------------
   -- Rebind_Env --
   ----------------

   function Rebind_Env
     (Base_Env : Lexical_Env; Rebindings : Env_Rebindings) return Lexical_Env
   is
   begin
      --  If no rebindings were passed, just return the original base env

      if Rebindings = null then
         Inc_Ref (Base_Env);
         return Base_Env;
      end if;

      return N : constant Lexical_Env :=
        Wrap (new Lexical_Env_Type'
                (Parent            => No_Env_Getter,
                 Transitive_Parent => False,
                 Node              => Base_Env.Env.Node,
                 Referenced_Envs   => <>,
                 Map               => null,
                 Default_MD        => Empty_Metadata,
                 Rebindings        => Rebindings,

                 --  This pool is only used on primary lexical environments, so
                 --  there is no need to convey it to synthetic lexical envs.
                 Rebindings_Pool => null,

                 Lookup_Cache        => Lookup_Cache_Maps.Empty_Map,
                 Lookup_Cache_Active => False,
                 Lookup_Cache_Valid  => False,

                 Ref_Count => <>),
              Owner => Base_Env.Owner)
      do
         Reference (N, Base_Env, Transitive => True);
      end return;
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
      Primary : constant Boolean := Is_Primary (Self);
   begin
      --  Do not free the internal map for ref-counted allocated environments
      --  as all maps are owned by analysis unit owned environments.

      Reset_Lookup_Cache (Self);

      if not Self.Is_Refcounted then
         for Elts of Self.Env.Map.all loop
            Internal_Map_Element_Vectors.Destroy (Elts);
         end loop;
         Destroy (Self.Env.Map);
      end if;

      for Ref_Env of Self.Env.Referenced_Envs loop
         declare
            Getter : Env_Getter := Ref_Env.Getter;
         begin
            Dec_Ref (Getter);
         end;
      end loop;
      Self.Env.Referenced_Envs.Destroy;

      if Primary and then Self.Env.Rebindings_Pool /= null then
         Destroy (Self.Env.Rebindings_Pool);
      end if;

      Free (Self.Env);
      Self := Null_Lexical_Env;
   end Destroy;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Lexical_Env) is
   begin
      if Self.Is_Refcounted then
         Self.Env.Ref_Count := Self.Env.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Lexical_Env) is
   begin
      if not Self.Is_Refcounted then
         return;
      end if;

      Self.Env.Ref_Count := Self.Env.Ref_Count - 1;
      if Self.Env.Ref_Count = 0 then
         Dec_Ref (Self.Env.Parent);
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
                     pragma Assert (R = Rebindings);
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
      while
         First_Rebindable_Parent /= Null_Lexical_Env
         and then
           (First_Rebindable_Parent.Env.Node = No_Element
            or else not Is_Rebindable (First_Rebindable_Parent.Env.Node))
      loop
         First_Rebindable_Parent :=
            Get_Env (First_Rebindable_Parent.Env.Parent);
      end loop;

      --  If there is no rebindable parent anywhere, it means we cannot have
      --  rebindings. In that case, shed them all, i.e. return null rebindings.
      if First_Rebindable_Parent = Null_Lexical_Env then
         return null;
      end if;

      --  If we fond a rebindable parent, then we will shed all rebindings
      --  between the top of the rebinding stack and the corresponding
      --  rebinding.
      while Result /= null and then Result.Old_Env /= First_Rebindable_Parent
      loop
         Result := Result.Parent;
      end loop;

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
            pragma Assert
              (L.Old_Env /= R.Old_Env and then L.New_Env /= R.New_Env);
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
        (if Self.Is_Refcounted
         then "<synthetic>" else Element_Image (Self.Env.Node));

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

      function Equivalent (L, R : Env_Getter) return Boolean;
      function Equivalent (L, R : Referenced_Env) return Boolean;

      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Env_Getter) return Boolean is
      begin
         if L.Dynamic then
            return (R.Dynamic
                    and then L.Node = R.Node
                    and then L.Resolver = R.Resolver);
         else
            return not R.Dynamic and then L.Env = R.Env;
         end if;
      end Equivalent;

      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Referenced_Env) return Boolean is
      begin
         return (L.Is_Transitive = R.Is_Transitive
                 and then Equivalent (L.Getter, R.Getter));
      end Equivalent;

   begin
      --  Optimization: if we have two primary lexical environments, don't go
      --  through structural comparison: we can use pointer equality.

      if Is_Primary (L) and then Is_Primary (R) then
         return L = R;
      end if;

      --  Compare all fields to see if they are equivalent. We can use strict
      --  equality for nodes and rebindings because there is only one
      --  value for each equivalence class.

      if not Equivalent (L.Env.Parent, R.Env.Parent)
         or else L.Env.Node /= R.Env.Node
         or else L.Env.Map /= R.Env.Map
         or else L.Env.Default_MD /= R.Env.Default_MD
         or else L.Env.Rebindings /= R.Env.Rebindings
      then
         return False;
      end if;

      if L.Env.Referenced_Envs.Length /= R.Env.Referenced_Envs.Length then
         return False;
      end if;
      for I in 1 .. L.Env.Referenced_Envs.Last_Index loop
         if not Equivalent (L.Env.Referenced_Envs.Get (I),
                            R.Env.Referenced_Envs.Get (I))
         then
            return False;
         end if;
      end loop;

      return True;
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
                   Element_Hash (Getter.Node),
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
         return Combine ((Boolean'Pos (Ref.Is_Transitive),
                          Hash (Ref.Getter)));
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

   begin
      if Env = null then
         return Initial_Hash;
      end if;

      return Combine
        ((Hash (Env.Parent),
          Element_Hash (Env.Node),
          Hash (Env.Referenced_Envs),
          Hash (Env.Map),
          Metadata_Hash (Env.Default_MD),
          Hash (Env.Rebindings)));
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
     (Symbol_Type,
      Element_Type    => Internal_Map_Element_Vectors.Vector,
      "<"             => "<",
      "="             => Internal_Map_Element_Vectors."=");

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
      Dump_Content   : Boolean := True) return String
   is

      Result : Unbounded_String;

      use Sorted_Envs;

      function Short_Image
        (N : Element_T) return String
      is (if N = No_Element then "<null>"
          else Image (Element_Image (N, False)));
      --  TODO??? This is slightly hackish, because we're converting a wide
      --  string back to string. But since we're using this solely for
      --  test/debug purposes, it should not matter. Still, would be good to
      --  have Text_Type everywhere at some point.

      function Image (El : Internal_Map_Element) return String is
        (Short_Image (El.Element));

      function Image is new Internal_Map_Element_Vectors.Image
        (Image);

      procedure Dump_Referenced
        (Name : String; Refs : in out Referenced_Envs_Vectors.Vector);

      First_Arg : Boolean := True;

      procedure New_Arg;
      procedure New_Arg is
      begin
         if First_Arg then
            First_Arg := False;
         else
            Append (Result, ", ");
         end if;
      end New_Arg;

      ---------------------
      -- Dump_Referenced --
      ---------------------

      procedure Dump_Referenced
        (Name : String; Refs : in out Referenced_Envs_Vectors.Vector)
      is
         Is_First : Boolean := True;
      begin
         for I in Refs.First_Index .. Refs.Last_Index loop
            declare
               G   : Env_Getter renames Refs.Get_Access (I).Getter;
               Env : Lexical_Env := Get_Env (G);
            begin
               if Env /= Empty_Env then
                  if Is_First then
                     Append (Result, "    " & Name & ":" & ASCII.LF);
                     Is_First := False;
                  end if;
                  Append (Result, "      ");
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
      end Dump_Referenced;

   begin
      if Self = Null_Lexical_Env then
         return "";
      end if;

      if Env_Id'Length /= 0 then
         Append (Result, Env_Id & " = ");
      end if;
      Append (Result, "LexEnv(");
      if Self = Empty_Env then
         New_Arg;
         Append (Result, "Empty");
      end if;
      if Self.Is_Refcounted then
         New_Arg;
         Append (Result, "Synthetic");
      end if;
      if Parent_Env_Id'Length > 0 then
         New_Arg;
         Append (Result,
                 "Parent="
                 & (if Self.Env.Parent /= No_Env_Getter
                    then Parent_Env_Id else "null"));
      end if;
      if Self.Env.Node /= No_Element then
         New_Arg;
         Append (Result, "Node="
                 & Image (Element_Image (Self.Env.Node, False)));
      end if;
      if Dump_Addresses then
         New_Arg;
         Append (Result, "0x" & System.Address_Image (Self.Env.all'Address));
      end if;
      Append (Result, ")");

      if not Dump_Content then
         return To_String (Result);
      end if;
      Append (Result, ":" & ASCII.LF);

      Dump_Referenced ("Referenced", Self.Env.Referenced_Envs);

      if Self.Env.Map = null then
         Append (Result, "    <null>" & ASCII.LF);
      elsif Self.Env.Map.Is_Empty then
         Append (Result, "    <empty>" & ASCII.LF);
      else
         for El in To_Sorted_Env (Self.Env.Map.all).Iterate loop
            Append (Result, "    ");
            Append
              (Result,
               Langkit_Support.Text.Image (Key (El).all) & ": "
               & Image (Element (El))
               & ASCII.LF);
         end loop;
      end if;

      return To_String (Result);
   end Lexical_Env_Image;

   -----------------------------------
   -- Dump_Lexical_Env_Parent_Chain --
   -----------------------------------

   function Lexical_Env_Parent_Chain (Env : Lexical_Env) return String is
      Id     : Positive := 1;
      E      : Lexical_Env := Env;
      Result : Unbounded_String;
   begin

      if E = Null_Lexical_Env then
         Append (Result, "<null>" & ASCII.LF);
      end if;

      while E /= Null_Lexical_Env loop
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
         E := Get_Env (E.Env.Parent);
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

   --------------------
   -- Get_Parent_Env --
   --------------------

   function Get_Parent_Env (Self : Lexical_Env) return Lexical_Env is
      Ret : constant Lexical_Env := Get_Env (Self.Env.Parent);
   begin
      return (if Ret = Null_Lexical_Env
              then Empty_Env
              else Ret);
   end Get_Parent_Env;

   --------------------------------
   -- Deactivate_Referenced_Envs --
   --------------------------------

   procedure Deactivate_Referenced_Envs (Self : Lexical_Env) is
      R : access Referenced_Env;
   begin
      if Self = Null_Lexical_Env then
         return;
      end if;

      Self.Env.Lookup_Cache_Valid := False;

      if Self.Env.Parent /= No_Env_Getter
        and then Self.Env.Parent.Dynamic
      then
         Self.Env.Parent.Computed := False;
      end if;

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
      if Self = Null_Lexical_Env then
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

   function Is_Stale (Env : Lexical_Env) return Boolean is
      L : Lexical_Env;
   begin
      if Env.Owner /= No_Unit then
         return Get_Version (Env.Owner) > Env.Version;
      else
         for I in Env.Env.Referenced_Envs.First_Index
               .. Env.Env.Referenced_Envs.Last_Index
         loop
            L := Get_Env (Env.Env.Referenced_Envs.Get_Access (I).Getter);
            if Is_Stale (L) then
               return True;
            end if;
            Dec_Ref (L);
         end loop;
         return False;
      end if;
   end Is_Stale;

end Langkit_Support.Lexical_Env;
