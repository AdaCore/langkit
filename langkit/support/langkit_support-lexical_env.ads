------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2020, AdaCore                     --
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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;

with System;

with GNATCOLL.Traces;

with Langkit_Support.Hashes;  use Langkit_Support.Hashes;
with Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;
with Langkit_Support.Types;   use Langkit_Support.Types;
with Langkit_Support.Vectors;

--  This package implements a scoped lexical environment data structure that
--  will then be used in AST nodes. Particularities:
--
--  - This data structure implements simple nesting via a Parent_Env link in
--    each env. If the parent is null you are at the topmost env.
--
--  - You can reference other envs, which are virtually treated like parent
--    envs too.
--
--  - You can annotate both whole environments and nodes with metadata, giving
--    more information about the fnodes. The consequence is that metadata needs
--    to be combinable, eg. you need to be able to create a single metadata
--    record from two metadata records.
--
--  TODO??? For the moment, everything is public, because it is not yet clear
--  what the interaction interface will be with the generated library. We might
--  want to make the type private at some point (or not).

generic

   type Precomputed_Symbol_Index is (<>);
   with function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type is <>;
   with package Symbols is new Langkit_Support.Symbols
      (Precomputed_Symbol_Index, Precomputed_Symbol);

   type Unit_T is private;
   No_Unit : Unit_T;
   with function Get_Unit_Version (Unit : Unit_T) return Version_Number;
   --  Used to retrieve the version number of the given Unit, for cache
   --  invalidation purposes.
   with function Get_Context_Version (Unit : Unit_T) return Integer;
   --  Used to retrieve the version number of the context associated with the
   --  given Unit, for cache invalidation purposes.

   type Node_Type is private;
   type Node_Metadata is private;
   No_Node        : Node_Type;
   Empty_Metadata : Node_Metadata;

   with function "<" (Left, Right : Node_Type) return Boolean is <>;
   with function "=" (Left, Right : Node_Type) return Boolean is <>;

   type Ref_Category is (<>);
   type Ref_Categories is array (Ref_Category) of Boolean;

   with function Node_Unit (Node : Node_Type) return Unit_T is <>;
   with function Node_Hash (Node : Node_Type) return Hash_Type;
   with function Metadata_Hash (Metadata : Node_Metadata) return Hash_Type;

   with function Combine (L, R : Node_Metadata) return Node_Metadata;

   with function Can_Reach (Node, From : Node_Type) return Boolean is <>;
   --  Function that will allow filtering nodes depending on the origin node of
   --  the request. In practice, this is used to implement sequential semantics
   --  for lexical envs, as-in, node declared after another is not yet visible.

   with function Is_Rebindable (Node : Node_Type) return Boolean is <>;
   --  Return whether a lexical environment whose node is Node can be rebound

   with function Node_Text_Image
     (Node  : Node_Type; Short : Boolean := True) return Text_Type;

   with procedure Register_Rebinding
     (Node : Node_Type; Rebinding : System.Address);
   --  Register a rebinding to be destroyed when Node is destroyed

package Langkit_Support.Lexical_Env is

   Activate_Lookup_Cache : Boolean := True;

   All_Cats : Ref_Categories := (others => True);

   pragma Compile_Time_Error
     (Ref_Categories'Length > 32,
      "Categories has to fit in a 32 bits Integer");

   function Text_Image (Cats : Ref_Categories) return Text_Type;

   pragma Suppress (Container_Checks);
   --  Remove container checks for standard containers

   use GNATCOLL;
   use Symbols;

   Debug_Mode : constant Boolean := True;

   Me : constant Traces.Trace_Handle :=
     Traces.Create ("LANGKIT.LEXICAL_ENV", Traces.From_Config);

   Rec : constant Traces.Trace_Handle :=
     Traces.Create ("LANGKIT.LEXICAL_ENV.RECURSIVE", Traces.From_Config);

   Caches_Trace : constant Traces.Trace_Handle :=
     Traces.Create ("LANGKIT.LEXICAL_ENV.CACHES", Traces.From_Config);

   --  Traces to debug lexical envs. This trace is meant to be activated on
   --  demand, when the client of lexical env wants more information about
   --  this specific lookup.

   function Has_Trace return Boolean
   is (Debug_Mode and then Traces.Active (Me));

   type Env_Rebindings_Type;
   type Env_Rebindings is access all Env_Rebindings_Type;
   --  Set of mappings from one lexical environment to another. This is used to
   --  temporarily substitute lexical environment during symbol lookup.

   --------------
   -- Entities --
   --------------

   type Entity_Info is record
      MD : Node_Metadata;
      --  External metadata for the node

      Rebindings : Env_Rebindings := null;
      --  Rebindings applying to this entity

      From_Rebound : Boolean := False;
      --  Whether this entity has been obtained out of a rebound environment
   end record
      with Convention => C;

   type Entity is record
      Node : Node_Type;
      Info : Entity_Info;
   end record;
   --  Wrapper structure to contain both the 'real' node that the user wanted
   --  to store, and its associated metadata.

   function Create_Entity (Node : Node_Type; MD : Node_Metadata) return Entity;
   --  Constructor that returns an Entity from an Node_Type and an
   --  Node_Metadata instances.

   function Equivalent (L, R : Entity) return Boolean;
   --  Return whether we can consider that L and R are equivalent entities

   function Equivalent (L, R : Entity_Info) return Boolean;
   --  Return whether we can consider that L and R are equivalent entity info

   ----------------------
   -- Lexical_Env Type --
   ----------------------

   type Lexical_Env_Kind is (Primary, Orphaned, Grouped, Rebound);
   --  Kind of lexical environment. Tells how a lexical environment was
   --  created.
   --
   --  Primary ones are all lexical environments that are not ref-counted.
   --  These are created by lexical environment population plus the special
   --  Empty_Env, and each context's root scope.
   --
   --  Orphaned ones are copies whose parents have been stripped.
   --
   --  Grouped ones are just a collection of environments glued together as if
   --  they were only one environment.
   --
   --  Rebound ones are copies annotated with environment rebindings.

   type Lexical_Env_Type;
   --  Value type for lexical envs

   type Lexical_Env_Access is access all Lexical_Env_Type;

   type Lexical_Env is record
      Env : Lexical_Env_Access;
      --  Referenced lexical environment

      Hash : Hash_Type;
      --  Env's hash. We need to pre-compute it so that the value is available
      --  even after Env is deallocated. This makes it possible to destroy a
      --  hash table that contains references to deallocated environments.

      Kind : Lexical_Env_Kind;
      --  The kind of Env. When it is Primary, we can avoid calling Dec_Ref at
      --  destruction time. This is useful because at analysis unit destruction
      --  time, this may be a dangling access to an environment from another
      --  unit.

      Owner : Unit_T := No_Unit;
      --  Unit that owns this lexical environment. Only Primary and Rebound
      --  lexical env will have a non-null value for this field.

      Version : Version_Number := 0;
      --  Version of the unit when this reference was made. Used to determine
      --  whether this reference is valid or not.
   end record;
   --  Reference to a lexical environment. This is the type that shall be used.

   Null_Lexical_Env : constant Lexical_Env := (null, 0, Primary, No_Unit, 0);

   type Lexical_Env_Array is array (Positive range <>) of Lexical_Env;

   type Lexical_Env_Resolver is access
     function (Ref : Entity) return Lexical_Env;
   --  Callback type for the lazy referenced env resolution mechanism

   ----------------
   -- Env_Getter --
   ----------------

   type Env_Getter (Dynamic : Boolean := False) is record
      Env : Lexical_Env := Null_Lexical_Env;
      --  If Dynamic, cache for the resolved lexical environment. To be used
      --  only when No_Entity_Info is used for the resolution. We consider that
      --  this cache contains a valid entry when Env is not Null_Lexical_Env
      --  and that it is not stale.

      case Dynamic is
         when True =>
            Node     : Node_Type;
            Resolver : Lexical_Env_Resolver;
            --  Data and callable to resolve this getter

         when False =>
            null;
      end case;
   end record;
   --  Link to an environment. It can be either a simple link (just a pointer)
   --  or a dynamic link (a function that recomputes the link when needed). See
   --  the two constructors below.

   No_Env_Getter : constant Env_Getter := (False, Null_Lexical_Env);

   procedure Resolve (Self : in out Env_Getter; Info : Entity_Info);
   --  Resolve the reference for this env getter. Info is forwarded to the
   --  resolver callback.

   function Simple_Env_Getter (E : Lexical_Env) return Env_Getter;
   --  Create a static Env_Getter (i.e. pointer to environment)

   function Dyn_Env_Getter
     (Resolver : Lexical_Env_Resolver; Node : Node_Type) return Env_Getter;
   --  Create a dynamic Env_Getter (i.e. function and closure to compute an
   --  environment).

   function Get_Env
     (Self : in out Env_Getter; Info : Entity_Info) return Lexical_Env;
   --  Return the environment associated to the Self env getter. If Self is
   --  dynamic, Info is forwarded to the resolver callback.

   function Equivalent (L, R : Env_Getter) return Boolean;
   --  If at least one of L and R is a dynamic env getter, raise a
   --  Constraint_Error. Otherwise, return whether the pointed environments are
   --  equal.

   procedure Inc_Ref (Self : Env_Getter);
   --  Shortcut to run Inc_Ref of the potentially embedded lexical environment

   procedure Dec_Ref (Self : in out Env_Getter);
   --  Shortcut to run Dec_Ref of the potentially embedded lexical environment

   --------------------
   -- Env_Rebindings --
   --------------------

   package Env_Rebindings_Vectors is new Langkit_Support.Vectors
     (Env_Rebindings);

   type Env_Rebindings_Type is record
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env;
      Children         : Env_Rebindings_Vectors.Vector;
   end record;
   --  Tree of remappings from one lexical environment (Old_Env) to another
   --  (New_Env). Note that both referenced environments must be primary and
   --  env rebindings are supposed to be destroyed when one of their
   --  dependencies (Parent, Old_Env or New_Env) is destroyed, so there is no
   --  need for ref-counting primitives.

   function Combine (L, R : Env_Rebindings) return Env_Rebindings;
   --  Return a new Env_Rebindings that combines rebindings from both L and R

   function OK_For_Rebindings (Self : Lexical_Env) return Boolean;
   --  Return whether Self is a lexical environment that can be used in
   --  environment rebindings (for old or new env).

   function Append
     (Self             : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings
      with Pre => OK_For_Rebindings (Old_Env)
                  and then OK_For_Rebindings (New_Env);
   --  Create a new rebindings and register it to Self and to
   --  Old_Env/New_Env's analysis units.

   function Append_Rebinding
     (Self    : Env_Rebindings;
      Old_Env : Lexical_Env;
      New_Env : Lexical_Env) return Env_Rebindings
      with Pre => OK_For_Rebindings (Old_Env)
                  and then OK_For_Rebindings (New_Env);

   function Hash is new Hashes.Hash_Access
     (Env_Rebindings_Type, Env_Rebindings);

   function Text_Image (Self : Env_Rebindings) return Text_Type;

   ----------------------------------
   -- Arrays of nodes and entities --
   ----------------------------------

   package Entity_Vectors is new Langkit_Support.Vectors
     (Entity, Small_Vector_Capacity => 2);
   --  Vectors used to store collections of nodes, as values of a lexical
   --  environment map. We want to use vectors internally.

   type Node_Array is array (Positive range <>) of Node_Type;

   subtype Entity_Array is Entity_Vectors.Elements_Array;
   --  Arrays of wrapped nodes stored in the environment maps

   -----------------------------
   -- Referenced environments --
   -----------------------------

   type Ref_Kind is (Transitive, Prioritary, Normal);
   --  Kind for a referenced env. Can be any of:
   --
   --  * Transitive: The reference is transitive, e.g. it will be explored in
   --    every case (whether the lookup is recursive or not). It will be
   --    explored *before* parent environments.
   --
   --  * Prioritary: The reference is non transitive, e.g. it will be
   --    explored only if the lookup on the env is recursive. It will be
   --    explored *before* parent environments.
   --
   --  * Normal: The reference is non transitive, e.g. it will be explored
   --    only if the lookup on the env is recursive. It will be explored
   --    *after* parent environments.

   type Refd_Env_State is (Active, Inactive);

   type Referenced_Env is record
      Kind : Ref_Kind := Normal;
      --  Kind for this referenced env

      Getter : Env_Getter;
      --  Closure to fetch the environment that is referenced

      Being_Visited : Boolean;
      --  Flag set to true when Referenced_Env is being visited. Used as a
      --  recursion guard. WARNING: Not thread safe.

      State : Refd_Env_State := Inactive;
      --  State of the referenced env, whether active or inactive

      Categories : Ref_Categories := All_Cats;
   end record;
   --  Represents a referenced env

   package Referenced_Envs_Vectors is new Langkit_Support.Vectors
     (Referenced_Env);
   --  Vectors of referenced envs, used to store referenced environments

   ------------------------------------
   -- Lexical environment public API --
   ------------------------------------

   type Entity_Resolver is access function (Ref : Entity) return Entity;
   --  Callback type for the lazy entity resolution mechanism. Such functions
   --  must take a "reference" entity (e.g. a name) and return the referenced
   --  entity.

   Empty_Env : constant Lexical_Env;
   --  Empty_Env is a magical lexical environment that will always be empty. We
   --  allow users to call Add on it anyway as a convenience, but this is a
   --  no-op. This makes sense as Empty_Env's purpose is to be used to
   --  represent missing scopes from erroneous trees.

   function Create_Lexical_Env
     (Parent            : Env_Getter;
      Node              : Node_Type;
      Transitive_Parent : Boolean := False;
      Owner             : Unit_T) return Lexical_Env
      with Post => Create_Lexical_Env'Result.Kind = Primary;
   --  Create a new primary lexical env

   procedure Add
     (Self     : Lexical_Env;
      Key      : Symbol_Type;
      Value    : Node_Type;
      MD       : Node_Metadata := Empty_Metadata;
      Resolver : Entity_Resolver := null)
      with Pre => Self.Kind = Primary;
   --  Add Value to the list of values for the key Key, with the metadata MD

   procedure Remove (Self : Lexical_Env; Key : Symbol_Type; Value : Node_Type)
      with Pre => Self.Kind = Primary;
   --  Remove Value from the list of values for the key Key

   procedure Reference
     (Self             : Lexical_Env;
      Referenced_From  : Node_Type;
      Resolver         : Lexical_Env_Resolver;
      Kind             : Ref_Kind := Normal;
      Categories       : Ref_Categories := All_Cats;
      Rebindings_Assoc : Boolean := False)
      with Pre => Self.Kind = Primary;
   --  Add a dynamic reference from Self to the lexical environment computed
   --  calling Resolver on Referenced_From. This makes the content of this
   --  dynamic environment accessible when performing lookups on Self (see the
   --  Get function).
   --
   --  Unless the reference is transitive, requests with an origin point (From
   --  parameter), the content will only be visible if:
   --
   --    * Can_Reach (Referenced_From, From) is True. Practically this means
   --      that the origin point of the request needs to be *after*
   --      Referenced_From in the file.
   --
   --  If ``Rebindings_Assoc`` is True, then the referenced env will be
   --  considered just as Self when shedding rebindings.

   procedure Reference
     (Self             : Lexical_Env;
      To_Reference     : Lexical_Env;
      Kind             : Ref_Kind := Normal;
      Categories       : Ref_Categories := All_Cats;
      Rebindings_Assoc : Boolean := False)
      with Pre => Self.Kind = Primary;
   --  Add a static reference from Self to To_Reference. See above for the
   --  meaning of arguments.

   procedure Deactivate_Referenced_Envs (Self : Lexical_Env)
      with Pre => Self.Kind = Primary;
   --  Invalidate caches in Self. This:
   --
   --    * invalidates the environment lookup cache;
   --    * invalidates the cached parent environment link (if the parent link
   --      is dynamic);
   --    * deactivate referenced environments.

   procedure Recompute_Referenced_Envs (Self : Lexical_Env)
      with Pre => Self.Kind = Primary;
   --  Recompute the referenced environments for this environment. In other
   --  words, re-resolve the R.Getter for all referenced environments R in
   --  Self.
   --
   --  Before calling this, one must call Deactivate_Referenced_Envs on every
   --  referenced environment reachable from Self: referenced environments in
   --  Self, but also referenced environments in Self's parents.

   procedure Reset_Caches (Self : Lexical_Env)
     with Pre => Self.Kind = Primary;
   --- Reset the caches for this env

   type Lookup_Kind_Type is (Recursive, Flat, Minimal);

   function Get
     (Self        : Lexical_Env;
      Key         : Symbol_Type;
      From        : Node_Type := No_Node;
      Lookup_Kind : Lookup_Kind_Type := Recursive;
      Categories  : Ref_Categories := All_Cats) return Entity_Vectors.Vector;
   --  Get the array of entities for this Key. If From is given, then nodes
   --  will be filtered according to the Can_Reach primitive given as parameter
   --  for the generic package.
   --
   --  If Recursive, look for Key in all Self's parents as well, and in
   --  referenced envs. Otherwise, limit the search to Self.
   --
   --  If Filter is not null, use it as a filter to disable lookup on envs for
   --  which Filter.all (From, Env) returns False.
   --
   --  If ``Key`` is null, return every entity in the scope regardless of name.

   function Get
     (Self        : Lexical_Env;
      Key         : Symbol_Type;
      From        : Node_Type := No_Node;
      Lookup_Kind : Lookup_Kind_Type := Recursive;
      Categories  : Ref_Categories := All_Cats) return Entity_Array;

   function Get_First
     (Self        : Lexical_Env;
      Key         : Symbol_Type;
      From        : Node_Type := No_Node;
      Lookup_Kind : Lookup_Kind_Type := Recursive;
      Categories  : Ref_Categories := All_Cats) return Entity;
   --  Like Get, but return only the first matching entity. Return a null
   --  entity if no entity is found.

   function Orphan (Self : Lexical_Env) return Lexical_Env;
   --  Return a dynamically allocated copy of Self that has no parent. If Self
   --  is a grouped environment or if it has any transitive parent, this raises
   --  a property error.

   function Parent (Self : Lexical_Env) return Lexical_Env;
   --  Return the parent lexical env for env Self or Empty_Env if Self has no
   --  parent.

   function Env_Node (Self : Lexical_Env) return Node_Type;
   --  Return the node associated to Self, if any

   function Group
     (Envs    : Lexical_Env_Array;
      With_Md : Node_Metadata := Empty_Metadata) return Lexical_Env;
   --  Return a lexical environment that logically groups together multiple
   --  lexical environments. Note that this does not modify the input
   --  environments, however it returns a new owning reference.
   --
   --  If this array is empty, Empty_Env is returned. Note that if Envs'Length
   --  is greater than 1, the result is dynamically allocated.
   --
   --  If With_Md is passed, the resulting env will have the passed metadata
   --  instance as default metadata. As a result, any node returned will have
   --  its metadata combined with the default metadata.

   function Rebind_Env
      (Base_Env : Lexical_Env;
       E_Info   : Entity_Info) return Lexical_Env;
   function Rebind_Env
      (Base_Env   : Lexical_Env;
       Rebindings : Env_Rebindings) return Lexical_Env;
   --  Return a new env based on Base_Env to include the given Rebindings

   procedure Inc_Ref (Self : Lexical_Env);
   --  If Self is a ref-counted lexical env, increment this reference count. Do
   --  nothing otherwise.

   procedure Dec_Ref (Self : in out Lexical_Env);
   --  If Self is a ref-counted lexical env, decrement this reference count and
   --  set it to null. Also destroy it if the count drops to 0. Do nothing
   --  otherwise.

   function Shed_Rebindings
     (E_Info : Entity_Info; Env : Lexical_Env) return Entity_Info;
   --  Return a new entity info from E_Info, shedding env rebindings that are
   --  not in the parent chain for the env From_Env.

   function Equivalent (L, R : Lexical_Env) return Boolean;
   --  Return whether L and R are equivalent lexical environments: same
   --  envs topology, same internal map, etc.

   function Hash (Env : Lexical_Env) return Hash_Type is (Env.Hash);

   ---------------------------------------
   -- Lexical environment lookup caches --
   ---------------------------------------

   type Lookup_Result_Item is record
      E : Entity;
      --  Returned entity

      Filter_From : Boolean;
      --  Whether to filter with Can_Reach

      Override_Filter_Node : Node_Type := No_Node;
      --  Node to use when filtering with Can_Reach, if different from the
      --  Entity.
   end record;
   --  Lexical environment lookup result item. Lookups return arrays of these.

   package Lookup_Result_Item_Vectors is new Langkit_Support.Vectors
     (Lookup_Result_Item, Small_Vector_Capacity => 2);

   subtype Lookup_Result_Vector is Lookup_Result_Item_Vectors.Vector;
   Empty_Lookup_Result_Vector : Lookup_Result_Vector renames
      Lookup_Result_Item_Vectors.Empty_Vector;

   subtype Lookup_Result_Array  is
      Lookup_Result_Item_Vectors.Elements_Array;
   Empty_Lookup_Result_Array : Lookup_Result_Array renames
      Lookup_Result_Item_Vectors.Empty_Array;

   type Lookup_Cache_Key is record
      Symbol : Symbol_Type;
      --  Symbol for this lookup

      Rebindings : Env_Rebindings;
      --  Rebindings used for this lookup

      Metadata : Node_Metadata;
      --  Metadata used for this lookup

      Categories : Ref_Categories := All_Cats;
   end record;
   --  Key in environment lookup caches. Basically the parameters for the Get
   --  functiont that are relevant for caching.

   type Lookup_Cache_Entry_State is (Computing, Computed, None);
   --  Status of an entry in lexical environment lookup caches.
   --
   --  Computing represents the dummy entry that is inserted during original
   --  computation. That means that a cache hit that returns a Computing entry
   --  reveals an infinite recursion (a lexical environment lookup that calls
   --  itself recursively).
   --
   --  Computed represents an entry whose elements are fine to be used as a
   --  cache hit.
   --
   --  None represents a cleared cache entry, i.e. getting it out of a cache
   --  means there's a cache miss. Using this state instead of just removing
   --  the cache is used to avoid destroying the cache map when clearing
   --  caches.

   type Lookup_Cache_Entry is record
      State    : Lookup_Cache_Entry_State;
      Elements : Lookup_Result_Item_Vectors.Vector;
   end record;
   --  Result of a lexical environment lookup

   No_Lookup_Cache_Entry : constant Lookup_Cache_Entry :=
     (None, Empty_Lookup_Result_Vector);

   function Hash (Self : Lookup_Cache_Key) return Hash_Type
   is
     (Combine
        (Combine (Hash (Self.Symbol), Hash (Self.Rebindings)),
         Metadata_Hash (Self.Metadata)));

   package Lookup_Cache_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Lookup_Cache_Key,
      Element_Type    => Lookup_Cache_Entry,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   ----------------------------------------
   -- Lexical environment representation --
   ----------------------------------------

   package Lexical_Env_Vectors is new Langkit_Support.Vectors (Lexical_Env);

   type Internal_Map_Node is record
      Node : Node_Type;
      --  If Resolver is null, this is the node that lexical env lookup must
      --  return. Otherwise, it is the argument to pass to Resolver in order to
      --  get the result.

      MD : Node_Metadata;
      --  Metadata associated to Node

      Resolver : Entity_Resolver;
   end record;

   package Internal_Map_Node_Vectors is new Langkit_Support.Vectors
     (Internal_Map_Node);

   subtype Internal_Map_Node_Array is
      Internal_Map_Node_Vectors.Elements_Array;

   package Internal_Map_Node_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Node_Type,
      Element_Type => Internal_Map_Node);

   type Internal_Map_Element is record
      Native_Nodes : Internal_Map_Node_Vectors.Vector;
      --  List of node that belong to the same unit as the lexical env that
      --  owns the map.

      Foreign_Nodes : Internal_Map_Node_Maps.Map;
      --  List of nodes that belong to other units (as keys), and associated
      --  metadata/resolvers when applicable (as values). Nodes are sorted by
      --  unit filename/sloc range to preserve determinism.
   end record;
   --  Set of nodes associated to a symbol in a lexical environment

   Empty_Internal_Map_Element : constant Internal_Map_Element :=
     (others => <>);

   package Internal_Envs is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Internal_Map_Element,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Internal_Map is access all Internal_Envs.Map;
   --  Internal maps of Symbols to vectors of nodes

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Internal_Envs.Map, Internal_Map);

   package Env_Rebindings_Pools is new Ada.Containers.Hashed_Maps
     (Key_Type        => Lexical_Env,
      Element_Type    => Env_Rebindings,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Env_Rebindings_Pool is access all Env_Rebindings_Pools.Map;
   --  Pool of env rebindings to be stored in a Lexical_Env

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Env_Rebindings_Pools.Map, Env_Rebindings_Pool);

   type Lexical_Env_Array_Access is access all Lexical_Env_Array;
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Lexical_Env_Array, Lexical_Env_Array_Access);

   type Lexical_Env_Type (Kind : Lexical_Env_Kind) is record
      case Kind is
         when Primary =>
            Parent : Env_Getter := No_Env_Getter;
            --  Parent environment for this env. Null by default.

            Transitive_Parent : Boolean := False;
            --  Whether the parent link is transitive or not

            Node : Node_Type := No_Node;
            --  Node for which this environment was created

            Referenced_Envs : Referenced_Envs_Vectors.Vector;
            --  A list of environments referenced by this environment

            Map : Internal_Map := null;
            --  Map containing mappings from symbols to nodes for this env
            --  instance. If the lexical env is refcounted, then it does not
            --  own this env.

            Rebindings_Pool : Env_Rebindings_Pool := null;
            --  Cache for all parent-less env rebindings whose Old_Env is the
            --  lexical environment that owns this pool. As a consequence, this
            --  is allocated only for primary lexical environments that are
            --  rebindable.

            Lookup_Cache : Lookup_Cache_Maps.Map;
            --  Cache for lexical environment lookups

            Lookup_Cache_Valid : Boolean := True;
            --  Whether Cached_Results contains lookup results that can be
            --  currently reused (i.e. whether they are not stale).

            Rebindings_Assoc_Ref_Env : Integer := -1;
            --  If present, index to the Referenced_Envs vector that points to
            --  an environment we want to look at when shedding rebindings. If
            --  the referenced env is not none, it will be considered in place
            --  of Self when shedding rebindings.

         when others =>
            Ref_Count : Integer := 1;
            --  Number of owners. It is initially set to 1. When it drops to 0,
            --  the env can be destroyed.

            case Kind is
               when Primary =>
                  null; --  Unreachable

               when Orphaned =>
                  Orphaned_Env : Lexical_Env;
                  --  Lexical environment that is orphaned

               when Grouped =>
                  Grouped_Envs : Lexical_Env_Array_Access;
                  --  Array of lexical environment that are grouped together

                  Default_MD : Node_Metadata := Empty_Metadata;
                  --  Default metadata to use for lookups

               when Rebound =>
                  Rebound_Env : Lexical_Env;
                  --  Lexical environment that is rebound

                  Rebindings : Env_Rebindings;
                  --  Rebindings for this rebound environment

                  Context_Version : Integer;
                  --  Version of the context at the time of creation of this
                  --  rebound env. This is used to determine if its content
                  --  (in particular the Rebindings field) has become stale
                  --  or not.
            end case;
      end case;
   end record;

   function Wrap
     (Env   : Lexical_Env_Access;
      Owner : Unit_T := No_Unit) return Lexical_Env;

   procedure Destroy (Self : in out Lexical_Env);
   --  Deallocate the resources allocated to the Self lexical environment. Must
   --  not be used directly for ref-counted envs.

   function Is_Stale (Self : Lexical_Env) return Boolean;
   --  Return whether Self points to a now defunct lexical env

   -------------------
   -- Debug helpers --
   -------------------

   function Lexical_Env_Image
     (Self           : Lexical_Env;
      Env_Id         : String := "";
      Parent_Env_Id  : String := "";
      Dump_Addresses : Boolean := False;
      Dump_Content   : Boolean := True;
      Prefix         : String := "") return String;
   --  Return a textual representation of Self.
   --
   --  If provided, Env_Id and Parent_Env_Id are used to designate Self and its
   --  parent environment.
   --
   --  If Dump_Addresses, include the hexadecimal address of each represented
   --  lexical environment.
   --
   --  If Dump_Content, show the inner data in lexical environments: referenced
   --  environments and internal map for primary environments, pointed
   --  environment in orphaned environments, etc. If Dump_Content is true, the
   --  result is a multi-line string, otherwise it's guaranteed to fit on a
   --  single line.
   --
   --  Prefix is used to prefix each emitted line.

   function Lexical_Env_Parent_Chain (Env : Lexical_Env) return String;

   procedure Dump_One_Lexical_Env
     (Self           : Lexical_Env;
      Env_Id         : String := "";
      Parent_Env_Id  : String := "";
      Dump_Addresses : Boolean := False;
      Dump_Content   : Boolean := True);

   procedure Dump_Lexical_Env_Parent_Chain (Env : Lexical_Env);

private

   function Hash (Env : Lexical_Env_Access) return Hash_Type;

   Empty_Env_Map    : aliased Internal_Envs.Map := Internal_Envs.Empty_Map;
   Empty_Env_Record : aliased Lexical_Env_Type :=
     (Kind                     => Primary,
      Parent                   => No_Env_Getter,
      Transitive_Parent        => False,
      Node                     => No_Node,
      Referenced_Envs          => <>,
      Map                      => Empty_Env_Map'Access,
      Rebindings_Pool          => null,
      Lookup_Cache_Valid       => False,
      Lookup_Cache             => Lookup_Cache_Maps.Empty_Map,
      Rebindings_Assoc_Ref_Env => -1);

   --  Because of circular elaboration issues, we cannot call Hash here to
   --  compute the real hash. Using a dummy precomputed one is probably enough.
   Empty_Env : constant Lexical_Env :=
     (Env     => Empty_Env_Record'Access,
      Hash    => 0,
      Kind    => Primary,
      Owner   => No_Unit,
      Version => 0);

end Langkit_Support.Lexical_Env;
