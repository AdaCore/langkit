--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;

with System;

with GNATCOLL.Traces;

with Langkit_Support.Hashes;  use Langkit_Support.Hashes;
with Langkit_Support.Types;   use Langkit_Support.Types;
with Langkit_Support.Vectors;

package Langkit_Support.Lexical_Envs is

   type Lookup_Cache_Kind is (Disabled, Toplevel_Only, Full);

   Lookup_Cache_Mode : Lookup_Cache_Kind := Full;
   --  Lookup cache mode for the lexical envs.
   --
   --  ``Full`` means that every env.get request, including intermediate ones
   --  happening as part of a user requested env.get, will be cached.
   --
   --  ``Toplevel_Only`` means that only top level requests, directly requested
   --  by the user of the Lexical_Envs API, will be cached.
   --
   --  ``Disabled`` means no caching will happen.
   --
   --  This setting is for debugging: caching all requests is the normal mode
   --  (maximum optimization), and the other modes reduce the amount of caching
   --  done (less optimization, thus taking longer to run) to ease the
   --  investigation of env caching bugs.

   -------------
   --  Traces --
   -------------

   --  Traces to debug lexical envs. Those traces are meant to be activated on
   --  demand, when the client of lexical env wants more information about
   --  this specific lookup.

   Me : constant GNATCOLL.Traces.Trace_Handle :=
      GNATCOLL.Traces.Create
        ("LANGKIT.LEXICAL_ENV", GNATCOLL.Traces.From_Config);
   --  This is the main trace for lexical environments, providing a basic level
   --  of logging for env.get requests.

   Rec : constant GNATCOLL.Traces.Trace_Handle :=
      GNATCOLL.Traces.Create
        ("LANGKIT.LEXICAL_ENV.RECURSIVE", GNATCOLL.Traces.From_Config);
   --  This is the recursive trace, providing info about recursive internal
   --  calls to env.get.

   Caches_Trace : constant GNATCOLL.Traces.Trace_Handle :=
      GNATCOLL.Traces.Create
        ("LANGKIT.LEXICAL_ENV.CACHES", GNATCOLL.Traces.From_Config);
   --  This a trace to show caching information

   Min : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.LEXICAL_ENV_MINIMAL", GNATCOLL.Traces.From_Config);
   --  This is a trace independent from the three last traces, that you can
   --  activate separately, and that will provide you the most basic level of
   --  logging for toplevel env.get requests.

   function Has_Trace return Boolean is (Me.Active);

   -----------------
   -- Lexical_Env --
   -----------------

   type Lexical_Env_Kind is
     (Static_Primary, Dynamic_Primary, Orphaned, Grouped, Rebound);
   --  Kind of lexical environment. Tells how a lexical environment was
   --  created.
   --
   --  Static_Primary ones are not ref-counted. Except for the special
   --  Empty_Env and each context's root scope, they are created by lexical
   --  environment population.
   --
   --  Dynamic_Primary are not ref-counted neither. They are created on-demand
   --  during semantic analysis, but their life cycle is tied to their owning
   --  analysis unit, just like Static_Primary envs. They carry no map, but
   --  instead use a property reference to dynamically compute environment
   --  associations (an array of Inner_Env_Assoc).
   --
   --  Orphaned ones are copies whose parents have been stripped.
   --
   --  Grouped ones are just a collection of environments glued together as if
   --  they were only one environment.
   --
   --  Rebound ones are copies annotated with environment rebindings.

   subtype Primary_Kind is
      Lexical_Env_Kind range Static_Primary ..  Dynamic_Primary;

   type Base_Lexical_Env_Record is abstract tagged null record;
   --  Root class of the lexical env type for all languages

   type Generic_Lexical_Env_Ptr is access all Base_Lexical_Env_Record'Class;
   --  Generic access to lexical environment records. The actual record type
   --  depends on each language, hence the generic pointer.

   No_Lexical_Env : constant Generic_Lexical_Env_Ptr := null;

   type Generic_Unit_Ptr is new System.Address;
   --  Likewise for analysis units

   No_Generic_Unit : constant Generic_Unit_Ptr :=
      Generic_Unit_Ptr (System.Null_Address);

   type Lexical_Env is record
      Env : Generic_Lexical_Env_Ptr;
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

      Owner : Generic_Unit_Ptr := No_Generic_Unit;
      --  Unit that owns this lexical environment. Only Primary and Rebound
      --  lexical env will have a non-null value for this field.

      Version : Version_Number := 0;
      --  Version of the unit when this reference was made. Used to determine
      --  whether this reference is valid or not.
   end record;
   --  Reference to a lexical environment. This is the type that shall be used.

   Null_Lexical_Env : constant Lexical_Env :=
     (No_Lexical_Env, 0, Static_Primary, No_Generic_Unit, 0);

   type Lexical_Env_Array is array (Positive range <>) of Lexical_Env;
   type Lexical_Env_Array_Access is access all Lexical_Env_Array;

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Lexical_Env_Array, Lexical_Env_Array_Access);

   function Is_Primary (Self : Lexical_Env) return Boolean
   is (Self.Kind in Primary_Kind);
   --  Return whether Self is a primary lexical environment

   function Has_Lookup_Cache (Self : Lexical_Env) return Boolean
   is
     (Self.Kind = Static_Primary);
   --  Whether lookup cache is available/enabled for the given lexical
   --  environment.

   function Hash (Env : Lexical_Env) return Hash_Type is (Env.Hash);

   type Lookup_Kind_Type is (Recursive, Flat, Minimal);

   --------------------
   -- Env_Rebindings --
   --------------------

   type Env_Rebindings_Type;
   type Env_Rebindings is access all Env_Rebindings_Type;
   --  Set of mappings from one lexical environment to another. This is used to
   --  temporarily substitute lexical environment during symbol lookup.

   package Env_Rebindings_Vectors is new Langkit_Support.Vectors
     (Env_Rebindings);

   type Env_Rebindings_Type is record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for env rebindings records: this
      --  allows us in language bindings to directly peek in this record rather
      --  than rely on (slow) calls to getters.

      Version : Version_Number;
      --  Allocated Env_Rebindings_Type records can be used multiple times
      --  for a given analysis context. Each time we re-use one, we bump its
      --  version number, so that we can reject the use of stale references.

      --  End of ABI area

      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env;
      Children         : Env_Rebindings_Vectors.Vector;
   end record
      with Convention => C;
   --  Tree of remappings from one lexical environment (Old_Env) to another
   --  (New_Env). Note that both referenced environments must be primary and
   --  env rebindings are supposed to be destroyed when one of their
   --  dependencies (Parent, Old_Env or New_Env) is destroyed, so there is no
   --  need for ref-counting primitives.

   function Hash is new Hashes.Hash_Access
     (Env_Rebindings_Type, Env_Rebindings);

   package Env_Rebindings_Pools is new Ada.Containers.Hashed_Maps
     (Key_Type        => Lexical_Env,
      Element_Type    => Env_Rebindings,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Env_Rebindings_Pool is access all Env_Rebindings_Pools.Map;
   --  Pool of env rebindings to be stored in a lexical environment

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Env_Rebindings_Pools.Map, Env_Rebindings_Pool);

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

end Langkit_Support.Lexical_Envs;
