with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Lexical_Envs_Impl;
with Langkit_Support.Symbols;
with Langkit_Support.Text;         use Langkit_Support.Text;
with Langkit_Support.Types;        use Langkit_Support.Types;

package Support is

   type String_Access is access all String;
   procedure Destroy is new Ada.Unchecked_Deallocation (String, String_Access);
   function "<" (Left, Right : String_Access) return Boolean is (False);

   type Metadata is record
      I : Integer;
   end record;

   Default_MD : constant Metadata := (I => 0);

   function Node_Hash (Dummy_S : String_Access) return Hash_Type is (0);
   function Node_Unit (Dummy_C : String_Access) return Generic_Unit_Ptr
   is (No_Generic_Unit);
   function Metadata_Hash (Dummy_MD : Metadata) return Hash_Type is (0);
   function Combine (L, R : Metadata) return Metadata is ((I => L.I + R.I));
   function Parent (S : String_Access) return String_Access is
     (new String'("Parent(" & S.all & ")"));
   function Can_Reach (Dummy_Node, Dummy_From : String_Access) return Boolean
   is (True);
   function Is_Rebindable (Dummy_Node : String_Access) return Boolean is (True);

   function Node_Image
     (Node : String_Access; Dummy_Short : Boolean := True) return Text_Type
   is (To_Text ("<" & Node.all & ">"));

   function Acquire_Rebinding
     (Dummy            : String_Access;
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings
   is (new Env_Rebindings_Type'(0, Parent, Old_Env, New_Env, Children => <>));
   procedure Register_Rebinding
     (Dummy_Node : String_Access; Dummy_Rebinding : Env_Rebindings) is null;

   function Get_Unit_Version (Dummy : Generic_Unit_Ptr) return Version_Number
   is (0);
   function Get_Context_Version (Dummy : Generic_Unit_Ptr) return Integer
   is (0);

   type Ref_Category is (No_Cat);
   type Ref_Categories is array (Ref_Category) of Boolean;

   type Inner_Env_Assoc is null record;
   function Get_Key
     (Self : Inner_Env_Assoc) return Langkit_Support.Symbols.Symbol_Type
   is (null);
   function Get_Node (Self : Inner_Env_Assoc) return String_Access is (null);
   function Get_Metadata (Self : Inner_Env_Assoc) return Metadata
   is (Default_MD);

   type Inner_Env_Assoc_Array is null record;
   function Length (Self : Inner_Env_Assoc_Array) return Natural is (0);
   function Get
     (Self : Inner_Env_Assoc_Array; Index : Positive) return Inner_ENv_Assoc
   is (raise Program_Error);
   procedure Dec_Ref (Self : in out Inner_Env_Assoc_Array) is null;

   package Envs is new Langkit_Support.Lexical_Envs_Impl
     (Get_Unit_Version      => Get_Unit_Version,
      Get_Context_Version   => Get_Context_Version,
      Node_Type             => String_Access,
      Node_Metadata         => Metadata,
      Empty_Metadata        => Default_MD,
      No_Node               => new String'(""),
      Node_Hash             => Node_Hash,
      Node_Unit             => Node_Unit,
      Metadata_Hash         => Metadata_Hash,
      Combine               => Combine,
      Can_Reach             => Can_Reach,
      Is_Rebindable         => Is_Rebindable,
      Node_Text_Image       => Node_Image,
      Register_Rebinding    => Register_Rebinding,
      Ref_Category          => Ref_Category,
      Ref_Categories        => Ref_Categories,
      Inner_Env_Assoc       => Inner_Env_Assoc,
      Inner_Env_Assoc_Array => Inner_Env_Assoc_Array);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Env_Rebindings_Type, Env_Rebindings);

end Support;
