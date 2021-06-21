with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Lexical_Envs_Impl;
with Langkit_Support.Symbols;
with Langkit_Support.Text;         use Langkit_Support.Text;
with Langkit_Support.Types;        use Langkit_Support.Types;

package Support is

   type Metadata is null record;
   Default_MD : constant Metadata := (null record);

   Property_Error: exception;

   function Get_Context_Version (Dummy_C : Character) return Version_Number
   is (0);

   function Node_Hash (Dummy_C : Character) return Hash_Type is (0);
   function Node_Unit (Dummy_C : Character) return Generic_Unit_Ptr
   is (No_Generic_Unit);
   function Metadata_Hash (Dummy_MD : Metadata) return Hash_Type is (0);
   function Combine (Dummy_L, Dummy_R : Metadata) return Metadata
   is ((null record));
   function Parent (Dummy_Node : Character) return Character is (' ');
   function Can_Reach (Dummy_Node, Dummy_From : Character) return Boolean
   is (True);
   function Is_Rebindable (Dummy_Node : Character) return Boolean is (True);

   function Node_Image
     (Node : Character; Dummy_Short : Boolean := True) return Text_Type
   is (To_Text ("'" & Node & "'"));

   function Acquire_Rebinding
     (Dummy            : Character;
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings
   is (raise Program_Error);
   procedure Register_Rebinding
     (Dummy_Node : Character; Dummy_Rebinding : Env_Rebindings) is null;

   function Get_Unit_Version (Dummy : Generic_Unit_Ptr) return Version_Number
   is (0);

   type Ref_Category is (No_Cat);
   type Ref_Categories is array (Ref_Category) of Boolean;

   type Inner_Env_Assoc is null record;
   function Get_Key
     (Self : Inner_Env_Assoc) return Langkit_Support.Symbols.Symbol_Type
   is (null);
   function Get_Node (Self : Inner_Env_Assoc) return Character is (' ');
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
      Node_Type             => Character,
      Node_Metadata         => Metadata,
      No_Node               => ' ',
      Empty_Metadata        => Default_MD,
      Node_Hash             => Node_Hash,
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

   procedure Put_Line (Elements : Envs.Entity_Array);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Env_Rebindings_Type, Env_Rebindings);

end Support;
