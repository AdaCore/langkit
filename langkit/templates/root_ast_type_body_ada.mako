## vim: filetype=makoada

with System;
with System.Storage_Elements; use System.Storage_Elements;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.PP_Utils; use Langkit_Support.PP_Utils;
with Langkit_Support.Text;     use Langkit_Support.Text;

package body AST is

   -----------
   -- Child --
   -----------

   function Child (Node  : ${root_node_type_name};
                   Index : Natural) return ${root_node_type_name}
   is
      Result : ${root_node_type_name};
      Exists : Boolean;
   begin
      Get_Child (Node, Index, Exists, Result);
      return (if Exists then Result else null);
   end Child;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : ${root_node_type_name};
      Visit : access function (Node : ${root_node_type_name})
              return Visit_Status)
     return Visit_Status
   is
      Status : Visit_Status := Into;

   begin
      if Node /= null then
         Status := Visit (Node);

         --  Skip processing the child nodes if the returned status is Over
         --  or Stop. In the former case the previous call to Visit has taken
         --  care of processing the needed childs, and in the latter case we
         --  must immediately stop processing the tree.

         if Status = Into then
            for I in 1 .. Child_Count (Node) loop
               declare
                  Cur_Child : constant ${root_node_type_name} := Child (Node, I - 1);

               begin
                  if Cur_Child /= null then
                     Status := Traverse (Cur_Child, Visit);
                     exit when Status /= Into;
                  end if;
               end;
            end loop;
         end if;
      end if;

      if Status = Stop then
         return Stop;

      --  At this stage the Over status has no sense and we just continue
      --  processing the tree.

      else
         return Into;
      end if;
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : ${root_node_type_name};
      Visit : access function (Node : ${root_node_type_name})
              return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : ${root_node_type_name};
      Snap : Boolean := False) return Source_Location_Range
   is
      Tokens : Token_Vectors.Vector renames Node.Token_Data.Tokens;
      Sloc_Start, Sloc_End : Source_Location;
      use Token_Vectors;
   begin
      if Snap then
         declare
            Tok_Start : constant Natural :=
              Natural'Max (Node.Token_Start - 1, 0);
            Tok_End : constant Natural :=
              Natural'Min (Node.Token_End + 1, Last_Index (Tokens));
         begin
            Sloc_Start := End_Sloc (Get (Tokens, Tok_Start).Sloc_Range);
            Sloc_End :=
              Start_Sloc (Get (Tokens, Tok_End).Sloc_Range);
         end;
      else
         Sloc_Start := Start_Sloc (Get (Tokens, Node.Token_Start).Sloc_Range);
         Sloc_End := End_Sloc (Get (Tokens, Node.Token_End).Sloc_Range);
      end if;
      return Make_Range (Sloc_Start, Sloc_End);
   end Sloc_Range;

   ------------
   -- Lookup --
   ------------

   function Lookup (Node : ${root_node_type_name};
                    Sloc : Source_Location;
                    Snap : Boolean := False) return ${root_node_type_name}
   is
      Position : Relative_Position;
      Result   : ${root_node_type_name};
   begin
      Lookup_Relative (Node, Sloc, Position, Result, Snap);
      return Result;
   end Lookup;

   -------------
   -- Compare --
   -------------

   function Compare (Node : ${root_node_type_name};
                     Sloc : Source_Location;
                     Snap : Boolean := False) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node, Snap), Sloc);
   end Compare;

   -------------------
   -- Get_Extension --
   -------------------

   function Get_Extension
     (Node : ${root_node_type_name};
      ID   : Extension_ID;
      Dtor : Extension_Destructor) return Extension_Access
   is
      use Extension_Vectors;
   begin
      for Slot of Node.Extensions loop
         if Slot.ID = ID then
            return Slot.Extension;
         end if;
      end loop;

      declare
         New_Ext : constant Extension_Access :=
           new Extension_Type'(Extension_Type (System.Null_Address));
      begin
         Append (Node.Extensions,
                 Extension_Slot'(ID        => ID,
                                 Extension => New_Ext,
                                 Dtor      => Dtor));
         return New_Ext;
      end;
   end Get_Extension;

   ---------------------
   -- Free_Extensions --
   ---------------------

   procedure Free_Extensions (Node : access ${root_node_value_type}) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Extension_Type, Extension_Access);
      use Extension_Vectors;
      Slot : Extension_Slot;
   begin
      --  Explicit iteration for perf
      for J in 0 .. Last_Index (Node.Extensions) loop
         Slot := Get (Node.Extensions, J);
         Slot.Dtor (${root_node_type_name} (Node), Slot.Extension.all);
         Free (Slot.Extension);
      end loop;
   end Free_Extensions;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative (Node       : ${root_node_type_name};
                              Sloc       : Source_Location;
                              Position   : out Relative_Position;
                              Node_Found : out ${root_node_type_name};
                              Snap       : Boolean := False) is
      Result : constant Relative_Position :=
        Compare (Node, Sloc, Snap);
   begin
      Position := Result;
      Node_Found := (if Result = Inside
                     then Node.Lookup_Children (Sloc, Snap)
                     else null);
   end Lookup_Relative;

   --------------
   -- Children --
   --------------

   function Children
     (Node : ${root_node_type_name})
     return ${root_node_type_name}_Arrays.Array_Type
   is
   begin
      return A : ${root_node_type_name}_Arrays.Array_Type (0 .. Child_Count (Node)) do
         for I in 0 .. Child_Count (Node) loop
            A (I) := Child (Node, I);
         end loop;
      end return;
   end Children;

   --------------------------
   -- Children_With_Trivia --
   --------------------------

   function Children_With_Trivia
     (Node : ${root_node_type_name}) return Children_Arrays.Array_Type
   is
      Ret_Vec : Children_Vectors.Vector;
      use Children_Vectors;

      procedure Append_Trivias (First, Last : Natural);
      --  Append all the trivias of tokens between indices First and Last to
      --  the returned vector.

      procedure Append_Trivias (First, Last : Natural) is
      begin
         for I in First .. Last loop
            for T of Get_Trivias (Node.Token_Data.all, I) loop
               Append (Ret_Vec, Child_Record'(Kind => Trivia, Trivia => T));
            end loop;
         end loop;
      end Append_Trivias;

      function Not_Null
        (N : ${root_node_type_name}) return Boolean is (N /= null);

      N_Children : constant ${root_node_type_name}_Arrays.Array_Type
        := ${root_node_type_name}_Arrays.Filter
          (Children (Node), Not_Null'Access);
   begin
      if N_Children'Length > 0
        and then Node.Token_Start /= N_Children (0).Token_Start
      then
         Append_Trivias (Node.Token_Start, N_Children (0).Token_Start - 1);
      end if;

      for I in N_Children'Range loop
         Append (Ret_Vec, Child_Record'(Child, N_Children (I)));
         Append_Trivias (N_Children (I).Token_End,
                         (if I = N_Children'Last
                          then Node.Token_End - 1
                          else N_Children (I + 1).Token_Start - 1));
      end loop;

      return A : constant Children_Arrays.Array_Type := To_Array (Ret_Vec) do
         --  Don't forget to free Ret_Vec, since its memory is not
         --  automatically managed.
         Destroy (Ret_Vec);
      end return;
   end Children_With_Trivia;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Node : ${root_node_type_name}; Level : Integer := 0) is
   begin
      Put_Line (Level, Kind_Name (Node));
      for C of Children_With_Trivia (Node) loop
         case C.Kind is
            when Trivia =>
               Put_Line (Level + 1, Image (C.Trivia.Text.all));
            when Child =>
               PP_Trivia (C.Node, Level + 1);
         end case;
      end loop;
   end PP_Trivia;

   use AST_Envs;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Node : ${root_node_type_name}) is

      Stack : Env_Stack;

      --  The internal algorithm, as well as the Do_Env_Action implementations,
      --  use a stack of environments. They represent nested environments,  the
      --  top environment on the stack being the most nested. We do that
      --  because:
      --
      --  - We want to be able to replace the topmost env that will be seen by
      --    subsequent nodes. This is to support constructs such as use clauses
      --    in ada where you can do stuff like this::
      --
      --        declare  -- new LexicalEnv Lex_1 introduced
      --           A : Integer;
      --           B : Integer := A; -- Will get A from Lex_1
      --           use Foo;
      --           -- We create a new env, Lex_2, from Lex_1, where you can now
      --           -- reference stuff from foo, and for every subsequent
      --           -- declaration, Lex_2 will be the lexical environment !
      --
      --           C : Integer := D
      --           -- F was gotten from Foo which is reachable from Lex_2
      --        begin
      --           ...
      --        end;
      --
      --    In this example, the topmost env on the stack will be *replaced*
      --    when we evaluate env actions for the use clause, but the env that
      --    was previously on the top of the stack *won't change*, so stuff
      --    from Foo will still not be reachable to declarations/statements
      --    before the use clause. This allows to decouple env construction and
      --    symbol resolution in two passes, rather than interleave the two
      --    like in the GNAT compiler.
      --
      --  - We don't to mutate the topmost env, because that would change what
      --    other nodes 'see'. So we create a new env and substitute it on the
      --    stack.

      procedure Populate_Internal (Node : ${root_node_type_name});

      -----------------------
      -- Populate_Internal --
      -----------------------

      procedure Populate_Internal (Node : ${root_node_type_name})
      is
         use Lexical_Env_Vectors;

         Env_Idx : Natural := 0;
         Env     : Lexical_Env;
      begin
         if Node = null then
            return;
         end if;

         --  Initial implementation: we always put a null on the stack, because
         --  not every node type will create a new nested scope. Those who do
         --  will replace the null by a Lexical Environment instance. This way we
         --  always pop when exiting Populate_Internal.

         --  TODO??? Alternate implementation possible: Node.Do_Env_Actions
         --  returns a boolean telling whether it pushed a new env on the stack or
         --  not.
         --  Pros: shallower stack, no nulls on it so easier to find last env.
         --  Cons: Conditional Pop.
         --  Conclusion: Probably worth it.
         Append (Stack, null);

         Find_Env : for I in reverse First_Index (Stack) .. Last_Index (Stack) loop
            if Get (Stack, I) /= null then
               Env_Idx := I;
               Env := Get (Stack, I);
               exit Find_Env;
            end if;
         end loop Find_Env;

         --  Set the lexical env of node to the found environment
         Node.Parent_Env := Env;

         --  Execute environment actions for Node. That might create a new env
         --  in the created stack slot, or mutate the Stack of envs.

         --  TODO??? To be pondered, but we probably never want to mutate
         --  another env on the stack than the topmost environment, in which
         --  case we could just use an in-out parameter set to the topmost env
         --  to get a new env to put on top of the stack if necessary.
         Node.Do_Env_Actions (Stack, Env_Idx);

         for Child of Children (Node) loop
            Populate_Internal (Child);
         end loop;

         --  We always pop the value on the stack, disregarding whether a new
         --  lex env was created or not.
         Pop (Stack);
      end Populate_Internal;
   begin
      Populate_Internal (Node);
   end Populate_Lexical_Env;

   -----------------
   -- Short_Image --
   -----------------

   function Short_Image (Node : ${root_node_type_name}) return String is
   begin
      return "<" & Kind_Name (Node) & " " & Image (Sloc_Range (Node)) & ">";
   end Short_Image;

   ------------------------
   -- Address_To_Id_Maps --
   ------------------------

   --  Those maps are used to give unique ids to lexical envs while pretty
   --  printing them.

   function Hash (S : Lexical_Env) return Hash_Type is
     (Hash_Type (To_Integer (S.all'Address)));

   package Address_To_Id_Maps is new Ada.Containers.Hashed_Maps
     (Lexical_Env, Positive, Hash, "=");

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Node : ${root_node_type_name}) is
      use Address_To_Id_Maps;

      Env_Ids        : Address_To_Id_Maps.Map;
      Current_Env_Id : Positive := 1;

      ----------------
      -- Get_Env_Id --
      ----------------

      function Get_Env_Id (E : Lexical_Env) return Integer is
         C        : Address_To_Id_Maps.Cursor;
         Inserted : Boolean;
      begin
         Env_Ids.Insert (E, Current_Env_Id, C, Inserted);
         if Inserted then
            Current_Env_Id := Current_Env_Id + 1;
         end if;
         return Address_To_Id_Maps.Element (C);
      end Get_Env_Id;
      --  Retrieve the Id for a lexical env. Assign one if none was yet
      --  assigned.

      Env : Lexical_Env := null;

      ----------
      -- Dump --
      ----------

      procedure Dump (Self : Lexical_Env) is
         use Internal_Envs;

         function Image (El : Env_Element) return String is
           (Short_Image (El.El));

         function Image is new Env_Element_Vectors.Image (Image);
      begin
         Put ("<LexEnv Id" & Get_Env_Id (Self)'Img & " Parent"
              & (if Self.Parent /= null then Get_Env_Id (Self.Parent)'Img
                 else " null") & " (");

         for El in Self.Env.Iterate loop
            Put (Langkit_Support.Text.Image (Key (El).all) & ": "
                 & Image (Element (El)));
         end loop;
         Put (")>");
      end Dump;
      --  This procedure dumps *one* lexical environment

      --------------
      -- Internal --
      --------------

      procedure Internal (Current : ${root_node_type_name}) is
      begin
         if Current = null then
            return;
         end if;

         if Current.Parent_Env /= Env then
            Env := Current.Parent_Env;
            Put ("<" & Kind_Name (Current) & " " & Image (Sloc_Range (Current)) & "> - ");
            Dump (Env);
            Put_Line ("");
         end if;

         for Child of Children (Current) loop
            Internal (Child);
         end loop;
      end Internal;
      --  This procedure implements the main recursive logic of dumping the
      --  environments.
   begin
      Internal (Node);
   end Dump_Lexical_Env;

end AST;
