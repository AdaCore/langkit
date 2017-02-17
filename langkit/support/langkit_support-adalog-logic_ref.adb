with Langkit_Support.Adalog.Debug;      use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Logic_Ref is

   pragma Warnings (Off, "always False");

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Var) is
   begin
      Self.Reset := True;
   end Reset;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (Self : Var) return Boolean is
   begin
      return not Self.Reset;
   end Is_Defined;

   ---------------
   -- Set_Value --
   ---------------

   function Set_Value (Self : in out Var; Data : Element_Type) return Boolean
   is
      Old   : aliased constant Var := Self;
   begin
      if Debug_State = Trace then
         Trace ("Setting the value of " & Image (Self) & " to "
                & Element_Image (Data));
         Trace ("Old value is " & Element_Image (Old.Value));
      end if;
      --  First set the value

      Self.Value := Data;
      Self.Reset := False;

      --  Then check if we have pending relations, and if they evaluate to
      --  True.

      for El of Pred_Sets.Elements (Self.Pending_Relations) loop
         if Debug_State = Trace then
            Trace ("Applying predicate on " & Image (Self));
         end if;

         if not El.Apply then
            Trace ("Applying predicate failed");
            Self := Old;

            if Debug_State = Trace then
               Trace ("Self element value is now "
                      & Element_Image (Self.Value));
            end if;

            return False;
         end if;
      end loop;

      return True;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Self : Var) return Element_Type is
   begin
      --  TODO??? We removed an assert about Self.Reset being False, because
      --  we want to be able to access the variable even if the element is
      --  unset, eg. null. However, we need to have a definite null value for
      --  elements, which could even replace the Reset flag altogether maybe.
      return Self.Value;
   end Get_Value;

   -----------
   -- Reset --
   -----------

   pragma Warnings (Off);
   procedure Reset (Self : in out Raw_Var) is
   begin
      Reset (Self.all);
   end Reset;
   pragma Warnings (On);

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (Self : Raw_Var) return Boolean is
   begin
      return Is_Defined (Self.all);
   end Is_Defined;

   ---------------
   -- Set_Value --
   ---------------

   pragma Warnings (Off);
   function Set_Value
     (Self : in out Raw_Var; Data : Element_Type) return Boolean
   is
   begin
      return Set_Value (Self.all, Data);
   end Set_Value;
   pragma Warnings (On);

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Self : Raw_Var) return Element_Type is
   begin
      return Get_Value (Self.all);
   end Get_Value;

   ------------
   -- Create --
   ------------

   function Create return Raw_Var is
   begin
      return new Var'(Reset => True, others => <>);
   end Create;

   -------------------
   -- Add_Predicate --
   -------------------

   procedure Add_Predicate (Self : in out Var; Pred : Var_Predicate) is
      use Pred_Sets;
      Dummy : Boolean := Add (Self.Pending_Relations, Pred);
   begin
      null;
   end Add_Predicate;

   ----------------------
   -- Remove_Predicate --
   ----------------------

   procedure Remove_Predicate (Self : in out Var; Pred : Var_Predicate) is
      use Pred_Sets;
      Dummy : Boolean := Remove (Self.Pending_Relations, Pred);
   begin
      Trace ("In remove predicate");
      null;
   end Remove_Predicate;

   ----------------------
   -- Remove_Predicate --
   ----------------------

   procedure Remove_Predicate (Self : Raw_Var; Pred : Var_Predicate) is
   begin
      Remove_Predicate (Self.all, Pred);
   end Remove_Predicate;

   -------------------
   -- Add_Predicate --
   -------------------

   procedure Add_Predicate (Self : Raw_Var; Pred : Var_Predicate) is
   begin
      Add_Predicate (Self.all, Pred);
   end Add_Predicate;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Var) is
   begin
      Pred_Sets.Destroy (Self.Pending_Relations);
   end Destroy;

end Langkit_Support.Adalog.Logic_Ref;
