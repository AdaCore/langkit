with Adalog.Debug; use Adalog.Debug;

package body Adalog.Operations is

   ----------
   -- Call --
   ----------

   function Solve_Impl (Inst : in out Or_Rec) return Boolean is
   begin
      Trace ("In Or solve");

      case Inst.State is
         when 0 => null;
         when 1 => goto State_1;
         when others => goto State_2;
      end case;

      if Inst.Left.Solve then
         Trace ("In Or solve: First alternative is True, return True");
         return True;
      end if;

      <<State_1>>
      Inst.State := 1;
      if Inst.Right.Solve then
         Trace ("In Or solve: Second alternative is True, return True");
         return True;
      end if;

      <<State_2>>

      Trace ("In Or solve: All is false, return False");

      Inst.State := 2;
      return False;
   end Solve_Impl;

   -----------
   -- Reset --
   -----------

   procedure Reset (Inst : in out Or_Rec) is
   begin
      Inst.State := 0;
      Inst.Left.Reset;
      Inst.Right.Reset;
   end Reset;

   ----------
   -- Call --
   ----------

   function Solve_Impl (Inst : in out And_Rec) return Boolean is
   begin

      Trace ("In And solve");

      case Inst.State is
         when 0 => goto State_0;
         when 1 => goto State_1;
         when others => goto State_2;
      end case;

      <<State_0>>
      while Inst.Left.Solve loop
         Trace ("In And solve: Left is True, try Right");
         Inst.State := 1;
         if Inst.Right.Solve then
            Trace ("In And solve: Right is True, return True");
            return True;
         else
            Trace ("In And solve: Right is False, will try left again");
            Inst.Right.Reset;
            Inst.State := 0;
         end if;
      end loop;
      goto State_2;

      <<State_1>>
      Inst.State := 1;
      if Inst.Right.Solve then
         Trace ("In And solve: Right is True, return True");
         return True;
      else
         Inst.Right.Reset;
         Inst.State := 0;
      end if;
      goto State_0;

      <<State_2>>
      Inst.State := 2;
      Trace ("In And solve: All is false, return false");
      return False;
   end Solve_Impl;

   -----------
   -- Reset --
   -----------

   procedure Reset (Inst : in out And_Rec) is
   begin
      Inst.State := 0;
      Inst.Left.Reset;
      Inst.Right.Reset;
   end Reset;

   ----------
   -- Free --
   ----------

   procedure Cleanup (Inst : in out And_Rec) is
   begin
      Dec_Ref (Inst.Left);
      Dec_Ref (Inst.Right);
   end Cleanup;

   ----------
   -- Free --
   ----------

   procedure Cleanup (Inst : in out Or_Rec) is
   begin
      Dec_Ref (Inst.Left);
      Dec_Ref (Inst.Right);
   end Cleanup;

   --------------
   -- Logic_Or --
   --------------

   function Logic_Or
     (L, R : Relation) return access I_Relation'Class
   is
      Result : constant access I_Relation'Class :=
         new Or_Rec'(Left => L, Right => R, others => <>);
   begin
      Inc_Ref (L);
      Inc_Ref (R);
      return Result;
   end Logic_Or;

   ---------------
   -- Logic_And --
   ---------------

   function Logic_And
     (L, R : Relation) return access I_Relation'Class
   is
      Result : constant access I_Relation'Class :=
         new And_Rec'(Left => L, Right => R, others => <>);
   begin
      Inc_Ref (L);
      Inc_Ref (R);
      return Result;
   end Logic_And;

end Adalog.Operations;
