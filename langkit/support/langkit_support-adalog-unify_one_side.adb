with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Unify_One_Side is

   use Var;

   function Create
     (Left   : Var.Var;
      Right  : R_Type;
      R_Data : R_Convert_Data) return Unify_Rec;
   --  Helper for the public Create function

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out Unify_Rec) return Boolean is
   begin
      Trace ("In Unify");
      if Is_Defined (Self.Left) then

         Trace ("Left defined");

         return C : Boolean do
            declare
               R_Val : constant L_Type := Convert (Self.R_Data, Self.Right);
               L_Val : constant L_Type := Get_Value (Self.Left);
            begin
               Trace (L_Image (R_Val));
               Trace (L_Image (L_Val));

               if Invert_Equals then
                  C := Equals (R_Val, L_Val);
               else
                  C := Equals (L_Val, R_Val);
               end if;
               Trace ("Returning " & C'Image);
            end;
         end return;
      else

         if Set_Value (Self.Left, Convert (Self.R_Data, Self.Right)) then
            Trace ("Setting left worked !");

            Self.Changed := True;
            return True;
         else
            Trace ("Setting left failed !");
            return False;
         end if;
      end if;
   end Apply;

   ------------
   -- Revert --
   ------------

   procedure Revert (Self : in out Unify_Rec) is
   begin
      if Self.Changed then
         Reset (Self.Left);
         Self.Changed := False;
      end if;
   end Revert;

   ----------------
   -- Solve_Impl --
   ----------------

   function Solve_Impl (Self : in out Member_T) return Boolean is
   begin
      if Self.Current_Index in Self.Values.all'Range then
         if Is_Defined (Self.Left) and then not Self.Changed then

            if Self.Domain_Checked then
               return False;
            end if;

            Self.Domain_Checked := True;
            for V of Self.Values.all loop
               if Get_Value (Self.Left) = Convert (Self.R_Data, V) then
                  return True;
               end if;
            end loop;
            return False;
         else
            loop
               Self.Current_Index := Self.Current_Index + 1;
               if Set_Value
                 (Self.Left, Convert
                    (Self.R_Data, Self.Values (Self.Current_Index - 1)))
               then
                  Self.Changed := True;
                  return True;
               end if;

               exit when Self.Current_Index not in Self.Values.all'Range;
            end loop;

            return False;
         end if;
      else
         if Self.Changed then
            Reset (Self.Left);
         end if;
         return False;
      end if;
   end Solve_Impl;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Member_T) is
   begin
      Self.Domain_Checked := False;
      if Self.Changed then
         Reset (Self.Left);
         Self.Changed := False;
      end if;
      Self.Current_Index := 1;
   end Reset;

   ------------
   -- Create --
   ------------

   function Create
     (Left : Var.Var; Right : R_Type; R_Data : R_Convert_Data) return Unify_Rec
   is
   begin
      return (Left    => Left,
              Right   => Right,
              Changed => False,
              R_Data  => R_Data);
   end Create;

   function Create
     (Left   : Var.Var;
      Right  : R_Type;
      R_Data : R_Convert_Data) return Relation
   is
   begin
      return new Unify'(Rel => Create (Left, Right, R_Data), others => <>);
   end Create;

   ------------
   -- Member --
   ------------

   function Member
     (R      : Var.Var;
      Vals   : R_Type_Array;
      R_Data : R_Convert_Data) return Relation
   is
   begin
      return new Member_T'
        (Left           => R,
         Values         => new R_Type_Array'(Vals),
         Current_Index  => 1,
         Changed        => False,
         Domain_Checked => False,
         R_Data         => R_Data,
         others         => <>);
   end Member;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Member_T) is
      procedure Unchecked_Free
      is new Ada.Unchecked_Deallocation (R_Type_Array, R_Type_Array_Access);
   begin
      Unchecked_Free (Self.Values);
   end Cleanup;

   ------------------
   -- Custom_Image --
   ------------------

   overriding function Custom_Image (Self : Member_T) return String is
      Res : Unbounded_String;
   begin
      Res := To_Unbounded_String ("Member ");

      Append (Res, Image (Self.Left));
      Append (Res, " {");

      for I in Self.Values.all'Range loop
         Append (Res, R_Image (Self.Values.all (I)));
         if I /= Self.Values.all'Last then
            Append (Res, ", ");
         end if;
      end loop;

      Append (Res, "}");

      return To_String (Res);
   end Custom_Image;

end Langkit_Support.Adalog.Unify_One_Side;
