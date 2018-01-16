## vim: filetype=makoada

with Ada.Unchecked_Deallocation;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

package body ${ada_lib_name}.Rewriting is

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
     (Get_Rewriting_Handle (Context));

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
     (Handle.Context);

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle
   is
      Result : constant Rewriting_Handle := new Rewriting_Handle_Type'
        (Context => Context,
         Units   => <>);
   begin
      Set_Rewriting_Handle (Context, Result);
      return Result;
   end Start_Rewriting;

   -----------
   -- Apply --
   -----------

   procedure Apply (Handle : in out Rewriting_Handle) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Rewriting_Handle_Type, Rewriting_Handle);
      Ctx : constant Analysis_Context := Context (Handle);
   begin
      Free (Handle);
      Set_Rewriting_Handle (Ctx, Handle);
   end Apply;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle is
      use Unit_Maps;

      Context        : constant Analysis_Context := Analysis.Context (Unit);
      Context_Handle : constant Rewriting_Handle := Handle (Context);
      Filename       : constant Unbounded_String :=
         To_Unbounded_String (Get_Filename (Unit));
      Cur            : constant Cursor := Context_Handle.Units.Find (Filename);
   begin
      if Cur /= No_Element then
         return Element (Cur);
      end if;

      declare
         Result : constant Unit_Rewriting_Handle :=
            new Unit_Rewriting_Handle_Type'(Context_Handle => Context_Handle,
                                            Unit           => Unit);
      begin
         Context_Handle.Units.Insert (Filename, Result);
         return Result;
      end;
   end Handle;

end ${ada_lib_name}.Rewriting;
