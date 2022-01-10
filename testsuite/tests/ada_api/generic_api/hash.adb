with Ada.Containers.Hashed_Maps;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;

with Libfoolang.Generic_API;

procedure Hash is
   Id : Language_Id renames Libfoolang.Generic_API.Foo_Lang_Id;

   package Context_Maps is new Ada.Containers.Hashed_Maps
     (Lk_Context, Integer, Hash, "=");
   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Lk_Unit, Integer, Hash, "=");
   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Lk_Node, Integer, Hash, "=");
   package Token_Maps is new Ada.Containers.Hashed_Maps
     (Lk_Token, Integer, Hash, "=");

   Ctx  : constant Lk_Context := Create_Context (Id);
   Ctx2 : constant Lk_Context := Create_Context (Id);
   U    : constant Lk_Unit := Ctx.Get_From_File ("example.txt");
   U2   : constant Lk_Unit := Ctx2.Get_From_File ("example2.txt");
   N    : constant Lk_Node := U.Root;
   N2   : constant Lk_Node := U2.Root;
   T    : constant Lk_Token := N.Token_Start;
   T2   : constant Lk_Token := N2.Token_Start;

   Context_Map : Context_Maps.Map;
   Unit_Map    : Unit_Maps.Map;
   Node_Map    : Node_Maps.Map;
   Token_Map   : Token_Maps.Map;
begin
   Context_Map.Insert (Ctx, 1);
   Context_Map.Insert (Ctx2, 2);
   if Context_Map.Element (Ctx) /= 1 then
      raise Program_Error;
   end if;
   if Context_Map.Element (U2.Context) /= 2 then
      raise Program_Error;
   end if;

   Unit_Map.Insert (U, 1);
   Unit_Map.Insert (U2, 2);
   if Unit_Map.Element (U) /= 1 then
      raise Program_Error;
   end if;
   if Unit_Map.Element (N2.Unit) /= 2 then
      raise Program_Error;
   end if;

   Node_Map.Insert (N, 1);
   Node_Map.Insert (N2, 2);
   if Node_Map.Element (N) /= 1 then
      raise Program_Error;
   end if;
   if Node_Map.Element (U2.Root) /= 2 then
      raise Program_Error;
   end if;

   Token_Map.Insert (T, 1);
   Token_Map.Insert (T2, 2);
   if Token_Map.Element (T) /= 1 then
      raise Program_Error;
   end if;
   if Token_Map.Element (N2.Token_Start) /= 2 then
      raise Program_Error;
   end if;

   Put_Line ("hash.adb: no error");
   New_Line;
end Hash;
