with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with Liblktlang.Common; use Liblktlang.Common;

package body Liblktlang.Default_Provider is

   package Dirs renames Ada.Directories;
   package Env renames Ada.Environment_Variables;
   package US renames Ada.Strings.Unbounded;
   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => US.Unbounded_String,
      "="          => US."=");

   type Default_Unit_Provider is new Internal_Unit_Provider with record
      Ref_Count   : Natural;
      Directories : String_Vectors.Vector;
   end record;
   type Default_Unit_Provider_Access is access all Default_Unit_Provider;

   overriding procedure Inc_Ref (Self : in out Default_Unit_Provider);
   overriding function Dec_Ref
     (Self : in out Default_Unit_Provider) return Boolean;
   overriding function Get_Unit_Filename
     (Self : Default_Unit_Provider;
      Name : Text_Type;
      Kind : Analysis_Unit_Kind) return String;
   overriding function Get_Unit
     (Self    : Default_Unit_Provider;
      Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Internal_Unit;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Self : in out Default_Unit_Provider) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref
     (Self : in out Default_Unit_Provider) return Boolean is
   begin
      Self.Ref_Count := Self.Ref_Count - 1;
      return Self.Ref_Count = 0;
   end Dec_Ref;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Self : Default_Unit_Provider;
      Name : Text_Type;
      Kind : Analysis_Unit_Kind) return String
   is
      Base_Filename : constant String := Unit_Base_Filename (Name);
   begin
      --  There are only unit bodies in Lkt
      if Kind /= Unit_Body or else Base_Filename = "" then
         return "";
      end if;

      --  Return the first matching file in the list of directories
      for Dir of Self.Directories loop
         declare
            Full : constant String := Dirs.Compose
              (US.To_String (Dir), Base_Filename);
         begin
            if Dirs.Exists (Full) then
               return Full;
            end if;
         end;
      end loop;

      return "";
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Self    : Default_Unit_Provider;
      Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Internal_Unit
   is
      Filename : constant String := Self.Get_Unit_Filename (Name, Kind);
   begin
      if Filename = "" then
         return Get_With_Error
           (Context,
            Unit_Base_Filename (Name),
            "Cannot open source file",
            Charset,
            Default_Grammar_Rule);
      end if;

      return Get_From_File
        (Context, Filename, Charset, Reparse, Default_Grammar_Rule);
   end Get_Unit;

   ------------------------
   -- Unit_Base_Filename --
   ------------------------

   function Unit_Base_Filename (Name : Text_Type) return String is
      Radix : String (Name'Range);
   begin
      --  Accept only names made of alphanumerics and underscores
      for I in Name'Range loop
         if Name (I) not in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' then
            return "";
         end if;
         Radix (I) := Character'Val (Character_Type'Pos (Name (I)));
      end loop;
      return Radix & ".lkt";
   end Unit_Base_Filename;

   ------------
   -- Create --
   ------------

   function Create return Internal_Unit_Provider_Access is
      Result : constant Default_Unit_Provider_Access :=
         new Default_Unit_Provider'(Internal_Unit_Provider with
                                    Ref_Count   => 1,
                                    Directories => <>);
      Path   : constant String := Env.Value (Path_Var_Name, "");

      procedure Append (S : String);
      --  If S is a non-empty string, append it as a lookup directory

      ------------
      -- Append --
      ------------

      procedure Append (S : String) is
      begin
         if S /= "" then
            Result.Directories.Append (US.To_Unbounded_String (S));
         end if;
      end Append;

   begin
      --  Sources in the current directory always come first
      Append (Dirs.Current_Directory);

      if Path /= "" then
         --  Import all directory names from the environment variable as lookup
         --  directories. Directory names are separated by the OS-dependent
         --  path separator.

         declare
            First : Positive := Path'First;
         begin
            for I in Path'Range loop
               if Path (I) = GNAT.OS_Lib.Path_Separator then
                  Append (Path (First .. I - 1));
                  First := I + 1;
               end if;
            end loop;
            Append (Path (First .. Path'Last));
         end;
      end if;

      return Internal_Unit_Provider_Access (Result);
   end Create;

end Liblktlang.Default_Provider;
