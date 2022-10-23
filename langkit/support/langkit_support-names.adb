--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Langkit_Support.Names is

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name
     (Name   : Text_Type;
      Casing : Casing_Convention := Camel_With_Underscores) return Boolean
   is
      subtype Alphanumerical is Character_Type with Static_Predicate =>
         Alphanumerical in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z';
      subtype Lower_Alphanumerical is Alphanumerical with Static_Predicate =>
         Lower_Alphanumerical in '0' .. '9' | 'a' .. 'z';
      subtype Upper_Alphanumerical is Alphanumerical with Static_Predicate =>
         Upper_Alphanumerical in '0' .. '9' | 'A' .. 'Z';

      Last_Was_Underscore : Boolean := False;
   begin
      --  Validate the first and last characters: first for invariants shared
      --  by all conventions, then for convention-specific invariants.

      if Name'Length = 0 then
         return False;
      end if;

      declare
         First : Character_Type renames Name (Name'First);
         Last : Character_Type renames Name (Name'Last);
      begin
         case Casing is
            when Camel_With_Underscores | Camel =>
               if First not in Upper_Alphanumerical
                  or else Last not in Alphanumerical
               then
                  return False;
               end if;

            when Lower =>
               if First not in Lower_Alphanumerical
                  or else Last not in Lower_Alphanumerical
               then
                  return False;
               end if;

            when Upper =>
               if First not in Upper_Alphanumerical
                  or else Last not in Upper_Alphanumerical
               then
                  return False;
               end if;
         end case;
      end;

      --  Validate all other characters

      for C of Name (Name'First + 1 .. Name'Last - 1) loop
         case C is
            when '_' =>
               --  Underscores are forbidden in Camel, and consecutive
               --  underscores are forbidden in all conventions.

               if Casing = Camel or else Last_Was_Underscore then
                  return False;
               end if;
               Last_Was_Underscore := True;

            when Alphanumerical =>

               --  Depending on the convention, characters following
               --  underscores either must be lower-case or upper-case.

               if Last_Was_Underscore then
                  case Casing is
                     when Camel_With_Underscores | Upper =>
                        if C not in Upper_Alphanumerical then
                           return False;
                        end if;

                     when Lower =>
                        if C not in Lower_Alphanumerical then
                           return False;
                        end if;

                     when Camel =>
                        raise Program_Error;
                  end case;

               else
                  case Casing is
                     when Lower =>
                        if C not in Lower_Alphanumerical then
                           return False;
                        end if;
                     when Upper =>
                        if C not in Upper_Alphanumerical then
                           return False;
                        end if;

                     when others =>
                        null;
                  end case;
               end if;
               Last_Was_Underscore := False;

            when others =>
               return False;
         end case;
      end loop;

      return True;
   end Is_Valid_Name;

   -----------------
   -- Create_Name --
   -----------------

   function Create_Name
     (Name   : Text_Type;
      Casing : Casing_Convention := Camel_With_Underscores) return Name_Type
   is
   begin
      if not Is_Valid_Name (Name, Casing) then
         raise Invalid_Name_Error;
      end if;

      --  Past this point, we know than Name contains alphanumericals and
      --  underscores only, so Image (Name) is just a conversion to ASCII (no
      --  escape sequence).

      declare
         N : String := Image (Name);
      begin
         --  Unless the casing is already Camel_With_Underscores, convert N to
         --  it.

         case Casing is
         when Camel_With_Underscores =>
            return To_Unbounded_String (N);

         when Camel =>
            return Result : Name_Type do

               --  Treat each upper-case letter as the start of a new word

               for C of N loop
                  case C is
                     when 'A' .. 'Z' =>
                        if Length (Result) > 0 then
                           Append (Result, '_');
                        end if;
                        Append (Result, C);

                     when '_' =>
                        null;

                     when others =>
                        Append (Result, C);
                  end case;
               end loop;
            end return;

         when Lower | Upper =>
            declare
               Start_Word : Boolean := True;
            begin
               --  Normalize casing: each start of a word (alphanumericals
               --  after underscores) must be upper-case, and the rest in lower
               --  case.

               for C of N loop
                  if Start_Word then
                     C := To_Upper (C);
                     Start_Word := False;

                  elsif C = '_' then
                     Start_Word := True;

                  else
                     C := To_Lower (C);
                  end if;
               end loop;
            end;
            return To_Unbounded_String (N);
         end case;
      end;
   end Create_Name;

   -----------------
   -- Format_Name --
   -----------------

   function Format_Name
     (Name : Name_Type; Casing : Casing_Convention) return Text_Type
   is
      N    : String := To_String (Name);
      Last : Natural := N'Last;
   begin
      if N'Length = 0 then
         raise Invalid_Name_Error;
      end if;

      case Casing is
      when Camel_With_Underscores =>
         null;

      when Camel =>
         --  Strip underscores and preserve all other characters

         Last := 0;
         for C of N loop
            if C /= '_' then
               Last := Last + 1;
               N (Last) := C;
            end if;
         end loop;

      when Lower =>
         for C of N loop
            C := To_Lower (C);
         end loop;

      when Upper =>
         for C of N loop
            C := To_Upper (C);
         end loop;

      end case;

      return To_Text (N (N'First .. Last));
   end Format_Name;

end Langkit_Support.Names;
