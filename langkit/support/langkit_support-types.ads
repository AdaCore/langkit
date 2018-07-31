--  Provide miscellaneous types to Langkit-generated libraries

with Interfaces;

package Langkit_Support.Types is

   type Version_Number is new Interfaces.Unsigned_64;
   --  Number associated to a resource. This number is supposed to be unique
   --  for some class of resource. For instance unique in all analysis contexts
   --  a process creates.

end Langkit_Support.Types;
