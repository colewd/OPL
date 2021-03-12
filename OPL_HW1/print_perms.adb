--Solution from: https://rosettacode.org/wiki/Permutations#Ada

with Ada.Text_IO, Ada.Command_Line, Generic_Perm;                --First will use the text IO package to front std.out
                                         --will take a command
procedure Print_Perms is                            --Link to Generic_perm (header)
   package CML renames Ada.Command_Line;                    --This is the main/driver
   package TIO renames Ada.Text_IO;                        --rename these for convenience
begin
   declare
      package Perms is new Generic_Perm(Positive'Value(CML.Argument(1)));    --begin by calling Generic_perm using the argument given by the user
      P : Perms.Permutation;                            --declare P
      Done : Boolean := False;                            --New variable set to false
 
      procedure Print(P: Perms.Permutation) is                    --Declare print function 
      begin                                    --Will print all permutations
         for I in P'Range loop                            --Go through all of P
            TIO.Put (Perms.Element'Image (P (I)));                --Output each number of each permutation
         end loop;                                --Done with loop
         TIO.New_Line;                                --Make a new line for the next permutation
      end Print;                                     --Done with print
   begin                                    --Ready for the next loop
      Perms.Set_To_First(P, Done);                        --Set set_to_first to done, so we can begin printing next set of permutations
      loop                                    --This will print the permutations
         Print(P);                                --Call print on new starting number
         exit when Done;                            --Exit call
         Perms.Go_To_Next(P, Done);                        --Start with next starting numer call Perms
      end loop;                                    --Will stay in this loop until all the permutations are done printing
   end;                                        --end outer loop
exception                                    --Declare an exception
   when Constraint_Error                             --Exception if the user does not enter and argument 
     => TIO.Put_Line ("*** Error: enter one numerical argument n with n >= 1"); --Print this Error
end Print_Perms;                                --End function

