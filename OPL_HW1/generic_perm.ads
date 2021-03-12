--Solution from: https://rosettacode.org/wiki/Permutations#Ada

generic                                    --This part is declaring the package
   N: positive;                                --Set N to positive
package Generic_Perm is                            --Defining Generic_Perm as the package
   subtype Element is Positive range 1 .. N;                --Element is the user input
   type Permutation is array(Element) of Element;            --Add elements to array
 
   procedure Set_To_First(P: out Permutation; Is_Last: out Boolean);    --Defining a procedure called set_to_first (set the permutation to the first element)
   procedure Go_To_Next(P: in out Permutation; Is_Last: out Boolean);     --Defining a procedure go_to_next (one done with the first element, move onto the next)
end Generic_Perm;                            --Ending the package