--Solution from: https://rosettacode.org/wiki/Permutations#Ada



--If the next element is bigger than the current element
        Is_Last := False;                            --Then set is last to false
        exit;                                --and exit
     end if;                                --no more if conditions
 
     -- next instruction will raise an exception if I = 1, so
     -- exit now (this is the last permutation)
     exit when I = 1;                            --Last Permutation
     I := I - 1;                                --Set I to I-1
      end loop;                                    --Done with this loop
 
      -- if all the elements of the permutation are in
      -- decreasing order, this is the last one
      if Is_Last then                                --IF the lements are in decreasing order already
     return;                                --We know this is the last permutation
      end if;                                    --no more if conditions
 
      -- sort the tail, i.e. reverse it, since it is in decreasing order
      J := I + 1;                                --Assign I+1 to J
      K := N;                                    --Assign N to K
      while J < K loop                                --This is reversing the list
     Swap (P (J), P (K));                            --Call swap on P[J] and P[K]
     J := J + 1;                                --J=J+1
     K := K - 1;                                --K-1
      end loop;                                    --Done sorting the tail
 
      -- find lowest element in the tail greater than the ith element
      J := N;                                    --Assign N to J
      while P (J) > P (I) loop                            --Searching for the next smallest number after the ith number
     J := J - 1;                                --J = J-1
      end loop;                                    --Done searching throught P
      J := J + 1;                                --J++
 
      -- exchange them
      -- this will give the next permutation in lexicographic order,
      -- since every element from ith to the last is minimum
      Swap (P (I), P (J));                        --Call swap on p[i] and p[j]
   end Go_To_Next;                            --Dont with go to next function
 
end Generic_Perm;                            --end of function
