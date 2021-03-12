# Python function to print permutations of a given list 
def permutation(lst): 
  
    # If the list is empty there arent any permutations
    if len(lst) == 0: 
        return [] 
  
    # If there is only one element then there's just one permutation
    if len(lst) == 1: 
        return [lst] 
  
    # The following is for any lists with more than 1 element
  
    l = [] # creates an empty list for the current permutation
  
    # iterates the input and calculates the permutation
    for i in range(len(lst)): 
       m = lst[i] 
  
       # takes lst[i] from the list and puts what is remaining in remLst
       remLst = lst[:i] + lst[i+1:] 
  
       # makes all permutations where m is the first element
       for p in permutation(remLst): 
           l.append([m] + p) 
    return l 
  
  
# Driver program to test above function 
# Prompts user for input and then creates a list able to hold that many elements
data = input("How many elements would you like in your list?")
my_list = [data] 

my_int = int(data) - 1

# a loop that appends each element to the list
for my_int in range(0, my_int):
    my_list.append(my_int + 1)

# list iterator
for my_int in range(1, my_int):
	my_int += 1

# outputs lists
for p in permutation(my_list): 
    print (p)

# found at https://www.geeksforgeeks.org/generate-all-the-permutation-of-a-list-in-python/