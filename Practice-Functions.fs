
//
// Various functions for practicing F#; homework #05.
//
// <<Sania Khaja>>
// U. of Illinois, Chicago
// CS 341, Spring 2023
//

namespace homework

module hw05 =
   //
   // NOTE: all types, functions in the module must be indented.
   //

   //
   // subset S L
   //
   // Returns true if S is a subset of L, false if not.
   // 
   // Examples: subset [3; 1; 2] [1; 4; 2; 0; 3] => yes
   //           subset [2; 1; 3] [1; 4; 0; 2; 8] => no
   //           subset [] []                     => yes
   //           subset [] [1; 2; 3]              => yes
   //           subset [1..1000] [1..1000]       => yes
   // 
   // NOTE: you can solve using tail-recursion, or using a
   // higher-order approach. Whatever you prefer. Beware
   // that List.length is an O(N) operation, since it 
   // traverses the entire list and counts the elements.
   // 
   // HINT: there are a variety of solutions. One idea
   // is write a helper function "contains e L" that 
   // returns true if e is an element of L, and false 
   // if not. Then build on that to define subset. This
   // leads to an O(N^2) solution, which is fine.
   // 
   let rec subset S L =
      // filter will retain the elements where the function returned true and goes through each element of S. Uses contains to return that true or false
      let check = List.filter (fun e -> List.contains e L) S
      // checks length of the elements checked and true and makes sure it matches the length of the subset S in order to know if all the elemnets of S were found in L
      List.length check = List.length S


   //
   // delete_tr e L
   //
   // Deletes all occurrences of e from the list L, 
   // returning the new list. This version is written
   // recursively, using a helper function that is 
   // tail-recursive.
   //
   // Examples: delete_tr 3  [3; 1; 2]   => [1; 2]
   //           delete_tr 99 [99; 0; 99] => [0]
   //           delete_tr -2 []          => []
   //           delete_tr "cat" ["dog"]  => ["dog"]
   //           delete_tr 99999 [1..99999] => [1..99998]
   // 
   // NOTE: write a tail-recursive version using a helper
   // function; do not change the API of the original 
   // delete function. You'll also need to build the new
   // list efficiently; you can use List.rev if need be.
   //
   let rec _delete_tr e L newList =
    // checking L's elements
     match L with
     | [] -> List.rev newList  // base case: empty list so reverse newList and return that
     | element::rest -> if element <> e then  // need to add element to newList since not equal
                          _delete_tr e rest (element::newList)
                        else  // no need to change list and continues tail recursion with the rest of the list
                          _delete_tr e rest newList

   
   let delete_tr e L =
      // calling helper function
      _delete_tr e L []


   //
   // delete_ho e L
   //
   // Deletes all occurrences of e from the list L, 
   // returning the new list. This version uses a
   // higher-order approach.
   //
   // Examples: delete_ho 3  [3; 1; 2]   => [1; 2]
   //           delete_ho 99 [99; 0; 99] => [0]
   //           delete_ho -2 []          => []
   //           delete_ho "cat" ["dog"]  => ["dog"]
   //           delete_ho 99999 [1..99999] => [1..99998]
   //
   let delete_ho e L =
    // filter will retain the elements where the function returned true and goes through each element of S. <> used to check for not equal to. So adding any elements not equal to e into a new list
     let newList = List.filter (fun s -> s <> e) L
     // returns the new list
     newList


   //
   // examAverages LT
   //
   // Given a list of tuples of the form ("netid", [score;score;score;...]),
   // computes each netid's average score as a real number and returns a 
   // list of tuples of the form ("netid", average).
   //
   // Example:
   //   examAverages [("sdeitz2",[100;90;91]); ("psankar",[100;100;100;100;98])]
   //     => [("sdeitz2",93.66666667); ("psankar",99.6)]
   //
   // NOTE: F# offers a function List.average L that computes the average of
   // a list of real numbers.  However, the list of scores in the tuple are
   // integers, so in order to use List.average, you would first need to convert
   // the integers to reals --- List.map float L would work nicely here.
   //
   // You can solve using recursion, or higher-order, or both; tail recursion
   // is not necessary.
   //
   let rec examAverages LT =
       // using high order approach
       // Using List.map float to convert the scores into reals and then computing the average
       // function allows the tuple to be split into netid and score that is used to create the new tuple
       // using List.map to create a new list with all these netids with there averages which are tuples
       // returns the list of tuples
       let examAvgs = List.map(fun (netid, scores) -> (netid, List.average (List.map float scores))) LT
       examAvgs


   //
   // pairwise L1 L2
   //
   // Given 2 lists L1 and L2, both the same length, merges the corresponding 
   // elements into pairs, returning a list of pairs.
   //
   // Example:
   //   pairwise [1;3;5;7] [10;20;30;40]
   //     => [(1,10); (3,20); (5,30); (7,40)] 
   //
   // You must solve using recursion; tail recursion is not necessary.
   // Note: there is a F# library function named zip that does this operation. 
   // You may not use that function in your solution. 
   //
   let rec pairwise L1 L2 =
      // going through L1 and L2 elements at once
      match L1, L2 with
      | [], [] -> []  // empty lists so base case
      // getting an element from each list and then putting it in a tuple and calling recursion to get the rest of the elements into pair tuples
      | element1::rest1, element2::rest2 -> (element1, element2)::pairwise rest1 rest2
