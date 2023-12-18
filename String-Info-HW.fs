//
// F# program to input a string and print out information
// about the # of vowels and digraphs in that string.
//
// Name:      Sania Khaja
// UIC NetID: saniak2
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode (S:string) = 
  List.ofArray (S.ToCharArray())

//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string. Example: implode ['t';'h';'e'] => "the"
//
let implode (L:char list) = 
  new string(List.toArray L)


// calculates length of a list
let rec length L = 
  match L with
  | [] -> 0  // if list is empty returns 0 length
  | head::tail -> 1 + length tail // adding 1 to tail of list. head is the current character and tail is the rest of the charcters in the list


// counts all vowels in list
// checking whole list using recursion
let rec numVowels L =
  // initializing list of vowels
  let vowels = ['a'; 'e'; 'i'; 'o'; 'u']
  match L with
  | [] -> 0  // empty list so 0 vowels
  | head::tail -> // head is char we are at in list and tail are the rest of the chars later in the list
      // checking if vowels list contains head where head is a char in the list
      if List.contains head vowels then
        1 + numVowels tail  // recursive call and incrementing vowel found by 1
      else
        numVowels tail  // recursive call but no vowel so did not add 1


// counts the vowel that was passed as a parameter
// checking whole list using recursion
let rec countVowel L vowel =
  match L with
  | [] -> 0  // empty list so 0 vowels
  | head::tail -> 
      // checking if head is the same as the vowel passed as a parameter
      if head = vowel then
        1 + countVowel tail vowel // recursive call and incrementing vowel found by 1
      else
        countVowel tail vowel  // recursive call but vowel was not head


// counts all digraphs in list
// checking whole list using recursion
let rec numDigraphs L =
  // initializing list of digraphs
  let digraphs = [['a'; 'i']; ['c'; 'h']; ['e'; 'a']; ['i'; 'e']; ['o'; 'u']; ['p'; 'h']; ['s'; 'h']; ['t'; 'h']; ['w'; 'h']]
  match L with
  | [] -> 0  // empty list so 0 digraphs
  | [_] -> 0  // empty list so 0 digraphs
  | head::next::tail -> // head is current char, next is the char right after that in the list (doing this since need two chars at once for digraph comparison)
      // list for head and next to allow for comparison with digraphs in list above
      let temp = [head; next]
      // checking if digraphs list contains head and next
      if List.contains temp digraphs then
        1 + numDigraphs (next::tail)  // recursive call and incrementing digraph found by 1
      else
        numDigraphs (next::tail)   // recursive call but no digraph so did not add 1


// counts the digraph that was passed as a parameter
// checking whole list using recursion
let rec countDigraph L digraph =
  match L with
  | [] -> 0  // empty list so 0 digraph
  | [_] -> 0  // empty list so 0 digraphs
  | head::next::tail -> 
      let temp = [head; next]
      // checking if current two character in temp is equal to the digraph passed in the parameter
      if temp = digraph then
        1 + countDigraph (next::tail) digraph // recursive call and incrementing digraph found by 1
      else
        countDigraph (next::tail) digraph  // recursive call for when current chars were not equal to the digraph parameter



[<EntryPoint>]
let main argv =
  printfn "Starting"
  printfn ""

  //
  // input string, output length and # of vowels:
  //
  printf("input> ")
  let input = System.Console.ReadLine()

  let L = explode input
  printfn "exploded: %A" L

  let len = length L
  printfn "length: %A" len

  let num = numVowels L
  printfn "vowels: %A" num

  //
  // prints count of each vowel by passing list and vowel:
  //
  let countVowelA = countVowel L 'a'
  printfn "'a': %A" countVowelA

  let countVowelE = countVowel L 'e'
  printfn "'e': %A" countVowelE

  let countVowelI = countVowel L 'i'
  printfn "'i': %A" countVowelI

  let countVowelO = countVowel L 'o'
  printfn "'o': %A" countVowelO

  let countVowelU = countVowel L 'u'
  printfn "'u': %A" countVowelU
  
  
  //
  // print number of digraphs, count of each by passing a digraph as a parameter:
  //
  let numDig = numDigraphs L
  printfn "digraphs: %A" numDig

  let countDigAI = countDigraph L ['a'; 'i']
  printfn "'a','i': %A" countDigAI

  let countDigCH = countDigraph L ['c'; 'h']
  printfn "'c','h': %A" countDigCH

  let countDigEA = countDigraph L ['e'; 'a']
  printfn "'e','a': %A" countDigEA
  
  let countDigIE = countDigraph L ['i'; 'e']
  printfn "'i','e': %A" countDigIE
  
  let countDigOU = countDigraph L ['o'; 'u']
  printfn "'o','u': %A" countDigOU

  let countDigPH = countDigraph L ['p'; 'h']
  printfn "'p','h': %A" countDigPH

  let countDigSH = countDigraph L ['s'; 'h']
  printfn "'s','h': %A" countDigSH
  
  let countDigTH = countDigraph L ['t'; 'h']
  printfn "'t','h': %A" countDigTH
  
  let countDigWH = countDigraph L ['w'; 'h']
  printfn "'w','h': %A" countDigWH

  //
  // done: implode list, print, and return
  //
  let S = implode L
  printfn "imploded: %A" S

  printfn ""
  printfn "Done"
  0  // return 0 => success, much like C++
