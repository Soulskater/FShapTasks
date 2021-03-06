﻿let rec findlast = 
    function 
    | [] -> None
    | [ a ] -> Some(a)
    | x :: xs -> findlast xs

//Find item at the specific position in the list
let rec findAt n arr = 
    match n, arr with
    | _, [] -> None
    | 0, _ -> None
    | 1, (x :: xs) -> Some(x)
    | n, x :: xs -> findAt (n - 1) xs

//Reverse the list
// TODO: What is the time complexity, can you make it more efficient?
let rec reverse = 
    function 
    | [] -> []
    | x :: xs -> reverse xs @ [ x ]

// Find last but one item in the list
// Since you already defined reverse below, can you use that and findlast to 
// simplify?
let rec findLastButOne (items : 'a list) = 
    match items |> reverse with
    | [] -> None
    | x :: xs -> Some(xs.Head)

// This is shorter :)
let isPalindrome arr = reverse arr = arr

// Technicially this is not a  list but a tree :)
// Also you're not using the 'a for anything.
type List<'a> = 
    | Single of int
    | NestedList of List<'a> list

// Here's an alternative
type Tree<'a> = 
    | Node of 'a
    | Branch of list<Tree<'a>>

// Good! Can be simplified by splitting into two different functions (see below)
let flatten (items : list<List<'a>>) = 
    let rec flat (items : list<List<'a>>, acc : list<int>) = 
        match items with
        | [] -> acc
        | Single(s) :: tail -> flat (tail, acc @ [ s ])
        | NestedList(n) :: tail -> flat (tail, flat (n, acc))
    flat (items, [])

// TODO
//let flat : Tree<'a> -> list<'a> = failwith ""
// TODO
//let flatList : list<Tree<'a>> -> list<'a> = failwith ""
let main = 
    let arr = [ 1; 2; 3; 4; 5; 4; 2; 2; 1 ]
    
    let arr1 = 
        [ Single(10)
          Single(20)
          NestedList([ Single(30)
                       Single(40) ]) ]
    printfn "Reversed list %A" (reverse arr)
    printfn "palindrome list %A" (isPalindrome arr)
    printfn "Flat list %A" (flatten (arr1))
    0

// TODO
let flat (tree : Tree<'a>) : list<'a> = failwith ""
// TODO
let flatList (ts : list<Tree<'a>>) : list<'a> = failwith ""

main |> ignore
