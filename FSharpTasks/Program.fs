// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
//Find last item in the list
let rec findlast = 
    function 
    | [] -> None
    | [ a ] -> Some(a)
    | x :: xs -> findlast xs

//Find last but one item in the list
let rec findLastButOne = 
    function 
    | [] -> None
    | [ a ] -> None
    | [ a; b ] -> Some(a)
    | x :: xs -> findLastButOne xs

//Find item at the specific position in the list
let rec findAt n arr = 
    match n, arr with
    | _, [] -> None
    | 0, _ -> None
    | 1, (x :: xs) -> Some(x)
    | n, x :: xs -> findAt (n - 1) xs

//Reverse the list
let rec reverse = 
    function 
    | [] -> []
    | x :: xs -> reverse xs @ [ x ]

//Is the list palindrome
let isPalindrome arr = 
    let rec equals arr1 arr2 = 
        match arr1, arr2 with
        | [], [] -> true
        | x :: xs, y :: ys -> 
            if x = y then equals xs ys
            else false
    
    let rev = reverse arr
    equals arr rev

type List<'a> = 
    | Single of int
    | NestedList of List<'a> list

//Flat a nested structure
let flatten (items : list<List<'a>>) = 
    let rec flat (items : list<List<'a>>, acc : list<int>) = 
        match items with
        | [] -> acc
        | Single(s) :: tail -> flat (tail, acc @ [ s ])
        | NestedList(n) :: tail -> flat (tail, flat (n, acc))
    flat (items, [])

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

main |> ignore
