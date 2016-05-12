// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
///Find last item in the list
let rec findlast = 
    function 
    | [] -> None
    | [ a ] -> Some(a)
    | x :: xs -> findlast xs

///Find last but one item in the list
let rec findLastButOne = 
    function 
    | [] -> None
    | [ a ] -> None
    | [ a; b ] -> Some(a)
    | x :: xs -> findLastButOne xs

///Find item at the specific position in the list
let rec findAt n arr = 
    match n, arr with
    | _, [] -> None
    | 0, _ -> None
    | 1, (x :: xs) -> Some(x)
    | n, x :: xs -> findAt (n - 1) xs

///Reverse the list
let rec reverse = 
    function 
    | [] -> []
    | x :: xs -> reverse xs @ [ x ]

///Is the list palindrome
let isPalindrome arr = 
    let rec equals arr1 arr2 = 
        match arr1, arr2 with
        | [], [] -> true
        | x :: xs, y :: ys -> 
            if x = y then equals xs ys
            else false
    
    let rev = reverse arr
    equals arr rev

//type 'a NestedList = E | N of 'a NestedList
//
/////Flatten the list
//let flatten arr = 
//    N(
//    let rec equals arr1 arr2 = 
//        match arr1, arr2 with
//        | [], [] -> true
//        | x :: xs, y :: ys -> 
//            if x = y then equals xs ys
//            else false
//    
//    let rev = reverse arr
//    equals arr rev

let main = 
    let arr = [ 1; 2; 3; 4; 5; 4; 2; 2; 1 ]
    printfn "Reversed list %A" (reverse arr)
    printfn "palindrome list %A" (isPalindrome arr)
    0 // return an integer exit code

main |> ignore
