// Learn more about F# at http://fsharp.org

open System

type Suit = Clubs | Diamonds | Hearts | Spades

[<StructuredFormatDisplay("{Face} of {Suit}")>]
type Card =
    {
        Suit: Suit;
        Face: int;
    }

let ToString (xs: list<Card>) = xs |> List.map (sprintf "%A") |> String.concat ", "

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs


[<EntryPoint>]
let main argv =
    let cards = List.sort [{ Suit= Spades; Face=2}; {Suit=Clubs; Face=4}; {Suit=Hearts; Face=11}; {Suit=Hearts; Face=13}; {Suit=Spades; Face=7}; {Suit=Clubs; Face=9}; {Suit=Hearts; Face=12}]
    printfn "%A" cards.Length
    let combinations = comb 5 cards
    let sorted = List.sort combinations
    let out = List.map(ToString) sorted
    List.iter (fun x -> printfn "%s" x) out
    printfn "Hello World from F#!"
    0 // return an integer exit code
