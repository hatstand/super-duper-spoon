// Learn more about F# at http://fsharp.org

open System

type Suit = Clubs | Diamonds | Hearts | Spades

[<StructuredFormatDisplay("{Face} of {Suit}")>]
type Card =
    {
        Suit: Suit;
        Face: int;
    }

type PokerHands =
      StraightFlush
    | FourOfAKind
    | FullHouse
    | Flush
    | Straight
    | ThreeOfAKind
    | TwoPair
    | Pair
    | HighCard

let ToString (xs: list<Card>) = xs |> List.map (sprintf "%A") |> String.concat ", "

let rec IsSequential (xs: list<Card>) =
    match xs with
    | [] -> true
    | [_] -> true
    | [x; y] -> x.Face + 1 = y.Face
    | head :: next :: tail -> IsSequential [head; next] && IsSequential (next :: tail)

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let GroupSuits (cards: list<Card>) = List.groupBy (fun x -> x.Suit) cards
let GroupCards (cards: list<Card>) = List.groupBy (fun x -> x.Face) cards

let Flushes (cards: list<Card>) = GroupSuits cards |> List.filter (fun (x, y) -> y.Length = 5) |> List.map (fun (x, y) -> x)
let FourOfAKind (cards: list<Card>) = GroupCards cards |> List.filter (fun (x, y) -> y.Length = 4) |> List.map (fun (x, y) -> x)
let ThreeOfAKind (cards: list<Card>) = GroupCards cards |> List.filter (fun (x, y) -> y.Length = 3) |> List.map (fun (x, y) -> x)
let Pairs (cards: list<Card>) = GroupCards cards |> List.filter (fun (x, y) -> y.Length = 2) |> List.map (fun (x, y) -> x)
let TwoPairs (cards: list<Card>) = comb 2 (Pairs cards)
let FullHouse (cards: list<Card>) = ThreeOfAKind cards @ Pairs cards

[<EntryPoint>]
let main argv =
    let cards = List.sort [{ Suit= Spades; Face=2}; {Suit=Clubs; Face=2}; {Suit=Hearts; Face=11}; {Suit=Hearts; Face=13}; {Suit=Spades; Face=7}; {Suit=Clubs; Face=9}; {Suit=Hearts; Face=12}]
    printfn "Available cards:\n%s" (ToString cards)
    let combinations = comb 5 cards
    let sorted = List.sort combinations
    let out = List.map(ToString) sorted
    printfn "Possible hands:"
    List.iter (fun x -> printfn "%s\nFlushes: %d\nFour of a Kind: %d\nPairs: %d" (ToString x) (Flushes x).Length (FourOfAKind x).Length (Pairs x).Length) sorted
    printfn "Sequential? %A" (IsSequential [{Suit=Hearts; Face=1}; {Suit=Hearts; Face=2}; {Suit=Clubs; Face=3}])
    0 // return an integer exit code
