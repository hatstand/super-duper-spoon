// Learn more about F# at http://fsharp.org

open System

type Suit = Clubs | Diamonds | Hearts | Spades

type Face =
 | Two = 1
 | Three = 2
 | Four = 3
 | Five = 4
 | Six = 5
 | Seven = 6
 | Eight = 7
 | Nine = 8
 | Ten = 9
 | Jack = 10
 | Queen = 11
 | King = 12
 | Ace = 13

[<StructuredFormatDisplay("{Face} of {Suit}")>]
type Card =
    {
        Suit: Suit;
        Face: Face;
    }

type PokerHands =
    | StraightFlush
    | FourOfAKind
    | FullHouse
    | Flush
    | Straight
    | ThreeOfAKind
    | TwoPair
    | Pair
    | HighCard

let ToString (xs: list<Card>) = xs |> List.map (sprintf "%A") |> String.concat ", "

let SortedAceHigh (xs: list<Card>) = xs |> List.sortBy (fun x -> int(x.Face))
let SortedAceLow (xs: list<Card>) = xs |> List.sortBy (fun x -> int(x.Face) % 13)

let rec IsSequential (xs: list<Card>) =
    match xs with
    | [] -> true
    | [_] -> true
    | [x; y] -> int(x.Face) % 13 + 1 = int(y.Face)
    | head :: next :: tail -> IsSequential [head; next] && IsSequential (next :: tail)

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let GroupSuits (cards: list<Card>) = List.groupBy (fun x -> x.Suit) cards
let GroupCards (cards: list<Card>) = List.groupBy (fun x -> x.Face) cards

let FourOfAKindCards (cards: list<Card>) = GroupCards cards |> List.filter (fun (x, y) -> y.Length = 4) |> List.map (fun (x, y) -> x)
let IsFourOfAKind (cards: list<Card>) = (FourOfAKindCards cards).IsEmpty |> not

let ThreeOfAKindCards (cards: list<Card>) = GroupCards cards |> List.filter (fun (x, y) -> y.Length = 3) |> List.map (fun (x, y) -> x)
let IsThreeOfAKind (cards: list<Card>) = (ThreeOfAKindCards cards).IsEmpty |> not

let PairsCards (cards: list<Card>) = GroupCards cards |> List.filter (fun (x, y) -> y.Length = 2) |> List.map (fun (x, y) -> x)
let IsPair cards = (PairsCards cards).IsEmpty |> not

let TwoPairsCards (cards: list<Card>) = comb 2 (PairsCards cards)
let IsTwoPair (cards: list<Card>) = (TwoPairsCards cards).IsEmpty |> not

// let FullHouseCards (cards: list<Card>) = ThreeOfAKindCards cards @ PairsCards cards
let IsFullHouse cards = IsThreeOfAKind cards && IsPair cards

let IsStraight (cards: list<Card>) = (SortedAceHigh cards |> IsSequential) || (SortedAceLow cards |> IsSequential)
let IsFlush (cards: list<Card>) = (GroupSuits cards).Length = 1
let IsStraightFlush (cards: list<Card>) = IsStraight cards && IsFlush cards

let BestHand (cards: list<Card>) =
    if IsStraightFlush cards then StraightFlush
    elif IsFourOfAKind cards then FourOfAKind
    elif IsFullHouse cards then FullHouse
    elif IsFlush cards then Flush
    elif IsStraight cards then Straight
    elif IsThreeOfAKind cards then ThreeOfAKind
    elif IsTwoPair cards then TwoPair
    elif IsPair cards then Pair
    else HighCard

[<EntryPoint>]
let main argv =
    let cards = List.sort [
        { Suit=Spades; Face=Face.Ace}
        { Suit=Spades;  Face=Face.Two}
        { Suit=Spades; Face=Face.Three}
        { Suit=Spades; Face=Face.Five}
        { Suit=Spades; Face=Face.Four}
        { Suit=Diamonds;  Face=Face.King}
        { Suit=Hearts; Face=Face.Queen}
    ]
    printfn "Available cards:\n%s" (ToString cards)
    let combinations = comb 5 cards
    let sorted = List.sort combinations
    // printfn "Possible hands:"
    // List.iter (fun x -> printfn "%A: %s" (BestHand x) (ToString x)) sorted

    printfn "Best hand:"
    let hand, cards = sorted |> List.map (fun x -> (BestHand x, x)) |> List.minBy (fun (x, y) -> x)
    printfn "%A: %A" hand cards
    0 // return an integer exit code
