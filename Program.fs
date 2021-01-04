type Suit = Clubs | Diamonds | Hearts | Spades

let suits = [Clubs; Diamonds; Hearts; Spades;]

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

let faces: Face list = [Face.Two; Face.Three; Face.Four; Face.Five; Face.Six; Face.Seven; Face.Eight; Face.Nine; Face.Ten; Face.Jack; Face.Queen; Face.King; Face.Ace;]

[<StructuredFormatDisplay("{Face} of {Suit}")>]
type Card =
    {
        Suit: Suit;
        Face: Face;
    }

let createDeck: Card list =
    List.allPairs faces suits |> List.map (fun (f, s) -> {Face=f; Suit=s;})

type TexasHand = Card * Card

type PlayerHand =
    | EmptyHand
    | TexasHand of Card * Card

type Player = {
    Hand: PlayerHand;
    Stack: int;
    Bet: int;
}

type InitialTable = {
    Players: Player list;
    Deck: Card list;
}

type DealTable = {
    Players: Player list;
    Deck: Card list;
}

type FlopTable = {
    Players: Player list;
    Deck: Card list;
    Flop: Card * Card * Card;
}

type TurnTable = {
    Players: Player list;
    Deck: Card list;
    Flop: Card * Card * Card;
    Turn: Card;
}

type RiverTable = {
    Players: Player list;
    Deck: Card list;
    Flop: Card * Card * Card;
    Turn: Card;
    River: Card;
}

type Table =
    | Initial of InitialTable
    | Deal of DealTable
    | Flop of FlopTable
    | Turn of TurnTable
    | River of RiverTable

let dealACard (deck: Card list): Card * Card list =
    match deck with
    | head :: tail ->
        (head, tail)
    | [] ->
        failwith "empty deck"

let deal (players: Player list) (deck: Card list): Player list * Card list =
    let (players, deck) = players |> List.scan (fun state player ->
        let (ps, deck) = state
        let (cards, newDeck) = deck |> List.splitAt 2
        let newP = { player with Hand=TexasHand (cards.Item 0, cards.Item 1) }
        (newP:: ps, newDeck)) ([], deck) |> List.last
    (players, deck)

let nextTable table: Table =
    match table with
    | Initial data ->
        let random = System.Random()
        let deck = data.Deck |> List.sortBy (fun _ -> random.Next())
        let (dealtPlayers, deck) = deal data.Players deck
        Deal {Players=dealtPlayers; Deck=deck}
    | Deal data ->
        let (flop, deck) = data.Deck |> List.splitAt 3
        Flop { Players=data.Players; Deck=deck; Flop=(flop.Item 0, flop.Item 1, flop.Item 2); }
    | Flop data ->
        let turn :: deck = data.Deck
        Turn { Players=data.Players; Deck=deck; Flop=data.Flop; Turn=turn; }
    | Turn data ->
        let river :: deck = data.Deck
        River { Players=data.Players; Deck=deck; Flop=data.Flop; Turn=data.Turn; River=river; }
    | _ ->
        table


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

    let printTable t =
        match t with
            | Deal d ->
                printfn "%A" d.Players
            | Initial(_) -> failwith "Not Implemented"
            | Flop f ->
                printfn "%A %A" f.Players f.Flop
            | Turn t ->
                printfn "%A %A %A" t.Players t.Flop t.Turn
            | River r ->
                printfn "%A %A %A %A %A" r.Players r.Flop r.Turn r.River r.Deck.Length

    let initial = Initial { Deck=createDeck; Players=[{Hand=EmptyHand; Stack=100; Bet=0;}; {Hand=EmptyHand; Stack=200; Bet=0;}]}
    let d = nextTable initial
    printTable d
    let f = nextTable d
    printTable f
    let t = nextTable f
    printTable t
    let r = nextTable t
    printTable r

    0 // return an integer exit code
