namespace Speed.Core

open Basis.Core
open Util
open Util.Collections

module Suit =
  let all =
    [
      Spade
      Clover
      Heart
      Diamond
    ]

  let color =
    function
    | Spade
    | Clover -> Black
    | Heart
    | Diamond -> Red

module Color =
  let all =
    [
      Black
      Red
    ]

module Rank =
  let all =
    [
      yield Ace
      for i in 2..10 do
        yield Number i
      yield Jack
      yield Queen
      yield King
    ]

  let toInt =
    function
    | Number i  -> i
    | Ace       -> 1
    | Jack      -> 11
    | Queen     -> 12
    | King      -> 13

module Card =
  let all =
    [
      for suit in Suit.all do
        for rank in Rank.all do
          yield Card (suit, rank)
    ]

  let isNextTo (Card (_, r1)) (Card (_, r2)) =
    let d =
      ((((r1 |> Rank.toInt) - ((r2 |> Rank.toInt))) + 13) % 13)
    in
      d = 1 || d = 12

module Player =
  let newId =
    makeCounter () >> PlayerId

  let state (self: Player): PlayerState =
    {
      PlayerId  = self.PlayerId
      Name      = self.Name
      Brain     = ()
      Hand      = self.Hand
      Deck      = self.Deck |> List.map (fun _ -> ())
    }

  let hand (pl: Player) =
    pl.Hand
    
module GameState =
  let players (g: GameState) =
    g.PlayerStore |> Map.toList |> List.map fst

module Game =
  let init agent ent1 ent2 =
    let (deck1, deck2) =
      Card.all
      |> List.partition (fun (Card (suit, _)) ->
          (suit |> Suit.color) = Black
          )
    let initPlayer deck (ent: Entrant) =
      let deck = deck |> Seq.shuffle
      let plId = Player.newId ()
      in
        {
          PlayerId  = plId
          Name      = ent.Name
          Brain     = ent.Brain plId agent
          Hand      = deck |> Seq.take 4 |> Seq.toList
          Deck      = deck |> Seq.skip 4 |> Seq.toList
        }
    let pl1 = ent1 |> initPlayer deck1
    let pl2 = ent2 |> initPlayer deck2
    in
      {
        PlayerStore =
          [
            (pl1.PlayerId, pl1)
            (pl2.PlayerId, pl2)
          ] |> Map.ofList
        Board =
          Map.empty
      }

  let state (g: Game): GameState =
    {
      PlayerStore = g.PlayerStore |> Map.map (fun _ -> Player.state)
      Board       = g.Board
    }

  let players (g: Game) =
    g.PlayerStore |> Map.toList |> List.map fst

  let player plId (g: Game) =
    g.PlayerStore |> Map.find plId

  let canPutTo dest card (g: Game) =
    match g.Board |> Map.tryFind dest with
    | None -> true
    | Some boardCard ->
        card |> Card.isNextTo boardCard

  let updatePlayer (pl: Player) (g: Game) =
    { g with PlayerStore = g.PlayerStore |> Map.add (pl.PlayerId) pl }

  let tryDraw plId (g: Game) =
    let pl = g |> player plId
    in
      pl.Deck
      |> List.tryUncons
      |> Option.map (fun (card, deck) ->
          let g = g |> updatePlayer { pl with Deck = deck }
          in (g, card)
          )

  let drawToHand plId (g: Game) =
    g
    |> tryDraw plId
    |> Option.map (fun (g, card) ->
        let pl = g |> player plId
        let pl = { pl with Hand = card :: pl.Hand }
        g |> updatePlayer pl
        )
    |> Option.getOr g

  let putCard plId card (g: Game) =
    { g with Board = g.Board |> Map.add plId card }

  let putFirstCard plId g =
    g
    |> tryDraw plId
    |> Option.get  // assert: ゲームの開始直後なのでデッキは満タン
    |> (fun (g, card) ->
        g |> putCard plId card
        )

  let tryPutCardFromHand plId handCard dest (g: Game) =
    let pl = g |> player plId
    let hand' =
      pl.Hand
      |> List.partitionOne handCard
      |> snd
    in
      if g |> canPutTo dest handCard then
        g
        |> updatePlayer { pl with Hand = hand' }
        |> putCard dest handCard
        |> drawToHand plId
        |> Some
      else
        None

  let puttableCards plId (g: Game) =
    let pl = g |> player plId
    in
      [
        for handCard in pl.Hand do
          for dest in g |> players do
            if g |> canPutTo dest handCard
            then yield (handCard, dest)
        ]

  let stuck (g: Game) =
    [
      for KeyValue (plId, _) in g.PlayerStore do
        yield g |> puttableCards plId |> List.isEmpty
      ]
    |> List.forall id

  let resetBoardIfNecessary (g: Game) =
    if g |> stuck then
      { g with Board = Map.empty } |> Some
    else
      None

  let hasNoCards plId (g: Game) =
    let pl = g |> player plId
    in
      pl.Hand |> List.isEmpty
      && pl.Deck |> List.isEmpty
