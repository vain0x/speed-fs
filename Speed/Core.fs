namespace Speed.Core

open System
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

  let suit (Card (s, _)) = s
  let rank (Card (_, r)) = r

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

module Game =
  let init agent ent1 ent2 =
    let (deck1, deck2) =
      Card.all
      |> List.partition (fun (Card (suit, _)) ->
          (suit |> Suit.color) = Black
          )
    let initPlayer deck (ent: Entrant) =
      let rng = Random()
      let deck = deck |> Seq.shuffle rng
      let plId = Player.newId ()
      in
        {
          PlayerId  = plId
          Name      = ent.Name
          Brain     = ent.Brain.Create(plId, agent)
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

  let players (g: GameT<_>) =
    g.PlayerStore |> Map.toList |> List.map fst

  let player plId (g: GameT<_>) =
    g.PlayerStore |> Map.find plId

  let canPutTo dest card (g: GameT<_>) =
    match g.Board |> Map.tryFind dest with
    | None -> true
    | Some boardCard ->
        card |> Card.isNextTo boardCard

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

  let hasNoCards plId (g: Game) =
    let pl = g |> player plId
    in
      pl.Hand |> List.isEmpty
      && pl.Deck |> List.isEmpty
      
  type GameUpdate =
    | Update          of list<GameUpdate>
    | UpdatePlayer    of Player
    | UpdateDraw      of PlayerId
    | UpdatePutCard   of PlayerId * Card * dest: PlayerId
    | UpdateReset
  with
    static member Unit = Update []

    static member Combine(l, r) =
      match (l, r) with
      | (Update l, Update r) -> Update (List.append l r)
      | (Update l, r) -> Update (l @ [r])
      | (l, Update r) -> Update (l :: r)
      | (l, r) -> Update [l; r]

    static member Apply(g, u) =
      let rec f g =
        function
        | Update us ->
            us |> List.fold f g

        | UpdatePlayer pl ->
            let store' = g.PlayerStore |> Map.add (pl.PlayerId) pl
            in { g with PlayerStore = store' }

        | UpdateDraw plId ->
            let pl = g |> player plId
            in
              match pl.Deck |> List.tryUncons with
              | None -> g
              | Some (card, deck) ->
                  let pl' =
                    { pl with
                        Deck = deck
                        Hand = card :: pl.Hand
                      }
                  in
                    f g (UpdatePlayer pl')

        | UpdatePutCard (plId, handCard, dest) ->
            let pl = g |> player plId
            let (handCard', hand') =
              pl.Hand
              |> List.partitionOne ((=) handCard)
            in
              if handCard' = Some handCard
                && g |> canPutTo dest handCard
              then
                let g =
                  f g (UpdatePlayer { pl with Hand = hand' })
                let g =
                  { g with Board = g.Board |> Map.add plId handCard }
                in
                  f g (UpdateDraw plId)
              else
                g

        | UpdateReset ->
            if g |> stuck
            then { g with Board = Map.empty }
            else g
      in
        f g u

  let up = UpdateMonad.up
  let get = UM (fun g -> (Update [], g))

  let getPlayers =
    update {
      let! g = get
      return g |> players
    }

  let getPlayer plId =
    update {
      let! g = get
      return g |> player plId
    }

  let tryDraw plId =
    update {
      let! pl = getPlayer plId
      match pl.Deck |> List.tryUncons with
      | None ->
          return None
      | Some (card, deck) ->
          do! up (UpdatePlayer { pl with Deck = deck })
          return Some card
    }

  let putFirstCard plId =
    update {
      let! cardOpt = tryDraw plId
      match cardOpt with
      | None -> assert false  // assert: ゲームの開始直後なのでデッキは満タン
      | Some card ->
          return! up (UpdatePutCard (plId, card, plId))
    }
