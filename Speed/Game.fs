namespace Speed

open Speed.Core
open Basis.Core
open Util
open Util.Collections

#nowarn "40"

module Game =
  type EventResult =
    | End         of GameResult
    | Update      of Game
    | NoUpdate

  let doEvent (agent: Post) ev g =
    match ev with
    | EvGameBegin ->
        printfn "Game start!"

        g.PlayerStore
        |> Map.fold (fun g plId _ ->
            g |> Game.putFirstCard plId
            ) g
        |> Update

    | EvGameEnd r ->
        printfn "Game ended with %A." r
        r |> End

    | EvPut (plId, card, dest) ->
        printfn "Player %s puts card %A."
          ((g |> Game.player plId).Name)
          card

        match g |> Game.tryPutCardFromHand plId card dest with
        | Some g ->
            if (plId, g) ||> Game.hasNoCards
            then agent.Post(EvGameEnd (Win plId))
            g |> Update
        | None -> NoUpdate

    | EvReset ->
        printfn "Reset."

        match g |> Game.resetBoardIfNecessary with
        | Some g -> g |> Update
        | None -> NoUpdate

  let play ent1 ent2 =
    let result = ref None

    let initialGame agent =
      (ent1, ent2)
      ||> Game.init agent

    let rec agent =
      let agentBody (inbox: MailboxProcessor<Event>) =
        let notifyUpdate ev g =
          g
          |> Game.players
          |> Seq.map (fun plId -> async {
              (g |> Game.player plId).Brain.Post(ev, g |> Game.state)
              })
          |> Async.Parallel
          |> Async.Ignore

        let rec msgLoop (g: Game) =
          async {
            let! ev = inbox.Receive()
            match g |> doEvent agent ev with
            | End (Win plId) ->
                result := ((g |> Game.player plId).Name |> Some)
                return ()
            | Update g ->
                printfn "-------------------------------"
                printfn "Board: %A" (g.Board |> Map.toList |> List.map snd)
                for KeyValue (_, pl) in g.PlayerStore do
                  printfn "Player %s's hand = %A"
                    (pl.Name) (pl.Hand)

                do! notifyUpdate ev g
                return! msgLoop g
            | NoUpdate ->
                return! msgLoop g
          }
        in
          msgLoop (initialGame agent)
      in
        MailboxProcessor.Start(agentBody)
    in
      async {
        do agent.Post(EvGameBegin)
        while (! result) |> Option.isNone do
          do! Async.Sleep(10)
        return (! result) |> Option.get
      }
