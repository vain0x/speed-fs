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
        g.PlayerStore
        |> Map.fold (fun g plId _ ->
            g |> Game.putFirstCard plId
            ) g
        |> Update

    | EvGameEnd r ->
        r |> End

    | EvPut (plId, card, dest) ->
        match g |> Game.tryPutCardFromHand plId card dest with
        | Some g ->
            if (plId, g) ||> Game.hasNoCards
            then agent.Post(EvGameEnd (Win plId))
            g |> Update
        | None -> NoUpdate

    | EvReset ->
        match g |> Game.resetBoardIfNecessary with
        | Some g -> 
            g |> Update
        | None -> NoUpdate

  let play audience ent1 ent2 =
    let result = ref (None: option<GameResult>)

    let initialGame agent =
      (ent1, ent2)
      ||> Game.init agent

    let rec agent =
      let agentBody (inbox: Post) =
        let notifyUpdate ev g =
          g
          |> Game.players
          |> Seq.map (fun plId -> async {
              (g |> Game.player plId).Brain.Post(ev, g |> Game.state)
              })
          |> Async.Parallel
          |> Async.Ignore

        let notifyToAudience g g' ev =
          audience
          |> List.iter (fun { Listen = listen } -> listen g g' ev)

        let rec msgLoop (g: Game) =
          async {
            let! ev = inbox.Receive()
            match g |> doEvent agent ev with
            | End r ->
                result := Some r
                do notifyToAudience g g ev
                do! notifyUpdate ev g
                return ()

            | Update g' ->
                do notifyToAudience g g' ev
                do! notifyUpdate ev g'
                return! msgLoop g'

            | NoUpdate ->
                return! msgLoop g
          }
        in
          msgLoop (initialGame agent)
      in
        MailboxProcessor.Start(agentBody)
    in
      // TODO: よりよい方法で停止する
      async {
        do agent.Post(EvGameBegin)
        while (! result) |> Option.isNone do
          do! Async.Sleep(10)
        return (! result) |> Option.get
      }
