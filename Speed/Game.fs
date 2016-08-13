namespace Speed

open Speed.Core
open Basis.Core
open Util
open Util.Collections

#nowarn "40"

module Game =
  type EventResult =
    | End         of GameResult
    | Kont

  let doEvent (agent: Post) ev =
    update {
      match ev with
      | EvGameBegin ->
          let! plIds = Game.getPlayers
          for plId in plIds do
            do! Game.putFirstCard plId
          return Kont
      | EvGameEnd r ->
          return End r
      | EvPut (plId, card, dest) ->
          do! Game.up (Game.UpdatePutCard (plId, card, dest))
          let! g = Game.get
          in
            if (plId, g) ||> Game.hasNoCards
            then agent.Post(EvGameEnd (Win plId))
          return Kont
      | EvReset ->
          do! Game.up (Game.UpdateReset)
          return Kont
    }

  let play audience ent1 ent2 =
    let result = ref (None: option<GameResult>)

    let initialGame agent =
      (ent1, ent2)
      ||> Game.init agent

    let rec agent =
      let agentBody (inbox: Post) =
        let notifyUpdate ev (g: Game) =
          g
          |> Game.players
          |> Seq.map (fun plId -> async {
              (g |> Game.player plId).Brain.Post(ev, g |> Game.state)
              })
          |> Async.Parallel
          |> Async.Ignore
          |> Async.Start

        let notifyToAudience g g' ev =
          audience
          |> List.iter (fun (au: Audience) -> au.Listen(g, g', ev))

        let rec msgLoop (g: Game) =
          async {
            let! ev = inbox.Receive()
            let u =
              update {
                let! evResult = doEvent agent ev
                in
                  match evResult with
                  | End r ->
                      do result := Some r
                      do notifyToAudience g g ev
                      do notifyUpdate ev g
                      return g
                  | Kont ->
                    let! g' = Game.get
                    in
                      if g = g'
                      then return g
                      else
                        do notifyToAudience g g' ev
                        do notifyUpdate ev g'
                        return g'
              }
            let g' = u |> UpdateMonad.run g
            return! msgLoop g'
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
