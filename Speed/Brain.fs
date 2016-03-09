namespace Speed

open Speed
open Speed.Core

module Brain =
  let tryFindPuttableCard plId (g: GameState) =
    let pl = g.PlayerStore |> Map.find plId
    let players = g.PlayerStore |> Map.toList |> List.map fst
    in
      pl.Hand
      |> List.tryPick (fun handCard ->
          players
          |> List.tryPick (fun dest ->
              let canPut =
                match g.Board |> Map.tryFind dest with
                | None -> true
                | Some boardCard -> handCard |> Card.isNextTo boardCard
              in
                if canPut
                then Some (handCard, dest)
                else None
              )
          )

  let naiveBrain sleepTime myId (agent: Post) =
    let body (inbox: Brain) =
      let rec msgLoop () =
        async {
          let! (ev, g') = inbox.Receive()
          in
            match ev with
            | _ ->
                match g' |> tryFindPuttableCard myId with
                | Some (handCard, dest) ->
                    agent.Post(EvPut (myId, handCard, dest))
                | None ->
                    agent.Post(EvReset)
          ; if sleepTime > 0 then
              do! Async.Sleep(sleepTime)
          ;
            return! msgLoop ()
        }
      in msgLoop ()
    in
      MailboxProcessor.Start(body)
