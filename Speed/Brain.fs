namespace Speed

open System
open Speed
open Speed.Core

module Brain =
  let tryFindPuttableCard plId (g: GameState) =
    let pl = g.PlayerStore |> Map.find plId
    in
      pl.Hand
      |> List.tryPick (fun handCard ->
          (g |> GameState.players)
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
          match ev with
          | EvGameEnd _ ->
              return ()
          | _ ->
              let myEv =
                match g' |> tryFindPuttableCard myId with
                | Some (handCard, dest) ->
                    EvPut (myId, handCard, dest)
                | None ->
                    EvReset
              let _ =
                agent.Post(myEv)
              in
                if sleepTime > 0 then
                  do! Async.Sleep(sleepTime)
              return! msgLoop ()
        }
      in msgLoop ()
    in
      MailboxProcessor.Start(body)

  type NaiveBrain (timeout) =
    interface BrainSpec with
      member this.Create(plId, post) =
        naiveBrain timeout plId post

  let guiBrain plId post =
    let body (inbox: Brain) =
      let rec msgLoop () =
        async {
          let! (ev, g') = inbox.Receive()
          match ev with
          | EvGameEnd _ ->
              return ()
          | _ ->
          return! msgLoop ()
        }
      in
        msgLoop ()
    in
      MailboxProcessor.Start(body)

  type GuiBrain () =
    interface BrainSpec with
      member this.Create(plId, post) =
        guiBrain plId post
