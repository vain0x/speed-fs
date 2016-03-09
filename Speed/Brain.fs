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

  let consoleBrain myId (agent: Post) =
    let readIntLessThan ub =
      let (|ToInt|_|) s =
        match s |> Int32.TryParse with
        | (true, n) -> Some n
        | _ -> None
      let rec loop () =
        match Console.ReadLine() with
        | null -> None
        | ToInt n when n < ub -> Some n
        | _ -> None
      in
        loop ()

    let body (inbox: Brain) =
      let rec msgLoop () =
        async {
          let! (ev, g) = inbox.Receive()
          let you = g.PlayerStore |> Map.find myId
          do
            printfn "(Which card do you put and where?)"
          ; you.Hand
            |> List.iteri (fun i card ->
                printfn "#%d %A" i card
                )
          ;
            match readIntLessThan (you.Hand |> List.length) with
            | None ->
                agent.Post(EvReset)
            | Some i ->
                let card = you.Hand |> Seq.nth i
                in
                  // 場の全枠に置くことを試みる
                  for dest in g |> GameState.players do
                    agent.Post(EvPut (myId, card, dest))

          return! msgLoop ()
        }
      in
        msgLoop ()
    in
      MailboxProcessor.Start(body)
