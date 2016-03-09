module Program

open System
open Speed
open Speed.Core
open Speed.Brain

module Brain =
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

module Audience =
  let consoleAudience =
    {
      Listen =
        fun g g' ev ->
          printfn "-------------------------------"
          printfn "Board: %A" (g'.Board |> Map.toList |> List.map snd)
          for KeyValue (_, pl) in g'.PlayerStore do
            printfn "Player %s's hand = %A"
              (pl.Name) (pl.Hand)

            match ev with
            | EvGameBegin ->
                printfn "Game start!"

            | EvGameEnd (Win plId as r) ->
                printfn "Player %s won." ((g' |> Game.player plId).Name)

            | EvPut (plId, card, dest) ->
                printfn "Player %s puts card %A."
                  ((g' |> Game.player plId).Name)
                  card

            | EvReset ->
                printfn "Board reset."
    }

[<AutoOpen>]
module Helper =
  let makeEntrant name brain =
    {
      Name = name
      Brain = brain
    }

[<EntryPoint>]
let main argv =

  let ent1 = makeEntrant "You" (Brain.consoleBrain)
  let ent2 = makeEntrant "CPU" (naiveBrain 5000)
  let audience =
    [Audience.consoleAudience]
  in
    Speed.Game.play audience ent1 ent2
    |> Async.RunSynchronously
    |> ignore

  // exit code
  0
