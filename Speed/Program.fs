module Program

open System
open Speed
open Speed.Core
open Speed.Brain

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

  let ent1 = makeEntrant "You" (consoleBrain)
  let ent2 = makeEntrant "CPU" (naiveBrain 5000)
  let audience =
    [Audience.consoleAudience]
  in
    Speed.Game.play audience ent1 ent2
    |> Async.RunSynchronously
    |> ignore

  // exit code
  0
