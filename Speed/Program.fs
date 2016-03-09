module Program

open System
open Speed
open Speed.Core

[<AutoOpen>]
module Helper =
  let makeEntrant name =
    {
      Name = name
      Brain = Brain.naiveBrain 100
    }

[<EntryPoint>]
let main argv =

  let ent1 = makeEntrant "P1"
  let ent2 = makeEntrant "P2"
  in
    Speed.Game.play ent1 ent2
    |> Async.RunSynchronously
    |> ignore

  // exit code
  0
