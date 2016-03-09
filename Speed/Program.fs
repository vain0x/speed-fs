module Program

open System
open Speed
open Speed.Core
open Speed.Brain

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
  in
    Speed.Game.play ent1 ent2
    |> Async.RunSynchronously
    |> ignore

  // exit code
  0
