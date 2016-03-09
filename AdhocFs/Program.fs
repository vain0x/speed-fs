module Program

open System
open Speed
open Speed.Core

[<AutoOpen>]
module Helper =
  let makeEntrant name =
    {
      Name = name
      Brain = Brain.naiveBrain
    }

[<EntryPoint>]
let main argv =

  let ent1 = makeEntrant "Player1"
  let ent2 = makeEntrant "Player2"
  in
    Speed.Game.play ent1 ent2
    |> Async.RunSynchronously
    |> printfn "%s"

  // exit code
  0
