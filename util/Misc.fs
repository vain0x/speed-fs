namespace Util

open System
open Basis.Core

[<AutoOpen>]
module Misc =
  /// constant function
  let inline konst x _ = x

  let inline flip f x y = f y x

  let inline curry2 f x0 x1    = f (x0, x1)
  let inline curry3 f x0 x1 x2 = f (x0, x1, x2)

  let inline uncurry2 f (x0, x1    ) = f x0 x1
  let inline uncurry3 f (x0, x1, x2) = f x0 x1 x2

  /// apply f to x; then return x
  let tap f x =
    do f x
    x

  /// ignore typed
  let inline ignore'<'T> (_ : 'T) = ()

  /// assert in pipeline
  let inline assert' pred =
    tap (fun x -> assert (pred x))

  let swap (x: byref<_>) (y: byref<_>) =
    let t = x
    do x <- y
    do y <- x

  let makeCounter () =
    let r = ref 0
    in
      fun () -> (! r) |> tap (fun k -> r := k + 1)

module Math =
  let numDigits n =
    if n = 0
    then 1
    else n |> Math.Abs |> float |> Math.Log10 |> int |> (+) 1

module Str =
  let private optFromIndex i =
    if i >= 0 then Some i else None

  let tryIndexOf     target = (Str.indexOf target) >> optFromIndex
  let tryLastIndexOf target = (Str.lastIndexOf target) >> optFromIndex

module Integer =
  let (|Positive|Zero|Negative|) i =
    if   i = 0 then Zero
    elif i > 0 then Positive i
    else Negative i

[<AutoOpen>]
module RegexExtension =
  open System.Text.RegularExpressions
  open System.Linq

  let (|Matches|) pattern input =
    let m = Regex.Matches(input, pattern)
    m.Cast<Match>()

  type Group with
    member this.TryValue =
      if this.Success then Some (this.Value) else None

module Encoding =
  let Shift_JIS = Text.Encoding.GetEncoding("Shift_JIS")

module Path =
  open System.IO

  let Escape escaper path =
    let rec esc path c =
        path |> Str.replace (string c) (escaper c)

    Path.GetInvalidFileNameChars()
    |> Array.fold esc path

module Reflection =
  open Microsoft.FSharp.Reflection

  type DU<'T> () =
    static member val CaseInfos =
        FSharpType.GetUnionCases(typeof<'T>)
        |> Array.toList

    static member val Names =
        DU<'T>.CaseInfos
        |> List.map (fun (case: UnionCaseInfo) -> case.Name)

    static member val UnitCases =
        DU<'T>.CaseInfos
        |> List.choose (fun ci ->
            if ci.GetFields().Length = 0
            then Some (FSharpValue.MakeUnion(ci, Array.empty) :?> 'T)
            else None
          )

    static member FromString str =
        let caseOpt =
            DU<'T>.CaseInfos
            |> List.tryFind (fun case -> case.Name = str)
        match caseOpt with
        | Some case -> FSharpValue.MakeUnion (case, [||])
        | None -> failwith ("unknown case of " + typeof<'T>.Name)

type UpdateMonad<'TState, 'TUpdate, 'T> =
  | UM of ('TState -> 'TUpdate * 'T)

module UpdateMonad =
  open System.Collections.Generic

  let inline unit< ^S when ^S: (static member Unit: ^S)> (): ^S =
    (^S : (static member Unit: ^S) ()) 

  let inline combine< ^S when ^S: (static member Combine: ^S * ^S -> ^S )> a b: ^S =
    (^S : (static member Combine: ^S * ^S -> ^S) (a, b)) 

  let inline apply< ^S, ^U when ^U : (static member Apply: ^S * ^U -> ^S )> s a: ^S =
    (^U : (static member Apply: ^S * ^U -> ^S) (s, a)) 

  type UpdateBuilder() = 
    member inline this.Return(v): UpdateMonad<'S, 'U, 'T> = 
      UM (fun s -> (unit (), v))

    member inline x.ReturnFrom(m: UpdateMonad<'S, 'P, 'T>) = m

    member inline this.Bind(UM u1, f: 'T -> UpdateMonad<'S, 'U, 'R>) =  
      UM (fun s -> 
        let (u1, x) = u1 s
        let (UM u2) = f x
        let (u2, y) = u2 (apply s u1)
        (combine u1 u2, y))
        
    member inline this.Zero() = this.Return(())

    member inline this.Delay(f) = this.Bind(this.Zero(), f)

    member inline this.Combine(c1, c2) = this.Bind(c1, fun () -> c2)

    member inline this.Using(r, f) =
      let body s =
        use rr = r
        let (UM g) = f rr
        in g s
      in (UM body)

    member inline this.For(sq: seq<'V>, f: 'V -> UpdateMonad<'S, 'P, unit>) = 
      let rec loop (en: IEnumerator<_>) = 
        if en.MoveNext()
        then this.Bind(f en.Current, fun _ -> loop en)
        else this.Zero()
      in
        this.Using(sq.GetEnumerator(), loop)

    member inline this.While(t, f: unit -> UpdateMonad<'S, 'P, unit>) =
      let rec loop () = 
        if t ()
        then this.Bind(f(), loop)
        else this.Zero()
      in loop ()

[<AutoOpen>]
module UpdateMonadSyntax =
  let update = UpdateMonad.UpdateBuilder()
