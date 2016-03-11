[<AutoOpen>]
module Speed.Gui.Util

open System
open System.Drawing
open System.Windows.Forms
open Speed
open Speed.Core
open Util
open Util.Collections

[<AutoOpen>]
module Misc =
  let yuGothic emSizeInt =
    new Font("Yu Gothic", emSizeInt |> float32)

  let pointF x y =
    new PointF(float32 x, float32 y)

  let blackBrush =
    new SolidBrush(System.Drawing.Color.Black)

  let whiteBrush =
    new SolidBrush(Drawing.Color.White)

  let redBrush =
    new SolidBrush(Drawing.Color.Red)

  let blackPen =
    new Pen(blackBrush)

  let rectFloat (rect: Rectangle) =
    RectangleF
      ( rect.Left   |> float32
      , rect.Top    |> float32
      , rect.Width  |> float32
      , rect.Height |> float32
      )
