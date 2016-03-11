namespace Speed.Gui

open System
open System.Drawing
open System.Windows.Forms
open Speed
open Speed.Core

[<AutoOpen>]
module Misc =
  let yuGothic emSizeInt =
    new Font("Yu Gothic", emSizeInt |> float32)
  let pointF x y =
    new PointF(float32 x, float32 y)
  let blackBrush =
    new SolidBrush(System.Drawing.Color.Black)

  type MainFormState =
    | BeforeGame
    | InGame      of GameState
    | AfterGame   of GameResult

type MainForm () =
  inherit Form
    ( Text = "Speed F#"
    )

  /// Some g iff game is running
  let mutable cur = BeforeGame

  let drawTitleScreen (gfx: Graphics) =
    do
      gfx.DrawString
        ("Speed F#"
        , yuGothic 35
        , blackBrush
        , pointF 30 50
        )
    do
      gfx.DrawString
        ("Click to Start!"
        , yuGothic 16
        , blackBrush
        , pointF 70 150
        )

  override this.OnPaint(e) =
    match cur with
    | BeforeGame ->
        drawTitleScreen(e.Graphics)
    | _ ->
        ()
