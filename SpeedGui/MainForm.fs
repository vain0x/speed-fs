namespace Speed.Gui

open System
open System.Drawing
open System.Windows.Forms
open Speed
open Speed.Core
open Util
open Util.Collections

type MainForm () as thisForm =
  inherit Form
    ( Text = "Speed F#"
    )

  /// Some g iff game is running
  let mutable cur = BeforeGame

  let updateTimer =
    new Timer()
    |> tap (fun timer ->
        do timer.Interval <- 17
        do timer.Tick.Add(fun e -> thisForm.Refresh())
        )

  let onClickEvent = new Event<Point * GameState>()

  let onUpdate g =
    do cur <- InGame g

  let onEnd (g: GameState, Win plId) =
    let wins =
      (g.PlayerStore |> Map.find plId).Name = "You"
    do cur <- AfterGame wins

  let beginGame () =
    assert (cur = BeforeGame)
    let ent1 =
      {
        Name    = "You"
        Brain   =
          Brain.GuiBrain(onUpdate, onEnd, onClickEvent.Publish)
      }
    let ent2 =
      {
        Name    = "CPU"
        Brain   = Brain.NaiveBrain(1000)
      }
    in
      Game.play [] ent1 ent2
      |> Async.Ignore
      |> Async.Start
    do updateTimer.Start()

  do
    thisForm.DoubleBuffered <- true

  override this.OnPaint(e: PaintEventArgs) =
    match cur with
    | BeforeGame ->
        drawTitleScreen (e.Graphics)
    | AfterGame r ->
        drawResultScreen r (e.Graphics)
    | InGame g ->
        let buffer =
          new Bitmap
            ( width  = e.ClipRectangle.Width
            , height = e.ClipRectangle.Height
            )
        let gfx = Graphics.FromImage(buffer)
        do drawGameScreen g gfx
        do
          e.Graphics.DrawImageUnscaled
            (buffer
            , 0, 0
            , e.ClipRectangle.Width
            , e.ClipRectangle.Height
            )

  override this.OnMouseClick(e: MouseEventArgs) =
    match cur with
    | BeforeGame
    | AfterGame _ ->
        do beginGame ()
    | InGame g ->
        do onClickEvent.Trigger(e.Location, g)
