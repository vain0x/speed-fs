namespace Speed.Gui

open System.Drawing
open Speed.Core

module GuiObj =
  let makeCard pt card =
    let rect =
      Rectangle(pt, Size(30, 50))
    in
      GOCard (card, rect)

  let makeCardList (pt: Point) cards =
    cards
    |> List.mapi (fun i card ->
        makeCard (Point (pt.X + i * 50, pt.Y)) card
        )

  let ofGame (g: GameState) =
    let hand =
      g.PlayerStore
      |> Map.toList
      |> List.zip [200; 10]
      |> List.collect (fun (y, (_, pl)) ->
          makeCardList (Point (50, y)) pl.Hand
          )
    let board =
      g.Board
      |> Map.toList
      |> List.map snd
      |> makeCardList (Point (100, 100))
    in
      board @ hand

  let rec draw (gfx: Graphics) =
    function
    | GOCard (card, rect) ->
        do gfx.FillRectangle(whiteBrush, rect)
        do gfx.DrawRectangle(blackPen, rect)
        do
          gfx.DrawString
            ( card |> Card.rank |> Rank.toInt |> string
            , yuGothic 12
            , (match card |> Card.suit |> Suit.color with
                | Black -> blackBrush
                | Red   -> redBrush
                )
            , rectFloat rect
            )
    | GOCardList cards ->
        cards |> List.iter (draw gfx)

  let rec hittest (pt: Point) =
    function
    | GOCard (card, rect) as self ->
        if rect.Contains(pt)
        then [self]
        else []
    | GOCardList cards as self ->
        match cards |> List.collect (hittest pt) with
        | [] -> []
        | xs -> self :: xs

[<AutoOpen>]
module DrawHelper =
  let drawTitleLogo (gfx: Graphics) =
    gfx.DrawString
      ("Speed F#"
      , yuGothic 35
      , blackBrush
      , pointF 30 50
      )

  let drawTitleScreen (gfx: Graphics) =
    do drawTitleLogo gfx
    do
      gfx.DrawString
        ("Click to Start!"
        , yuGothic 16
        , blackBrush
        , pointF 70 150
        )

  let drawResultScreen wins (gfx: Graphics) =
    do drawTitleLogo gfx
    let msg =
      (if wins then "You win!" else "You lose...")
      + "\r\nClick to restart."
    do
      gfx.DrawString
        (msg
        , yuGothic 16
        , blackBrush
        , pointF 70 150
        )

  let drawGameScreen g gfx =
    g
    |> GuiObj.ofGame
    |> List.iter (GuiObj.draw gfx)
