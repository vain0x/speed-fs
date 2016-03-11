namespace Speed.Gui

open System.Drawing
open Speed.Core

[<AutoOpen>]
module Types =
  type AppState =
    | BeforeGame
    | InGame      of GameState
    | AfterGame   of wins: bool
    
  type GuiObj =
    | GOCard      of Card * Rectangle
    | GOCardList  of GuiObj list
