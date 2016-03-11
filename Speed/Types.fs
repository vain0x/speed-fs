namespace Speed.Core

[<AutoOpen>]
module TrumpTypes =
  type Suit =
    | Spade
    | Clover
    | Heart
    | Diamond

  type Color =
    | Black
    | Red

  type Rank =
    | Number of int
    | Ace
    | Jack
    | Queen
    | King

  type Card =
    | Card of Suit * Rank
    //| Joker of Color

[<AutoOpen>]
module Types =
  type PlayerId =
    internal
    | PlayerId of int

  type Hand =
    list<Card>

  type Board =
    Map<PlayerId, Card>

  type GameResult =
    | Win of PlayerId

  type Event =
    | EvPut       of plId: PlayerId * card: Card * dest: PlayerId
    | EvReset
    | EvGameBegin
    | EvGameEnd   of GameResult

  type Post =
    MailboxProcessor<Event>

  type PlayerT<'Brain, 'CardBack> =
    {
      PlayerId    : PlayerId
      Name        : string
      Brain       : 'Brain
      Hand        : Hand
      Deck        : list<'CardBack>
    }

  // プレイヤー本人または相手視点での自身の状態
  type PlayerState = PlayerT<unit, unit>

  type GameT<'Pl> =
    {
      PlayerStore : Map<PlayerId, 'Pl>
      Board       : Board
    }

  type GameState =
    GameT<PlayerState>

  type Brain =
    MailboxProcessor<Event * GameState>

  type BrainSpec =
    abstract member Create: PlayerId * Post -> Brain

  type Player = PlayerT<Brain, Card>

  type Game = GameT<Player>

  type Entrant =
    {
      Name        : string
      Brain       : BrainSpec
    }

  type Audience =
    abstract member Listen: Game * Game * Event -> unit
