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

  type Brain =
    MailboxProcessor<Event * GameState>

  and BrainSpec =
    PlayerId -> Post -> Brain

  and GameState =
    {
      PlayerStore : Map<PlayerId, PlayerState>
      Board       : Board
    }

  // プレイヤー本人または相手視点での自身の状態
  and PlayerState =
    {
      PlayerId    : PlayerId
      Name        : string
      Hand        : Hand
      DeckCount   : int
    }

  type Player =
    {
      PlayerId    : PlayerId
      Name        : string
      Brain       : Brain
      Hand        : Hand
      Deck        : list<Card>
    }

  type Game =
    {
      PlayerStore : Map<PlayerId, Player>
      Board       : Board
    }

  type Entrant =
    {
      Name        : string
      Brain       : BrainSpec
    }

  type Audience =
    {
      Listen      : Game -> Game -> Event -> unit
    }
