namespace Speed.Gui

open System
open System.Drawing
open Speed.Core

module Brain =
  type GuiBrain
    (onUpdate
    , onEnd
    , onClickEvent: IEvent<Point * GameState>
    ) =
    let mutable data = (None: option<PlayerId * Post>)

    let body myId (post: Post) (inbox: Brain) =
      let rec msgLoop () =
        async {
          let! (ev, g) = inbox.Receive()
          match ev with
          | EvGameEnd r ->
              onEnd (g, r)
          | _ ->
              onUpdate g
          return! msgLoop ()
        }
      let onClick (pt, g) =
        g
        |> GuiObj.ofGame
        |> List.collect (GuiObj.hittest pt)
        |> List.iter
            (function
              | GOCard (card, _) ->
                  for dest in g |> Game.players do
                    post.Post(EvPut(myId, card, dest))
              | _ -> 
                  post.Post(EvReset)
              )
      let () =
        onClickEvent.Add(onClick)
      in
        msgLoop ()

    interface BrainSpec with
      member this.Create(plId, post) =
        MailboxProcessor.Start(body plId post)
