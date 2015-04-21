module Meetup where

import String
import List
import Signal
import Html (..)
import Html.Events (..)
import Html.Attributes (..)
import Debug
import Window
import Json.Decode ( customDecoder )

-- MODEL
type alias Addressable =
    { name : String
    , street : String
    , houseNumber : Int
    , city : String
    , postcode : String
    , country : String
    }

type Partecipant = Name String
                 | Anonymous

type alias Meetup =
    { name : String
    --, members : List Partecipant
    --, pastEvents : List Event
    }

hhHaskellMeetup : Meetup
hhHaskellMeetup = { name = "Hamburg Haskell Meetup" }

type alias Event =
    { topic : String
    , location : Addressable
    --, date : String
    , meetup : Meetup
    , partecipants : List Partecipant
    , aboutToJoin : String
    }

jimdoGmbH : Addressable
jimdoGmbH =
    { name = "Jimdo GmbH"
    , street = "Stresemannst."
    , houseNumber = 375
    , city = "Hamburg"
    , postcode = "22761"
    , country = "Germany"
    }

noLocation : Addressable
noLocation =
    { name = ""
    , street = ""
    , houseNumber = -1
    , city = ""
    , postcode = ""
    , country = ""
    }

emptyMeetupEvent : Event
emptyMeetupEvent =
    { topic = ""
    , location = noLocation
    --, date = toDate "2015-04-14T19:00+02:00"
    , meetup = { name = "" }
    , partecipants = []
    , aboutToJoin = ""
    }

hamburgHaskellMeetupEvent : Event
hamburgHaskellMeetupEvent =
    { emptyMeetupEvent | topic <- "Web UI Design and Functional Reactive Programming"
    , location <- jimdoGmbH
    , meetup <- hhHaskellMeetup
    }

numberOfPartecipants : Event -> Int
numberOfPartecipants meetup = List.length meetup.partecipants

isJoinAnonymously : Event -> Bool
isJoinAnonymously event =
    String.isEmpty event.aboutToJoin

-- UPDATE
type Action = Join
            | Clear
            | Joining String

updateChannel : Signal.Channel Action
updateChannel =
    Signal.channel Clear

update : Action -> (Result String Bool, Event) -> (Result String Bool, Event)
update action (_, event) =
    case action of
      Join -> join event
      Clear -> (Ok True, hamburgHaskellMeetupEvent)
      Joining s -> (Ok False, { event | aboutToJoin <- s })

join : Event -> (Result String Bool, Event)
join event =
    case isJoinAnonymously event of
      True  -> (Err "Cannot join a meetup anonymously", event)
      False ->
          (Ok True, { event | partecipants <- event.partecipants ++ [ Name event.aboutToJoin ]
          , aboutToJoin <- ""
          })

model : Signal (Result String Bool, Event)
model = Signal.subscribe updateChannel
      |> Signal.foldp update (Ok True, hamburgHaskellMeetupEvent)

-- VIEW
view : (Int, Int) -> (Result String Bool, Event) -> String -> Html
view window (_, event) now =
    div [ style [ ("padding", "16px") ] ]
        [ h1 [] [ text event.meetup.name ]
        , h2 [] [ text event.topic ]
        , infoView window event
        , partecipantsCountView window event
        , partecipantsView window (Debug.watchSummary "Partecipant List" (\e -> e.partecipants) event)
        , joinMeetupView window (Debug.watch "About to join" event.aboutToJoin)
        , clearView window
        , text <| "Page last modified: " ++ now
        ]

infoView : (Int, Int) -> Event -> Html
infoView window event =
    let l = event.location
    in
      div [] [ hr [] []
             , h3 [] [ text l.name ]
             , h4 [] [ text (String.join ", " [ l.street ++ " " ++ toString l.houseNumber
                                              , l.postcode
                                              , l.city
                                              ])
                     ]
             , hr [] []
             ]

partecipantsCountView : (Int, Int) -> Event -> Html
partecipantsCountView window event =
    let title =
            case event.partecipants of
              []  -> "So far noboby joined this meetup...be the first!"
              [x] -> "So far 1 person joined this meetup"
              _   ->
                  "So far " ++ (numberOfPartecipants event |> toString) ++ " people joined this meetup"
    in h3 [] [ text title ]

partecipantsView : (Int, Int) -> Event -> Html
partecipantsView window event =
    let partecipantList =
            case event.partecipants of
              [] -> []
              ps -> List.map partecipantView ps
    in div [] [ ul [] partecipantList ]

partecipantView : Partecipant -> Html
partecipantView p =
    let displayName =
            case p of
              Name name -> name
              Anonymous -> "Shy dude"
    in div [] [ li [] [ text displayName ] ]

joinMeetupView : (Int, Int) -> String -> Html
joinMeetupView window joinerName =
    div [] [ input [ type' "text"
                   , placeholder "Enter your name..."
                   , value joinerName
                   , onEnter
                   , on "input" targetValue (Signal.send updateChannel << Joining)
                   ] []
           , button [ onClick (Signal.send updateChannel Join) ] [ text "Join" ]
           ]

onEnter : Attribute
onEnter =
    on "keyup" (customDecoder keyCode is13) (always <| Signal.send updateChannel Join)

is13 : Int -> Result String String
is13 code =
    if code == 13 then Ok "Join" else Err "Not the desired key"

clearView : (Int, Int) -> Html
clearView window =
    div [] [ button [ onClick (Signal.send updateChannel Clear) ] [ text "Clear Partecipant list" ] ]

-- PORTS
-- outgoing signal
port showError : Signal String
port showError = Signal.map errMsg model
               |> Signal.keepIf (not << String.isEmpty) ""

errMsg : (Result String Bool, Event) -> String
errMsg (res, _) =
    case res of
      Err msg -> msg
      _       -> ""

port getNow : Signal ()
port getNow = Signal.map identity model
            |> Signal.keepIf pageChanged (Ok True, emptyMeetupEvent)
            |> Signal.map (\_ -> ())

pageChanged : (Result String Bool, Event) -> Bool
pageChanged (res, _) =
    case res of
      Ok True -> True
      _       -> False

-- incoming signal
port now : Signal String



main : Signal Html
main =
    Signal.map3 view Window.dimensions model now
