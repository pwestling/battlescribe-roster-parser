module Frontend exposing (main, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser as Browser
import Debug as Debug
import File exposing (File)
import File.Download as Download
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E
import List as L
import Markdown
import Maybe exposing (withDefault)


main : Program () Model Msg
main =
    Browser.document { init = \f -> init, view = viewWithTitle, update = update, subscriptions = subs }


type alias RosterTranslation =
    { roster : Maybe D.Value
    , missingUnits : List String
    }


type alias RosterId =
    { id : String }


type Msg
    = NoOp
    | UploadRoster (List File)
    | GotTTSJson (Result Error RosterId)
    | ToggleScripting
    | SetUiWidth String
    | SetUiHeight String
    | NavbarMsg Navbar.State


type alias Model =
    { rosterCode : Maybe String
    , navbarState : Navbar.State
    , message : Maybe String
    , addScript : Bool
    , uiWidth : String
    , uiHeight : String
    }


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { rosterCode = Nothing
      , navbarState = navbarState
      , message = Nothing
      , addScript = True
      , uiWidth = "700"
      , uiHeight = "450"
      }
    , navbarCmd
    )


subs : Model -> Sub Msg
subs model =
    Sub.none


asUrl : String -> List ( String, String ) -> String
asUrl base params =
    base ++ "?" ++ String.join "&" (List.map (\( s1, s2 ) -> s1 ++ "=" ++ s2) params)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        UploadRoster files ->
            case L.head files of
                Just file ->
                    ( { model | message = Just "Uploading..." }
                    , Http.post
                        { url =
                            asUrl
                                "https://backend.battlescribe2tts.net/roster"
                                [ ( "addScripts"
                                  , if model.addScript then
                                        "true"

                                    else
                                        "false"
                                  )
                                , ( "uiWidth", model.uiWidth )
                                , ( "uiHeight", model.uiHeight )
                                ]
                        , body = multipartBody [ filePart "roster" file ]
                        , expect = Http.expectJson GotTTSJson rosterDecoder
                        }
                    )

                Nothing ->
                    ( { model | message = Just "Could not find file" }, Cmd.none )

        GotTTSJson uploadResult ->
            case uploadResult of
                Ok content ->
                    ( { model | rosterCode = Just content.id, message = Nothing }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | message = Just (Debug.toString err) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ToggleScripting ->
            ( { model | addScript = not model.addScript }, Cmd.none )

        SetUiHeight y ->
            ( { model | uiHeight = y }, Cmd.none )

        SetUiWidth x ->
            ( { model | uiWidth = x }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg


viewWithTitle : Model -> Browser.Document Msg
viewWithTitle model =
    { title = "Battlescribe2TTS", body = [ view model ] }


view : Model -> Html Msg
view model =
    Grid.container []
        -- Responsive fixed width container
        [ CDN.stylesheet -- Inlined Bootstrap CSS for use with reactor
        , navbar model -- Interactive and responsive menu
        , uploadPage model
        , instructions
        ]


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ href "#" ]
            [ img [ src "assets/bs2tts.png", style "width" "30%" ] []
            , span [ style "font-size" "3em", style "margin" "1em" ] [ text "Battlescribe2TTS" ]
            ]
        |> Navbar.view model.navbarState


uploadPage : Model -> Html Msg
uploadPage model =
    div []
        [ span [ style "padding" "1em", style "margin-top" "2em" ]
            [ span [ style "font-size" "2em", style "margin-right" "2em" ]
                [ text "Upload Roster" ]
            , input
                [ type_ "file"
                , multiple False
                , on "change" (D.map UploadRoster filesDecoder)
                ]
                []
            , div [ style "display" "block", style "float" "right", style "font-size" "0.7em", style "background-color" "rgb(50, 115, 115);" ]
                [ div [ style "font-size" "1.4em" ] [ text "Advanced Options" ]
                , div [] [ checkbox ToggleScripting "Enable Model Scripting" model.addScript ]
                , div [] [ textinput SetUiWidth "UI Width" model.uiWidth ]
                , div [] [ textinput SetUiHeight "UI Height" model.uiHeight ]
                ]
            ]
        , case model.rosterCode of
            Just id ->
                div [ style "font-size" "2.5em", style "padding" "1em" ] [ text ("Your roster code is: " ++ id) ]

            Nothing ->
                div [] []
        , div [] [ text (withDefault "" model.message) ]
        ]


instructions : Html Msg
instructions =
    div [ style "margin-top" "5em", style "margin-bottom" "5em" ] <|
        Markdown.toHtml Nothing """
## How To Use
        
This website is designed to be used with the in-game tool for Tabletop Simulator provided 
[here](https://steamcommunity.com/sharedfiles/filedetails/?id=1793303279). This tool helps with 
importing Warhammer 40k rosters created in the [Battlescribe](https://battlescribe.net/) army builder tool into TTS.

To use this tool first create a roster with [Battlescribe](https://battlescribe.net/) and save 
the roster file (the file created should have the .rosz extension). Use the button above to upload 
the .rosz file. When the roster is uploaded, you'll be provided a code - copy this to your clipboard. 
Your code will remain valid for 1 hour

In Tabletop Simulator, paste the code into the Battlescribe2TTS tool and then click "Submit Code". A series of buttons 
will be created corresponding to the various selections in your army. To tell the tool which in-game model to 
use for a particular selection, first click the button, then pick up the model you want to use. The button should change 
colors to indicate the model has been saved.

Once all models have been selected, click the "Create Army" button. The models for your army will be 
spawned into the game, organized by unit, with their names and descriptions filled in from your Battlescribe data. 
You're ready to play!

An example roster created using this tool:

![Example Roster](assets/example-roster.png "Example Roster")

## In-Game Scripting

### Datasheets

Every unit can have a full datasheet brought up by pressing Scripting Button 1 over a model from the unit. 
Scripting Button 1 is the "1" key on the keypad by default, but you can map it to some other key in the 
Tabletop Simulator options.

An example datasheet:

![Example Datasheet](assets/ttsui.jpeg "Example Datasheet")

### Coherency

The datasheet will give a read out of all the models in the unit that are in coherency. Models closer to the 
center of the table are preferred, and models are not counted if they are flipped over, so you can remove models 
from the table or flip them upside down with the flip key to remove them from the unit count. The highlight button 
will toggle a highlight on the models the script believes are in coherency.

## Saved Models

When you select a model with the Battlescribe2TTS tool, the model you've selected is saved 
into the tool. You can avoid having to re-select models for similar armies in the future by 
adding the tool to your "Saved Objects" after selecting models. Your selections will be saved 
and automatically populated in the future for matching units

## Reporting Issues

If you have issues trying to use this tool, please create an issue on 
[github](https://github.com/pwestling/battlescribe-roster-parser/issues). Please note that 
none of this is supported by the Battlescribe or Tabletop Simulator teams in any way, so don't complain to them!
        """


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name isChecked =
    label
        [ style "padding" "0.1em" ]
        [ input [ type_ "checkbox", onClick msg, checked isChecked ] []
        , text name
        ]


textinput : (String -> msg) -> String -> String -> Html msg
textinput msg name currentValue =
    label
        [ style "padding" "0.1em" ]
        [ input [ onInput msg, value currentValue ] []
        , text name
        ]


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)


rosterDecoder : D.Decoder RosterId
rosterDecoder =
    D.map (\id -> { id = id })
        (D.field
            "id"
            D.string
        )
