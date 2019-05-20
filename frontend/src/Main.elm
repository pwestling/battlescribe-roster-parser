module Frontend exposing (main, view)

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
import Maybe exposing (withDefault)


main : Program () Model Msg
main =
    Browser.document { init = \f -> init, view = viewWithTitle, update = update, subscriptions = subs }


type alias RosterTranslation =
    { roster : Maybe D.Value
    , missingUnits : List String
    }


type Msg
    = NoOp
    | UploadRoster (List File)
    | GotTTSJson (Result Error RosterTranslation)


type alias Model =
    { error : String }


init : ( Model, Cmd Msg )
init =
    ( { error = "" }, Cmd.none )


subs : Model -> Sub Msg
subs model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadRoster files ->
            case L.head files of
                Just file ->
                    ( { error = "Uploading..." }
                    , Http.post
                        { url = "http://localhost:8080/roster"
                        , body = multipartBody [ filePart "roster" file ]
                        , expect = Http.expectJson GotTTSJson rosterDecoder
                        }
                    )

                Nothing ->
                    ( { error = "Could not find file" }, Cmd.none )

        GotTTSJson uploadResult ->
            case uploadResult of
                Ok content ->
                    ( { error = "The following units had no models: \n" ++ String.join "\n" content.missingUnits }
                    , Download.string "roster.json" "application/json" (E.encode 2 (withDefault (E.object []) content.roster))
                    )

                Err err ->
                    ( { error = Debug.toString err }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


viewWithTitle : Model -> Browser.Document Msg
viewWithTitle model =
    { title = "Battlescribe2TTS", body = [ view model ] }


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "file"
            , multiple False
            , on "change" (D.map UploadRoster filesDecoder)
            ]
            []
        , div [] [ pre [] [ text model.error ] ]
        ]


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)


rosterDecoder : D.Decoder RosterTranslation
rosterDecoder =
    D.map2 (\roster missing -> { roster = roster, missingUnits = missing })
        (D.maybe
            (D.field "roster" D.value)
        )
        (D.field
            "missingUnits"
            (D.list D.string)
        )
