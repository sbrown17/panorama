module Main exposing (main)

import Browser
import Html exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Dict exposing (Dict)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Model
    = Failure
    | Loading
    | Success (List Breed)

type alias Breed = 
    { name : String
    , subBreeds : List String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getAllBreeds )


type Msg
    = GotBreeds (Result Http.Error (List Breed))
    -- | LookAtBreed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBreeds result ->
            case result of
                Ok breeds ->
                    ( Success breeds, Cmd.none )
                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "Sorry, there was an error getting the dog breeds."
        Loading ->
            text "Loading..."
        Success dogBreeds ->
            div []
                [
                    ul [] (List.map viewBreed dogBreeds)
                ]
            

viewBreed : Breed -> Html Msg
viewBreed breed =
    if List.isEmpty breed.subBreeds then
        li [] [text breed.name] -- needs button
    else div []
        [ text breed.name
        , ul []
            (List.map viewSubBreed breed.subBreeds)
        ]

viewSubBreed : String -> Html Msg
viewSubBreed subBreed =
    li [] [ text subBreed ] -- needs button

getAllBreeds : Cmd Msg
getAllBreeds =
  Http.get
    { url = "https://dog.ceo/api/breeds/list/all"
    , expect = Http.expectJson GotBreeds breedsDecoder
    }

breedsDecoder : Decoder (List Breed)
breedsDecoder =
    Json.Decode.field "message" (Json.Decode.dict (Json.Decode.list Json.Decode.string))
        |> Json.Decode.map dictToBreeds


-- can probably get rid of the dict and custom it
dictToBreeds : Dict String (List String) -> List Breed
dictToBreeds dict =
    Dict.toList dict
        |> List.map (\(name, subs) -> { name = name, subBreeds = subs })
        |> List.sortBy .name