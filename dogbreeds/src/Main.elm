module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, width, height)
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
    | GotImages (List String)

type alias Breed = 
    { name : String
    , subBreeds : List String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getAllBreeds )


type Msg
    = GotBreeds (Result Http.Error (List Breed))
    | GetBreedDetails String
    | GotBreedDetails (Result Http.Error (List String)) 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBreeds result ->
            case result of
                Ok breeds ->
                    ( Success breeds, Cmd.none )
                Err _ ->
                    ( Failure, Cmd.none )
        GetBreedDetails breed ->
            getBreedImages breed
        GotBreedDetails result ->
            case result of
                Ok images ->
                    ( GotImages images, Cmd.none )
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
                [ ul [] (List.map viewBreed dogBreeds)
                ]
        GotImages images ->
            viewBreedDetails images

viewBreed : Breed -> Html Msg
viewBreed breed =
    if List.isEmpty breed.subBreeds then
        li [] [button [onClick (GetBreedDetails breed.name)] [text breed.name]]
    else div []
        [ text breed.name
        , ul []
            (List.map (viewSubBreed breed.name) breed.subBreeds)
        ]

viewSubBreed : String -> String -> Html Msg
viewSubBreed parentBreed subBreed =
    li [] [button [onClick (GetBreedDetails (parentBreed ++ "/" ++ subBreed))] [text subBreed]]

viewBreedDetails : List String -> Html Msg
viewBreedDetails images =
    div []
                [text (String.fromInt (List.length images) ++ " total images")
    , ul [] 
        ( images
            |> List.take 20
            |> List.map (\image -> img [ src image, width 200, height 200 ] [] )
        )]

getBreedImages : String -> (Model, Cmd Msg)
getBreedImages breed =
    (Loading, Http.get
        { url = "https://dog.ceo/api/breed/" ++ breed ++ "/images"
        , expect = Http.expectJson GotBreedDetails breedDetailDecoder
        })

breedDetailDecoder : Decoder (List String)
breedDetailDecoder =
    Json.Decode.field "message" (Json.Decode.list Json.Decode.string)

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

dictToBreeds : Dict String (List String) -> List Breed
dictToBreeds dict =
    Dict.toList dict
        |> List.map (\(name, subs) -> { name = name, subBreeds = subs })
        |> List.sortBy .name