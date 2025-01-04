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

type Status
    = Loading
    | Ready
    | Error String

type alias Model =
    { breeds : List Breed
    , breedCache : Maybe (List Breed)
    , selectedBreedImages : List String
    , breedImageCache : Dict String (List String)
    , status : Status
    , currentBreed : Maybe String
    , viewState : ViewState
    }

type ViewState
    = ViewingBreeds
    | ViewingImages
type alias Breed = 
    { name : String
    , subBreeds : List String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ({ breeds = []
     , breedCache = Nothing
     , selectedBreedImages = []
     , breedImageCache = Dict.empty
     , status = Loading
     , currentBreed = Nothing
     , viewState = ViewingBreeds
    }
    , getAllBreeds
    )


type Msg
    = GotBreeds (Result Http.Error (List Breed))
    | GetBreedDetails String
    | GotBreedDetails String (Result Http.Error BreedImages) 
    | ShowBreeds
    | CachedBreedsLoaded (List Breed)

type alias BreedImages =
    { status : String
    , images : List String
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBreeds result ->
            case result of
                Ok breeds ->
                    ( { model
                      | breeds = breeds
                      , breedCache = Just breeds
                      , status = Ready
                    }
                    , Cmd.none )
                Err _ ->
                    ( { model | status = Error "Failed to retrieve or load breeds." }
                    , Cmd.none 
                    )
        GetBreedDetails breed ->
            case Dict.get breed model.breedImageCache of
                Just cachedImages ->
                    ( { model 
                      | selectedBreedImages = cachedImages
                      , status = Ready
                      , currentBreed = Just breed
                      , viewState = ViewingImages
                      }
                    , Cmd.none
                    )
                Nothing -> 
                    ( { model 
                      | status = Loading
                      , currentBreed = Just breed
                      }
                    , getBreedImages breed
                    )
        GotBreedDetails breed result ->
            case result of
                Ok images ->
                    ( { model
                      | selectedBreedImages = images.images
                      , breedImageCache = Dict.insert breed images.images model.breedImageCache
                      , status = Ready
                      , viewState = ViewingImages
                    }
                    , Cmd.none  )
                Err _ ->
                    ( { model | status = Error "Failed to retrieve or load images." }
                    , Cmd.none 
                    )
        ShowBreeds ->
            ( { model
              | viewState = ViewingBreeds
            }
            , Cmd.none
            )
        CachedBreedsLoaded breeds ->
            ( { model
              | breeds = breeds
              , status = Ready
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model.status of
        Error message ->
            text message
        Loading ->
            text "Loading..."
        Ready ->
            case model.viewState of
                ViewingBreeds ->
                    div []
                        [ ul [] (List.map viewBreed model.breeds)
                        ]
                ViewingImages ->
                    viewBreedDetails model.selectedBreedImages

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
    [ button [onClick ShowBreeds] [text "Go back"]
    , text (String.fromInt (List.length images) ++ " total images")
    , ul [] 
        ( images
            |> List.take 20
            |> List.map (\image -> img [ src image, width 200, height 200 ] [] )
        )
    ]

getBreedImages : String -> Cmd Msg
getBreedImages breed =
    Http.get
        { url = "https://dog.ceo/api/breed/" ++ breed ++ "/images"
        , expect = Http.expectJson (GotBreedDetails breed) breedDetailDecoder
        }

breedDetailDecoder : Decoder BreedImages
breedDetailDecoder =
    Json.Decode.map2 BreedImages
        (Json.Decode.field "status" Json.Decode.string)
        (Json.Decode.field "message" (Json.Decode.list Json.Decode.string))

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