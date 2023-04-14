module Photogroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class , classList , id , name , src ,title , type_)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Http
import Random
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)



type ThumbnailSize = Small | Medium | Large

initialCmd : Cmd Msg
initialCmd = Http.get 
            {
                url = "http://elm-in-action.com/photos/list.json"
                ,expect = Http.expectJson GotPhotos (list photoDecoder)

            }

type Status = Loading | Loaded (List Photo) String | Errored String

type alias Photo = { 
                    url : String
                    ,size : Int
                    ,title : String                    
                  }

photoDecoder: Decoder Photo
photoDecoder =
            succeed Photo
             |> required "url" string
             |> required "size" int
             |> optional "title" string "(untitled)"





type alias Model = {
                    status: Status
                    ,chosenSize:ThumbnailSize
                }

type  Msg =  ClickedPhoto String 
            | ClickedSize ThumbnailSize 
            | ClickedSurpriseMe
            | GotRandomPhoto Photo
            | GotPhotos (Result Http.Error  (List Photo))


initialModel:Model
initialModel = {
                
                 status = Loading
                ,chosenSize = Small

                }

-- photoArray : Array Photo
-- photoArray = Array.fromList  initialModel.photos


viewSizeChooser : ThumbnailSize -> Html Msg

viewSizeChooser  size = 
    label []
        [ input [onClick (ClickedSize size),type_ "radio", name "size" ] []
        , text (sizeToString size)
       
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of 
        Small ->
            "small"
        Medium ->
            "med"
        Large ->
            "large"    


urlPrefix: String
urlPrefix = "http://elm-in-action.com/"

view: Model -> Html Msg
view model = 
    div [class "content"] <|

            case model.status of
                Loaded photos selectedUrl ->
                    viewLoaded photos selectedUrl model.chosenSize
                Loading ->
                    []
                Errored errorMessage ->
                    [text ("Error:" ++ errorMessage)]
        
        

viewLoaded: List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
            [ h1 [] [ text "Photo Groove" ]
            , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
            , h3 [] [ text "Thumbnail Size:" ]
            , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
            , div [ id "thumbnails", class (sizeToString chosenSize) ]
            (List.map (viewThumbnail selectedUrl) photos)
            , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ selectedUrl)
            ]
            []
            ]

viewThumbnail: String -> Photo -> Html Msg        
viewThumbnail  selectedUrl thumb =
           img
            [ src (urlPrefix ++ thumb.url)
            , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
            , classList [ ( "selected", selectedUrl == thumb.url ) ]
            , onClick (ClickedPhoto thumb.url)
            ]
            []



-- randomPhotoPicker : Random.Generator Int
-- randomPhotoPicker =
--                     Random.int 0 (Array.length photoArray - 1)




-- getPhotoUrl : Int -> String
-- getPhotoUrl index =
--         case Array.get index photoArray of
--             Just photo ->
--                 photo.url
            
--             Nothing ->
--                 ""


update : Msg -> Model -> (Model , Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url-> 
            ({ model | status = selectUrl url model.status}, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->

                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model
                                       

                Loaded [] _ ->

                    (model,Cmd.none)    

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->

                    ( model, Cmd.none )

        ClickedSize size-> 
             ({ model | chosenSize = size}, Cmd.none )

        GotRandomPhoto photo ->
             ({ model |  status = selectUrl photo.url model.status}, Cmd.none )
        
        GotPhotos (Ok photos) ->

                    case photos of
                        first :: rest  ->

                             ({ model | status = Loaded photos first.url }, Cmd.none )
                        
                        [] ->
                        
                            ({ model | status = Errored "0 photos found" }, Cmd.none)

                
        GotPhotos (Err _) ->
                    ( { model | status = Errored "Server error!" }, Cmd.none )








selectUrl : String -> Status -> Status
selectUrl url status =
        case status of
            Loaded photos _ ->
                Loaded photos url
            Loading ->
               status 
            Errored errorMessage ->
               status






main: Program () Model Msg
main =
    Browser.element{
        init = \_ -> (initialModel , initialCmd)
        ,view = view
        ,update = update
        ,subscriptions = \_ -> Sub.none
    }