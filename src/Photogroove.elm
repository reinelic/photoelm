module Photogroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random



type ThumbnailSize = Small | Medium | Large



type Status = Loading | Loaded (List Photo) String | Errored String

type alias Photo = { url : String}

type alias Model = {
                    status: Status
                    ,selectedUrl: String
                    ,chosenSize:ThumbnailSize
                }

type  Msg =  ClickedPhoto String 
            | ClickedSize ThumbnailSize 
            | ClickedSurpriseMe
            | GotSelectedIndex Int


initialModel:Model
initialModel = {
                
                 status = Loading
                ,selectedUrl = "1.jpeg"
                ,chosenSize = Small

                }

photoArray : Array Photo
photoArray = Array.fromList  initialModel.photos


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
    div [class "content"]
        [
            
            h1 [] [text "photo Groove"]
            ,button
                [ onClick ClickedSurpriseMe ]
                [ text "Surprise Me!" ]
            ,h3 [] [ text "Thumbnail Size:" ]
            ,div [ id "choose-size" ]
                (List.map viewSizeChooser [ Small, Medium, Large ])
            ,div [id "thumbnails" , class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail  model.selectedUrl) model.photos)
            ,img 
            [
                class "large"
                ,src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
            
 
        ]
        
        
viewThumbnail: String -> Photo -> Html Msg        
viewThumbnail  selectedUrl thumb =
   
        img
        [ src (urlPrefix ++ thumb.url)
        , classList [("selected", selectedUrl == thumb.url)]
        ,onClick (ClickedPhoto thumb.url)
        ]
        []


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
                    Random.int 0 (Array.length photoArray - 1)




getPhotoUrl : Int -> String
getPhotoUrl index =
        case Array.get index photoArray of
            Just photo ->
                photo.url
            
            Nothing ->
                ""

update : Msg -> Model -> (Model , Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url-> 
            ({ model | selectedUrl = url}, Cmd.none )

        ClickedSurpriseMe ->
            ({ model | selectedUrl = "2.jpeg"}, Random.generate GotSelectedIndex randomPhotoPicker)

        ClickedSize size-> 
             ({ model | chosenSize = size}, Cmd.none )

        GotSelectedIndex index ->
             ({ model | selectedUrl = getPhotoUrl index }, Cmd.none )




main: Program () Model Msg
main =
    Browser.element{
        init = \flags -> (initialModel , Cmd.none)
        ,view = view
        ,update = update
        ,subscriptions = \model -> Sub.none
    }