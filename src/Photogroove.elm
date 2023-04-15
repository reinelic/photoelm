port module Photogroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes  as Attr exposing (class , classList , id , name , src ,title , type_)
import Html.Events exposing (onClick,on)
import Array exposing (Array)
import Http
import Json.Encode as Encode
import Random
import Json.Decode exposing (Decoder, int, list, string, succeed,at)
import Json.Decode.Pipeline exposing (optional, required)



type ThumbnailSize = Small | Medium | Large

initialCmd : Cmd Msg
initialCmd = Http.get 
            {
                url = "http://elm-in-action.com/photos/list.json"
                ,expect = Http.expectJson GotPhotos (list photoDecoder)

            }





type Status = Loading | Loaded (List Photo) String | Errored String


port setFilters : FilterOptions -> Cmd msg

port activityChanges : (String -> msg) -> Sub msg

type alias FilterOptions = {
                            url:String
                            ,filters: List { name : String , amount : Float}

                            }
  
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
                    ,hue:Int
                    ,ripple: Int
                    ,noise : Int
                    ,activity:String
                }

type  Msg =  ClickedPhoto String 
            | ClickedSize ThumbnailSize 
            | ClickedSurpriseMe
            | GotRandomPhoto Photo
            | GotPhotos (Result Http.Error  (List Photo))
            | SlideHue Int
            | SlideRipple Int
            | SlideNoise Int
            | GotActivity String


initialModel:Model
initialModel = {
                
                 status = Loading
                ,chosenSize = Small
                ,hue = 5
                ,ripple = 5
                ,noise = 5
                ,activity = ""

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
                    viewLoaded photos selectedUrl model
                Loading ->
                    []
                Errored errorMessage ->
                    [text ("Error:" ++ errorMessage)]
        
        
rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


viewFilter: (Int -> Msg)-> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [class "filter-slider"]
        [label [] [text name]
        ,rangeSlider
            [
                Attr.max "11"
                ,Attr.property "val" (Encode.int magnitude)
                ,onSlide toMsg
            ]
            []
        ,label [][ text (String.fromInt magnitude)]
        ]


onSlide:(Int -> msg) -> Attribute msg
onSlide toMsg =
    let 
        detailUserSlidTo : Decoder Int
        detailUserSlidTo =
            at [ "detail", "userSlidTo" ] int

        msgDecoder : Decoder msg
        msgDecoder =
            Json.Decode.map toMsg detailUserSlidTo
    in
    on "slide" msgDecoder     



viewLoaded: List Photo -> String -> Model ->List (Html Msg)
viewLoaded photos selectedUrl  model =
            [ h1 [] [ text "Photo Groove" ]
            , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
            , div [class "activity" ] [ text model.activity ]
            ,div [ class "filters"]
            [
                viewFilter SlideHue "Hue" model.hue
                ,viewFilter SlideRipple "Ripple" model.ripple
                ,viewFilter  SlideNoise "Noise"  model.noise
            ]
            , h3 [] [ text "Thumbnail Size:" ]
            , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
            , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail selectedUrl) photos)
            , canvas
            [ id "main-canvas"
            , class "large"
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
          
          applyFilters{ model | status = selectUrl url model.status }

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
            applyFilters{ model | status = selectUrl photo.url model.status }
        
        GotPhotos (Ok photos) ->
                    case photos of
                        first :: rest  ->

                             applyFilters { model |
                                    status = 
                                        case List.head photos of

                                            Just photo ->
                                                 
                                                 Loaded photos photo.url
                                        
                                            Nothing ->
                                                
                                                Loaded [] ""

                                            }
                        [] ->
                                ( { model | status = Errored "0 photos found" }, Cmd.none)

                
        GotPhotos (Err _) ->
                    ( { model | status = Errored "Server error!" }, Cmd.none )


        SlideHue hue ->
                   applyFilters { model | hue = hue }

        SlideRipple ripple ->

                   applyFilters { model | ripple = ripple }

        SlideNoise noise ->
        
                    applyFilters { model | noise = noise}

        GotActivity activity ->
                    ({ model | activity = activity} , Cmd.none)





applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
        case model.status of

            Loaded photos selectedUrl ->

                let
                    filters =[ { name = "Hue", amount = toFloat model.hue / 11}
                        , { name = "Ripple", amount = toFloat model.ripple /11 }
                        , { name = "Noise", amount = toFloat model.noise / 11}
                        ]

                    url = urlPrefix ++ "large/" ++ selectedUrl

                in

                    ( model, setFilters { url = url, filters = filters } )

            Loading ->
            
                ( model, Cmd.none )

            Errored errorMessage ->

                ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
        case status of
            Loaded photos _ ->
                Loaded photos url
            Loading ->
               status 
            Errored errorMessage ->
               status


subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity


init : Float ->(Model ,Cmd Msg)
init flags =
    let
        activity = 
            "Initialiazing Pasta v" ++ String.fromFloat flags

    in
        ( { initialModel | activity = activity }, initialCmd )   

main: Program Float Model Msg
main =
    Browser.element{
        init = init
        ,view = view
        ,update = update
        ,subscriptions = subscriptions
    }