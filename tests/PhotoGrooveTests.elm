module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr exposing (src)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Model, Msg(..), Photo, Status(..), initialModel, photoDecoder, update, urlPrefix, view)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (unititled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")


sliders : Test
sliders =
    describe "slider sets the desired field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "no thumbnails render when there are no photos to rendner" <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailsWork : Test
thumbnailsWork =
    fuzz urlFuzzer "URLS render as thumnails" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")


clickThumbnail : Test
clickThumbnail =
    describe "selecting thumbnails"
        [ fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
            \urlsBefore urlToSelect urlsAfter ->
                let
                    url =
                        urlToSelect ++ ".jpeg"

                    photos =
                        (urlsBefore ++ url :: urlsAfter)
                            |> List.map photoFromUrl

                    srcToClick =
                        urlPrefix ++ url
                in
                { initialModel | status = Loaded photos "" }
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                    |> Event.simulate Event.click
                    |> Event.expect (ClickedPhoto url)
        , test "selected photos are recognized as such in state" <|
            \_ ->
                let
                    loadedPhotos =
                        List.map photoFromUrl [ "1.jpeg", "2.jpeg" ]
                in
                { initialModel | status = Loaded loadedPhotos "1.jpeg" }
                    |> update (ClickedPhoto "2.jpeg")
                    |> Tuple.first
                    |> .status
                    |> Expect.equal (Loaded loadedPhotos "2.jpeg")
        ]
