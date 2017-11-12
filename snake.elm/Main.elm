module Main exposing (..)



import Html exposing (Html, text)

import Random
import Svg
import Svg.Attributes as SvgA

import Keyboard exposing (KeyCode, downs)
import AnimationFrame exposing (diffs)
import Time exposing (Time, millisecond)



main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

pixel = 20
world = Point 32 18
refresh_ms = 1000/15

-- MODEL
type alias Point =
    { x : Int
    , y : Int
    }

type alias Model =
    { apple : Point
    , nextApple : Point
    , snake : Point
    , velocity : Point
    , snake_body : List Point  -- drop 2 [1,2,3,4,5] == [3,4,5]
    , score : Int                 -- score = snake.length
    }


init : (Model, Cmd Msg)
init =
    ({
        apple = Point 15 15
      , nextApple = Point 0 0
      , snake = Point 16 9 -- (floor world.x/2) (floor world.y/2)
      , velocity = Point 0 0
      , snake_body = []
      , score = 5
      }
      , Cmd.none)



-- UPDATE
type Msg
    = Tick Time
    | KeyDown KeyCode
    | Apple Point

applyPhysics : Time -> Model -> Model
applyPhysics dt model =
    let
        wrap border val =
            if (val < 0) then
                border - 1
            else if (val > border - 1) then
                0
            else
                val

        new_x =
                model.velocity.x
                |> toFloat
                |> (*) dt
                |> floor
                |> (+) model.snake.x
                |> wrap world.x

        new_y =
                model.velocity.y
                |> toFloat
                |> (*) dt
                |> floor
                |> (+) model.snake.y
                |> wrap world.y

        new_body
            = model.snake_body
            |> (::) (Point new_x new_y)
            |> List.take model.score

        hit = if (new_x == model.apple.x && new_y == model.apple.y) then 1
              else 0

    in
        { model |
             snake = Point new_x new_y,
             snake_body = new_body,
             score = model.score + hit,
             apple = if hit == 1 then model.nextApple else model.apple
         }


keyDown : KeyCode -> Model -> Model
keyDown code model =
    case code of
        37 -> { model | velocity = (Point -1 0) }
        38 -> { model | velocity = (Point 0 -1) }
        39 -> { model | velocity = (Point 1 0) }
        40 -> { model | velocity = (Point 0 1) }
        _ -> model


randomPoint : Random.Generator Point
randomPoint =
    Random.map2 Point (Random.int 0 (world.x-1)) (Random.int 0 (world.y-1))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Tick _ ->
          let
              dt = 1
          in
              ( applyPhysics dt model, Random.generate Apple randomPoint )

      Apple point ->
          ({ model | nextApple = point }, Cmd.none)

      KeyDown code ->
          ( keyDown code model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    -- text (toString model)
    render model




-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (refresh_ms * millisecond) Tick
        , downs KeyDown
        ]


render : Model -> Html msg
render model  =
    let
        renderPixel : String -> Point -> Html msg
        renderPixel col point =
            Svg.rect
                  [ SvgA.x (point.x |> (*) pixel |> toString)
                  , SvgA.y (point.y |> (*) pixel |> toString)
                  , SvgA.width "18"
                  , SvgA.height "18"
                  , SvgA.fill col
                  ]
                  []

        background =
            Svg.rect
                [ SvgA.width "640"
                , SvgA.height "360"
                , SvgA.fill "black"]
                []

        apple = renderPixel "red" model.apple

        snake_body = model.snake_body
                   |>  List.map (renderPixel "green")

        result = (List.singleton apple)
               |> List.append snake_body
               |> List.append (List.singleton background)


    in
        Svg.svg
            [ SvgA.width "640"
            , SvgA.height  "360"
            ]
            result
