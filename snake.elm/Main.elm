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
    , snake : Point
    , velocity : Point
    , snake_body : List Point  -- drop 2 [1,2,3,4,5] == [3,4,5]
    , score : Int                 -- score = snake.length
    }


init : (Model, Cmd Msg)
init =
    ({
        apple = Point 15 15
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

applyPhysics : Time -> Model -> Model
applyPhysics dt model =
    let
        wrap : Int -> Int -> Int
        wrap border val =
            if (val < 0) then
                border - 1
            else if (val >= border - 1) then
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

    in
        { model | snake = Point new_x new_y }


keyDown : KeyCode -> Model -> Model
keyDown code model =
    case code of
        37  -> { model | velocity = (Point -1 0) }
        38    -> { model | velocity = (Point 0 -1) }
        39 -> { model | velocity = (Point 1 0) }
        40  -> { model | velocity = (Point 0 1) }
        _ -> model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Tick _ ->
          let
              dt = 1
          in
              ( applyPhysics dt model, Cmd.none )

      KeyDown code ->
          ( keyDown code model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    text (toString model)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (refresh_ms * millisecond) Tick
        , downs KeyDown
        ]
