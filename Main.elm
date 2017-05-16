module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Array as A

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


-- MODEL

type alias Cell = {val : String, col : Int, row : Int}

init = (initModel, Cmd.none)

initModel = {
  cols = A.initialize 10 (\c -> (A.initialize 10 (\r -> {col=c, row=r, val= toString (r, c)}))),
  editing = Just (2,3)
 }

type alias Model = { cols: A.Array (A.Array Cell), editing : Maybe (Int, Int) }


-- UPDATE

type Msg = Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)


-- VIEW

activeCellStyle : List (String, String)
activeCellStyle = [
  ("background-color", "pink"),
  ("font-size", "14px")
  ]

inactiveCellStyle = [
  ("background-color", "light gray")
    ]

transpose : List (List Cell) -> List (List Cell)
transpose ls =
  let
    first = List.filterMap (List.head) ls
    rest = List.filterMap (List.tail) ls
  in
    case List.head rest |> Maybe.andThen (List.head) of
      Nothing -> [first]
      Just _ -> first :: transpose rest

gridToList : A.Array (A.Array Cell) -> List (List Cell)
gridToList cols = A.toList <| A.map (A.toList) cols

drawCell : Maybe (Int, Int) -> Cell -> Html Msg
drawCell mactive cell =
  let active = case mactive of
    Nothing -> False
    Just (r,c) -> (r,c) == (cell.row, cell.col)
  in
  if active then
    td [width 100, height 50] [
      input [ defaultValue cell.val, onInput (always Noop), size 5, width 5, readonly False,
              style activeCellStyle] []
    ]
  else
    td [width 100, height 50] [
      input [ defaultValue cell.val, onInput (always Noop), size 5, width 5, readonly True,
              style inactiveCellStyle ] []
      -- text cell.val
    ]
    -- text cell.val

drawRow : Maybe (Int, Int) -> List Cell -> Html Msg
drawRow mactive row = tr [] <| List.map (drawCell mactive) row

drawGrid : Maybe (Int, Int) -> A.Array (A.Array Cell) -> Html Msg
drawGrid mactive cols =
  let rows = transpose <| gridToList <| cols in
  table [] <| List.map (drawRow mactive) rows


view : Model -> Html Msg
view model =
  div [] <| [drawGrid model.editing model.cols]


