module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Keyboard
import Task
import Dom

import Array as A

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

type alias Cell = {val : String, col : Int, row : Int}

init = (initModel, Cmd.none)

type alias Model = {
  cols: A.Array (A.Array Cell),
  active : (Int, Int),
  editing : Bool,
  offset : (Int, Int),
  showWidth : Int,
  showHeight : Int
}

initModel = {
  cols = A.initialize 10 (\c -> (A.initialize 10 (\r -> {col=c, row=r, val= toString (r, c)}))),
  active = (2,3),
  editing = False,
  offset = (0, 0),
  showWidth = 8,
  showHeight = 10
 }



-- UPDATE

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
          Keyboard.presses KeyMsg
        ]

type Msg = Noop | KeyMsg Keyboard.KeyCode

posToStr : (Int, Int) -> String
posToStr (r,c) = toString r ++ "," ++ toString c

focusOnCell pos = Task.attempt (always Noop) (Dom.focus (posToStr pos))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    -- KeyMsg code -> Debug.crash (toString code)
    KeyMsg code ->
      if model.editing then case code of
        27 -> ({ model | editing = False }, Cmd.none)
        -- 13 -> ({ model | editing = False } |> updateCurrentCell, Cmd.none) Here it should be in onClick of field?
        -- todo have additional currentEdit field, when enter pressed while editing, then set value of current field
        -- to currentEditing
        _  -> (model, Cmd.none)
      else
        case code of
          13 -> ({ model | editing = True }, focusOnCell model.active)
          37 -> (moveActive model (0, -1), Cmd.none)
          38 -> (moveActive model (-1, 0), Cmd.none)
          39 -> (moveActive model (0, 1), Cmd.none)
          40 -> (moveActive model (1, 0), Cmd.none)
          _  -> (model, Cmd.none)

moveActive : Model -> (Int, Int) -> Model
moveActive model (dx, dy) =
  let (x,y) = model.active
  in  { model | active = (x+dx, y+dy) }

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

drawCell : Model -> Cell -> Html Msg
drawCell model cell =
  let
    pos = (cell.row, cell.col)
    isActive = model.active == pos
  in
  if isActive then
    if model.editing then
      td [width 100, height 50] [
        input [ defaultValue cell.val, onInput (always Noop), size 5, width 5, readonly False,
                style activeCellStyle, id (posToStr pos)] []
      ]
    else
      td [width 100, height 50] [
        input [ value cell.val, onInput (always Noop), size 5, width 5, readonly True,
                style activeCellStyle, id (posToStr pos)] []
      ]
  else
    td [width 100, height 50] [
      input [ value cell.val, onInput (always Noop), size 5, width 5, readonly True,
              style inactiveCellStyle, id (posToStr pos) ] []
    ]

drawRow : Model -> List Cell -> Html Msg
drawRow model row = tr [] <| List.map (drawCell model) row

drawGrid : Model -> A.Array (A.Array Cell) -> Html Msg
drawGrid model cols =
  let rows = transpose <| gridToList <| getGridSlice model.offset model.showHeight model.showWidth <| cols in
  table [] <| List.map (drawRow model) rows

getGridSlice : (Int,Int) -> Int -> Int -> A.Array (A.Array Cell) -> A.Array (A.Array Cell)
getGridSlice (h0, w0) h w cols =
  A.slice w0 (w0+w) cols |> A.map (A.slice h0 (h0+h))

view : Model -> Html Msg
view model =
  -- text "lol"
  div [] <| [drawGrid model model.cols]


