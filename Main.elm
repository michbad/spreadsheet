module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Keyboard
import Task
import Dom

import Grid exposing (..)
-- import Cell exposing (..)
import CellEval exposing (..)
import CellParse exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

-- type alias Cell = {val : String, col : Int, row : Int}

init = (initModel, Cmd.none)

type alias Model = {
  cols: Grid Cell,
  active : (Int, Int),
  editing : Bool,
  offset : (Int, Int),
  showWidth : Int,
  showHeight : Int,
  currentEdit : String
}

initModel = {
  cols = Grid.makeGrid 25 25 (\pos -> emptyCell pos),
  active = (0, 0),
  editing = False,
  offset = (0, 0),
  showWidth = 10,
  showHeight = 10,
  currentEdit = ""
 }



-- UPDATE

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
          Keyboard.presses KeyMsg
        ]

type Msg = Noop | KeyMsg Keyboard.KeyCode | Edit String

posToStr : (Int, Int) -> String
posToStr (r,c) = toString r ++ "," ++ toString c

focusOnCell pos = Task.attempt (always Noop) (Dom.focus (posToStr pos))
blurCell pos = Task.attempt (always Noop) (Dom.blur (posToStr pos))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    -- KeyMsg code -> Debug.crash (toString code)
    Edit str -> ({ model | currentEdit = str}, Cmd.none)
    KeyMsg code ->
      if model.editing then case code of
        27 -> ({ model | editing = False }, blurCell model.active)
        13 ->
          let
            newCell = makeCell model.currentEdit model.active model.cols
            newCols = updateElem model.active newCell model.cols
          in
            ({ model | editing = False, currentEdit = "", cols=newCols }, blurCell model.active)
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
      (offX, offY) = model.offset
      (newX, newY) = (Basics.max (x+dx) 0, Basics.max (y+dy) 0)
      newOffX =
        if newX < offX then
          newX
        else if newX - model.showWidth >= offX then
          newX - model.showWidth + 1
        else
          offX
      newOffY =
        if newY < offY then
          newY
        else if newY - model.showWidth >= offY then
          newY - model.showWidth + 1
        else
          offY
      newCols = extendGrid (newOffX + model.showHeight + 1, newOffY + model.showWidth + 1) (\pos -> emptyCell pos) model.cols
  in  { model | active = (newX, newY), offset = (newOffX, newOffY), cols = newCols }

-- VIEW

activeCellStyle : List (String, String)
activeCellStyle = [
  ("background-color", "light gray"),
  -- ("font-size", "15px"),
  ("border", "1px solid black"),
  ("text-align", "center"),
  ("padding", "0px")
  ]

inactiveCellStyle = [
  ("background-color", "light gray"),
  ("text-align", "center"),
  ("padding", "0px")
    ]



emptyCell (row, col) = {row=row, col=col, val=emptyVal, text="", expr=emptyExpr}

makeCell text (row, col) cols =
  let
    expr = parseExpr text
    exprDefault = Result.withDefault (emptyExpr) expr
    val = evalParsed expr (row, col) cols
  in
    {row=row, col=col, text=text, val=val, expr=exprDefault}

viewCell : Cell -> String
viewCell cell =
  if String.isEmpty cell.text then
    ""
  else
    CellEval.valToString cell.val


drawCell : Model -> Cell -> Html Msg
drawCell model cell =
  let
    pos = (cell.row, cell.col)
    isActive = model.active == pos
  in
  if isActive then
    if model.editing then
      td [width 100, height 50, style activeCellStyle] [
        input [ placeholder cell.text, onInput (always Noop), size 5, width 5, readonly False,
                id (posToStr pos), onInput Edit] []
      ]
    else
      td [width 100, height 50, style activeCellStyle] [
        input [ value (viewCell cell), onInput (always Noop), size 5, width 5, readonly True,
                id (posToStr pos)] []
      ]
  else
    td [width 100, height 50, style inactiveCellStyle] [
      input [ value (viewCell cell), onInput (always Noop), size 5, width 5, readonly True,
               id (posToStr pos) ] []
    ]

drawRow : Model -> Int -> List Cell -> Html Msg
drawRow model rowIdx row =
  let
    header = td [width 50, height 50, style [("text-align", "center")]] [text <| toString rowIdx]
    entries = List.map (drawCell model) row
  in
    tr [] (header :: entries)

drawGrid : Model -> Grid Cell -> Html Msg
drawGrid model cols =
  let
    rows = transpose <| gridToList <| getGridSlice model.offset model.showHeight model.showWidth <| cols
    (rowOffset, colOffset) = model.offset
    header = tr [] <| (td [] []) :: (List.map (\i -> td [style [("text-align", "center")]] [text <| toString i]) (List.range colOffset <| colOffset + model.showWidth - 1))
    rowEntries = List.indexedMap (\i row -> drawRow model (i + rowOffset) row) rows
  in
    table [] <| header :: rowEntries








view : Model -> Html Msg
view model =
  div [] <| [drawGrid model model.cols]


