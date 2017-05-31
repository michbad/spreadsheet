module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Keyboard
import Task
import Dom
import Window

import Grid exposing (..)
import CellEval exposing (..)
import CellParse exposing (parseExpr, emptyExpr)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

init = (initModel, getSize)

type alias Model = {
  cols: Grid Cell,
  active : (Int, Int),
  editing : Bool,
  offset : (Int, Int),
  showWidth : Int,
  showHeight : Int,
  currentEdit : String,
  cellWidth : Int,
  cellHeight : Int
}

initModel = {
  cols = Grid.makeGrid 25 25 (\pos -> emptyCell pos),
  active = (0, 0),
  editing = False,
  offset = (0, 0),
  showWidth = 10,
  showHeight = 10,
  currentEdit = "",
  cellWidth = 100,
  cellHeight = 50
 }

-- UPDATE

type Msg = Noop | KeyMsg Keyboard.KeyCode | Edit String | NewSize Window.Size

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
          Keyboard.presses KeyMsg,
          Window.resizes NewSize
        ]

posToStr : (Int, Int) -> String
posToStr (r,c) = toString r ++ "," ++ toString c

focusOnCell pos = Task.attempt (always Noop) (Dom.focus (posToStr pos))
blurCell pos = Task.attempt (always Noop) (Dom.blur (posToStr pos))

getSize = Task.perform NewSize Window.size

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    NewSize sz -> (resizeModel sz model, Cmd.none)
    Edit str -> ({ model | currentEdit = str}, Cmd.none)
    KeyMsg code ->
      if model.editing then case code of
        27 -> ({ model | editing = False }, blurCell model.active)
        13 ->
          let
            newCell = makeCell model.currentEdit model.active model.cols
            newCols = updateElem model.active newCell model.cols
          in
            ({ model | editing = False, currentEdit = "", cols=newCols },blurCell model.active)
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
      newCols = extendGrid
                (newOffX + model.showHeight + 1, newOffY + model.showWidth + 1)
                (\pos -> emptyCell pos) model.cols
  in { model | active = (newX, newY), offset = (newOffX, newOffY), cols = newCols }

emptyCell (row, col) =
  Cell {row=row, col=col, val=emptyVal, text="", expr=emptyExpr}

makeCell text (row, col) cols =
  let
    expr = parseExpr text
    exprDefault = Result.withDefault (emptyExpr) expr
    val = evalParsed expr (row, col)
  in
    Cell {row=row, col=col, text=text, val=val, expr=exprDefault}

-- VIEW

resizeModel sz model =
  let
    newShowWidth =  (round <| 0.9 * toFloat sz.width) // model.cellWidth
    newShowHeight = (round <| 0.9 * toFloat sz.height) // model.cellHeight
  in
   { model | showWidth = newShowWidth, showHeight = newShowHeight}

tableStyle = [("border-collapse", "collapse")]

activeCellStyle : List (String, String)
activeCellStyle = [
  ("background-color", "white"),
  ("border", "2px solid green"),
  ("padding", "0px"),
  ("text-align", "center")
  ]

inactiveCellStyle = [
  ("background-color", "white"),
  ("text-align", "center"),
  ("padding", "0px"),
  ("border", "1px solid gray"),
  ("border-collapse", "collapse")
    ]

textboxStyle = [
  ("background-color", "white"),
  ("border", "0px solid"),
  ("text-align", "center"),
  ("font-size", "15px"),
  ("font-family", "sans-serif")
  ]



viewCell : Cell -> Grid Cell -> String
viewCell (Cell cell) grid =
  if String.isEmpty cell.text then
    ""
  else
    CellEval.valToString cell.val grid


drawCell : Model -> Cell -> Html Msg
drawCell model (Cell cellBody as cell) =
  let
    pos = (cellBody.row, cellBody.col)
    isActive = model.active == pos
    grid = model.cols
  in
  if isActive then
    if model.editing then
      td [width model.cellWidth, height model.cellHeight, style activeCellStyle]
      [
        input [ placeholder cellBody.text, onInput (always Noop), size 5, width 5, readonly False,
                id (posToStr pos), onInput Edit, style textboxStyle ] []
      ]
    else
      td [width model.cellWidth, height model.cellHeight, style activeCellStyle] [
        input [ value (viewCell cell grid), onInput (always Noop), size 5, width 5, readonly True,
                id (posToStr pos), style textboxStyle] []
      ]
  else
    td [width model.cellWidth, height model.cellHeight, style inactiveCellStyle] [
      input [ value (viewCell cell grid), onInput (always Noop), size 5, width 5, readonly True,
               id (posToStr pos), style textboxStyle ] []
    ]

drawRow : Model -> Int -> List Cell -> Html Msg
drawRow model rowIdx row =
  let
    header = td [width (model.cellWidth // 2), height model.cellHeight, style [("text-align", "center")]]
                [text <| toString rowIdx]
    entries = List.map (drawCell model) row
  in
    tr [] (header :: entries)

drawGrid : Model -> Grid Cell -> Html Msg
drawGrid model cols =
  let
    rows = transpose <| gridToList <| getGridSlice model.offset model.showHeight model.showWidth <| cols
    (rowOffset, colOffset) = model.offset
    header = tr [] <|
      (td [] []) :: (
        List.map
        (\i -> td [style [("text-align", "center")]] [text <| toString i])
        (List.range colOffset <| colOffset + model.showWidth - 1)
      )
    rowEntries = List.indexedMap (\i row -> drawRow model (i + rowOffset) row) rows
  in
    table [style tableStyle] <| header :: rowEntries

view : Model -> Html Msg
view model =
  div [] <| [drawGrid model model.cols]


