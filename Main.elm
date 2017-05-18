module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Keyboard
import Task
import Dom

-- import Array as A
import Array.Hamt as A

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
type alias Grid = A.Array (A.Array Cell)

init = (initModel, Cmd.none)

type alias Model = {
  cols: A.Array (A.Array Cell),
  active : (Int, Int),
  editing : Bool,
  offset : (Int, Int),
  showWidth : Int,
  showHeight : Int,
  currentEdit : String
}

initModel = {
  cols = A.initialize 12 (\c -> (A.initialize 10 (\r -> {col=c, row=r, val= toString (r, c)}))),
  active = (2,3),
  editing = False,
  offset = (2, 2),
  showWidth = 5,
  showHeight = 5,
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    -- KeyMsg code -> Debug.crash (toString code)
    Edit str -> ({ model | currentEdit = str}, Cmd.none)
    KeyMsg code ->
      if model.editing then case code of
        27 -> ({ model | editing = False }, Cmd.none)
        13 ->
          let
            newCols = updateCell model.cols model.active model.currentEdit
          in
            ({ model | editing = False, currentEdit = "", cols=newCols }, Cmd.none)
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
      newCols = extendGrid (newOffX + model.showHeight + 1, newOffY + model.showWidth + 1) model.cols
  in  { model | active = (newX, newY), offset = (newOffX, newOffY), cols = newCols }

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

emptyCell row col = {row=row, col=col, val=""}

updateCell : Grid -> (Int, Int) -> String -> Grid
updateCell cols (r, c) val =
  let
    col = A.get c cols |> Maybe.withDefault A.empty
    newCol = A.set r {row=r, col=c, val=val} col
  in
    A.set c newCol cols


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
                style activeCellStyle, id (posToStr pos), onInput Edit] []
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

extendGrid : (Int, Int) -> Grid -> Grid
extendGrid (targetH0, targetW0) cols =
  let
    curW = A.length cols
    curH = A.get 0 cols |> Maybe.map A.length |> Maybe.withDefault 0
    targetW = Basics.max targetW0 curW
    targetH = Basics.max targetH0 curH
    -- longerCols =
    --   if curH < targetH then
    --     A.indexedMap (\i col -> extendCol i (targetH - curH) col) cols
    --   else
    --     cols
    longerCols = A.indexedMap (\i col -> ensureColLen i targetH col) cols
    moreCols =
      if curW < targetW then
        A.initialize (targetW - curW) (\i -> ensureColLen (curW+i) targetH A.empty)
      else
        A.empty
  in
    A.append cols moreCols


ensureColLen : Int -> Int -> A.Array Cell -> A.Array Cell
ensureColLen colIdx targetLen col =
  let _ = Debug.log "colIdx, targetLen" (colIdx, targetLen) in
  if A.length col >= targetLen then
    col
  else let
    rowOffset = A.length col
    toAppend = A.initialize (targetLen - A.length col) (\r -> emptyCell (r+rowOffset) colIdx)
  in
    let _ = Debug.log "after append" (A.length <| A.append col toAppend) in
    A.append col toAppend

view : Model -> Html Msg
view model =
  -- text "lol"
  div [] <| [drawGrid model model.cols]


