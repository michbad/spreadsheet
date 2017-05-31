module CellEval exposing (valToString, evalParsed, emptyVal, Cell(..))

import CellParse exposing (CellExpr(..), ExprListItem(..), ExprList)
import Grid exposing (getElem, Grid)
import Lazy exposing (lazy, force, Lazy)

import Result exposing (..)
import Result.Extra exposing (combine)

type Cell = Cell {val : AlmostVal, text : String, expr : CellExpr, col : Int, row : Int}
type alias CellVal = Result String Float

type alias AlmostVal = Lazy (Grid Cell -> CellVal)


valToString : AlmostVal -> Grid Cell -> String
valToString val grid = case (force val) grid of
  Err txt -> "Error: " ++ txt
  Ok num -> toString num

evalParsed : Result String CellExpr -> (Int, Int) -> AlmostVal
evalParsed parsed pos =
  case parsed of
    Err msg -> lazy <| \_ -> always (Err msg)
    Ok tree -> eval tree pos

emptyVal = lazy <| \_ -> always (Ok 0)

eval : CellExpr -> (Int, Int) -> AlmostVal
eval expr pos =
  evalCheck pos expr pos

-- checkPos is the position of the cell we're initially calculating,
-- to avoid circular references
evalCheck : (Int, Int) -> CellExpr -> (Int, Int) -> AlmostVal
evalCheck checkPos expr (row, col) =
  lazy <| \_ -> \grid ->
    case expr of
      Num n           -> Ok n
      RowNo           -> Ok (toFloat row)
      ColNo           -> Ok (toFloat col)
      Add e1 e2       -> map2 (+)
        ((force <| evalCheck checkPos e1 (row, col)) grid)
        ((force <| evalCheck checkPos e2 (row, col)) grid)

      Sub e1 e2       -> map2 (-)
        ((force <| evalCheck checkPos e1 (row, col)) grid)
        ((force <| evalCheck checkPos e2 (row, col)) grid)

      Mult e1 e2      -> map2 (*)
        ((force <| evalCheck checkPos e1 (row, col)) grid)
        ((force <| evalCheck checkPos e2 (row, col)) grid)

      Div e1 e2       -> map2 (/)
        ((force <| evalCheck checkPos e1 (row, col)) grid)
        ((force <| evalCheck checkPos e2 (row, col)) grid)

      CellRef er ec ->
        let
          vr = (force <| evalCheck checkPos er (row, col)) grid
          vc = (force <| evalCheck checkPos ec (row, col)) grid
        in
          case (vr, vc) of
            (Err msg, _) -> Err msg
            (_, Err msg) -> Err msg
            (Ok r, Ok c) ->
              if r < 0 || c < 0 || not (isInt r) || not (isInt c) then
                Err "invalid reference"
              else if (round r, round c) == checkPos then
                Err "circular reference"
              else
                let
                  refCell = getElem (round r, round c) grid
                in case refCell of
                  -- Nothing returned means out of bounds access,
                  -- which means the cell must be empty
                  Nothing -> (force emptyVal) grid
                  Just (Cell cell) ->
                    -- This unfortunately recomputes the child values
                    force (evalCheck checkPos cell.expr (cell.row, cell.col)) grid

      FunApp fname eitems ->
        case rangeItemsToExps eitems of
          Err msg -> Err msg
          Ok exps ->
            let
              vals = List.map (\e -> (force <| evalCheck checkPos e (row, col)) grid) exps
            in
              applyFun fname vals

rangeItemsToExps : ExprList -> Result String (List CellExpr)
rangeItemsToExps items =
  let
    fun item = case item of
      Single e -> Ok [e]
      CellRange rowRange colRange ->
        refsFromRange rowRange colRange
  in
    Result.map List.concat <| combine <| List.map fun items

posToRef : (Int, Int) -> Result String CellExpr
posToRef (r, c) =
  if r < 0 || c < 0 then
    Err "invalid reference"
  else
    Ok <| CellRef (Num (toFloat r)) (Num (toFloat c))

refsFromRange : (Int, Int) -> (Int, Int) -> Result String (List CellExpr)
refsFromRange (startr, endr) (startc, endc) =
  if startr > endr || startc > endc then
    Err "bad range"
  else
    combine <|
      List.concatMap
      (\r -> List.map (\c -> posToRef (r,c)) (List.range startc endc))
      (List.range startr endr)


applyFun : String -> List CellVal -> CellVal
applyFun fname vals =
  let
    fun_ = getFun fname
    nums_ = combine vals
  in case (fun_, nums_) of
    (Err msg, _)      -> Err msg
    (_, Err msg)      -> Err msg
    (Ok fun, Ok nums) -> Ok (fun nums)

getFun : String -> Result String (List Float -> Float)
getFun str =
  case str of
    "sum" -> Ok List.sum
    "min" -> Ok (\lst -> List.minimum lst |> Maybe.withDefault 0)
    "max" -> Ok (\lst -> List.maximum lst |> Maybe.withDefault 0)
    _     -> Err "invalid function name"


isInt : Float -> Bool
isInt x = toFloat (round x) == x

