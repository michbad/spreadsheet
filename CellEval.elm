module CellEval exposing (..)

import CellParse exposing (CellExpr(..), ExprListItem(..), ExprList)
import Grid exposing (getElem, Grid)
import Lazy exposing (lazy, force, Lazy)

import Result exposing (..)

type Cell = Cell {val : AlmostVal, text : String, expr : CellExpr, col : Int, row : Int}
type alias CellVal = Result String Float

type alias AlmostVal = Lazy (Grid Cell -> CellVal)

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

      FunApp fname es -> Debug.crash "TODO"
      --applyFun fname (es |> map eval) grid
      CellRef r_ c_  ->
        if r_ < 0 || c_ < 0 then
          Err "invalid reference"
        else if (r_, c_) == checkPos then
          Err "circular reference"
        else
          let
            refCell = getElem (r_, c_) grid
          in case refCell of
            -- Nothing returned means out of bounds access,
            -- which means the cell must be empty
            Nothing -> (force emptyVal) grid
            Just (Cell cell) ->
              force (evalCheck checkPos cell.expr (cell.row, cell.col)) grid
          --
          -- in
          --   case cellVal of
          --
          --     Nothing -> (force emptyVal) grid
          --     Just val -> val
      -- CellRef r_ c_   -> getCellVal (r_, c_) grid


-- applyFun : String -> List CellVal -> Grid Cell -> CellVal
-- applyFun fname vals grid =
--   Debug.crash "TODO"
--
-- getCellVal (r, c) grid =
--   Ok 0

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