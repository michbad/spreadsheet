module CellEval exposing (..)

import CellParse exposing (CellExpr(..), ExprListItem(..), ExprList)
import Grid exposing (getElem, Grid)
-- import Cell exposing (..)

import Result exposing (..)

type alias Cell = {val : CellVal, text : String, expr : CellExpr, col : Int, row : Int}
type alias CellVal = Result String Float

eval : CellExpr -> (Int, Int) -> Grid Cell -> CellVal
eval expr (row, col) grid =
  case expr of
    Num n           -> Ok n
    RowNo           -> Ok (toFloat row)
    ColNo           -> Ok (toFloat col)
    Add e1 e2       -> map2 (+) (eval e1 (row, col) grid) (eval e2 (row, col) grid)
    Sub e1 e2       -> map2 (-) (eval e1 (row, col) grid) (eval e2 (row, col) grid)
    Mult e1 e2      -> map2 (*) (eval e1 (row, col) grid) (eval e2 (row, col) grid)
    Div e1 e2       -> map2 (/) (eval e1 (row, col) grid) (eval e2 (row, col) grid)
    FunApp fname es -> Debug.crash "TODO"
    --applyFun fname (es |> map eval) grid
    CellRef r_c c_  -> Debug.crash "TODO"
    -- CellRef r_ c_   -> getCellVal (r_, c_) grid


-- applyFun : String -> List CellVal -> Grid Cell -> CellVal
-- applyFun fname vals grid =
--   Debug.crash "TODO"
--
-- getCellVal (r, c) grid =
--   Ok 0

valToString : CellVal -> String
valToString val = case val of
  Err txt -> "Error: " ++ txt
  Ok num -> toString num

evalParsed expr pos grid = andThen (\e -> eval e pos grid) expr