module Cell exposing ( .. )

-- Note: Part of the code responsible for arithmetic expressions
-- is taken from an elm-combine library example.

import Combine exposing (..)
import Combine.Num exposing (float, int)

type CellExpr =
  Num Float
  | RowNo
  | ColNo
  | CellRef Int Int
  | Add CellExpr CellExpr
  | Sub CellExpr CellExpr
  | Mult CellExpr CellExpr
  | Div CellExpr CellExpr
  | Power CellExpr CellExpr


addop : Parser s (CellExpr -> CellExpr -> CellExpr)
addop = choice [ Add <$ string "+"
               , Sub <$ string "-"
               ]

mulop : Parser s (CellExpr -> CellExpr -> CellExpr)
mulop = choice [ Mult  <$ string "*"
               , Div <$ string "/"
               ]

expr : Parser s CellExpr
expr =
  let
    go () =
      chainl addop term
  in
    lazy go

term : Parser s CellExpr
term =
  let
    go () =
      chainl mulop factor
  in
    lazy go

factor : Parser s CellExpr
factor =
  whitespace *> (parens expr <|> num <|> cellref) <* whitespace

num = map Num (float <|> map toFloat int)

cellref =
  CellRef
    <$> (string "#" *> int <* string ",")
    <*> int

{-| Compute the result of an expression. -}
calc : String -> Result String CellExpr
calc s =
  case parse (expr <* end) s of
    Ok (_, _, n) ->
      Ok n

    Err (_, stream, ms) ->
      Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString stream))
