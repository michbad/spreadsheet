module CellParse exposing ( .. )

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
  | FunApp String ExprList

type alias ExprList = List ExprListItem
type ExprListItem = Single CellExpr | CellRange (Int, Int) (Int, Int)

-- type FunApp = FunApp String ExprList

spaced parser = whitespace *> parser <* whitespace

spacedstring s = whitespace *> string s <* whitespace

addop : Parser s (CellExpr -> CellExpr -> CellExpr)
addop = choice [ Add <$ string "+"
               , Sub <$ string "-"
               ]

mulop : Parser s (CellExpr -> CellExpr -> CellExpr)
mulop = choice [ Mult <$ string "*"
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
  spaced (
    lazy (\_ ->parens expr) <|> num <|> cellref <|> lazy (\_ ->funapp) <|>
     empty <|> rowno <|> colno
    )

num = map Num (float <|> map toFloat int)

cellref =
  CellRef
    <$> (string "[" *> int <* string ",")
    <*> (int <* string "]")

cellrange =
  CellRange
    <$> ( (,) <$> (spacedstring "[" *> int <* spacedstring ":") <*> int )
    <*> ( (,) <$> (spacedstring "," *> int <* spacedstring ":") <*> int <* spacedstring "]" )

rowno = string "#r" *> succeed RowNo

colno = string "#c" *> succeed ColNo

exprlistitem = cellrange <|> (Single <$> lazy (\_ -> expr))

exprlist = sepBy (spacedstring ",") (lazy (\_ -> exprlistitem))

funapp = FunApp <$> regex "[a-zA-Z]+" <*> parens (lazy (\_ -> exprlist))

empty = end *> succeed emptyExpr

parseExpr : String -> Result String CellExpr
parseExpr s =
  case parse (expr <* end) s of
    Ok (_, _, n) ->
      Ok n

    Err (_, stream, ms) ->
      Err "parse error"

emptyExpr = Num 0

