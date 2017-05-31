module Grid exposing (makeGrid, transpose, updateElem, getElem, gridToList, getGridSlice, extendGrid, Grid)
-- import Array as A
import Array.Hamt as A

type alias Grid a = A.Array (A.Array a)

makeGrid : Int -> Int -> ((Int, Int) -> a) -> Grid a
makeGrid nRows nCols fillRowCol =
  A.initialize nCols (\c -> (A.initialize nRows (\r -> fillRowCol (r,c))))

transpose : List (List a) -> List (List a)
transpose ls =
  let
    first = List.filterMap (List.head) ls
    rest = List.filterMap (List.tail) ls
  in
    case List.head rest |> Maybe.andThen (List.head) of
      Nothing -> [first]
      Just _ -> first :: transpose rest

updateElem : (Int, Int) -> a -> Grid a -> Grid a
updateElem (r, c) val cols =
  let
    col = A.get c cols |> Maybe.withDefault A.empty
    newCol = A.set r val col
  in
    A.set c newCol cols

getElem : (Int, Int) -> Grid a -> Maybe a
getElem (r, c) cols =
  A.get c cols |> Maybe.andThen (A.get r)


gridToList : A.Array (A.Array a) -> List (List a)
gridToList cols = A.toList <| A.map (A.toList) cols

getGridSlice : (Int,Int) -> Int -> Int -> Grid a -> Grid a
getGridSlice (h0, w0) h w cols =
  A.slice w0 (w0+w) cols |> A.map (A.slice h0 (h0+h))

extendGrid : (Int, Int) -> ((Int, Int) -> a) -> Grid a -> Grid a
extendGrid (targetH0, targetW0) fillByRowCol cols =
  let
    curW = A.length cols
    curH = A.get 0 cols |> Maybe.map A.length |> Maybe.withDefault 0
    targetW = Basics.max targetW0 curW
    targetH = Basics.max targetH0 curH
    longerCols = A.indexedMap (\c col ->
        ensureColLen c targetH (\r -> fillByRowCol (r, c)) col
      ) cols
    moreCols =
      if curW < targetW then
        A.initialize (targetW - curW) (\c -> ensureColLen (curW+c) targetH (\r -> fillByRowCol (r, curW+c)) A.empty)
      else
        A.empty
  in
    A.append longerCols moreCols

ensureColLen : Int -> Int -> (Int -> a) -> A.Array a -> A.Array a
ensureColLen colIdx targetLen fillFunc col =
  let _ = Debug.log "colIdx, targetLen" (colIdx, targetLen) in
  if A.length col >= targetLen then
    col
  else let
    rowOffset = A.length col
    toAppend = A.initialize (targetLen - A.length col) (\r -> fillFunc (r + rowOffset))
  in
    let _ = Debug.log "after append" (A.length <| A.append col toAppend) in
    A.append col toAppend
