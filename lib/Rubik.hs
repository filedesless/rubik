module Rubik where

data Orientation = F | B | L | R | T | D deriving (Show, Eq)
-- Initial and current orientation
data Face = Face Orientation Orientation deriving (Show, Eq)
class Facefull a where
  faces :: a -> [Face]
  faceMap :: (Face -> Face) -> a -> a

data Edge = Edge Face Face deriving (Show, Eq)
instance Facefull Edge where
  faces (Edge f1 f2) = [f1, f2]
  faceMap f (Edge f1 f2) = Edge (f f1) (f f2)

data Corner = Corner Face Face Face deriving (Show, Eq)
instance Facefull Corner where
  faces (Corner f1 f2 f3) = [f1, f2, f3]
  faceMap f (Corner f1 f2 f3) = Corner (f f1) (f f2) (f f3)

data Cube = Cube [Edge] [Corner] deriving (Show, Eq)
instance Facefull Cube where
  faces (Cube edges corners) = (edges >>= faces) ++ (corners >>= faces)
  faceMap f (Cube edges corners) = Cube (faceMap f <$> edges) (faceMap f <$> corners)

starter :: Cube
starter = Cube
  [  Edge (Face T T) (Face F F)
  ,  Edge (Face T T) (Face L L)
  ,  Edge (Face T T) (Face R R)
  ,  Edge (Face T T) (Face B B)
  ,  Edge (Face L L) (Face F F)
  ,  Edge (Face R R) (Face F F)
  ,  Edge (Face L L) (Face B B)
  ,  Edge (Face R R) (Face B B)
  ,  Edge (Face D D) (Face F F)
  ,  Edge (Face D D) (Face L L)
  ,  Edge (Face D D) (Face R R)
  ,  Edge (Face D D) (Face B B) ]
  [ Corner (Face T T) (Face L L) (Face F F)
  , Corner (Face T T) (Face L L) (Face B B)
  , Corner (Face T T) (Face R R) (Face F F)
  , Corner (Face T T) (Face R R) (Face B B)
  , Corner (Face B B) (Face L L) (Face F F)
  , Corner (Face B B) (Face L L) (Face B B)
  , Corner (Face B B) (Face R R) (Face F F)
  , Corner (Face B B) (Face R R) (Face B B)]

solvedFace :: Face -> Bool
solvedFace (Face initial current) = initial == current

solved :: Cube -> Bool
solved = all solvedFace . faces

facing :: Orientation -> Face -> Bool
facing o (Face _ f) = o == f

opposite :: Orientation -> Orientation
opposite r = case r of
  F -> B; B -> F; T -> D; D -> T; L -> R; R -> L

clockwise :: Orientation -> Orientation -> Orientation
clockwise R r = case r of
  R -> R; L -> L; F -> T; T -> B; B -> D; D -> F
clockwise L r = case r of
  L -> L; R -> R; _ -> opposite (clockwise R r)
clockwise T r = case r of
  T -> T; D -> D; F -> L; L -> B; B -> R; R -> F
clockwise D r = case r of
  T -> T; D -> D; _ -> opposite (clockwise T r)
clockwise F r = case r of
  F -> F; B -> B; T -> R; R -> D; D -> L; L -> T
clockwise B r = case r of
  F -> F; B -> B; _ -> opposite (clockwise F r)

anticlockwise :: Orientation -> Orientation -> Orientation
anticlockwise axis r
  | axis == r = r
  | axis == opposite r = r
  | otherwise = opposite (clockwise axis r)

rotateFace :: (Orientation -> Orientation) -> Face -> Face
rotateFace f face@(Face initial current) = Face initial (f current)

rotate :: Orientation -> (Orientation -> Orientation -> Orientation) -> Cube -> Cube
rotate r f (Cube edges corners) = Cube (rotateFaces <$> edges) (rotateFaces <$> corners)
  where
    rotateFaces block
      | any (facing r) (faces block) = faceMap (rotateFace (f r)) block
      | otherwise = block

rotateN :: Int -> Orientation -> (Orientation -> Orientation -> Orientation) -> Cube -> Cube
rotateN 0 _ _ cube = cube
rotateN n r f cube = rotateN (n - 1) r f (rotate r f cube)

parse :: Char -> Orientation
parse 'R' = R
parse 'L' = L
parse 'T' = T
parse 'D' = D
parse 'F' = F
parse 'B' = B

executeAlg :: String -> Cube -> Cube
executeAlg [] cube         = cube
executeAlg [c] cube        = rotate (parse c) clockwise cube
executeAlg (c:'\'':t) cube = executeAlg t $ rotate (parse c) anticlockwise cube
executeAlg (c:t) cube      = executeAlg t $ rotate (parse c) clockwise cube

executeAlgN :: Int -> String -> Cube -> Cube
executeAlgN 0 _ cube = cube
executeAlgN n s cube = executeAlgN (n - 1) s (executeAlg s cube)
