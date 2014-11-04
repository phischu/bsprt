module Main where

import Graphics.Gloss.Raster.Field (
    playField,Display(InWindow),Color,red,white)
import Graphics.Gloss.Interface.Pure.Game (Event(EventMotion))

import Linear (V3(V3),dot,(^-^),(^+^),(*^),cross)

import Data.Maybe (catMaybes,listToMaybe,fromMaybe)

type Point = V3 Float
type Direction = V3 Float
type Normal = V3 Float
type T = Float

data Tree b a = Leaf a | Branch (Tree b a) b (Tree b a)
    deriving (Show)

data Plane = Plane Point Normal
    deriving (Show)

data Ray = Ray Point Direction
    deriving (Show)

type BSPTree a = Tree Plane a

distance :: Point -> Plane -> Float
distance rayposition (Plane planeposition planenormal) = (planeposition ^-^ rayposition) `dot` planenormal

intersections :: Ray -> BSPTree a -> Tree T a
intersections _ (Leaf a) = Leaf a
intersections ray (Branch l plane r) = Branch leftintersections t righintersections where
    Ray rayposition raydirection = ray
    Plane _ planenormal = plane
    (leftintersections,righintersections) = if raydirection `dot` planenormal > 0
        then (intersections ray l,intersections ray r)
        else (intersections ray r,intersections ray l)
    t = distance rayposition plane / raydirection `dot` planenormal

hits :: T -> T -> Tree T a -> [a]
hits _ _      (Leaf a) = [a]
hits near far (Branch l t' r)
    | t' < near = []
    | t' > far  = []
    | otherwise = hits near t' l ++ hits t' far r

box :: BSPTree (Maybe Color)
box = Branch (Leaf Nothing) (Plane (V3 (-1) 0 0) (V3 1 0 0)) (
      Branch (Leaf Nothing) (Plane (V3 1 0 0) (V3 (-1) 0 0)) (
      Branch (Leaf Nothing) (Plane (V3 0 (-1) 0) (V3 0 1 0)) (
      Branch (Leaf Nothing) (Plane (V3 0 1 0) (V3 0 (-1) 0)) (
      Branch (Leaf Nothing) (Plane (V3 0 0 (-1)) (V3 0 0 1)) (
      Branch (Leaf Nothing) (Plane (V3 0 0 1) (V3 0 0 (-1))) (
      Leaf (Just red)))))))

trace :: Ray -> BSPTree (Maybe Color) -> Maybe Color
trace ray = listToMaybe . catMaybes . hits 0 1000 . intersections ray

render :: Camera -> BSPTree (Maybe Color) -> (Float,Float) -> Color
render camera tree (x,y) = fromMaybe white (trace (cameraRay camera x y) tree)

updateCamera :: Event -> Camera -> Camera
updateCamera (EventMotion (x,y)) (Camera cameraposition theta phi) =
    Camera cameraposition theta'' phi'' where
        theta' = theta + 0.0001 * y
        theta'' = if theta' > 1.4 then 1.4 else if theta' < -1.4 then -1.4 else theta'
        phi' = phi + 0.0001 * x
        phi'' = if phi' > pi then phi' - 2*pi else if phi' < -pi then phi' + 2*pi else phi'
updateCamera _ camera = camera

main :: IO ()
main = playField
    (InWindow "BSPRT" (600,600) (300,300))
    (10,10)
    50
    testCamera
    (\camera (x,y) -> render camera box (x,y))
    updateCamera
    (const id)

testCamera :: Camera
testCamera = Camera (V3 1 2 2) (-1) 0

type Theta = Float
type Phi = Float

data Camera = Camera Point Theta Phi
    deriving (Show)

cameraRay :: Camera -> Float -> Float -> Ray
cameraRay camera@(Camera cameraposition _ _) x y =
    Ray cameraposition (cameraForward camera ^+^ (x *^ cameraRight camera) ^+^ (y *^ cameraUp camera))

cameraForward :: Camera -> Direction
cameraForward (Camera _ theta phi) = V3 (sin phi * sin theta) (sin theta) (cos phi * sin theta)

cameraRight :: Camera -> Direction
cameraRight camera = cameraForward camera `cross` (V3 0 1 0)

cameraUp :: Camera -> Direction
cameraUp camera = cameraRight camera `cross` cameraForward camera





