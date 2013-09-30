module Main where

import Graphics.Gloss.Raster.Field (
    playField,Display(InWindow),Color,red,white)
import Graphics.Gloss.Interface.Pure.Game (Event(EventMotion))

import Linear (V3(V3),dot,negated,(^-^),(^+^),(*^),cross)

import Data.Maybe (catMaybes,listToMaybe,fromMaybe)

type Point = V3 Float
type Direction = V3 Float
type Normal = V3 Float
type T = Float

data Tree b a = Leaf a | Branch (Tree b a) b (Tree b a)

data Plane = Plane Point Normal

data Ray = Ray Point Direction

type BSPTree a = Tree Plane a

type Theta = Float
type Phi = Float
data Camera = Camera Point Theta Phi

cameraForward :: Camera -> Direction
cameraForward (Camera _ theta phi) = V3 (sin phi * sin theta) (sin theta) (cos phi * sin theta)

cameraRight :: Camera -> Direction
cameraRight camera = cameraForward camera `cross` (V3 0 1 0)

cameraUp :: Camera -> Direction
cameraUp camera = cameraRight camera `cross` cameraForward camera

cameraRay :: Camera -> Float -> Float -> Ray
cameraRay camera@(Camera cameraposition _ _) x y =
    Ray cameraposition (cameraForward camera ^+^ (x *^ cameraRight camera) ^+^ (y *^ cameraUp camera))

distance :: Point -> Plane -> Float
distance point (Plane planeposition planenormal) = (point ^-^ planeposition) `dot` planenormal

align :: Ray -> BSPTree a -> BSPTree a
align _ (Leaf a) = Leaf a
align (Ray _ raydirection) (Branch l (Plane planeposition planenormal) r)
    | raydirection `dot` planenormal < 0 = Branch r (Plane planeposition (negated planenormal)) l
    | otherwise = Branch l (Plane planeposition planenormal) r

intersections :: Ray -> BSPTree a -> Tree T a
intersections _ (Leaf a) = Leaf a
intersections
    ray@(Ray rayposition raydirection)
    (Branch l plane@(Plane _ planenormal) r) =
        Branch
            (intersections ray l)
            (distance rayposition plane / raydirection `dot` planenormal)
            (intersections ray r)

hits :: T -> Tree T a -> [a]
hits _ (Leaf a) = [a]
hits t (Branch l t' r)
    | t < t'    = hits t l ++ hits t' r
    | otherwise = hits t r

box :: BSPTree (Maybe Color)
box = Branch (Leaf Nothing) (Plane (V3 (-1) 0 0) (V3 1 0 0)) (
      Branch (Leaf Nothing) (Plane (V3 1 0 0) (V3 (-1) 0 0)) (
      Branch (Leaf Nothing) (Plane (V3 0 (-1) 0) (V3 0 1 0)) (
      Branch (Leaf Nothing) (Plane (V3 0 1 0) (V3 0 (-1) 0)) (
      Branch (Leaf Nothing) (Plane (V3 0 0 (-1)) (V3 0 0 1)) (
      Branch (Leaf Nothing) (Plane (V3 0 0 1) (V3 0 0 (-1))) (
      Leaf (Just red)))))))

trace :: Ray -> BSPTree (Maybe Color) -> Maybe Color
trace ray = listToMaybe . catMaybes . hits 0 . intersections ray . align ray

testCamera :: Camera
testCamera = Camera (V3 50 50 50) (-1) 0

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



