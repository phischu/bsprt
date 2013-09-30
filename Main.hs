module Main where

import Graphics.Gloss.Raster.Field (animateField,Display(InWindow),red)

import Linear (V3,dot,negated,(^-^))

type Point = V3 Float
type Direction = V3 Float
type Normal = V3 Float
type T = Float

data Tree b a = Leaf a | Branch (Tree b a) b (Tree b a)

data Plane = Plane Point Normal

data Ray = Ray Point Direction

type BSPTree a = Tree Plane a

distance :: Point -> Plane -> Float
distance point (Plane planeposition planenormal) = (point ^-^ planeposition) `dot` planenormal

between :: Plane -> Plane -> a -> BSPTree a
between = undefined

align :: Ray -> BSPTree a -> BSPTree a
align ray@(Ray _ raydirection) (Branch l (Plane planeposition planenormal) r)
    | raydirection `dot` planenormal < 0 = Branch r (Plane planeposition (negated planenormal)) l
    | otherwise = Branch l (Plane planeposition planenormal) r

intersections :: Ray -> BSPTree a -> Tree T a
intersections _ (Leaf a) = Leaf a
intersections
    (Ray rayposition raydirection)
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

main :: IO ()
main = animateField
    (InWindow "BSPRT" (600,600) (300,300))
    (1,1)
    (const (const red))



