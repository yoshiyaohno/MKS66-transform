module Transform where

import           Line
import           Control.Applicative
import qualified Data.Set        as S
import qualified Data.List       as L
import qualified Data.Map.Strict as M

type Transform a = Vect (Vect a)

ident :: (Num a) => Transform a
ident =
    Vect (Vect 1 0 0 0)
         (Vect 0 1 0 0)
         (Vect 0 0 1 0)
         (Vect 0 0 0 1)

scale :: (Num a) => a -> a -> a -> Transform a
scale x y z =
    Vect (Vect x 0 0 0)
         (Vect 0 y 0 0)
         (Vect 0 0 z 0)
         (Vect 0 0 0 1)

trans :: (Num a) => a -> a -> a -> Transform a
trans x y z =
    Vect (Vect 1 0 0 x)
         (Vect 0 1 0 y)
         (Vect 0 0 1 z)
         (Vect 0 0 0 1)

drawEdges :: (RealFrac a) => Color -> [Vect a] -> Screen -> Screen
drawEdges c edges = mconcat . map (drawLine c . uncurry Line) $ (pairOff redges)
    where redges = map (fmap round) edges

mmult :: (Num a, Functor f) => Transform a -> f (Vect a) -> f (Vect a)
mmult t = fmap (pmult t)

pmult :: (Num a) => Transform a -> Vect a -> Vect a
pmult t = liftA2 dot t . pure

dot :: (Num a) => Vect a -> Vect a -> a
dot p = sum . liftA2 (*) p
