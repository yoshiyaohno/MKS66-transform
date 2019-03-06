module Transform where

import           Line
import           Control.Applicative
import qualified Data.Set        as S
import qualified Data.List       as L
import qualified Data.Map.Strict as M

type Transform a = Vect (Vect a)

drawEdges :: Color -> [Vect Int] -> Screen -> Screen
drawEdges c edges = mconcat . map (drawLine c . uncurry Line) $ (pairOff edges)

mmult :: (Num a, Functor f) => Transform a -> f (Vect a) -> f (Vect a)
mmult t = fmap (pmult t)

pmult :: (Num a) => Transform a -> Vect a -> Vect a
pmult t = liftA2 dot t . pure

dot :: (Num a) => Vect a -> Vect a -> a
dot p = sum . liftA2 (*) p
