{-# LANGUAGE FlexibleContexts #-}
import Line
import qualified Transform as T
import System.IO
import System.Environment
import Control.Monad.State

import qualified Data.Map.Strict as M

type DrawMats = (Screen, T.Transform Double, [Vect Double])

noArgs  = ["ident", "apply", "display", "save"]

main = do
    args <- getArgs
    script <- readFile (head args)
    putStrLn "oof"

parse :: (MonadState DrawMats m, MonadIO m) => [String] -> [m ()]
parse (a:b:xs)
    | a `elem` noArgs = 

save :: (MonadState DrawMats m, MonadIO m) => String -> m ()
save path = do
    (scrn, _, _) <- get
    display red
    liftIO $ writeFile path (printPixels (500, 500) scrn)

display :: (MonadState DrawMats m) => Color -> m ()
display color = modify $
    \(scrn, tform, edges) ->
        (T.drawEdges color edges M.empty, tform, edges)

line :: (MonadState DrawMats m) => Line Double -> m ()
line ln = modify $
    \(s, t, edges) -> (s, t, addLine ln edges)

ident :: (MonadState DrawMats m) => m ()
ident = modify $
    \(s, _, es) -> (s, T.ident, es)

scale :: (MonadState DrawMats m) => Double -> Double -> Double -> m ()
scale x y z = modify $
    \(scrn, tform, edges) -> (scrn, T.scale x y z <> tform, edges)

move :: (MonadState DrawMats m) => Double -> Double -> Double -> m ()
move x y z = modify $
    \(scrn, tform, edges) -> (scrn, T.trans x y z <> tform, edges)

clear :: (MonadState DrawMats m) => m ()
clear = modify $
    \(scrn, tform, _) -> (scrn, tform, [])

apply :: (MonadState DrawMats m) => m ()
apply = modify $
    \(scrn, tform, edges) -> (scrn, tform, T.mmult tform edges)
