import Line
import qualified Transform as T
import System.IO
import System.Environment
import Control.Monad.State

import qualified Data.Map.Strict as M

type DrawMats = (T.Transform Double, [Vect Double])

noArgs  = ["ident", "apply", "display", "save"]

main = do
    args <- getArgs
    script <- openFile (head args) ReadMode
    putStrLn "oof"
    scriptLines <- hGetContents script
    hClose script

display :: State DrawMats (Maybe DrawAction)
display = do
    (_, edges) <- get
    return (Just (T.drawEdges red edges . (\_ -> M.empty)))

line :: Line Double -> State DrawMats (Maybe DrawAction)
line ln = do
    (tform, edges) <- get
    put (tform, addLine ln edges)
    return Nothing

ident :: State DrawMats (Maybe DrawAction)
ident = do
    (_, edges) <- get
    put (T.ident, edges)
    return Nothing

scale :: Double -> Double -> Double -> State DrawMats (Maybe DrawAction)
scale x y z = do
    (tform, edges) <- get
    put (T.scale x y z <> tform, edges)
    return Nothing

move :: Double -> Double -> Double ->  State DrawMats (Maybe DrawAction)
move x y z = do
    (tform, edges) <- get
    put (T.trans x y z <> tform, edges)
    return Nothing

clear :: State DrawMats (Maybe DrawAction)
clear = do
    (tform, _) <- get
    put (tform, [])
    return Nothing

apply :: State DrawMats (Maybe DrawAction)
apply = do
    (tform, edges) <- get
    put (tform, T.mmult tform edges)
    return Nothing
