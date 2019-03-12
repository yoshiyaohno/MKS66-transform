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

display :: State DrawMats DrawAction 
display = do
    (_, edges) <- get
    return (T.drawEdges red edges . (\_ -> M.empty))

ident :: State DrawMats ()
ident = do
    (_, edges) <- get
    put (T.ident, edges)

apply :: State DrawMats ()
apply = do
    (tform, edges) <- get
    put (tform, T.mmult tform edges)

line :: Line Double -> State DrawMats ()
line ln = do
    (tform, edges) <- get
    put (tform, addLine ln edges)

scale :: Double -> Double -> Double -> State DrawMats ()
scale x y z = do
    (tform, edges) <- get
    put (T.scale x y z <> tform, edges)

move :: Double -> Double -> Double ->  State DrawMats ()
move x y z = do
    (tform, edges) <- get
    put (T.trans x y z <> tform, edges)

clear :: State DrawMats ()
clear = do
    (tform, _) <- get
    put (tform, [])
