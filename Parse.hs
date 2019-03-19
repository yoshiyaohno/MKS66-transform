{-# LANGUAGE FlexibleContexts #-}
import Line
import qualified Transform as T

import System.Directory
import System.IO
import System.Environment
import Control.Monad.State
import System.Process
import qualified Data.Map.Strict as M

type DrawMats = (Screen, T.Transform Double, [Vect Double])

noArgs :: (MonadState DrawMats m, MonadIO m) => [(String, m ())]
noArgs = [ ("ident", ident)
         , ("apply", apply)
         , ("display", display)
         , ("clear", clear)
         ]

wArgs :: (MonadState DrawMats m, MonadIO m) => [(String, [String] -> m ())]
wArgs = [ ("save", save)
        , ("line", line)
        , ("scale", scale)
        , ("move", move)
        , ("rotate", rote)
        ]

main = do
    args <- getArgs
    script <- readFile (head args)
    let cmds = parse $ lines script :: [StateT DrawMats IO ()]
    runStateT (sequence_ cmds) (M.empty, T.ident, [])

parse :: (MonadState DrawMats m, MonadIO m) => [String] -> [m ()]
parse []  = []
parse [a] =
    case lookup a noArgs of
        Just c  -> [c]
        Nothing -> []
parse (a:b:xs) =
    case lookup a noArgs of
        Just c0 -> c0 : (parse (b:xs))
        Nothing -> 
            case lookup a wArgs of
                Just c1 -> (c1 $ words b) : (parse xs)
                Nothing -> parse xs

save :: (MonadState DrawMats m, MonadIO m) => [String] -> m ()
save args = do
    let path = head args
    modify $ \(s, t, e) -> (T.drawEdges red e M.empty, t, e)
    (scrn, _, _) <- get
    liftIO $ writeFile path (printPixels (500, 500) scrn)

display :: (MonadState DrawMats m, MonadIO m) => m ()
display = do
    modify $ \(s, t, e) -> (T.drawEdges red e M.empty, t, e)
    (scrn, _, _) <- get
    liftIO $ do
        (tempName, tempHandle) <- openTempFile "." "disp.ppm"
        hPutStr tempHandle (printPixels (500, 500) scrn)
        callProcess "display" [tempName]
        hClose tempHandle 
        removeFile tempName

line :: (MonadState DrawMats m) => [String] -> m ()
line args = do
    let [x0, y0, z0, x1, y1, z1] = map read args
        ln = Line (Vect x0 y0 z0 1) (Vect x1 y1 z1 1)
            in modify $ \(s, t, edges) -> (s, t, addLine ln edges)

ident :: (MonadState DrawMats m) => m ()
ident = modify $
    \(s, _, es) -> (s, T.ident, es)

scale :: (MonadState DrawMats m) => [String] -> m ()
scale args = modify $
    \(scrn, tform, edges) -> (scrn, T.scale x y z <> tform, edges)
    where [x, y, z] = map read args

rote :: (MonadState DrawMats m) => [String] -> m ()
rote s = modify $
    \(scrn, tform, edges) -> (scrn, roti s <> tform, edges)
    where roti args
            | axis == "x"   = T.rotX theta
            | axis == "y"   = T.rotY theta
            | axis == "z"   = T.rotZ theta
            where axis  = args !! 0
                  theta = read $ args !! 1

move :: (MonadState DrawMats m) => [String] -> m ()
move args = modify $
    \(scrn, tform, edges) -> (scrn, T.trans x y z <> tform, edges)
    where [x, y, z] = map read args

clear :: (MonadState DrawMats m) => m ()
clear = modify $
    \(scrn, tform, _) -> (scrn, tform, [])

apply :: (MonadState DrawMats m) => m ()
apply = modify $
    \(scrn, tform, edges) -> (scrn, tform, T.mmult tform edges)
