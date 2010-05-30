module Stack
    ( Stack
    , listToStack
    , rotateL
    , rotateR
    , focus
    , addW
    , removeW
    , getActive
    ) where

import Graphics.Win32.GDI.Types
import Data.List (delete)

data Stack = W [HWND] [HWND] deriving Show

------------------------------------------------------------
-- create

listToStack list = W [] list

------------------------------------------------------------
-- operation

rotateL (W l []) = W [] (reverse l)
rotateL (W l r)  = W (head r : l) (tail r)
rotateR (W [] r) = W (reverse r) []
rotateR (W l r)  = W (tail l) (head l : r)

focus :: Stack -> HWND -> Stack
focus (W l r) hwnd
  | elem hwnd l = W (delete hwnd l) (hwnd : r)
  | elem hwnd r = W l (hwnd : delete hwnd r)
  | otherwise = W l r

addW :: Stack -> HWND -> Stack
addW (W l r) hwnd = W [] (hwnd : (reverse l) ++ r)

removeW :: Stack -> HWND -> Stack
removeW (W l r) hwnd
  | elem hwnd l = W (delete hwnd l) r
  | elem hwnd r = W l (delete hwnd r)
  | otherwise = W l r

------------------------------------------------------------
-- get
getActive (W _ (r:_)) = r
getActive _ = error "no window"
