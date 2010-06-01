module Stack
    ( WindowManager(WM)
    , wmStack
    , newWM
    , rotateL
    , rotateR
    , focus
    , add
    , remove
    , getActive
    ) where

import Graphics.Win32.GDI.Types
import Data.List (delete)

data WindowManager = WM
    { wmStack :: Stack
    } deriving Show

data Stack = W [HWND] [HWND] deriving Show

------------------------------------------------------------
-- create

newWM list = WM { wmStack = listToStack list }

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

add :: Stack -> HWND -> Stack
add (W l r) hwnd = W [] (hwnd : (reverse l) ++ r)

remove :: Stack -> HWND -> Stack
remove (W l r) hwnd
  | elem hwnd l = W (delete hwnd l) r
  | elem hwnd r = W l (delete hwnd r)
  | otherwise = W l r

------------------------------------------------------------
-- get
getActive (W _ (r:_)) = r
getActive _ = error "no window"
