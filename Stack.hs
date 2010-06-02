module Stack
    ( WindowManager()
    , calcLayout
    , newWM
    , updateStack
    , updateMasterSize
    , changeMaster
    , getStack
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
    , masterSize :: Double -- [0..1]
    } deriving Show

data Stack = W [HWND] [HWND] deriving Show

-- TODO subtract Shell_TrayWnd area from display area
drawArea = (0,0,1870,1080)

calcLayout wm
  = let (W l r) = wmStack wm
        (master:subs) = reverse l ++ r
        rate = masterSize wm
        (x,y,w,h) = drawArea
    in (master, (x,y,w*rate,h))
       : map (\(n,hwnd) -> (hwnd, (w*rate,(h `div` length subs)*n,w*(1-rate),(h `div` length subs)))) (zip [0..] subs)

------------------------------------------------------------
-- create

newWM list = WM { wmStack = listToStack list, masterSize = 0.5 }

listToStack list = W [] list

------------------------------------------------------------
-- operation

updateStack f wm = wm { wmStack = f (wmStack wm) }
updateMasterSize f wm = wm { masterSize = f (masterSize wm) }

changeMaster (W ls (w:rs)) = W (ls ++ [w]) rs

getStack = wmStack

rotateL (W l []) = W [] (reverse l)
rotateL (W l r)  = W (head r : l) (tail r)
rotateR (W [] r) = W (reverse r) []
rotateR (W l r)  = W (tail l) (head l : r)

focus :: HWND -> Stack -> Stack
focus hwnd (W l r)
  | elem hwnd l = W (delete hwnd l) (hwnd : r)
  | elem hwnd r = W l (hwnd : delete hwnd r)
  | otherwise = W l r

add :: HWND -> Stack -> Stack
add hwnd (W l r) = W [] (hwnd : (reverse l) ++ r)

remove :: HWND -> Stack -> Stack
remove hwnd (W l r)
  | elem hwnd l = W (delete hwnd l) r
  | elem hwnd r = W l (delete hwnd r)
  | otherwise = W l r

------------------------------------------------------------
-- get
getActive (W _ (r:_)) = r
getActive _ = error "no window"
