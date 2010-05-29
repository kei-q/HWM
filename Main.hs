{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import System.Win32.Types
import System.Win32.DLL
import Graphics.Win32

import Control.Monad (when, forM_)

import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Concurrent (forkIO)
import Control.Applicative ((<$>))
import Control.Monad (mplus)
import Data.List (delete)
import Foreign.Ptr (plusPtr, nullPtr)
import Data.Bits ((.&.))
import Windows

mOD_ALT = 1
mOD_CONTROL = 2
mOD_SHIFT = 4
mOD_WIN = 8
wM_HOTKEY = 0x312

main :: IO ()
main = do
  t <- makeClock
  (shellSink, shellEvent) <- makeEvent t
  (keySink, keyEvent)     <- makeEvent t
  
  shi <- registerWindowMessage "SHELLHOOK" -- registerShellHookWindowのメッセージがながれるようにする

  mainhwnd <- simpleInitWindow "HWM" (wndProc shi shellSink keySink)

  registerHotKeys mainhwnd

  -- shellhookの登録に成功したら実行
  whenM (registerShellHookWindow mainhwnd) $ do
    hwnds <- enumWindows
    mapM printHWNDInfo hwnds

    forkIO $ adaptE $ action <$> (scanlE aux (W [] hwnds) $ (fmap convShell shellEvent) `mplus` (fmap convKey keyEvent))
    allocaMessage pump

registerHotKeys :: HWND -> IO ()
registerHotKeys hwnd = do
  -- registerHotKey mainhwnd 0xAFAF mOD_CONTROL vK_LEFT
  registerHotKey hwnd 0x10 3 0x41 -- Alt+Ctrl+a
  registerHotKey hwnd 0x20 3 0x42 -- Alt+Ctrl+b
  return ()
  

convShell (4,hwnd) = flip focus $ hwnd
convShell (2,hwnd) = flip addW $ hwnd
convShell (1,hwnd) = flip removeW $  hwnd
convShell _ = id
convKey 0x10 = rotateL
convKey 0x20 = rotateR

aux w f = f w

rotateL (W l []) = W [] (reverse l)
rotateL (W l r)  = W (head r : l) (tail r)
rotateR (W [] r) = W (reverse r) []
rotateR (W l r)  = W (tail l) (head l : r)

focus :: Windows -> HWND -> Windows
focus (W l r) hwnd
  | elem hwnd l = W (delete hwnd l) (hwnd : r)
  | elem hwnd r = W l (hwnd : delete hwnd r)
  | otherwise = W l r

addW :: Windows -> HWND -> Windows
addW (W l r) hwnd = W [] (hwnd : (reverse l) ++ r)

removeW :: Windows -> HWND -> Windows
removeW (W l r) hwnd
  | elem hwnd l = W (delete hwnd l) r
  | elem hwnd r = W l (delete hwnd r)
  | otherwise = W l r

action hwnds@(W _ (r:_)) = print hwnds >> setForegroundWindow r >> return ()
action hwnds = print hwnds

data Windows = W [HWND] [HWND] deriving Show

-- wndProc :: WindowMessage -> HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc shellhookid shellSink keySink hwnd msg wp lp
  | msg == wM_HOTKEY   = keyProc
  | msg == shellhookid = shellProc -- registerWindowMessageで登録した値を比較し、shellhook関係か確認 
  | otherwise = defWindowProc (Just hwnd) msg wp lp
  where
    -- wp -> keyID
    keyProc = nilAct $ keySink wp
    -- lp -> message
    -- wp -> HWND
    shellProc = let hwnd = castUINTToPtr $ toEnum $ fromEnum lp
                in do b <- (isWindowVisible hwnd)
		      if b
		        then nilAct $ shellSink (wp .&. 0x7fff, hwnd)
		        else return 0

printHWNDInfo :: HWND -> IO ()
printHWNDInfo hwnd = do
  rect <- getWindowRect hwnd
  print rect
  getWindowText hwnd >>= print
  print hwnd


------------------------------------------------------------
-- util
whenM :: Monad m => m Bool -> m () -> m ()
whenM test act = test >>= \t -> if t then act else return ()

nilAct act = act >> return 0
