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
import Control.Monad (mplus, filterM)
import Data.List (delete)

import Windows

mOD_ALT = 1
mOD_CONTROL = 2
mOD_SHIFT = 4
mOD_WIN = 8
wM_HOTKEY = 0x312

foreign import stdcall unsafe "windows.h SetForegroundWindow"
  setForegroundWindow :: HWND -> IO HWND

main :: IO ()
main = do
  let clsName = mkClassName "My Window Class"
  hinst <- getModuleHandle Nothing
  _     <- registerClass (cS_DBLCLKS, hinst, Nothing, Nothing, Nothing, Nothing, clsName)
  shi   <- registerWindowMessage "SHELLHOOK" -- registerShellHookWindowのメッセージがながれるようにする

  t <- makeClock
  (shellSink, shellEvent) <- makeEvent t
  (keySink, keyEvent) <- makeEvent t

  mainhwnd <- createWindow 
    clsName
    "HWM"
    wS_THICKFRAME
    Nothing 
    Nothing 
    Nothing 
    Nothing 
    Nothing 
    Nothing 
    hinst
    (wndProc shi shellSink keySink)

  -- registerHotKey mainhwnd 0xAFAF mOD_CONTROL vK_LEFT
  registerHotKey mainhwnd 0x10 3 0x41 -- Alt+Ctrl+a
  registerHotKey mainhwnd 0x20 3 0x42 -- Alt+Ctrl+b

  -- shellhookの登録に成功したら実行
  whenM (registerShellHookWindow mainhwnd) $ do
    hwnds <- enumWindows >>= filterM filterHWND
    mapM_ printHWNDInfo hwnds

    -- forkIO $ adaptE $ putStrLn <$> (fmap show shellEvent) `mplus` (fmap show keyEvent)
    forkIO $ adaptE $ action <$> (scanlE aux (W [] hwnds) $ (fmap convShell shellEvent) `mplus` (fmap convKey keyEvent))
    allocaMessage pump

convShell = snd
convKey = id
aux (W l []) 0x10 = W [] (reverse l)
aux (W l r) 0x10 = W (head r : l) (tail r)
aux (W [] r) 0x20 = W (reverse r) []
aux (W l r) 0x20 = W (tail l) (head l : r)
aux hwnds 2 = hwnds
aux hwnds 4 = hwnds
aux hwnds 6 = hwnds
aux hwnds _ = hwnds
action hwnds@(W _ (r:_)) = print hwnds >> setForegroundWindow r >>= print >> return ()
action hwnds = print hwnds

data Windows = W [HWND] [HWND] deriving Show

filterHWND hwnd = do
  title <- getWindowText hwnd
  return $ title /= "" && title /= "Program Manager"
  

pump :: LPMSG -> IO ()
pump lpmsg = whenM (getMessage lpmsg Nothing) $ do
  _ <- translateMessage lpmsg
  _ <- dispatchMessage lpmsg
  pump lpmsg

-- wndProc :: WindowMessage -> HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc shellhookid shellSink keySink hwnd msg wp lp
  | msg == wM_HOTKEY = keyProc
  | shellhookid /= msg = defWindowProc (Just hwnd) msg wp lp
  | otherwise = wndProc' -- registerWindowMessageで登録した値を比較し、shellhook関係か確認
  where
    keyProc = do
      keySink wp
      return 0
    wndProc' = do
      shellSink (lp,wp) -- '4'はアクティブウィンドウが切り替わったとき
      return 0

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
