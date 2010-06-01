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
import Foreign.Ptr (plusPtr, nullPtr)
import Data.Bits ((.&.))
import Data.Char (ord)

import Windows
import Stack

mOD_ALT = 1
mOD_CONTROL = 2
mOD_SHIFT = 4
mOD_WIN = 8
wM_HOTKEY = 0x312

-- (id, mod, key)
keys = [ (0x10,3,0x41) -- Ctrl + Shift + A
       , (0x20,3,0x42) -- Ctrl + Shift + B
       ]

main :: IO ()
main = do
  t <- makeClock
  (shellSink, shellEvent) <- makeEvent t
  (keySink  , keyEvent)   <- makeEvent t

  shi <- registerWindowMessage "SHELLHOOK" 

  mainhwnd <- simpleInitWindow "HWM" (wndProc shi shellSink keySink)

  registerHotKeys mainhwnd

  whenM (registerShellHookWindow mainhwnd) $ do
    hwnds <- enumWindows
    forkIO $ adaptE $ action <$> mainEvent hwnds shellEvent keyEvent
    allocaMessage pump

registerHotKeys :: HWND -> IO ()
registerHotKeys hwnd = forM_ keys (curry3 $ registerHotKey hwnd)

wndProc shellhookid shellSink keySink hwnd msg wp lp
  | msg == wM_HOTKEY   = keyProc
  | msg == shellhookid = shellProc
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

mainEvent hwnds shellEvent keyEvent = (scanlE aux (newWM hwnds) $ (fmap convShell shellEvent) `mplus` (fmap convKey keyEvent))

convShell (4,hwnd) = flip focus  $ hwnd
convShell (2,hwnd) = flip add    $ hwnd
convShell (1,hwnd) = flip remove $ hwnd
convShell _ = id
convKey 0x10 = rotateL
convKey 0x20 = rotateR

aux w f = WM { wmStack = f (wmStack w) }

action wm = print wm >> setForegroundWindow (getActive $ wmStack wm) >> return ()

------------------------------------------------------------
-- util
whenM :: Monad m => m Bool -> m () -> m ()
whenM test act = test >>= \t -> if t then act else return ()

nilAct act = act >> return 0

curry3 f (a,b,c) = f a b c
