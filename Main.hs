{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternGuards #-}
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
import Data.List (find)

import Windows
import qualified Stack as S

mOD_ALT = 1
mOD_CONTROL = 2
mOD_SHIFT = 4
mOD_WIN = 8
wM_HOTKEY = 0x312

-- (id, mod, key)
keys = [ (0x10,3,0x41) -- Ctrl + Shift + A
       , (0x20,3,0x42) -- Ctrl + Shift + B
       ]

maps = [ (0x10,S.rotateL)
       , (0x20,S.rotateR)
       ]

main :: IO ()
main = do
  t <- makeClock
  (shellSink, shellEvent) <- makeEvent t
  (keySink  , keyEvent)   <- makeEvent t

  shi <- registerWindowMessage "SHELLHOOK" 

  mainhwnd <- simpleInitWindow "HWM" (wndProc shi shellSink keySink)

  registerHotKeys mainhwnd
  let shellE = fmap convShell shellEvent
  let keyE = fmap (convKey maps) keyEvent

  whenM (registerShellHookWindow mainhwnd) $ do
    runMainThread $ shellE `mplus` keyE
    allocaMessage pump
  where
    convKey maps keyID
      | Just (_,f) <- find ((==keyID).fst) maps = f
      | otherwise = id

convShell (4,hwnd) = flip S.focus  $ hwnd
convShell (2,hwnd) = flip S.add    $ hwnd
convShell (1,hwnd) = flip S.remove $ hwnd
convShell _ = id

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
    shellProc = whenM0 (isWindowVisible $ convLPARAM2HWND lp) $ do
                  nilAct $ shellSink (wp .&. 0x7fff, hwnd)
      where convLPARAM2HWND = castUINTToPtr . toEnum . fromEnum 

runMainThread e = do
  hwnds <- enumWindows
  forkIO $ adaptE $ action <$> scanlE (flip S.updateStack) (S.newWM hwnds) e
  where
    action wm = print wm >> setForegroundWindow (S.getActive $ S.getStack wm) >> return ()

------------------------------------------------------------
-- util
whenMmain :: Monad m => a -> m Bool -> m a -> m a
whenMmain ret test act = test >>= \t -> if t then act else return ret

whenM :: Monad m => m Bool -> m () -> m ()
whenM = whenMmain ()

whenM0 :: Monad m => m Bool -> m LRESULT -> m LRESULT
whenM0 = whenMmain 0

nilAct act = act >> return 0

curry3 f (a,b,c) = f a b c
