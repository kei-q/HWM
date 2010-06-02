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

toW = toEnum . fromEnum .ord
-- (id, mod, key)
keys = [ (0x10,3,toW 'J') 
       , (0x20,3,toW 'K') 
       , (0x30,3,0x0D) -- Enter
--       , (0x40,3,0x20) -- space
       , (0x50,3,toW 'H') 
       , (0x60,3,toW 'L') 
--       , (0x70,3,toW 'C') 
--       , (0x80,3,toW 'F') 
--       , (0x90,3,toW '1') 
--       , (0xa0,3,toW '2') 
       ]

maps = [ (0x10,S.updateStack S.rotateL)
       , (0x20,S.updateStack S.rotateR)
       , (0x30,S.updateStack S.changeMaster)
       , (0x50,S.updateMasterSize (subtract 0.05))
       , (0x60,S.updateMasterSize (+ 0.05))
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

convShell (4,hwnd) = S.updateStack (S.focus  hwnd)
convShell (2,hwnd) = S.updateStack (S.add    hwnd)
convShell (1,hwnd) = S.updateStack (S.remove hwnd)
convShell _ = id

registerHotKeys :: HWND -> IO ()
registerHotKeys hwnd = forM_ keys (curry3 $ registerHotKey hwnd)

wndProc shellhookid shellSink keySink hwnd msg wp lp
  | msg == wM_HOTKEY   = keyProc
  | msg == shellhookid = shellProc
  | otherwise = defWindowProc (Just hwnd) msg wp lp
  where
    -- wp -> keyID
    keyProc = keySink wp >> return 0
    -- lp -> message
    -- wp -> HWND
    shellProc = whenM0 (isWindowVisible $ convLPARAM2HWND lp) $ do
                  shellSink (wp .&. 0x7fff, hwnd) >> return 0
      where convLPARAM2HWND = castUINTToPtr . toEnum . fromEnum 

runMainThread e = do
  hwnds <- enumWindows
  forkIO $ adaptE $ action <$> scanlE (\a b -> b a) (S.newWM hwnds) e
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

curry3 f (a,b,c) = f a b c
