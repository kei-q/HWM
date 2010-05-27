{-# LANGUAGE ForeignFunctionInterface #-}
module Windows
    ( registerShellHookWindow
    , isWindowVisible
    , isWindow
    , registerHotKey
    , getWindowText
    , enumWindows
    ) where

import Graphics.Win32.GDI.Types 
import System.Win32.Types
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)

foreign import stdcall unsafe "windows.h RegisterShellHookWindow"
  registerShellHookWindow :: HWND -> IO BOOL

foreign import stdcall unsafe "windows.h IsWindowVisible"
  isWindowVisible :: HWND -> IO BOOL

foreign import stdcall unsafe "windows.h IsWindow"
  isWindow :: HWND -> IO BOOL

foreign import stdcall unsafe "windows.h RegisterHotKey"
  registerHotKey :: HWND -> INT -> UINT -> UINT -> IO BOOL

------------------------------------------------------------
-- getWindowText
foreign import stdcall unsafe "windows.h GetWindowTextW"
  c_getWindowText :: HWND -> LPCTSTR -> INT -> IO INT

getWindowText :: HWND -> IO String
getWindowText hwnd = do
  withTString (replicate 256 '\0') $ \buf -> do
    c_getWindowText hwnd buf 256
    peekTString buf

------------------------------------------------------------
-- enumWindows
foreign import ccall "enumWindows.h enumWindows"
  c_enumWindows :: IO (Ptr HWND)

foreign import ccall "enumWindows.h hwndLength"
  c_hwndLength :: IO Int

enumWindows :: IO [HWND]
enumWindows = do
  p_hwnds <- c_enumWindows
  hwndsLength <- c_hwndLength
  peekArray hwndsLength p_hwnds

