{-# LANGUAGE ForeignFunctionInterface #-}
module Windows
    ( registerShellHookWindow
    , isWindowVisible
    , isWindow
    , registerHotKey
    , getWindowText
    , enumWindows
    , setForegroundWindow
    
    -- util
    , simpleInitWindow
    , pump
    ) where


import System.Win32.DLL
import Graphics.Win32
import Graphics.Win32.GDI.Types 
import System.Win32.Types
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)

import Control.Monad (filterM)

simpleInitWindow :: String -> WindowClosure -> IO HWND
simpleInitWindow title proc = do
  let clsName = mkClassName "My Window Class"
  hinst <- getModuleHandle Nothing
  _     <- registerClass (cS_DBLCLKS, hinst, Nothing, Nothing, Nothing, Nothing, clsName)

  createWindow 
    clsName
    title
    wS_THICKFRAME
    Nothing 
    Nothing 
    Nothing 
    Nothing 
    Nothing 
    Nothing 
    hinst
    proc

pump :: LPMSG -> IO ()
pump lpmsg = do
  res <- (getMessage lpmsg Nothing)
  if res
    then do
      _ <- translateMessage lpmsg
      _ <- dispatchMessage lpmsg
      pump lpmsg
    else return ()

------------------------------------------------------------
-- foreign imports
------------------------------------------------------------

foreign import stdcall unsafe "windows.h RegisterShellHookWindow"
  registerShellHookWindow :: HWND -> IO BOOL

foreign import stdcall unsafe "windows.h IsWindowVisible"
  isWindowVisible :: HWND -> IO BOOL

foreign import stdcall unsafe "windows.h IsWindow"
  isWindow :: HWND -> IO BOOL

foreign import stdcall unsafe "windows.h RegisterHotKey"
  registerHotKey :: HWND -> INT -> UINT -> UINT -> IO BOOL

foreign import stdcall unsafe "windows.h SetForegroundWindow"
  setForegroundWindow :: HWND -> IO HWND

----------------------------------------
-- getWindowText
foreign import stdcall unsafe "windows.h GetWindowTextW"
  c_getWindowText :: HWND -> LPCTSTR -> INT -> IO INT

getWindowText :: HWND -> IO String
getWindowText hwnd = do
  withTString (replicate 256 '\0') $ \buf -> do
    c_getWindowText hwnd buf 256
    peekTString buf

----------------------------------------
-- enumWindows
foreign import ccall "enumWindows.h enumWindows"
  c_enumWindows :: IO (Ptr HWND)

foreign import ccall "enumWindows.h hwndLength"
  c_hwndLength :: IO Int

enumWindows :: IO [HWND]
enumWindows = do
  p_hwnds <- c_enumWindows
  hwndsLength <- c_hwndLength
  hwnds <- peekArray hwndsLength p_hwnds
  filterM (\hwnd -> do
    title <- getWindowText hwnd
    return $ title /= "" && title /= "Program Manager") hwnds

