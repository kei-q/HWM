#include "enumWindows.h"
#include <stdio.h>

static HWND hwnds[128];
static int size;

BOOL CALLBACK callback(HWND,LPARAM);

HWND * enumWindows(void)
{
  size = 0;
  EnumWindows(callback, 0);
  
  return &hwnds[0];
}

int hwndLength(void)
{
  return size;
}

BOOL CALLBACK callback(HWND hwnd, LPARAM lParam)
{
  if (IsWindowVisible(hwnd) == TRUE
      && IsWindow(hwnd) == TRUE
      && hwnd != FindWindow("Shell TrayWnd", "")) {
    hwnds[size++] = hwnd;
  }

  return 1;
}
